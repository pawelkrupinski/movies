#!/usr/bin/env bash
#
# Train the worker image's AOT cache on a replayed boot, and ship the image with it.
#
#   train-worker-aot.sh <untrained image> <shipped image> <fixtures root> <fixture directory> <seconds>
#
# WHY A REPLAYED BOOT. The Dockerfile's class-loading training archives the class list taken from
# production heap dumps. A run that EXECUTES the worker also archives what only running creates —
# ~15.9k classes against ~13.7k, lambdas and generated classes among them — so they no longer load
# into metaspace at every restart: 6–11 MB less non-heap per worker (2026-10-02).
#
# WHY HERE AND NOT IN THE DOCKERFILE. The run needs a Mongo and the recorded corpus (~725 MB,
# outside the build context), and it must run on the image's OWN classpath — the JVM refuses a
# cache trained on any other — so it runs the built image itself, then layers the cache on top.
#
# HERMETIC BY CONSTRUCTION: the training container sits on an --internal network whose only other
# member is its Mongo. ReplayWorkerWiring replays every fetch seam from the corpus; anything else
# that reaches for the internet fails fast instead of reaching it.
set -euo pipefail

untrained=${1:?untrained image}
shipped=${2:?shipped image}
fixtures=${3:?fixtures root}
fixture_directory=${4:?fixture directory}
seconds=${5:?training seconds}

network=aot-training
mongo=aot-mongo
out=$(mktemp -d)
chmod 777 "$out"
cleanup() { docker rm -f "$mongo" >/dev/null 2>&1 || true; docker network rm "$network" >/dev/null 2>&1 || true; rm -rf "$out"; }
trap cleanup EXIT

started=$SECONDS
docker pull --quiet "$untrained" >/dev/null
docker network create --internal "$network" >/dev/null
docker run -d --name "$mongo" --network "$network" mongo:8.3.11 --replSet rs0 --bind_ip_all >/dev/null

mongosh_eval() { docker exec "$mongo" mongosh --quiet --eval "$1"; }
deadline=$((SECONDS + 120))
until mongosh_eval 'db.runCommand({ping:1})' >/dev/null 2>&1; do
    [ "$SECONDS" -lt "$deadline" ] || { echo "::error::training Mongo never answered"; docker logs --tail 50 "$mongo"; exit 1; }
    sleep 1
done
mongosh_eval "rs.initiate({_id:'rs0',members:[{_id:0,host:'$mongo:27017'}]})" >/dev/null
until mongosh_eval 'rs.status().myState' 2>/dev/null | grep -q '^1$'; do
    [ "$SECONDS" -lt "$deadline" ] || { echo "::error::training Mongo never became PRIMARY"; exit 1; }
    sleep 1
done
prepared=$SECONDS

# The image's launcher, not its CMD: the CMD appends -XX:AOTCache, and a run that both reads and
# writes a cache is not a training run. -Xmx512m as the Dockerfile's training: the compressed-pointer
# range every pod's heap is in. -XX:-AOTRecordTraining: classes only, NO method profiles — a profile
# of a replayed Polish boot saved no JIT or CPU in production and cost worker-es ~13% of its boot CPU
# (2026-10-02); the classes the replay adds to the archive are the win (6–11 MB less non-heap).
docker run --rm --network "$network" \
    -v "$fixtures:/fixtures:ro" -v "$out:/out" \
    -e MONGODB_URI="mongodb://$mongo:27017/?directConnection=true" -e MONGODB_DB=kinowo_aot_training \
    -e KINOWO_FIXTURE_ROOT=/fixtures \
    -e JAVA_OPTS="-Xmx512m -XX:+UnlockDiagnosticVMOptions -XX:-AOTRecordTraining -XX:AOTCacheOutput=/out/classes.aot" \
    --entrypoint bin/worker "$untrained" -main modules.AotTrainingMain /app/lib "$fixture_directory" "$seconds"
test -s "$out/classes.aot"
trained=$SECONDS

# The cache must map under the options it was trained with: -XX:AOTMode=on refuses to start otherwise.
docker run --rm -v "$out:/out:ro" -e JAVA_OPTS="-Xmx512m -XX:AOTMode=on -XX:AOTCache=/out/classes.aot" \
    --entrypoint bin/worker "$untrained" -main modules.AotTrainingMain check

printf 'FROM %s\nCOPY classes.aot /app/classes.aot\n' "$untrained" > "$out/Dockerfile"
docker build --quiet -t "$shipped" "$out" >/dev/null
docker push --quiet "$shipped" >/dev/null

echo "AOT training: $((prepared - started))s pull + Mongo, $((trained - prepared))s training, $((SECONDS - trained))s check + bake + push; cache $(du -h "$out/classes.aot" | cut -f1)"
