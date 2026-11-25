"""THE LOCAL, PASSWORD-FREE MONGO the prod mirror, the it/ suites and the identity tools share (scripts/lib;
tool-conventions.sh's local_mongo_port / local_mongo_uri, own-mongod.sh's throwaway_replica_set_initiate): its port
(LOCAL_MIRROR_PORT, default 28017), its URI (a direct connection), whether something answers on a port, and the
mongosh expression that initiates a throwaway single-node replica set; and the prod mirror's database of a country."""
from __future__ import annotations

import socket
from typing import Mapping

DEFAULT_PORT = "28017"


def local_mongo_port(env: Mapping[str, str]) -> str:
    return env.get("LOCAL_MIRROR_PORT") or DEFAULT_PORT


def local_mongo_server(env: Mapping[str, str]) -> str:
    """The local Mongo's address as a bare URI (`mongodb://127.0.0.1:<port>`), a database path still to append."""
    return f"mongodb://127.0.0.1:{local_mongo_port(env)}"


def local_mongo_uri(env: Mapping[str, str], database: str = "") -> str:
    return f"{local_mongo_server(env)}/{database}?directConnection=true"


def mirror_database(cc: str) -> str:
    """The prod mirror's database of country `cc` (any case): Poland's is the unprefixed one, as in prod."""
    return "kinowo_prod_mirror" if cc.lower() == "pl" else f"kinowo_{cc.lower()}_prod_mirror"


def answers(port: str, host: str = "127.0.0.1", timeout: float = 2.0) -> bool:
    """Whether something accepts a TCP connection on `host`:`port` (the shell tools' `echo > /dev/tcp/...`)."""
    try:
        with socket.create_connection((host, int(port)), timeout=timeout):
            return True
    except (OSError, ValueError):
        return False


def throwaway_replica_set_initiate(member: str) -> str:
    """The mongosh expression initiating a throwaway single-node set rs0 on `member` (host:port), majority acknowledged
    on apply, not on a journal sync: a dropDatabase always waits for majority, and with the journal default on a single
    node's majority is its journal fsync, which a loaded Mac starves past that wait (scripts/own-mongod.sh)."""
    return ("rs.initiate({_id: 'rs0', writeConcernMajorityJournalDefault: false, members: [{_id: 0, host: '"
            + member + "'}]})")
