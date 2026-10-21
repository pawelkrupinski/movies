// mongosh --quiet URI/db export.js  -> JSONL on stdout
const d=db;
const mv={};d.getCollection('movies').find({},{tmdbId:1,imdbId:1,filmwebUrl:1}).forEach(m=>mv[m._id]=m);
const raw={};d.getCollection('movie_slots').find({},{'slot.rawTitle':1,'slot.title':1}).forEach(s=>raw[s._id]=s.slot);
const tb={};d.getCollection('identity_traces').find({},{basis:1,film:1,rules:1,weighedFilm:1,vetoedBy:1}).forEach(t=>tb[t._id]=t);
const L={};
d.getCollection('identity_listings').find().forEach(v=>v.films.forEach(f=>{const m=f.movie;const k=v._id+'\u0001'+(m.rawTitle||m.title);
 if(!L[k]) L[k]={rt:m.runtimeMinutes,yr:m.releaseYear,yub:m.releaseYearUpperBound,ot:m.originalTitle,g:m.genres,dir:f.director,cast:(f.cast||[]).length,ex:f.externalIds,url:f.filmUrl,syn:(f.synopsis||'').slice(0,300),age:f.ageRating};}));
const need=new Set();const rows=[];
d.getCollection('screenings').find().forEach(s=>{const m=mv[s.filmId]||{};const sl=raw[s._id]||{};const venue=s.slotKey.split('␟')[0];
 const rt=sl.rawTitle||sl.title; const t=tb[s.listingKey]||{}; const li=L[venue+'\u0001'+rt]||L[venue+'\u0001'+sl.title]||{};
 const ds=s.showtimes.map(x=>x.dateTime.getTime()); const fmt=new Set(); s.showtimes.forEach(x=>(x.format||[]).forEach(f=>fmt.add(f)));
 if(m.tmdbId) need.add(String(m.tmdbId)); if(t.weighedFilm) need.add(String(t.weighedFilm));
 rows.push({v:venue,rt:rt,ti:sl.title,tmdb:m.tmdbId||null,imdb:m.imdbId||null,fw:m.filmwebUrl||null,basis:t.basis||null,rules:t.rules||[],veto:t.vetoedBy||null,wf:t.weighedFilm||null,
  n:ds.length,d0:Math.min(...ds),d1:Math.max(...ds),fmt:[...fmt],li});});
const F={};d.getCollection('tmdb_films').find({_id:{$in:[...need]}}).forEach(f=>{const r=f.record||{};const l=f.local||{};F[f._id]={t:r.title,ot:r.originalTitle,y:r.year,rt:r.runtime,dir:r.directors,rel:l.releases||null,rd:l.release_date||null,pop:r.popularity,alt:(r.alternativeTitles||[]).slice(0,5)};});
rows.forEach(r=>{r.f=r.tmdb?F[String(r.tmdb)]||null:null; r.wff=r.wf?F[String(r.wf)]||null:null; print(JSON.stringify(r));});
