const out=[];
db.screenings.find({}).forEach(s=>{const st=s.showtimes||[]; const k=st.filter(x=>(x.format||[]).some(f=>f.toLowerCase()=='omeu')).length;
 if(k>0){ const mv=db.movies.findOne({_id:s.filmId},{tmdbId:1,tmdbBasis:1,searchTitle:1});
  const lk=(s.listingKey||'').split('\u0000');
  const t=db.identity_traces.findOne({_id:s.listingKey},{film:1,basis:1});
  out.push({venue:lk[1], title:lk[2], year:lk[3]||null, director:lk[4]||null, slotKey:s.slotKey, filmId:s.filmId, tmdb:mv?mv.tmdbId:null, tmdbBasis:mv?mv.tmdbBasis:null, model:t?t.film:null, modelBasis:t?t.basis:null, n:st.length, k:k, fmts:[...new Set(st.flatMap(x=>x.format||[]))]});}});
print(JSON.stringify(out));
