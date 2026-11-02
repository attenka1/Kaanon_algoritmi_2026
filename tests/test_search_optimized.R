source('R/kaanoni.R')
hash <- function(n) paste(n$voice,n$onset,n$duration,n$pitch,collapse=';')
# Vertailu: kaikki kokonaiset yhdistelmat suoraan, ilman pari- tai aikakarsintaa.
brute <- function(themes,o) {
  found <- character()
  walk <- function(n,root,voice) {
    if(voice>o$voices) {
      if(kaanoni_evaluate(n,o)$valid) found <<- union(found,hash(n))
      return()
    }
    for(t in if(o$mode=='canon') root else seq_along(themes))
      for(f in o$forms) for(a in o$factors) for(d in o$delays) for(p in o$transpositions) {
        x <- .variation(themes[[t]],f,a,d,p,voice)
        duplicate <- any(vapply(unique(n$voice),function(v) {
          q <- n[n$voice==v,]
          nrow(q)==nrow(x) && all(abs(q$onset-x$onset)<1e-8) &&
            all(abs(q$duration-x$duration)<1e-8) && all(q$pitch==x$pitch)
        },logical(1)))
        if(!duplicate) walk(rbind(n,x),root,voice+1L)
      }
  }
  for(t in seq_along(themes)) {
    o$meter <- themes[[t]]$measures
    walk(.variation(themes[[t]]),t,2L)
  }
  sort(found)
}
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),rep(60,4))))
for(v in 2:4) {
  o <- kaanoni_options(voices=v,delays=1:2,transpositions=c(0,7),forbid_parallels=FALSE,
      max_harmony_errors=5,seconds=20,max_results=10000)
  expected <- brute(t,o); actual <- kaanoni_generate(t,o)
  stopifnot(actual$status=='exhaustive',identical(sort(vapply(actual$results,function(x) hash(x$notes),character(1))),expected))
  if(v==4) stopifnot(actual$stats$pair_cache_hits>0,actual$stats$variant_cache_hits>0)
}
# Eri teemat, inversiot, epaiskuja salliva saanto ja kokonaisjoukkoraja.
t2 <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:7,rep(1,8),c(60,62,64,65,60,60,60,60))),bars=1)
for(sets in list(NULL,c(kaanoni_pc_key(c(60,64)),kaanoni_pc_key(60)))) {
  o <- kaanoni_options(voices=3,mode='counterpoint',forms='I',delays=1,
    transpositions=c(0,4),forbid_parallels=FALSE,allow_weak_steps=TRUE,
    max_harmony_errors=2,allowed_sets=sets,seconds=20,max_results=10000)
  expected <- brute(t2,o); actual <- kaanoni_generate(t2,o)
  stopifnot(actual$status=='exhaustive',identical(sort(vapply(actual$results,function(x) hash(x$notes),character(1))),expected))
}
# Aikakarsinta ja nopean haun erillinen lopetustila.
o <- kaanoni_options(voices=3,delays=c(1,2,20),transpositions=c(0,12),
  forbid_parallels=FALSE,stop_after_results=TRUE,max_results=1,seconds=3)
r <- kaanoni_generate(t,o)
stopifnot(r$status=='result_limit',r$found_total==1)
o$stop_after_results <- FALSE
r <- kaanoni_generate(t,o)
stopifnot(r$stats$temporal_rejects>0)
# Kaikki lahtoteemat saavat oman hakuvuoron ennen ensimmaisen loppuun tutkimista.
base <- t[[1]]
many <- lapply(1:5,function(i) {x<-base;x$name<-paste0('T',i);x$notes$source<-paste0(x$name,':',1:4);x})
o <- kaanoni_options(voices=4,forms=c('I','R','RI'),factors=c(0.5,1,2),
 delays=1:8,transpositions=-12:12,seconds=1,max_candidates=100000)
r <- kaanoni_generate(many,o)
stopifnot(length(r$stats$roots_visited)==5)
cat('Optimoitu haku vastaa suoraa hakua; vuorottelu ja pysaytys OK\n')
