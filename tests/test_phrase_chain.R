source('R/kaanoni.R')
# Kaksi viiden tahdin saetta: musiikki kestaa nelja neljasosaa, loppu on taukoa.
n <- kaanoni_notes(c(0:3,20:23),rep(1,8),c(rep(60,4),rep(62,4)))
x <- kaanoni_input(n,end=40)
o <- kaanoni_options(delays=1:2,transpositions=c(0,4),forbid_parallels=FALSE,
  max_harmony_errors=0,max_results=3,seconds=5)
r <- kaanoni_phrase_chain(x,bars=5,options=o)
stopifnot(length(r$results)>0,r$phrase_count==2)
for(result in r$results) {
 stopifnot(length(result$phrases)==2,result$phrases[[2]]$start==20,
   length(unique(result$settings$transposition[result$settings$voice==2]))==1,
   all(vapply(result$phrases,function(p)p$evaluation$harmony<=o$max_harmony_errors,logical(1))))
}
# Virheraja on per sae: kokonaismaara saa ylittaa yksittaisen sakeen rajan.
n <- kaanoni_notes(c(0:2,20:22),rep(1,6),rep(c(60,62,64),2))
x <- kaanoni_input(n,end=40)
o <- kaanoni_options(delays=0,transpositions=1,forbid_parallels=FALSE,
 max_harmony_errors=1,max_results=2,seconds=3)
r <- kaanoni_phrase_chain(x,5,o)
stopifnot(length(r$results)>0,r$results[[1]]$evaluation$harmony==2)
# MIDI ja XML sisaltavat kokonaisen sakeiden ketjun kahdella aanella.
folder <- tempfile();dir.create(folder)
for(ext in c('mid','musicxml')) {
 path <- file.path(folder,paste0('phrases.',ext));kaanoni_write(r,path)
 back <- kaanoni_read(path)
 stopifnot(length(unique(back$notes$voice))==2,nrow(back$notes)==12)
 if(ext=='musicxml') stopifnot(any(grepl('sae 2',readLines(path,warn=FALSE),fixed=TRUE)))
}
unlink(folder,recursive=TRUE)
# Jos yhteista transpositiota ei ole kaikille sakeille, ei muodosteta osittaista ketjua.
o$max_harmony_errors <- 0
stopifnot(length(kaanoni_phrase_chain(x,5,o)$results)==0)
cat('Saekaanonien yhteinen transpositio, jarjestys ja saevirherajat OK\n')
# Nouseva ja laskeva sae tarvitsevat eri vastaaanen transposition.
x <- kaanoni_input(kaanoni_notes(c(0:3,20:23),rep(1,8),c(60,62,64,66,66,64,62,60)),end=40)
o <- kaanoni_options(delays=1,transpositions=c(-2,2),allowed_intervals=0,
  max_harmony_errors=0,forbid_parallels=FALSE,max_results=3,seconds=3)
stopifnot(length(kaanoni_phrase_chain(x,5,o,same_transposition=TRUE)$results)==0)
r <- kaanoni_phrase_chain(x,5,o,same_transposition=FALSE)
stopifnot(length(r$results)>0)
q <- r$results[[1]]
stopifnot(identical(q$phrase_transpositions,c(2,-2)),!q$same_transposition,
  is.finite(q$boundary_movement),all(vapply(q$phrases,function(p)p$evaluation$harmony==0,logical(1))))
# Tasavertaisten ketjujen jarjestys suosii pienempaa saerajojen liiketta.
a <- q;b <- q;a$boundary_movement <- 10;b$boundary_movement <- 2
stopifnot(kaanoni_rank(list(a,b))[[1]]$boundary_movement==2)
cat('Saekohtaiset alkuintervallit ja saerajojen tasapistejarjestys OK\n')
