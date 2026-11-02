source('R/kaanoni.R')
# Toistuva savelkin lasketaan uutena nuottiparina, mutta pitka sointi vain kerran.
make <- function(onset,duration,pitch,voice) {
  data.frame(onset=onset,duration=duration,pitch=pitch,voice=voice)
}
long <- rbind(make(0,4,60,1),make(0,4,64,2))
o <- kaanoni_options(forbid_parallels=FALSE,min_sonorities=2)
e <- kaanoni_evaluate(long,o)
stopifnot(e$interval_count==1,!e$valid)
a <- rbind(make(0,4,60,1),make(0:3,1,64,2))
o$min_sonorities <- 4L
stopifnot(kaanoni_evaluate(a,o)$valid,kaanoni_evaluate(a,o)$sonority_count==4)
o$min_sonorities <- 5L
stopifnot(!kaanoni_evaluate(a,o)$valid)
# Kolmi- ja neliaanisessa lasketaan joko parit tai kaikkien aanten soinnut.
for(v in 3:4) {
  a <- do.call(rbind,lapply(seq_len(v),function(j) make(0:3,1,c(60,64,67,71)[j],j)))
  o <- kaanoni_options(voices=v,forbid_parallels=FALSE,allowed_intervals=0:11,
    min_sonorities=4,sonority_mode='chords')
  e <- kaanoni_evaluate(a,o)
  stopifnot(e$valid,e$chord_count==4,e$interval_count==4*choose(v,2))
  o$sonority_mode <- 'intervals';o$min_sonorities <- 4*choose(v,2)
  stopifnot(kaanoni_evaluate(a,o)$valid)
  o$min_sonorities <- o$min_sonorities+1
  stopifnot(!kaanoni_evaluate(a,o)$valid)
}
# Pelkka nuotin loppuminen ei kasvata jaljelle jaavan intervallin lukumaaraa.
a <- rbind(make(0,4,60,1),make(0,4,64,2),make(0,1,67,3))
o <- kaanoni_options(voices=3,forbid_parallels=FALSE,allowed_intervals=0:11)
e <- kaanoni_evaluate(a,o)
stopifnot(e$interval_count==3,e$chord_count==1)
# Generointi karsii liian vahaisen yhteissoinnin myos korjaustilassa.
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),rep(60,4))))
o <- kaanoni_options(delays=1,transpositions=0,min_sonorities=3,seconds=1)
stopifnot(length(kaanoni_generate(t,o)$results)==1)
o$min_sonorities <- 4L;o$repair <- TRUE
r <- kaanoni_generate(t,o)
stopifnot(length(r$results)==0,r$repair_attempted==0)
stopifnot(inherits(tryCatch(kaanoni_options(min_sonorities=1.5),error=identity),'error'))
cat('Yhteissointien lukumaaran testit OK\n')
