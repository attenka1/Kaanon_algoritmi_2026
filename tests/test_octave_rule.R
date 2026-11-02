source('R/kaanoni.R')
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),c(60,62,64,65))))
for(shift in c(-24,-12,0,12,24)) {
 n <- rbind(.variation(t[[1]]),.variation(t[[1]],voice=2,transposition=shift))
 o <- kaanoni_options(max_harmony_errors=100,forbid_parallels=FALSE,min_overlap=0)
 e <- kaanoni_evaluate(n,o)
 stopifnot(!e$valid,any(e$errors$type=='doubling'))
 # Eri sisääntulo merkitsee erillista kaanonääntä.
 n$onset[n$voice==2] <- n$onset[n$voice==2]+1
 stopifnot(!any(kaanoni_evaluate(n,o)$errors$type=='doubling'))
}
# Haussa kaksinnus ei voi tayttaa lisa-aanen paikkaa edes virheita sallittaessa.
r <- kaanoni_generate(t,kaanoni_options(delays=0,transpositions=c(-12,0,12),
  max_harmony_errors=100,forbid_parallels=FALSE,seconds=2))
stopifnot(length(r$results)==0)
# Muut samanaikaiset transpositiot eivat ole oktaavikaksinnuksia.
a <- .variation(t[[1]]);b <- .variation(t[[1]],transposition=4,voice=2)
stopifnot(!.same_octave_voice(a,b))
cat('Oktaavikaksinnus ei tayta erillisen teeman paikkaa OK\n')
