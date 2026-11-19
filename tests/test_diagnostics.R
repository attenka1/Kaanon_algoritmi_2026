source('R/kaanoni.R')
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),c(60,62,64,65))))
run <- function(...) kaanoni_generate(t,kaanoni_options(seconds=2,...))
a <- run(delays=0,transpositions=0)
stopifnot(a$diagnostics$counts['doubling']>0)
a <- run(delays=0,transpositions=7)
stopifnot(a$diagnostics$counts['parallel']>0,
  any(grepl('Rinnakkaiset',kaanoni_debug(a),fixed=TRUE)))
a <- run(delays=0,transpositions=1,forbid_parallels=FALSE)
stopifnot(a$diagnostics$counts['harmony_limit']>0)
a <- run(delays=20,transpositions=0)
stopifnot(a$diagnostics$counts['overlap']>0)
a <- run(delays=1,transpositions=24,ranges=list(c(36,70),c(36,70)))
stopifnot(a$diagnostics$counts['range']>0)
a <- run(delays=1,transpositions=0,min_sonorities=100,forbid_parallels=FALSE,max_harmony_errors=100)
stopifnot(a$diagnostics$counts['sonority']>0)
# Mittarit eivat vaita karsintalukuja kokonaisiksi ehdokkaiksi.
stopifnot(any(grepl('karsittuja haaroja',kaanoni_debug(a),fixed=TRUE)))
x <- kaanoni_input(kaanoni_notes(c(0:2,20:22),rep(1,6),rep(c(60,62,64),2)),end=40)
a <- kaanoni_phrase_chain(x,5,kaanoni_options(delays=0,transpositions=1,forbid_parallels=FALSE,seconds=2))
stopifnot(length(a$results)==0,nrow(a$diagnostics$phrase_attempts)>=1,
  a$diagnostics$phrase_attempts$found[1]==0,length(kaanoni_debug(a))>0)
# Esimerkit pysyvat rajattuina.
stopifnot(nrow(a$diagnostics$children[[1]]$examples)<=20)
cat('Hylkayssyyt ja tyhjan saekaanonihaun diagnostiikka OK\n')
# Pelkka sointujoukkovirhe erotetaan intervallivirheista.
flat <- kaanoni_input(kaanoni_notes(0:7,rep(1,8),rep(60,8)))
o <- kaanoni_options(delays=1,transpositions=4,allowed_sets=kaanoni_pc_key(60))
a <- kaanoni_generate(kaanoni_themes(flat),o)
stopifnot(a$diagnostics$counts['set']==1L,a$diagnostics$counts['harmony_limit']==0L,
          a$diagnostics$examples$reason[1]=='set')
