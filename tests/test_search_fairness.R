source('R/kaanoni.R')
# Lyhyt asteikko. Korjaustilan ei saa estaa myohempien ehjien ehdokkaiden loytymista.
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:7/2,rep(0.5,8),c(60,62,64,65,67,69,71,72))))
o <- kaanoni_options(delays=1:3,transpositions=c(-12,-7,0,7,12),
   seconds=3,max_results=100)
plain <- kaanoni_generate(t,o)
stopifnot(length(plain$results)>1)
o$repair <- TRUE
fixed <- kaanoni_generate(t,o)
hash <- function(x) paste(x$notes$voice,x$notes$onset,x$notes$pitch,collapse=';')
stopifnot(all(vapply(plain$results,hash,character(1)) %in%
              vapply(fixed$results,hash,character(1))))
stopifnot(fixed$unchanged_found==length(plain$results))
# Keskeytetty korjaushaku jatkuu seuraavasta tilasta, ei alusta.
x <- t[[1]]
a <- rbind(.variation(x),.variation(x,form='I',transposition=1,voice=2))
o$forbid_parallels <- FALSE
first <- .repair(a,o,proc.time()[3]+2,batch_nodes=1)
if(!first$evaluation$valid && first$status=='repair_limit') {
 next_part <- .repair(a,o,proc.time()[3]+2,resume=first$resume,batch_nodes=1)
 stopifnot(next_part$nodes==first$nodes+1)
}
cat('Asteikon hakujarjestyksen regressiotestit OK\n')
