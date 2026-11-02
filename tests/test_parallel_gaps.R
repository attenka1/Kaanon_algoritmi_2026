source('R/kaanoni.R')
# Kuvan mukainen kvinttiketju: ylaaani A-G#-A-B-C-B-A-G#, ylapuolinen vastaaani +7.
p <- c(69,68,69,71,72,71,69,68)
a <- kaanoni_notes(0:7,rep(0.9479167,8),p)
b <- a;b$voice <- 2;b$pitch <- b$pitch+7
n <- rbind(a,b)
o <- kaanoni_options(max_harmony_errors=100,allow_weak_steps=TRUE)
e <- kaanoni_evaluate(n,o)
stopifnot(!e$valid,sum(e$errors$type=='parallel')==7)
# Sama myos oktaaveissa, kun aanten rytmit eivat ole taysin identtiset.
b$pitch <- a$pitch+12;b$duration[8] <- 0.8
stopifnot(any(kaanoni_evaluate(rbind(a,b),o)$errors$type=='parallel'))
# Pitka todellinen tauko katkaisee rinnakkaisliikkeen yhteyden.
a <- kaanoni_notes(c(0,2),c(0.9,0.9),c(60,62))
b <- a;b$voice <- 2;b$pitch <- b$pitch+7
stopifnot(!any(kaanoni_evaluate(rbind(a,b),o)$errors$type=='parallel'))
# Moniaanisen satsin muiden aanien note-off ei saa vaikuttaa parin kieltoon.
a <- kaanoni_notes(0:1,c(0.95,0.95),c(60,62))
b <- a;b$voice <- 2;b$pitch <- b$pitch+7
c <- kaanoni_notes(0,0.5,64,voice=3)
o$voices <- 3;o$ranges <- rep(list(c(36,96)),3)
stopifnot(any(kaanoni_evaluate(rbind(a,b,c),o)$errors$type=='parallel'))
cat('Rinnakkaiskvintit ja -oktaavit artikulaatiokatkojen yli OK\n')
