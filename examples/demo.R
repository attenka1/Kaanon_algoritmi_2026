source('R/kaanoni.R')
# Pieni melodia, neljan tahdin jakso, lopussa taukoa.
n <- kaanoni_notes(0:7,rep(1,8),c(60,62,64,65,67,65,64,62))
teemat <- kaanoni_themes(kaanoni_input(n,end=16),bars=4)
asetukset <- kaanoni_options(voices=2, forms=c('I','R','RI'), factors=c(1,2),
  delays=c(1,2,4),transpositions=c(-12,-7,0,7,12),seconds=5,
  max_harmony_errors=1,max_results=5)
tulos <- kaanoni_generate(teemat,asetukset)
print(tulos)
if(length(tulos$results)) {
  kaanoni_write(tulos,'examples/demo.mid')
  kaanoni_write(tulos,'examples/demo.musicxml')
}
# Oma tiedosto:
# syote <- kaanoni_read('/absoluuttinen/polku/teemat.musicxml')
# teemat <- kaanoni_themes(syote,bars=4) # Yksi pitka teema: bars=NULL.
# tulos <- kaanoni_generate(teemat,kaanoni_options(repair=TRUE))
