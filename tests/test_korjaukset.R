# Testaa korjauslogiikka suorittamatta MIDI-vienteja tai koko hakua.
polku='KONTRAPUNKTIALGORITMI_4X_KPT_260624_Dorico_korjattu.R'
koodi=parse(polku)
e=new.env(parent=baseenv())
for(x in koodi) {
  if(is.call(x) && identical(x[[1]],as.name('<-')) &&
     identical(x[[2]],as.name('korjaa_31'))) eval(x,e)
}
nuotit=cbind(Track=2:5,Tick=0,Dur=1024,Channel=1:4,
             Pitch=c(60,61,62,67),Velo=64,PC=c(0,1,2,7))
a=e$korjaa_31(nuotit)
stopifnot(!identical(a[,5],nuotit[,5]),all(a[,7]==a[,5]%%12),
          identical(a,e$korjaa_31(a)),identical(a,e$korjaa_31(nuotit)))
# Paattyva nuotti ei soi uuden nuotin alkamishetkella.
b=nuotit
b[,2]=c(0,1024,1024,1024)
stopifnot(identical(b,e$korjaa_31(b)))
# Transpositio vaikuttaa yhdistelman savelluokkamaaraan.
stopifnot(length(unique(c(c(0,4,7),(c(0,4,7)+1)%%12)))==6)
# Muutettu joukko ratkaisee hyvaksynnan; korjaa kaikki saman sl:n nuotit.
m=cbind(Tick=c(0,0,0),PC=c(0,0,1),Pitch=c(60,72,61))
r=which(m[,1]==0 & m[,2]==0)
m[r,3]=m[r,3]-1
m[r,2]=m[r,3]%%12
stopifnot(identical(as.numeric(m[,3]),c(59,71,61)),all(m[,2]==m[,3]%%12))
cat('Korjausten testit OK\n')
