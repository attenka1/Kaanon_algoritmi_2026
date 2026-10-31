source('R/kaanoni.R')
# C–D–E vastaan E: epaiskun D/E-sekunti sallitaan vain option ollessa paalla.
a <- data.frame(onset=c(0,1,2,0),duration=c(1,1,1,3),pitch=c(60,62,64,64),voice=c(1,1,1,2))
o <- kaanoni_options(forbid_parallels=FALSE,min_overlap=0)
stopifnot(kaanoni_evaluate(a,o)$harmony==1)
o$allow_weak_steps <- TRUE
stopifnot(kaanoni_evaluate(a,o)$harmony==0,kaanoni_evaluate(a,o)$valid)
b <- a;b$pitch[1:3] <- c(64,62,60)
stopifnot(kaanoni_evaluate(b,o)$harmony==0)
# Kaantyminen, hyppy ja tauko eivat ole yhtenainen asteikkoliike.
b <- a;b$pitch[3] <- 60
stopifnot(kaanoni_evaluate(b,o)$harmony==1)
b <- a;b$pitch[1] <- 58
stopifnot(kaanoni_evaluate(b,o)$harmony>=1)
b <- a;b$duration[1] <- 0.5
stopifnot(kaanoni_evaluate(b,o)$harmony==1)
# Sama lomasavel vahvalla iskulla hylataan.
b <- a;b$onset[1:3] <- c(1,2,3);b$duration[4] <- 4
stopifnot(kaanoni_evaluate(b,o)$harmony==1)
# Epaiskulla alkava pitka nuotti ei saa ohittaa vahvaa iskua.
b <- a;b$duration[2] <- 2;b$onset[3] <- 3;b$duration[4] <- 4
stopifnot(kaanoni_evaluate(b,o)$harmony==1)
# Kahdeksasosien epaiskut: vahvojen neljasosaiskujen vali 1.
b <- a;b$onset <- b$onset/2;b$duration <- b$duration/2
o$strong_beat_period <- 1
stopifnot(kaanoni_evaluate(b,o)$valid)
# Kielto pysyy erillisena: asteikkoliikkeen rinnakkaiskvintit hylataan.
b <- rbind(a[1:3,],transform(a[1:3,],voice=2,pitch=pitch+7))
o$strong_beat_period <- 2;o$forbid_parallels <- TRUE
stopifnot(kaanoni_evaluate(b,o)$hard>0)
# Sama saanto toimii jokaiselle aaniparille myos 3–4 aanessa.
for(v in 3:4) {
  b <- a
  for(j in 3:v) b <- rbind(b,data.frame(onset=0,duration=3,pitch=if(j==3)60 else 72,voice=j))
  ov <- kaanoni_options(voices=v,allow_weak_steps=TRUE,forbid_parallels=FALSE,min_overlap=0)
  stopifnot(kaanoni_evaluate(b,ov)$valid)
}
cat('Epaiskujen asteikkoliikkeen testit OK\n')
# Kolmijakoinen metri: toinen isku myos seuraavassa tahdissa (t=4) on heikko.
a3 <- a;a3$onset[1:3] <- a3$onset[1:3]+3;a3$duration[4] <- 6
m3 <- data.frame(start=c(0,3),length=3,numerator=3,denominator=4)
o <- kaanoni_options(allow_weak_steps=TRUE,forbid_parallels=FALSE,min_overlap=0,meter=m3)
stopifnot(kaanoni_evaluate(a3,o)$valid)
o$metric_mode <- 'manual'
stopifnot(!kaanoni_evaluate(a3,o)$valid)
o$metric_mode <- 'auto'
# 3/8: sama saanto suhteutuu kahdeksasosaiskuihin.
b3 <- a3;b3$onset <- b3$onset/2;b3$duration <- b3$duration/2
o$meter <- transform(m3,start=start/2,length=length/2,denominator=8)
stopifnot(kaanoni_evaluate(b3,o)$valid)
# Kohotahti: t=0 on kolmannen iskun kohosavel, tayden tahdin vahva alku on t=1.
pickup <- data.frame(start=c(0,1),length=c(1,3),numerator=3,denominator=4)
strong <- .metric_strong_times(pickup,7)
stopifnot(!0 %in% strong,all(c(1,4,7) %in% strong))
# Tahtilajinvaihdos 4/4 -> 3/4; kolmannen tahdin alku on t=7.
change <- data.frame(start=c(0,4),length=c(4,3),numerator=c(4,3),denominator=4)
strong <- .metric_strong_times(change,11)
stopifnot(all(c(0,2,4,7,10) %in% strong),!6 %in% strong)
# Teemajako sailyttaa kunkin teeman oman metrin.
inp <- kaanoni_input(kaanoni_notes(c(0,4),c(1,1),c(60,62)),change,end=7)
themes <- kaanoni_themes(inp,bars=1)
stopifnot(themes[[2]]$measures$start==0,themes[[2]]$measures$numerator==3)
cat('Kolmijakoisen metrin, kohotahdin ja tahtilajinvaihdoksen testit OK\n')
