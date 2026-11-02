source('R/kaanoni.R')
a <- data.frame(onset=0:3,duration=1,pitch=c(60,62,64,65),voice=1)
b <- transform(a,pitch=pitch-12,voice=2)
c <- transform(b,pitch=pitch+12,voice=3)
d <- transform(a,onset=onset+1,voice=4)
n <- rbind(a,b,c,d)
# Aanet 1 ja 3 ovat tarkkoja unisonokopioita. Viiveella tuleva aani sailyy.
r <- kaanoni_export_voices(n)
stopifnot(length(unique(r$notes$voice))==3,r$mapping$representative[3]==1)
# Myos oktaavikahdennus voidaan kirjoittaa vain kerran.
r <- kaanoni_export_voices(n,octaves=TRUE)
stopifnot(length(unique(r$notes$voice))==2,identical(unique(r$notes$voice),1:2))
# Osittainen yhtalaisyys tai yksikin muuttunut savel ei oikeuta yhdistamista.
bad <- n;bad$pitch[bad$voice==3][4] <- 66
stopifnot(length(unique(kaanoni_export_voices(bad,TRUE)$notes$voice))==3)
# Molemmat tiedostomuodot sailyttavat itsenaiset aanet ja yhdistamiskartan.
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),rep(60,4))))
result <- kaanoni_generate(t,kaanoni_options(delays=0,transpositions=4,forbid_parallels=FALSE,seconds=2))
stopifnot(length(result$results)==1)
# Vanha hakutulos, jossa on oktaavikahdennus: vienti osaa edelleen yhdistaa sen.
q <- result$results[[1]]$notes
q$pitch[q$voice==2] <- q$pitch[q$voice==2]+8
result$results[[1]]$notes <- q
folder <- tempfile();dir.create(folder)
for(ext in c('mid','musicxml')) {
 path <- file.path(folder,paste0('merged.',ext))
 kaanoni_write(result,path,merge_octaves=TRUE)
 loaded <- kaanoni_read(path)
 saved <- readRDS(paste0(path,'.rds'))
 stopifnot(length(unique(loaded$notes$voice))==1,nrow(loaded$notes)==4,
   saved$results[[1]]$written_voices==1,nrow(saved$results[[1]]$notes)==8)
}
unlink(folder,recursive=TRUE)
cat('Unisonojen ja oktaavikahdennusten vientitestit OK\n')
