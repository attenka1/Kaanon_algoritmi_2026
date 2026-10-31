source('R/kaanoni.R')
check <- function(x) stopifnot(isTRUE(x))
throws <- function(expr) inherits(tryCatch(force(expr),error=identity),'error')
n <- kaanoni_notes(c(0,1,2),c(1,1,1),c(60,62,64))
input <- kaanoni_input(n,end=16)
themes <- kaanoni_themes(input,bars=4)
check(length(themes)==1 && themes[[1]]$span==16)
check(all(.variation(themes[[1]],'R')$onset==c(13,14,15)))
check(all(.variation(themes[[1]],'I')$pitch==c(60,58,56)))
check(all(.variation(themes[[1]],'RI')$pitch==c(56,58,60)))
check(all(.variation(themes[[1]],factor=0.5)$duration==0.5))
check(identical(kaanoni_options(forms=character())$forms,'O'))
check(throws(kaanoni_notes(c(0,0.5),c(1,1),c(60,62))))
n2 <- kaanoni_notes(c(0,16),c(1,1),c(60,62))
t2 <- kaanoni_themes(kaanoni_input(n2,end=32),bars=4)
check(length(t2)==2 && t2[[2]]$notes$onset==0)
# Nuotin paattyminen muuttaa harmoniaa, eika vanha nuotti soi rajalla.
a <- rbind(.variation(themes[[1]],voice=1),.variation(themes[[1]],delay=1,transposition=1,voice=2))
e <- kaanoni_evaluate(a,kaanoni_options())
check(any(e$errors$type=='harmony'))
# Pelkka pysyva kvintti ei ole rinnakkaisliike; samansuuntainen liike on.
x <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:1,c(1,1),c(60,62))))[[1]]
a <- rbind(.variation(x),.variation(x,transposition=-7,voice=2))
check(any(kaanoni_evaluate(a,kaanoni_options())$errors$type=='parallel'))
# Korjauksen pitaa sailyttaa yhteisen teeman vastaavuus myos inversiossa.
o <- kaanoni_options(repair=TRUE,forbid_parallels=FALSE,max_shift=2,max_changes=2,repair_nodes=100,seconds=5)
a <- rbind(.variation(x),.variation(x,form='I',transposition=1,voice=2))
r <- .repair(a,o,proc.time()[3]+5)
check(r$evaluation$valid)
for(key in unique(r$notes$source)) {
  q <- r$notes[r$notes$source==key,]
  check(length(unique((q$pitch-q$base)/q$sign))==1)
}
check(identical(a$onset,r$notes$onset) && identical(a$duration,r$notes$duration))
# Matalan ja korkean rekisterin rajat ovat ehdottomat.
check(kaanoni_evaluate(a,kaanoni_options(ranges=list(c(80,90),c(80,90))))$hard>0)
# 2–4 aanen toimiva vakaa kaanon, augmentaatio ja usean ratkaisun vienti.
constant <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),rep(60,4))))
for(v in 2:4) {
  o <- kaanoni_options(voices=v,delays=1:3,transpositions=c(0,12),
       forbid_parallels=TRUE,seconds=5,max_candidates=100,max_results=3)
  result <- kaanoni_generate(constant,o)
  check(length(result$results)>0)
  check(all(vapply(result$results,function(r) r$evaluation$valid,logical(1))))
}
tmp <- tempfile(); dir.create(tmp)
for(ext in c('mid','musicxml')) {
  p <- file.path(tmp,paste0('roundtrip.',ext)); kaanoni_write(result,p)
  reread <- kaanoni_read(p)
  check(nrow(reread$notes)==sum(vapply(result$results,function(r) nrow(r$notes),integer(1))))
  check(length(unique(reread$notes$voice))==4)
  check(all(reread$notes$duration>0))
  expected <- list(); cursor <- 0
  for(i in seq_along(result$results)) {
    a <- result$results[[i]]$notes[,c('onset','duration','pitch','voice')]
    a$onset <- a$onset+cursor; expected[[i]] <- a
    cursor <- ceiling((max(a$onset+a$duration)-1e-8)/4)*4+4
  }
  expected <- do.call(rbind,expected)
  sortnotes <- function(a) a[order(a$voice,a$onset),c('onset','duration','pitch','voice')]
  check(isTRUE(all.equal(unname(as.matrix(sortnotes(expected))),unname(as.matrix(sortnotes(reread$notes))),tolerance=1e-7)))
}
# MusicXML:n tahtirajan yli sidottu nuotti palautuu yhdeksi nuotiksi.
long <- constant[[1]]; long$notes <- long$notes[1,,drop=FALSE]; long$notes$duration <- 6
long$span <- 8
result2 <- kaanoni_generate(list(long),kaanoni_options(delays=0,transpositions=12,seconds=2))
p <- file.path(tmp,'ties.musicxml'); kaanoni_write(result2,p)
b <- kaanoni_read(p)
check(nrow(b$notes)==2 && all(b$notes$duration==6))
# Yhtenainen pitka dissonanssi on yksi virhe muiden tapahtumista riippumatta.
sustain <- kaanoni_themes(kaanoni_input(kaanoni_notes(0,4,60)))[[1]]
a <- rbind(.variation(sustain),.variation(sustain,transposition=1,voice=2))
check(kaanoni_evaluate(a,kaanoni_options())$harmony==1)
# Rajallinen haku kertoo aikarajan sijaan ehdokasrajan tayttymisenkin.
limited <- kaanoni_generate(constant,kaanoni_options(delays=1:3,transpositions=c(0,12),max_candidates=1))
check(limited$status=='search_limit')
# Kolmen ja neljan aanen korjaus sailyttaa kaikkien lahdesavelten vastaavuuden.
for(v in 3:4) {
  o <- kaanoni_options(voices=v,repair=TRUE,forbid_parallels=FALSE,
    allowed_intervals=0:11,max_leap=0,max_shift=2,max_changes=2,repair_nodes=100)
  a <- do.call(rbind,lapply(seq_len(v),function(j) .variation(x,voice=j,transposition=12*(j-1))))
  rr <- .repair(a,o,proc.time()[3]+5)
  check(rr$evaluation$valid && any(rr$delta!=0))
  check(length(unique(rr$notes$pitch-rr$notes$base))<=2)
}
unlink(tmp,recursive=TRUE)
cat('Kaikki uuden algoritmin testit OK\n')
