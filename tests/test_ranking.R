source('R/kaanoni.R')
item <- function(name,errors,changes,movement,consonance,overlap) {
 list(name=name,evaluation=list(harmony=errors,overlap=overlap),changed_sources=changes,
      movement=movement,consonance=consonance)
}
r <- list(item('A',0,0,0,0.6,2),item('B',0,1,1,1,3),item('C',0,0,0,0.5,8),item('D',1,0,0,1,10))
names_of <- function(x) vapply(x,function(a)a$name,character(1))
stopifnot(identical(names_of(kaanoni_rank(r,'balanced')),c('A','C','B','D')),
          identical(names_of(kaanoni_rank(r,'consonance')),c('B','A','C','D')),
          identical(names_of(kaanoni_rank(r,'overlap')),c('C','B','A','D')))
# Vie vain kaksi parasta vaikka alkuperainen lista olisi vaarassa jarjestyksessa.
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),rep(60,4))))
r <- kaanoni_generate(t,kaanoni_options(delays=1:3,transpositions=c(0,4),
  forbid_parallels=FALSE,ranking='overlap',seconds=2))
stopifnot(length(r$results)>2)
r$results <- rev(r$results)
expected <- kaanoni_rank(r$results,'overlap')[1:2]
folder <- tempfile();dir.create(folder)
for(ext in c('mid','musicxml')) {
 path <- file.path(folder,paste0('best.',ext))
 kaanoni_write(r,path,best_n=2)
 exported <- readRDS(paste0(path,'.rds'))
 original_results <- lapply(exported$results,function(x) {x$export_notes<-NULL;x$voice_map<-NULL;x$written_voices<-NULL;x})
 stopifnot(length(exported$results)==2,identical(original_results,expected))
 stopifnot(nrow(kaanoni_read(path)$notes)==sum(vapply(expected,function(x)nrow(x$notes),integer(1))))
}
unlink(folder,recursive=TRUE)
cat('Hyvyysjarjestys ja vain parhaiden vienti OK\n')
