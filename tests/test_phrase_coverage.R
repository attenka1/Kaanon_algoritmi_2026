source('R/kaanoni.R')
# Raportin ongelma: useita sakeita, runsaasti paikallisia vaihtoehtoja ja pieni yhteinen raja.
count <- 20L
onsets <- unlist(lapply(0:(count-1),function(i)i*20+0:3))
x <- kaanoni_input(kaanoni_notes(onsets,rep(1,length(onsets)),rep(60,length(onsets))),end=count*20)
o <- kaanoni_options(delays=1:10,transpositions=c(0,4,7),forbid_parallels=FALSE,
 max_candidates=30,max_results=3,seconds=5)
events <- list()
r <- kaanoni_phrase_chain(x,5,o,same_transposition=FALSE,progress=function(p){events[[length(events)+1L]]<<-p})
stopifnot(length(r$results)>0,r$tested<=30,length(r$results[[1]]$phrases)==count)
d <- r$diagnostics$phrase_attempts
seed <- which(d$pass=='first_solution')
improve <- which(d$pass=='improvement')
stopifnot(length(seed)==count,all(d$found[seed]>0),
          !length(improve) || min(improve)>max(seed),
          is.finite(r$diagnostics$first_complete_elapsed))
stopifnot(any(vapply(events,function(p)p$stage=='phrase_seed',logical(1))))
# Liian pieni ehdokasraja kerrotaan tasmallisesti eika tulosteta vajaita ketjuja.
o$max_candidates <- 3
r <- kaanoni_phrase_chain(x,5,o,same_transposition=FALSE)
stopifnot(length(r$results)==0,r$diagnostics$termination=='candidate_limit',
  any(grepl('Lopetussyy: Ehdokasraja tayttyi',kaanoni_debug(r),fixed=TRUE)))
cat('Kaikki sakeet saavat ensiratkaisun ennen lisahakua; ehdokasraja raportoitu OK\n')
# Lisahaku jatkaa ensiratkaisusta ja tutkii vain jaljella olevat ehdokkaat.
flat <- kaanoni_input(kaanoni_notes(0:7,rep(1,8),rep(60,8)))
o <- kaanoni_options(delays=1:3,transpositions=-7,forbid_parallels=FALSE,
  max_candidates=30,max_results=10,seconds=5)
r <- kaanoni_phrase_chain(flat,1,o)
d <- r$diagnostics$phrase_attempts
stopifnot(r$tested==6L,all(d$tested[d$pass=='first_solution']==1L),
  all(d$tested[d$pass=='improvement']==2L),r$repair_attempted==0L,r$repair_limited==0L)
