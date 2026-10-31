source('R/kaanoni.R')
t <- kaanoni_themes(kaanoni_input(kaanoni_notes(0:3,rep(1,4),rep(60,4))))
events <- list()
r <- kaanoni_generate(t,kaanoni_options(delays=1:2,transpositions=c(0,12),seconds=2),
  progress=function(p) { events[[length(events)+1L]] <<- p })
stopifnot(events[[1]]$stage=='start',tail(events,1)[[1]]$stage=='done')
last <- tail(events,1)[[1]]
stopifnot(last$tested==r$tested,last$found==r$found_total,
 all(diff(vapply(events,function(p)p$elapsed,numeric(1)))>=0),
 all(vapply(events,function(p)p$budget_fraction>=0 && p$budget_fraction<=1,logical(1))))
stopifnot(any(vapply(events,function(p)p$stage=='scan' && p$theme==1,logical(1))))
stopifnot(kaanoni_options()$seconds==240)
cat('Haun edistymismittarin testit OK\n')
