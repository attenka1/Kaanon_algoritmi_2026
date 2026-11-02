# Kaanoni: aika mitataan neljasosina. Ei sivuvaikutuksia source-kutsussa.
.kaanoni_root <- local({
  files <- vapply(sys.frames(),function(e) {
    f <- e$ofile
    if(is.null(f)) '' else as.character(f)[1]
  },character(1))
  f <- tail(files[basename(files)=='kaanoni.R'],1)
  if(!length(f)) getwd() else dirname(dirname(normalizePath(f)))
})

kaanoni_notes <- function(onset, duration, pitch, voice=1L) {
  n <- data.frame(onset=as.numeric(onset), duration=as.numeric(duration),
                  pitch=as.integer(pitch), voice=as.integer(voice))
  if (!nrow(n) || anyNA(n) || any(!is.finite(as.matrix(n))) ||
      any(n$onset < 0 | n$duration <= 0 | n$pitch < 0 | n$pitch > 127))
    stop('Nuotit tarvitsevat ei-negatiivisen alkuajan, positiivisen keston ja MIDI-korkeuden 0–127.')
  n <- n[order(n$voice,n$onset),]; rownames(n) <- NULL
  for (v in unique(n$voice)) {
    a <- n[n$voice==v,]
    if(nrow(a)>1 && any(head(a$onset+a$duration,-1)>tail(a$onset,-1)+1e-8))
      stop('Syotteen on oltava yksiääninen kussakin aanessa. Valitse melodiastemmat.')
  }
  n$id <- seq_len(nrow(n)); n
}

kaanoni_input <- function(notes, measures=NULL, end=NULL) {
  notes <- kaanoni_notes(notes$onset,notes$duration,notes$pitch,notes$voice)
  if(is.null(end)) end <- max(notes$onset+notes$duration)
  if(is.null(measures)) {
    starts <- seq(0,max(end-1e-8,0),by=4)
    measures <- data.frame(start=starts,length=4,numerator=4,denominator=4)
  }
  if(end+1e-8 < max(notes$onset+notes$duration)) stop('Loppuaika on ennen nuottien loppua.')
  list(notes=notes, measures=measures, end=end)
}

.notation_run <- function(action,path,folder) {
  python <- Sys.which('python3')
  if(!nzchar(python)) stop('Notaation tiedostokasittely tarvitsee Python 3:n.')
  log <- suppressWarnings(system2(python,c(shQuote(file.path(.kaanoni_root,'tools','notation_io.py')),
      action,shQuote(path),shQuote(folder)),stdout=TRUE,stderr=TRUE))
  status <- attr(log,'status')
  if(!is.null(status) && status!=0) {
    stop(paste(c('Notaation tiedostokasittely epäonnistui:',log),collapse='\n'),call.=FALSE)
  }
  invisible(NULL)
}

kaanoni_read <- function(path, voices=NULL) {
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp,recursive=TRUE))
  .notation_run('read',normalizePath(path),tmp)
  n <- read.csv(file.path(tmp,'notes.csv')); m <- read.csv(file.path(tmp,'measures.csv'))
  if(!is.null(voices)) n <- n[n$voice %in% voices,]
  kaanoni_input(n,m,as.numeric(readLines(file.path(tmp,'end.txt'))))
}

kaanoni_themes <- function(input, bars=NULL) {
  if(!is.null(bars) && (length(bars)!=1 || bars<1 || bars!=as.integer(bars))) stop('Teematahtimaara on positiivinen kokonaisluku.')
  m <- input$measures
  if(is.null(bars)) bounds <- c(0,input$end) else {
    bounds <- c(m$start[seq(1,nrow(m),by=bars)],max(input$end,tail(m$start+m$length,1)))
  }
  themes <- list()
  for(v in unique(input$notes$voice)) for(i in seq_len(length(bounds)-1)) {
    a <- bounds[i]; b <- bounds[i+1]
    n <- input$notes[input$notes$voice==v & input$notes$onset<b-1e-8 &
                       input$notes$onset+input$notes$duration>a+1e-8,]
    if(!nrow(n)) next
    # Teemarajan ylittava nuotti katkaistaan rajalle; tyhja loppu sailyy span-arvossa.
    ends <- pmin(n$onset+n$duration,b)
    n$onset <- pmax(n$onset,a); n$duration <- ends-n$onset; n$onset <- n$onset-a
    k <- length(themes)+1L
    n$source <- paste0('T',k,':',seq_len(nrow(n)))
    themes[[k]] <- list(name=paste0('T',k), notes=n, span=b-a,
                        measures=transform(m[m$start>=a-1e-8 & m$start<b-1e-8,,drop=FALSE],start=start-a),
                        source_voice=v, first_measure=which.min(abs(m$start-a)))
  }
  if(!length(themes)) stop('Ei melodiateemoja.')
  themes
}

kaanoni_options <- function(voices=2L, mode='canon', forms='O', factors=1,
    delays=0:8, transpositions=0:11, ranges=rep(list(c(36,96)),voices),
    allowed_intervals=c(0,3,4,7,8,9), allowed_sets=NULL,
    max_harmony_errors=0L, forbid_parallels=TRUE, forbid_crossing=FALSE,
    max_leap=12L, min_overlap=1, repair=FALSE, repair_mode='strict',
    max_shift=2L, max_changes=4L, repair_nodes=500L,
    max_candidates=10000L, max_results=30L, seconds=240,
    dissonances=FALSE, min_sonorities=0L, sonority_mode=c('intervals','chords'),
    allow_weak_steps=FALSE, strong_beat_period=2,
    metric_mode=c('auto','manual'), meter=NULL, stop_after_results=FALSE,
    ranking=c('balanced','consonance','overlap'), parallel_gap_tolerance=0.125) {
  if(!voices %in% 2:4 || length(voices)!=1) stop('Aanimaara on 2–4.')
  mode <- match.arg(mode,c('canon','counterpoint')); repair_mode <- match.arg(repair_mode,c('strict','free'))
  if(!is.logical(allow_weak_steps) || length(allow_weak_steps)!=1 || is.na(allow_weak_steps) ||
     length(strong_beat_period)!=1 || !is.finite(strong_beat_period) || strong_beat_period<=0)
    stop('Tarkista epaiskujen asteikkoliikkeen asetukset.')
  if(!is.logical(stop_after_results) || length(stop_after_results)!=1 || is.na(stop_after_results)) stop('Virheellinen pysaytysasetus.')
  if(length(parallel_gap_tolerance)!=1 || !is.finite(parallel_gap_tolerance) || parallel_gap_tolerance<0)
    stop('Rinnakkaisliikkeiden katkotoleranssin on oltava ei-negatiivinen.')
  ranking <- match.arg(ranking)
  metric_mode <- match.arg(metric_mode)
  sonority_mode <- match.arg(sonority_mode)
  if(voices==2L) sonority_mode <- 'intervals'
  if(length(min_sonorities)!=1 || !is.finite(min_sonorities) || min_sonorities<0 || min_sonorities!=floor(min_sonorities))
    stop('Yhteissointien vahimmaismaara on ei-negatiivinen kokonaisluku.')
  forms <- unique(c('O',forms)); if(!all(forms %in% c('O','I','R','RI'))) stop('Tuntematon teemamuoto.')
  if(any(!is.finite(c(factors,delays,transpositions))) || !length(factors) ||
     !length(delays) || !length(transpositions) || any(factors<=0) || any(delays<0) ||
     any(transpositions!=round(transpositions))) stop('Virheelliset muunnosparametrit.')
  if(length(ranges)!=voices || any(vapply(ranges,function(x) length(x)!=2 || any(!is.finite(x)) || x[1]>x[2] || x[1]<0 || x[2]>127,logical(1)))) stop('Virheelliset aanialat.')
  if(any(!is.finite(c(max_harmony_errors,max_leap,min_overlap,max_shift,max_changes,repair_nodes,max_candidates,max_results,seconds))) ||
     any(c(max_harmony_errors,max_leap,min_overlap,max_shift,max_changes)<0) ||
     any(c(repair_nodes,max_candidates,max_results,seconds)<=0)) stop('Virheelliset hakurajat.')
  as.list(environment())
}

.variation <- function(theme, form='O', factor=1, delay=0, transposition=0, voice=1) {
  n <- theme$notes
  sign <- if(form %in% c('I','RI')) -1 else 1
  axis <- theme$notes$pitch[1] # Kiintea akseli, ei muutu korjausten aikana.
  if(form %in% c('R','RI')) n$onset <- theme$span-n$onset-n$duration
  n$base <- if(sign==-1) 2*axis-n$pitch else n$pitch
  n$base <- n$base+transposition; n$sign <- sign
  n$pitch <- n$base; n$onset <- n$onset*factor+delay; n$duration <- n$duration*factor
  n$voice <- voice; n$form <- form; n$theme <- theme$name
  n[order(n$onset),]
}

# Sallittu joukko ilmaistaan transpositiosta riippumattomana PC-avaimena.
kaanoni_pc_key <- function(p) {
  p <- sort(unique(p%%12)); if(!length(p)) return('')
  min(vapply(p,function(a) paste(sort((p-a)%%12),collapse=','),character(1)))
}

# Epaiskun lomasavel: molemmat viereiset liikkeet 1–2 puolisavelaskelta samaan suuntaan.
# Ajallinen jatkuvuus vaaditaan. Nuotti ei saa jatkua seuraavan vahvan iskun yli.
# Metrinen ruudukko ensimmaisen aanen alkuperaisista tahtirajoista.
# Vajaa alkutahti tulkitaan kohotahdiksi; viimeista tahtilajia jatketaan tarvittaessa.
.metric_strong_times <- function(meter,end) {
  if(is.null(meter) || !nrow(meter)) return(NULL)
  m <- meter[order(meter$start),]
  while(tail(m$start+m$length,1)<=end+1e-8) {
    last <- tail(m,1); last$start <- last$start+last$length
    last$length <- last$numerator*4/last$denominator
    m <- rbind(m,last)
  }
  strong <- numeric()
  for(i in seq_len(nrow(m))) {
    unit <- 4/m$denominator[i]; num <- m$numerator[i]
    nominal <- num*unit
    period <- if(num==3 || num==9) nominal else if(num==4) 2*unit else
      if(num>3 && num%%3==0) 3*unit else if(num==2) nominal else unit
    offset <- max(0,nominal-m$length[i])
    points <- m$start[i]-offset+seq(0,nominal,by=period)
    strong <- c(strong,points[points>=m$start[i]-1e-8 & points<=m$start[i]+m$length[i]+1e-8])
  }
  sort(unique(round(strong,9)))
}

.weak_scale_note <- function(n,r,t,period,strong_times=NULL) {
  weak <- function(x) if(is.null(strong_times)) abs(x/period-round(x/period))>1e-8 else
    !any(abs(strong_times-x)<1e-8)
  if(!weak(t) || !weak(n$onset[r])) return(FALSE)
  rows <- which(n$voice==n$voice[r]); rows <- rows[order(n$onset[rows])]
  at <- match(r,rows)
  if(at==1L || at==length(rows)) return(FALSE)
  prev <- rows[at-1L]; next_note <- rows[at+1L]
  if(abs(n$onset[prev]+n$duration[prev]-n$onset[r])>1e-8 ||
     abs(n$onset[r]+n$duration[r]-n$onset[next_note])>1e-8) return(FALSE)
  next_strong <- if(is.null(strong_times)) (floor(n$onset[r]/period+1e-8)+1)*period else
    min(c(strong_times[strong_times>n$onset[r]+1e-8],Inf))
  if(n$onset[r]+n$duration[r]>next_strong+1e-8) return(FALSE)
  a <- n$pitch[r]-n$pitch[prev]; b <- n$pitch[next_note]-n$pitch[r]
  abs(a) %in% 1:2 && abs(b) %in% 1:2 && a*b>0
}

.same_octave_voice <- function(a,b) {
  if(!nrow(a) || nrow(a)!=nrow(b)) return(FALSE)
  a <- a[order(a$onset),]; b <- b[order(b$onset),]
  if(any(abs(a$onset-b$onset)>1e-8) || any(abs(a$duration-b$duration)>1e-8)) return(FALSE)
  d <- a$pitch-b$pitch
  length(unique(d))==1L && d[1]%%12==0
}

kaanoni_evaluate <- function(n, o) {
  empty <- data.frame(type=character(),start=numeric(),end=numeric(),voices=character(),notes=character(),detail=character())
  errors <- empty
  add <- function(type,a,b,vs,rs,detail) {
    errors[nrow(errors)+1,] <<- list(type,a,b,paste(vs,collapse=','),paste(rs,collapse=','),detail)
  }
  voice_ids <- sort(unique(n$voice))
  if(length(voice_ids)>1) for(pair in combn(voice_ids,2,simplify=FALSE)) {
    a <- n[n$voice==pair[1],]; b <- n[n$voice==pair[2],]
    if(.same_octave_voice(a,b)) add('doubling',min(a$onset),max(a$onset+a$duration),pair,
      which(n$voice %in% pair),'Sama teema samanaikaisesti unisonossa tai oktaavissa')
  }
  times <- sort(unique(round(c(n$onset,n$onset+n$duration),9)))
  strong_times <- if(isTRUE(o$allow_weak_steps) && identical(o$metric_mode,'auto'))
    .metric_strong_times(o$meter,max(times)) else NULL
  overlap <- 0
  parallel_history <- new.env(hash=TRUE,parent=emptyenv())
  gap_tolerance <- if(is.null(o$parallel_gap_tolerance)) 0.125 else o$parallel_gap_tolerance
  counted_pairs <- character(); counted_chords <- character()
  for(k in seq_len(length(times)-1)) {
    t <- times[k]; end <- times[k+1]
    rows <- which(n$onset<=t+1e-8 & n$onset+n$duration>t+1e-8)
    active <- n[rows,,drop=FALSE]
    if(nrow(active)==o$voices) {
      overlap <- overlap+end-t
      if(o$voices>=3L) counted_chords <- union(counted_chords,paste(sort(rows),collapse=','))
    }
    if(nrow(active)>1) for(pair in combn(seq_len(nrow(active)),2,simplify=FALSE)) {
      q <- active[pair,]; rs <- rows[pair]
      ordering <- order(q$voice);q <- q[ordering,];rs <- rs[ordering]
      int <- abs(diff(q$pitch))%%12
      counted_pairs <- union(counted_pairs,paste(sort(rs),collapse=','))
      ok <- int %in% o$allowed_intervals
      if(!ok && isTRUE(o$allow_weak_steps)) {
        ok <- any(vapply(rs,function(r) .weak_scale_note(n,r,t,o$strong_beat_period,strong_times),logical(1)))
      }
      if(!ok && o$dissonances) {
        # Heikolla neljasosan puoliskolla asteittainen loma-/sivusavel.
        for(j in seq_len(2)) {
          r <- rs[j]; same <- which(n$voice==n$voice[r]); at <- match(r,same)
          if(at>1 && at<length(same) && abs(t-round(t))>1e-8 && abs(n$onset[r]-t)<1e-8) {
            a <- n$pitch[r]-n$pitch[same[at-1]]; b <- n$pitch[same[at+1]]-n$pitch[r]
            other <- rs[3-j]
            held <- n$onset[other]<=n$onset[same[at-1]]+1e-8 &&
              n$onset[other]+n$duration[other]>n$onset[same[at+1]]+1e-8
            neighbors <- (a*b>0 || a == -b) && abs(a) %in% 1:2 && abs(b) %in% 1:2
            endpoints <- all(abs(n$pitch[same[c(at-1,at+1)]]-n$pitch[other])%%12 %in% o$allowed_intervals)
            if(held && neighbors && endpoints) ok <- TRUE
          }
        }
      }
      if(!ok) add('harmony',t,end,q$voice,rs,paste0('interval=',int))
      if(o$forbid_crossing && q$pitch[order(q$voice)][1]<q$pitch[order(q$voice)][2])
        add('crossing',t,end,q$voice,rs,'Aanten jarjestys rikkoutuu')
      if(o$forbid_parallels) {
        # Muisti aaniparia kohti: note-off tai muun aanen tapahtuma ei saa unohtaa intervallia.
        key <- paste(q$voice,collapse=',')
        if(exists(key,parallel_history,inherits=FALSE)) {
          old <- get(key,parallel_history,inherits=FALSE)
          movement <- q$pitch-old$pitch
          if(t-old$until<=gap_tolerance+1e-8 && all(movement!=0) &&
             prod(movement)>0 && int %in% c(0,7) && old$interval==int)
            add('parallel',t,end,q$voice,rs,'Rinnakkainen kvintti tai oktaavi (myos lyhyen artikulaatiokatkon yli)')
        }
        assign(key,list(pitch=q$pitch,interval=int,
          until=min(q$onset+q$duration)),parallel_history)
      }
    }
    if(nrow(active)>1 && !is.null(o$allowed_sets) && !kaanoni_pc_key(active$pitch) %in% o$allowed_sets)
      add('set',t,end,active$voice,rows,kaanoni_pc_key(active$pitch))
  }
  for(v in seq_len(o$voices)) {
    rs <- which(n$voice==v); a <- n[rs,]
    bad <- which(a$pitch<o$ranges[[v]][1] | a$pitch>o$ranges[[v]][2])
    for(j in bad) add('range',a$onset[j],a$onset[j]+a$duration[j],v,rs[j],'Aaniala')
    for(j in which(abs(diff(a$pitch))>o$max_leap))
      add('leap',a$onset[j+1],a$onset[j+1]+a$duration[j+1],v,rs[c(j,j+1)],'Liian suuri hyppy')
  }
  # Yhtenainen sama rikkomus lasketaan kerran, riippumatta muiden aanten tapahtumista.
  if(nrow(errors)) {
    errors <- errors[order(errors$type,errors$voices,errors$detail,errors$start),]
    keep <- rep(TRUE,nrow(errors)); last <- 1L
    if(nrow(errors)>1) for(i in 2:nrow(errors)) {
      if(all(errors[i,c('type','voices','detail')]==errors[last,c('type','voices','detail')]) && abs(errors$start[i]-errors$end[last])<1e-8 && errors$type[i]!='parallel') {
        errors$end[last] <- errors$end[i]; errors$notes[last] <- paste(unique(c(strsplit(errors$notes[last],',')[[1]],strsplit(errors$notes[i],',')[[1]])),collapse=','); keep[i] <- FALSE
      } else last <- i
    }
    errors <- errors[keep,]; rownames(errors) <- NULL
  }
  harmony <- sum(errors$type %in% c('harmony','set')); hard <- sum(!errors$type %in% c('harmony','set'))
  interval_count <- length(counted_pairs); chord_count <- length(counted_chords)
  sonority_mode <- if(is.null(o$sonority_mode) || o$voices==2L) 'intervals' else o$sonority_mode
  minimum <- if(is.null(o$min_sonorities)) 0L else o$min_sonorities
  sonority_count <- if(sonority_mode=='intervals') interval_count else chord_count
  list(errors=errors,harmony=harmony,hard=hard,overlap=overlap,
       interval_count=interval_count,chord_count=chord_count,sonority_count=sonority_count,
       valid=hard==0 && harmony<=o$max_harmony_errors && overlap+1e-8>=o$min_overlap && sonority_count>=minimum)
}

.repair <- function(n,o,deadline,resume=NULL,batch_nodes=Inf) {
  n$key <- if(o$repair_mode=='strict') n$source else paste(n$voice,n$source,sep='/')
  keys <- unique(n$key); zero <- setNames(rep(0,length(keys)),keys)
  build <- function(d) { a <- n; a$pitch <- a$base+(if(o$repair_mode=='strict') a$sign else 1)*d[a$key]; a }
  score <- function(e,d) c(e$hard,max(0,e$harmony-o$max_harmony_errors),sum(d!=0),sum(abs(d)),max(abs(d)))
  if(is.null(resume)) {
    d <- zero; ev <- kaanoni_evaluate(n,o)
    queue <- list(list(d=d,e=ev)); seen <- new.env(hash=TRUE,parent=emptyenv())
    assign(paste(d,collapse=','),TRUE,seen); visited <- 0L
  } else {
    queue <- resume$queue; seen <- resume$seen; visited <- resume$visited
  }
  batch_end <- visited+batch_nodes
  while(length(queue) && visited<o$repair_nodes && visited<batch_end && proc.time()[3]<deadline) {
    ranks <- do.call(rbind,lapply(queue,function(x) score(x$e,x$d)))
    ix <- do.call(order,as.data.frame(ranks))[1]; state <- queue[[ix]]; queue <- queue[-ix]
    visited <- visited+1L; d <- state$d; ev <- state$e
    if(ev$valid) return(list(notes=build(d),evaluation=ev,delta=d,nodes=visited,status='found_heuristic'))
    offender <- unique(as.integer(unlist(strsplit(ev$errors$notes,',',fixed=TRUE))))
    if(!length(offender)) break
    for(key in unique(n$key[offender])) for(step in c(-1,1)) {
      nextd <- d; nextd[key] <- nextd[key]+step
      if(abs(nextd[key])>o$max_shift || sum(nextd!=0)>o$max_changes) next
      hash <- paste(nextd,collapse=','); if(exists(hash,seen,inherits=FALSE)) next
      assign(hash,TRUE,seen); a <- build(nextd)
      if(any(a$pitch<0 | a$pitch>127)) next
      queue[[length(queue)+1L]] <- list(d=nextd,e=kaanoni_evaluate(a,o))
    }
  }
  list(notes=n,evaluation=kaanoni_evaluate(n,o),delta=zero,nodes=visited,
       status=if(length(queue)) 'repair_limit' else 'repair_exhausted_local',
       resume=list(queue=queue,seen=seen,visited=visited))
}

# Leksikografinen hyvyysjarjestys: jokainen aiempi kriteeri ratkaisee ennen seuraavaa.
kaanoni_rank <- function(results,ranking='balanced') {
  ranking <- match.arg(ranking,c('balanced','consonance','overlap'))
  if(!length(results)) return(results)
  key <- function(x) {
    consonance <- if(is.finite(x$consonance)) x$consonance else -Inf
    overlap <- if(is.null(x$evaluation$overlap)) 0 else x$evaluation$overlap
    switch(ranking,
      balanced=c(x$evaluation$harmony,x$changed_sources,x$movement,-consonance,-overlap),
      consonance=c(x$evaluation$harmony,-consonance,x$changed_sources,x$movement,-overlap),
      overlap=c(x$evaluation$harmony,-overlap,x$changed_sources,x$movement,-consonance))
  }
  keys <- do.call(rbind,lapply(results,function(x) c(key(x),
    if(is.null(x$boundary_movement)) 0 else x$boundary_movement)))
  results[do.call(order,as.data.frame(keys))]
}

kaanoni_generate <- function(themes, options=kaanoni_options(), progress=NULL) {
  o <- options; started <- proc.time()[3]; deadline <- started+o$seconds
  # Varaa korjauksille osa kokonaisajasta, mutta etsi ensin korjaamattomat ratkaisut.
  scan_deadline <- if(o$repair) started+o$seconds*0.6 else deadline
  pending <- list(); repair_attempted <- 0L
  results <- list(); seen <- new.env(hash=TRUE,parent=emptyenv()); tested <- 0L; repair_limited <- 0L
  exhausted <- TRUE
  if(!is.null(progress) && !is.function(progress)) stop('progress on funktio tai NULL.')
  expanded <- 0L; current_theme <- 0L; last_progress <- -Inf
  emit_progress <- function(stage,force=FALSE) {
    now <- proc.time()[3]
    if(is.null(progress) || (!force && now-last_progress<0.2)) return(invisible(NULL))
    last_progress <<- now
    progress(list(stage=stage,elapsed=unname(now-started),seconds=o$seconds,
      budget_fraction=min(1,(now-started)/o$seconds),
      theme=current_theme,themes=length(themes),expanded=expanded,
      tested=tested,candidate_limit=o$max_candidates,found=if(stage=='done') found_total else length(results),
      repair_attempted=repair_attempted,repair_candidates=length(pending)))
    invisible(NULL)
  }
  emit_progress('start',TRUE)
  add_result <- function(n,ev,delta,settings,status) {
    hash <- paste(n$voice,round(n$onset,8),round(n$duration,8),n$pitch,collapse=';')
    if(exists(hash,seen,inherits=FALSE)) return(invisible(NULL))
    assign(hash,TRUE,seen)
    changed <- if(length(delta)) sum(delta!=0) else 0L
    occurrences <- sum(n$pitch!=n$base)
    movement <- if(length(delta)) sum(abs(delta)) else 0
    # Kestopainotettu konsonanssi: hyvaksyttyjen harmonisten aikavalien osuus.
    times <- sort(unique(c(n$onset,n$onset+n$duration))); good <- total <- 0
    for(i in seq_len(length(times)-1)) {
      a <- n[n$onset<=times[i]+1e-8 & n$onset+n$duration>times[i]+1e-8,]
      if(nrow(a)<2) next
      ints <- combn(a$pitch,2,function(p) abs(diff(p))%%12)
      w <- times[i+1]-times[i]; total <- total+w; good <- good+w*mean(ints %in% o$allowed_intervals)
    }
    results[[length(results)+1L]] <<- list(notes=n,settings=settings,evaluation=ev,
      kind=if(o$mode=='canon' && o$repair_mode=='free' && occurrences>0) 'modified_counterpoint' else o$mode,
      changes=data.frame(source=names(delta)[delta!=0],semitones=unname(delta[delta!=0])),
      changed_sources=changed,changed_occurrences=occurrences,movement=movement,
      consonance=if(total>0) good/total else NA_real_,status=status)
  }
  # Muunnokset muodostetaan laiskasti kerran kutakin aanipaikkaa varten.
  grid <- expand.grid(p=o$transpositions,d=o$delays,a=o$factors,f=o$forms,
    t=seq_along(themes),KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  variants <- new.env(hash=TRUE,parent=emptyenv())
  pairs <- new.env(hash=TRUE,parent=emptyenv())
  variant_hits <- pair_hits <- temporal_rejects <- 0L
  roots_visited <- integer()
  stopped_results <- FALSE
  enough <- function() isTRUE(o$stop_after_results) && length(results)>=o$max_results
  get_variant <- function(id,voice) {
    key <- paste(voice,id,sep='/')
    if(exists(key,variants,inherits=FALSE)) {
      variant_hits <<- variant_hits+1L
      return(get(key,variants,inherits=FALSE))
    }
    g <- grid[id,]
    x <- .variation(themes[[g$t]],g$f,g$a,g$d,g$p,voice)
    assign(key,x,variants); x
  }
  common_duration <- function(n) {
    ts <- sort(unique(c(n$onset,n$onset+n$duration)))
    change <- tabulate(match(n$onset,ts),nbins=length(ts))-
      tabulate(match(n$onset+n$duration,ts),nbins=length(ts))
    sum(diff(ts)[head(cumsum(change),-1)==length(unique(n$voice))])
  }
  pair_score <- function(left,right,left_id,right_id,lv,rv,root) {
    key <- paste(root,lv,rv,left_id,right_id,sep='/')
    if(exists(key,pairs,inherits=FALSE)) {
      pair_hits <<- pair_hits+1L
      return(get(key,pairs,inherits=FALSE))
    }
    po <- o; po$allowed_sets <- NULL
    e <- kaanoni_evaluate(rbind(left,right),po)
    value <- c(hard=e$hard,harmony=e$harmony)
    assign(key,value,pairs); value
  }
  # Jokaisen teeman DFS-pino ja seuraavan muunnoksen indeksi sailyvat hakuvuorojen valilla.
  roots <- lapply(seq_along(themes),function(t) {
    n <- .variation(themes[[t]],voice=1)
    settings <- data.frame(voice=1,theme=themes[[t]]$name,form='O',factor=1,delay=0,transposition=0)
    list(stack=list(list(notes=n,settings=settings,ids=0L,next_id=1L)),
      choices=if(o$mode=='canon') which(grid$t==t) else seq_len(nrow(grid)))
  })
  while(any(vapply(roots,function(x) length(x$stack)>0,logical(1)))) {
    if(proc.time()[3]>=scan_deadline || tested>=o$max_candidates || enough()) break
    for(root in seq_along(roots)) {
      if(proc.time()[3]>=scan_deadline || tested>=o$max_candidates || enough()) break
      if(!length(roots[[root]]$stack)) next
      current_theme <- root
      first_visit <- !root %in% roots_visited
      roots_visited <- union(roots_visited,root)
      if(first_visit) emit_progress('scan',TRUE)
      o$meter <- themes[[root]]$measures
      slice_end <- min(scan_deadline,proc.time()[3]+0.10)
      steps <- 0L
      while(length(roots[[root]]$stack) && steps<100L && proc.time()[3]<slice_end &&
            tested<o$max_candidates && !enough()) {
        depth <- length(roots[[root]]$stack)
        frame <- roots[[root]]$stack[[depth]]
        ids <- roots[[root]]$choices
        if(frame$next_id>length(ids)) {
          roots[[root]]$stack <- roots[[root]]$stack[-depth]
          next
        }
        id <- ids[frame$next_id]
        roots[[root]]$stack[[depth]]$next_id <- frame$next_id+1L
        steps <- steps+1L; expanded <- expanded+1L; emit_progress('scan')
        voice <- length(frame$ids)+1L
        x <- get_variant(id,voice); n <- frame$notes
        duplicate <- any(vapply(unique(n$voice),function(v)
          .same_octave_voice(n[n$voice==v,],x),logical(1)))
        if(duplicate) next
        if(!o$repair && any(x$pitch<o$ranges[[voice]][1] | x$pitch>o$ranges[[voice]][2])) next
        combined <- rbind(n,x)
        # Lisattava aani voi vain vahentaa kaikkien aanten yhteista soivaa aikaa.
        if(common_duration(combined)+1e-8<o$min_overlap) {
          temporal_rejects <- temporal_rejects+1L; next
        }
        if(!o$repair) {
          harmony <- 0L; bad <- FALSE
          for(v in seq_len(voice-1L)) {
            e <- pair_score(n[n$voice==v,],x,frame$ids[v],id,v,voice,root)
            harmony <- harmony+e['harmony']
            if(e['hard']>0 || harmony>o$max_harmony_errors) { bad <- TRUE; break }
          }
          if(bad) next
          # Vanhojen aaniparien virheetkin kuluttavat yhteista virhebudjettia.
          total_harmony <- (if(is.null(frame$harmony)) 0 else frame$harmony)+harmony
          if(total_harmony>o$max_harmony_errors) next
        } else total_harmony <- 0
        g <- grid[id,]
        settings <- rbind(frame$settings,data.frame(voice=voice,theme=themes[[g$t]]$name,
          form=g$f,factor=g$a,delay=g$d,transposition=g$p))
        if(voice==o$voices) {
          tested <- tested+1L
          # Lopullinen tarkistus on aina sama kaikissa hakutavoissa.
          ev <- kaanoni_evaluate(combined,o)
          if(ev$sonority_count<o$min_sonorities) next
          if(ev$valid) add_result(combined,ev,numeric(),settings,'unchanged') else if(o$repair) {
            pending[[length(pending)+1L]] <- list(notes=combined,evaluation=ev,
              settings=settings,meter=o$meter,resume=NULL)
          }
        } else {
          roots[[root]]$stack[[depth+1L]] <- list(notes=combined,settings=settings,
            ids=c(frame$ids,id),next_id=1L,harmony=total_harmony)
        }
      }
    }
  }
  stopped_results <- enough()
  exhausted <- !any(vapply(roots,function(x) length(x$stack)>0,logical(1)))
  # Korjaa ehdokkaita vuorotellen. Vaikea ensimmainen ehdokas ei saa koko aikabudjettia.
  if(length(pending) && !enough()) {
    emit_progress('repair',TRUE)
    rank <- do.call(rbind,lapply(pending,function(x) c(x$evaluation$hard,x$evaluation$harmony)))
    pending <- pending[do.call(order,as.data.frame(rank))]
    active <- seq_along(pending)
    while(length(active) && proc.time()[3]<deadline && !enough()) {
      next_round <- integer()
      for(i in active) {
        if(proc.time()[3]>=deadline || enough()) break
        candidate <- pending[[i]]
        current_theme <- match(candidate$settings$theme[1],vapply(themes,function(x)x$name,character(1)))
        emit_progress('repair')
        if(is.null(candidate$resume)) repair_attempted <- repair_attempted+1L
        repair_options <- o; repair_options$meter <- candidate$meter
        rr <- .repair(candidate$notes,repair_options,min(deadline,proc.time()[3]+0.15),
                      resume=candidate$resume,batch_nodes=10L)
        pending[[i]]$resume <- rr$resume
        if(rr$evaluation$valid) {
          add_result(rr$notes,rr$evaluation,rr$delta,candidate$settings,rr$status)
          pending[[i]]$done <- TRUE
        } else if(rr$status=='repair_limit' && rr$nodes<o$repair_nodes) {
          next_round <- c(next_round,i)
        } else pending[[i]]$done <- TRUE
      }
      active <- next_round
    }
    repair_limited <- sum(!vapply(pending,function(x) isTRUE(x$done),logical(1)))
  }
  stopped_results <- enough()
  emit_progress('ranking',TRUE)
  found_total <- length(results)
  unchanged_found <- sum(vapply(results,function(x) x$status=='unchanged',logical(1)))
  if(length(results)) {
    results <- head(kaanoni_rank(results,if(is.null(o$ranking)) 'balanced' else o$ranking),o$max_results)
  }
  emit_progress('done',TRUE)
  structure(list(elapsed=unname(proc.time()[3]-started),results=results,tested=tested,found_total=found_total,unchanged_found=unchanged_found,repair_limited=repair_limited,
    stats=list(expanded=expanded,roots_visited=roots_visited,variant_cache_hits=variant_hits,
      pair_cache_hits=pair_hits,temporal_rejects=temporal_rejects),
    status=if(stopped_results) 'result_limit' else if(exhausted && !o$repair) 'exhaustive' else if(exhausted && repair_limited==0) 'geometry_exhausted_heuristic_repair' else 'search_limit',
    repair_attempted=repair_attempted,options=o),class='kaanoni_result')
}

.phrase_boundary_movement <- function(parts) {
  total <- 0
  if(length(parts)<2) return(total)
  for(i in 2:length(parts)) for(v in 1:2) {
    prev <- parts[[i-1]]$notes;prev <- prev[prev$voice==v,]
    next_notes <- parts[[i]]$notes;next_notes <- next_notes[next_notes$voice==v,]
    total <- total+abs(prev$pitch[which.max(prev$onset)]-next_notes$pitch[which.min(next_notes$onset)])
  }
  total
}

# Kukin sae ratkaistaan erikseen; transpositio voidaan sitoa yhteiseksi tai valita saekohtaisesti.
kaanoni_phrase_chain <- function(input,bars=5,options=kaanoni_options(),progress=NULL,same_transposition=TRUE) {
  if(!is.logical(same_transposition) || length(same_transposition)!=1 || is.na(same_transposition)) stop('Tarkista yhteisen alkuintervallin valinta.')
  if(length(unique(input$notes$voice))!=1L) stop('Valitse saekaanonille yksi syotemelodia.')
  if(options$voices!=2L || options$mode!='canon') stop('Saekaanonitila tarvitsee kaksi aanta ja kaanonhaun.')
  themes <- kaanoni_themes(input,bars=bars)
  started <- proc.time()[3]; deadline <- started+options$seconds
  all <- list(); tested <- 0L; limited <- FALSE; attempted <- 0L
  groups <- if(same_transposition) unique(options$transpositions) else NA_real_
  sub_budget <- options$seconds/(length(groups)*length(themes))
  bind <- function(parts) {
    columns <- unique(unlist(lapply(parts,names)))
    parts <- lapply(parts,function(x) {for(k in setdiff(columns,names(x))) x[[k]] <- rep(NA,nrow(x));x[,columns,drop=FALSE]})
    do.call(rbind,parts)
  }
  combine <- function(parts,transposition) {
    cursor <- 0; notes <- settings <- errors <- changes <- list(); phrases <- list()
    for(i in seq_along(parts)) {
      r <- parts[[i]]; n <- r$notes; n$onset <- n$onset+cursor;n$phrase <- i
      notes[[i]] <- n
      settings[[i]] <- r$settings;settings[[i]]$phrase <- i;settings[[i]]$phrase_start <- cursor
      e <- r$evaluation$errors;e$start <- e$start+cursor;e$end <- e$end+cursor;e$phrase <- rep(i,nrow(e));errors[[i]] <- e
      changes[[i]] <- r$changes
      phrases[[i]] <- list(number=i,theme=themes[[i]]$name,start=cursor,
        first_measure=themes[[i]]$first_measure,evaluation=r$evaluation,settings=r$settings)
      local_end <- max(r$notes$onset+r$notes$duration)
      m <- tail(themes[[i]]$measures,1); bar <- m$numerator*4/m$denominator
      # Saa sailyttaa lopputauon. Pidentynyt vasta-aihe ei saa menna seuraavan saetyksen paalle.
      span <- themes[[i]]$span
      cursor <- cursor+span+max(0,ceiling((local_end-span-1e-8)/bar))*bar
    }
    weights <- vapply(parts,function(r) r$evaluation$overlap,numeric(1))
    cons <- vapply(parts,function(r) r$consonance,numeric(1));cons[!is.finite(cons)] <- 0
    list(notes=bind(notes),settings=bind(settings),evaluation=list(errors=bind(errors),
      harmony=sum(vapply(parts,function(r)r$evaluation$harmony,numeric(1))),hard=0,
      overlap=sum(weights),sonority_count=sum(vapply(parts,function(r)r$evaluation$sonority_count,numeric(1))),
      valid=all(vapply(parts,function(r)r$evaluation$valid,logical(1)))),
      changes=bind(changes),changed_sources=sum(vapply(parts,function(r)r$changed_sources,numeric(1))),
      changed_occurrences=sum(vapply(parts,function(r)r$changed_occurrences,numeric(1))),
      movement=sum(vapply(parts,function(r)r$movement,numeric(1))),
      consonance=if(sum(weights)>0) weighted.mean(cons,weights) else 0,
      kind='phrase_chain',status='per_phrase',phrases=phrases,common_transposition=transposition,
      phrase_transpositions=vapply(parts,function(r)r$settings$transposition[r$settings$voice==2],numeric(1)),
      same_transposition=same_transposition,boundary_movement=.phrase_boundary_movement(parts))
  }
  for(p in groups) {
    if(proc.time()[3]>=deadline || tested>=options$max_candidates) {limited<-TRUE;break}
    pools <- list(); complete <- TRUE
    for(i in seq_along(themes)) {
      remaining <- deadline-proc.time()[3]
      if(remaining<=0 || tested>=options$max_candidates) {limited<-TRUE;complete<-FALSE;break}
      local <- options;local$transpositions <- if(same_transposition) p else options$transpositions
      local$seconds <- min(remaining,sub_budget)
      local$max_candidates <- options$max_candidates-tested
      local$stop_after_results <- FALSE
      callback <- if(is.null(progress)) NULL else function(event) {
        event$stage <- 'phrases';event$theme <- i;event$themes <- length(themes)
        event$elapsed <- unname(proc.time()[3]-started);event$seconds <- options$seconds
        event$budget_fraction <- min(1,event$elapsed/options$seconds)
        event$tested <- tested+event$tested;event$found <- length(all)
        event$transposition <- p;progress(event)
      }
      r <- kaanoni_generate(themes[i],local,progress=callback)
      attempted <- attempted+1L;tested <- tested+r$tested
      if(r$status!='exhaustive') limited <- TRUE
      if(!length(r$results)) {complete<-FALSE;break}
      pools[[i]] <- r$results
    }
    if(!complete) next
    # Rajattu yhdistelmavalikoima: parhaat loydetyt kokonaisuudet kasvavat sae kerrallaan.
    paths <- lapply(pools[[1]],function(x)list(x))
    if(length(pools)>1) for(i in 2:length(pools)) {
      candidates <- unlist(lapply(paths,function(path)
        lapply(pools[[i]],function(x)c(path,list(x)))),recursive=FALSE)
      scores <- lapply(seq_along(candidates),function(k) {
        path <- candidates[[k]]
        w <- vapply(path,function(r)r$evaluation$overlap,numeric(1))
        cs <- vapply(path,function(r)r$consonance,numeric(1));cs[!is.finite(cs)] <- 0
        list(index=k,evaluation=list(harmony=sum(vapply(path,function(r)r$evaluation$harmony,numeric(1))),overlap=sum(w)),
          changed_sources=sum(vapply(path,function(r)r$changed_sources,numeric(1))),
          movement=sum(vapply(path,function(r)r$movement,numeric(1))),
          consonance=if(sum(w)>0)weighted.mean(cs,w) else 0,
          boundary_movement=.phrase_boundary_movement(path))
      })
      order <- vapply(head(kaanoni_rank(scores,options$ranking),options$max_results),function(x)x$index,integer(1))
      paths <- candidates[order]
    }
    all <- c(all,lapply(paths,function(path)combine(path,p)))
    if(isTRUE(options$stop_after_results) && length(all)>=options$max_results) break
  }
  found <- length(all)
  all <- head(kaanoni_rank(all,options$ranking),options$max_results)
  structure(list(results=all,options=options,tested=tested,found_total=found,
    unchanged_found=sum(vapply(all,function(r)r$changed_sources==0,logical(1))),
    repair_attempted=attempted,repair_limited=0,elapsed=unname(proc.time()[3]-started),
    status=if(isTRUE(options$stop_after_results) && found>=options$max_results) 'result_limit' else
      if(limited) 'search_limit' else 'phrase_search_complete',phrase_count=length(themes),phrase_bars=bars,same_transposition=same_transposition),class='kaanoni_result')
}

print.kaanoni_result <- function(x,...) {
  cat('Ratkaisuja:',length(x$results),' Tarkastettuja kokonaisuuksia:',x$tested,' Tila:',x$status,'\n')
  if(!is.null(x$phrase_count)) cat('Sakeita kokonaisuudessa:',x$phrase_count,' Virheraja koskee kutakin saetta erikseen.\n')
  if(!is.null(x$found_total)) cat('Loytyi yhteensa:',x$found_total,
      ' Joista ilman korjauksia:',x$unchanged_found,'\n')
  if(isTRUE(x$options$repair)) cat('Korjaushaussa kasitelty:',x$repair_attempted,
      ' Kesken aikarajalla:',x$repair_limited,'\n')
  for(i in seq_along(x$results)) {
    r <- x$results[[i]]; cat(i,': muutettuja lahdesavelia',r$changed_sources,
      ', nuottiesiintymia',r$changed_occurrences,', harmoniavirheita',r$evaluation$harmony,', yhteissointeja',r$evaluation$sonority_count,'\n')
  }
  invisible(x)
}

# Vientia varten: taydellinen rytmi ja saveljono verrataan, ei pelkkaa teeman nimea.
kaanoni_export_voices <- function(notes,octaves=FALSE) {
  voices <- sort(unique(notes$voice)); kept <- integer(); mapping <- list()
  for(v in voices) {
    a <- notes[notes$voice==v,]; a <- a[order(a$onset),]
    representative <- v; shift <- 0
    for(k in kept) {
      b <- notes[notes$voice==k,]; b <- b[order(b$onset),]
      if(nrow(a)!=nrow(b) || any(abs(a$onset-b$onset)>1e-8) ||
         any(abs(a$duration-b$duration)>1e-8)) next
      delta <- a$pitch-b$pitch
      if(length(unique(delta))==1 && (delta[1]==0 || (octaves && delta[1]%%12==0))) {
        representative <- k; shift <- delta[1]; break
      }
    }
    if(representative==v) kept <- c(kept,v)
    mapping[[length(mapping)+1L]] <- data.frame(original=v,representative=representative,
      output=match(representative,kept),semitones=shift)
  }
  mapping <- do.call(rbind,mapping)
  out <- notes[notes$voice %in% kept,]
  out$voice <- match(out$voice,kept)
  list(notes=out,mapping=mapping)
}

kaanoni_prepare_export <- function(result,merge_duplicates=TRUE,merge_octaves=FALSE) {
  result$results <- lapply(result$results,function(r) {
    converted <- kaanoni_export_voices(r$notes,octaves=merge_octaves)
    if(!merge_duplicates) {
      ids <- sort(unique(r$notes$voice))
      converted <- list(notes=r$notes,mapping=data.frame(original=ids,
        representative=ids,output=seq_along(ids),semitones=0))
    }
    r$export_notes <- converted$notes
    r$voice_map <- converted$mapping
    r$written_voices <- length(unique(converted$notes$voice))
    r
  })
  result
}

kaanoni_write <- function(result,path,gap_bars=1,time=c(4,4),tempo=60,best_n=NULL,
    merge_duplicates=TRUE,merge_octaves=FALSE) {
  if(!length(result$results)) stop('Ei vietavia ratkaisuja.')
  result$results <- kaanoni_rank(result$results,if(is.null(result$options$ranking)) 'balanced' else result$options$ranking)
  if(!is.null(best_n)) {
    if(length(best_n)!=1 || !is.finite(best_n) || best_n<1 || best_n!=floor(best_n))
      stop('Vietavien ratkaisujen maara on positiivinen kokonaisluku.')
    result$results <- head(result$results,best_n)
  }
  if(length(time)!=2 || any(time<=0) || gap_bars<0 || tempo<=0) stop('Virheelliset vientiasetukset.')
  result <- kaanoni_prepare_export(result,merge_duplicates,merge_octaves)
  bar <- time[1]*4/time[2]; cursor <- 0; all <- list(); labels <- list()
  for(i in seq_along(result$results)) {
    r <- result$results[[i]]; n <- r$export_notes
    labels[[length(labels)+1L]] <- data.frame(onset=cursor,text=paste0('Ratkaisu ',i,'; muutokset ',r$changed_sources,
      '/',r$changed_occurrences,'; virheet ',r$evaluation$harmony,'; ',
      paste(apply(r$settings,1,function(s) paste(s,collapse='/')),collapse=' | '),
      '; kirjoitettu aania ',r$written_voices,'/',length(unique(r$notes$voice)),
      if(any(r$voice_map$original!=r$voice_map$representative)) paste0('; yhdistetty ',
        paste(with(subset(r$voice_map,original!=representative),
          paste0(original,'->',representative,' (',semitones,' st)')),collapse=', ')) else ''))
    if(!is.null(r$phrases) && length(r$phrases)>1) for(j in 2:length(r$phrases)) {
      ph <- r$phrases[[j]]
      labels[[length(labels)+1L]] <- data.frame(onset=cursor+ph$start,
        text=paste0('Ratkaisu ',i,', sae ',j,'; transpositio ',ph$settings$transposition[ph$settings$voice==2],
          '; virheet ',ph$evaluation$harmony,'; ',
          paste(apply(ph$settings,1,function(s) paste(s,collapse='/')),collapse=' | ')))
    }
    n$onset <- n$onset+cursor; n$solution <- i
    all[[i]] <- n[,c('onset','duration','pitch','voice','solution')]
    cursor <- ceiling((max(n$onset+n$duration)-1e-8)/bar)*bar+gap_bars*bar
  }
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp,recursive=TRUE))
  write.csv(do.call(rbind,all),file.path(tmp,'notes.csv'),row.names=FALSE)
  write.csv(do.call(rbind,labels),file.path(tmp,'labels.csv'),row.names=FALSE)
  writeLines(paste(time[1],time[2],tempo),file.path(tmp,'settings.txt'))
  .notation_run('write',normalizePath(path,mustWork=FALSE),tmp)
  # Koneluettava raportti sisaltaa myos kaikkien virheiden paikat.
  saveRDS(result,paste0(path,'.rds'))
  report <- unlist(lapply(seq_along(result$results),function(i) {
    r <- result$results[[i]]
    c('Virhe- ja muutosluvut kuvaavat alkuperaista hakuratkaisua ennen aanten yhdistamista.',
      if(!is.null(r$phrase_transpositions)) paste('Sakeiden transpositiot:',paste(r$phrase_transpositions,collapse=', ')),
      if(!is.null(r$boundary_movement)) paste('Saerajojen savelaskelliike yhteensa:',r$boundary_movement),
      paste('Kirjoitettuja aania:',r$written_voices),capture.output(print(r$voice_map,row.names=FALSE)),
      paste('Ratkaisu',i,'Tyyppi:',r$kind,'Korjaushaun tila:',r$status),capture.output(print(r$settings,row.names=FALSE)),
      paste('Yhteissointeja:',r$evaluation$sonority_count,'Laskutapa:',result$options$sonority_mode),
      paste('Muutettuja lahdesavelia:',r$changed_sources,'Nuottiesiintymia:',r$changed_occurrences),
      capture.output(print(r$changes,row.names=FALSE)),capture.output(print(r$evaluation$errors,row.names=FALSE)))
  }))
  writeLines(c(paste('Haun tila:',result$status),report),paste0(path,'.txt'))
  invisible(path)
}
