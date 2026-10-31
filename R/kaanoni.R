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
    max_candidates=10000L, max_results=30L, seconds=30,
    dissonances=FALSE, min_sonorities=0L, sonority_mode=c('intervals','chords'),
    allow_weak_steps=FALSE, strong_beat_period=2,
    metric_mode=c('auto','manual'), meter=NULL) {
  if(!voices %in% 2:4 || length(voices)!=1) stop('Aanimaara on 2–4.')
  mode <- match.arg(mode,c('canon','counterpoint')); repair_mode <- match.arg(repair_mode,c('strict','free'))
  if(!is.logical(allow_weak_steps) || length(allow_weak_steps)!=1 || is.na(allow_weak_steps) ||
     length(strong_beat_period)!=1 || !is.finite(strong_beat_period) || strong_beat_period<=0)
    stop('Tarkista epaiskujen asteikkoliikkeen asetukset.')
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

kaanoni_evaluate <- function(n, o) {
  empty <- data.frame(type=character(),start=numeric(),end=numeric(),voices=character(),notes=character(),detail=character())
  errors <- empty
  add <- function(type,a,b,vs,rs,detail) {
    errors[nrow(errors)+1,] <<- list(type,a,b,paste(vs,collapse=','),paste(rs,collapse=','),detail)
  }
  times <- sort(unique(round(c(n$onset,n$onset+n$duration),9)))
  strong_times <- if(isTRUE(o$allow_weak_steps) && identical(o$metric_mode,'auto'))
    .metric_strong_times(o$meter,max(times)) else NULL
  overlap <- 0; previous <- NULL
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
      q <- active[pair,]; rs <- rows[pair]; int <- abs(diff(q$pitch))%%12
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
      if(o$forbid_parallels && !is.null(previous)) {
        old <- previous[match(q$voice,previous$voice),,drop=FALSE]
        if(!anyNA(old$pitch)) {
          movement <- q$pitch-old$pitch
          if(all(movement!=0) && prod(movement)>0 && int %in% c(0,7) && abs(diff(old$pitch))%%12==int)
            add('parallel',t,end,q$voice,rs,'Rinnakkainen kvintti tai oktaavi')
        }
      }
    }
    if(nrow(active)>1 && !is.null(o$allowed_sets) && !kaanoni_pc_key(active$pitch) %in% o$allowed_sets)
      add('set',t,end,active$voice,rows,kaanoni_pc_key(active$pitch))
    previous <- active
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

kaanoni_generate <- function(themes, options=kaanoni_options()) {
  o <- options; started <- proc.time()[3]; deadline <- started+o$seconds
  # Varaa korjauksille osa kokonaisajasta, mutta etsi ensin korjaamattomat ratkaisut.
  scan_deadline <- if(o$repair) started+o$seconds*0.6 else deadline
  pending <- list(); repair_attempted <- 0L
  results <- list(); seen <- new.env(hash=TRUE,parent=emptyenv()); tested <- 0L; repair_limited <- 0L
  exhausted <- TRUE
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
  search <- function(n,settings,root,voice) {
    if(proc.time()[3]>=scan_deadline || tested>=o$max_candidates) { exhausted <<- FALSE; return() }
    if(voice>o$voices) {
      tested <<- tested+1L; ev <- kaanoni_evaluate(n,o)
      if(ev$overlap+1e-8<o$min_overlap || ev$sonority_count<o$min_sonorities) return()
      if(ev$valid) add_result(n,ev,numeric(),settings,'unchanged') else if(o$repair) {
        pending[[length(pending)+1L]] <<- list(notes=n,evaluation=ev,settings=settings,meter=o$meter,resume=NULL)
      }
      return()
    }
    choices <- if(o$mode=='canon') root else seq_along(themes)
    for(t in choices) for(f in o$forms) for(a in o$factors) for(d in o$delays) for(p in o$transpositions) {
      if(proc.time()[3]>=scan_deadline || tested>=o$max_candidates) { exhausted <<- FALSE; return() }
      x <- .variation(themes[[t]],f,a,d,p,voice)
      # Identtiset aanet samassa ajassa eivat muodosta uutta kontrapunktia.
      duplicate <- any(vapply(unique(n$voice),function(v) {
        old <- n[n$voice==v,]
        nrow(old)==nrow(x) && all(abs(old$onset-x$onset)<1e-8) &&
          all(abs(old$duration-x$duration)<1e-8) && all(old$pitch==x$pitch)
      },logical(1)))
      if(duplicate) next
      if(!o$repair && any(x$pitch<o$ranges[[voice]][1] | x$pitch>o$ranges[[voice]][2])) next
      combined <- rbind(n,x)
      if(!o$repair) {
        partial <- kaanoni_evaluate(combined,o)
        if(partial$hard>0 || (is.null(o$allowed_sets) && partial$harmony>o$max_harmony_errors)) next
      }
      search(combined,rbind(settings,data.frame(voice=voice,theme=themes[[t]]$name,form=f,factor=a,delay=d,transposition=p)),root,voice+1L)
    }
  }
  for(t in seq_along(themes)) {
    o$meter <- themes[[t]]$measures
    n <- .variation(themes[[t]],voice=1)
    settings <- data.frame(voice=1,theme=themes[[t]]$name,form='O',factor=1,delay=0,transposition=0)
    search(n,settings,t,2L)
    if(!exhausted) break
  }
  # Korjaa ehdokkaita vuorotellen. Vaikea ensimmainen ehdokas ei saa koko aikabudjettia.
  if(length(pending)) {
    rank <- do.call(rbind,lapply(pending,function(x) c(x$evaluation$hard,x$evaluation$harmony)))
    pending <- pending[do.call(order,as.data.frame(rank))]
    active <- seq_along(pending)
    while(length(active) && proc.time()[3]<deadline) {
      next_round <- integer()
      for(i in active) {
        if(proc.time()[3]>=deadline) break
        candidate <- pending[[i]]
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
  found_total <- length(results)
  unchanged_found <- sum(vapply(results,function(x) x$status=='unchanged',logical(1)))
  if(length(results)) {
    rank <- do.call(rbind,lapply(results,function(x) c(x$evaluation$harmony,x$changed_sources,x$movement,-x$consonance)))
    results <- results[head(do.call(order,as.data.frame(rank)),o$max_results)]
  }
  structure(list(results=results,tested=tested,found_total=found_total,unchanged_found=unchanged_found,repair_limited=repair_limited,
    status=if(exhausted && !o$repair) 'exhaustive' else if(exhausted && repair_limited==0) 'geometry_exhausted_heuristic_repair' else 'search_limit',
    repair_attempted=repair_attempted,options=o),class='kaanoni_result')
}

print.kaanoni_result <- function(x,...) {
  cat('Ratkaisuja:',length(x$results),' Tarkastettuja kokonaisuuksia:',x$tested,' Tila:',x$status,'\n')
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

kaanoni_write <- function(result,path,gap_bars=1,time=c(4,4),tempo=60) {
  if(!length(result$results)) stop('Ei vietavia ratkaisuja.')
  if(length(time)!=2 || any(time<=0) || gap_bars<0 || tempo<=0) stop('Virheelliset vientiasetukset.')
  bar <- time[1]*4/time[2]; cursor <- 0; all <- list(); labels <- list()
  for(i in seq_along(result$results)) {
    r <- result$results[[i]]; n <- r$notes
    labels[[i]] <- data.frame(onset=cursor,text=paste0('Ratkaisu ',i,'; muutokset ',r$changed_sources,
      '/',r$changed_occurrences,'; virheet ',r$evaluation$harmony,'; ',
      paste(apply(r$settings,1,function(s) paste(s,collapse='/')),collapse=' | ')))
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
    c(paste('Ratkaisu',i,'Tyyppi:',r$kind,'Korjaushaun tila:',r$status),capture.output(print(r$settings,row.names=FALSE)),
      paste('Yhteissointeja:',r$evaluation$sonority_count,'Laskutapa:',result$options$sonority_mode),
      paste('Muutettuja lahdesavelia:',r$changed_sources,'Nuottiesiintymia:',r$changed_occurrences),
      capture.output(print(r$changes,row.names=FALSE)),capture.output(print(r$evaluation$errors,row.names=FALSE)))
  }))
  writeLines(c(paste('Haun tila:',result$status),report),paste0(path,'.txt'))
  invisible(path)
}
