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

.weak_scale_note <- function(on, en, pitch, r, prev, next_note, t, period, strong_times) {
  weak <- function(x) if(is.null(strong_times)) abs(x/period-round(x/period))>1e-8 else
    !any(abs(strong_times-x)<1e-8)
  if(is.na(prev) || is.na(next_note) || !weak(t) || !weak(on[r])) return(FALSE)
  if(abs(en[prev]-on[r])>1e-8 || abs(en[r]-on[next_note])>1e-8) return(FALSE)
  next_strong <- if(is.null(strong_times)) (floor(on[r]/period+1e-8)+1)*period else
    min(c(strong_times[strong_times>on[r]+1e-8],Inf))
  if(en[r]>next_strong+1e-8) return(FALSE)
  a <- pitch[r]-pitch[prev]; b <- pitch[next_note]-pitch[r]
  abs(a) %in% 1:2 && abs(b) %in% 1:2 && a*b>0
}

# Hakumuunnokset ovat valmiiksi aikajarjestyksessa.
.same_octave_voice <- function(a,b) {
  if(!nrow(a) || nrow(a)!=nrow(b)) return(FALSE)
  d <- a$pitch-b$pitch
  d[1]%%12==0 && all(d==d[1]) && all(abs(a$onset-b$onset)<=1e-8) &&
    all(abs(a$duration-b$duration)<=1e-8)
}

.octave_match <- function(on, duration, pitch, a, b) {
  if(!length(a) || length(a)!=length(b)) return(FALSE)
  if(any(abs(on[a]-on[b])>1e-8) || any(abs(duration[a]-duration[b])>1e-8)) return(FALSE)
  d <- pitch[a]-pitch[b]
  d[1]%%12==0 && all(d==d[1])
}

kaanoni_evaluate <- function(n, o) .evaluate(n,o)

.empty_errors <- data.frame(type=character(),start=numeric(),end=numeric(),
                            voices=character(),notes=character(),detail=character())

.evaluate <- function(n, o, pair_only=FALSE) {
  on <- n$onset; duration <- n$duration; en <- on+duration
  pitch <- n$pitch; voice <- n$voice
  by_voice <- split(seq_along(voice),voice)
  ordered <- lapply(by_voice,function(rows) rows[order(on[rows])])
  prev <- following <- weak_prev <- weak_next <- rep(NA_integer_,length(on))
  if(o$dissonances || isTRUE(o$allow_weak_steps)) {
    for(rows in by_voice) {
      prev[rows] <- c(NA_integer_,head(rows,-1L))
      following[rows] <- c(tail(rows,-1L),NA_integer_)
    }
    for(rows in ordered) {
      weak_prev[rows] <- c(NA_integer_,head(rows,-1L))
      weak_next[rows] <- c(tail(rows,-1L),NA_integer_)
    }
  }
  err <- list()
  add <- function(type,a,b,vs,rs,detail) {
    err[[length(err)+1L]] <<- list(type=type,start=a,end=b,voices=paste(vs,collapse=','),
                                 notes=paste(rs,collapse=','),detail=detail)
  }
  if(length(ordered)>1L) for(pair in combn(seq_along(ordered),2,simplify=FALSE)) {
    a <- ordered[[pair[1]]]; b <- ordered[[pair[2]]]
    if(.octave_match(on,duration,pitch,a,b)) add('doubling',min(on[a]),max(en[a]),
      c(voice[a[1]],voice[b[1]]),sort(c(a,b)),'Sama teema samanaikaisesti unisonossa tai oktaavissa')
  }
  times <- sort(unique(round(c(on,en),9)))
  raw_times <- if(!pair_only) sort(unique(c(on,en))) else NULL
  # Pidä konsonanssin alkuperäinen pyöristämätön aikaruudukko.
  timeline <- if(pair_only || identical(times,raw_times)) times else sort(unique(c(times,raw_times)))
  strong_times <- if(length(times) && isTRUE(o$allow_weak_steps) && identical(o$metric_mode,'auto'))
    .metric_strong_times(o$meter,max(times)) else NULL
  overlap <- good <- total <- 0
  parallel_history <- new.env(hash=TRUE,parent=emptyenv())
  gap_tolerance <- if(is.null(o$parallel_gap_tolerance)) 0.125 else o$parallel_gap_tolerance
  counted_pairs <- new.env(hash=TRUE,parent=emptyenv())
  counted_chords <- new.env(hash=TRUE,parent=emptyenv())
  allowed <- (0:11) %in% o$allowed_intervals
  starts <- order(on); ends <- order(en)
  thresholds <- timeline+1e-8
  started <- findInterval(thresholds,on[starts])
  ended <- findInterval(thresholds,en[ends])
  start_at <- end_at <- 0L
  rows <- integer()
  rule_at <- match(timeline,times,nomatch=0L)
  raw_at <- if(!pair_only) match(timeline,raw_times,nomatch=0L)
  for(k in seq_along(timeline)) {
    i <- rule_at[k]; j <- if(pair_only) 0L else raw_at[k]
    rules <- i>0L && i<length(times)
    consonance <- j>0L && j<length(raw_times)
    if(!rules && !consonance) next
    t <- timeline[k]
    if(started[k]>start_at) {
      rows <- sort(c(rows,starts[seq.int(start_at+1L,started[k])]))
      start_at <- started[k]
    }
    if(ended[k]>end_at) {
      rows <- rows[!rows %in% ends[seq.int(end_at+1L,ended[k])]]
      end_at <- ended[k]
    }
    size <- length(rows)
    if(rules && !pair_only && size==o$voices) {
      overlap <- overlap+times[i+1L]-t
      if(o$voices>=3L) assign(paste(rows,collapse=','),TRUE,counted_chords)
    }
    if(size<2L) next
    end <- if(rules) times[i+1L]
    good_pairs <- pair_count <- 0L
    for(a in seq_len(size-1L)) for(b in seq.int(a+1L,size)) {
      rs <- rows[c(a,b)]
      if(voice[rs[1]]>voice[rs[2]]) rs <- rev(rs)
      p <- pitch[rs]; vs <- voice[rs]
      int <- abs(p[2]-p[1])%%12
      ok <- allowed[int+1L]
      if(consonance) {
        pair_count <- pair_count+1L
        good_pairs <- good_pairs+ok
      }
      if(!rules) next
      if(!pair_only) assign(paste(rows[a],rows[b],sep=','),TRUE,counted_pairs)
      if(!ok && isTRUE(o$allow_weak_steps)) for(r in rs) {
        if(.weak_scale_note(on,en,pitch,r,weak_prev[r],weak_next[r],t,o$strong_beat_period,strong_times)) {
          ok <- TRUE
          break
        }
      }
      if(!ok && o$dissonances) for(r in rs) {
        before <- prev[r]; after <- following[r]
        if(!is.na(before) && !is.na(after) && abs(t-round(t))>1e-8 && abs(on[r]-t)<1e-8) {
          left <- pitch[r]-pitch[before]; right <- pitch[after]-pitch[r]
          other <- rs[if(r==rs[1]) 2L else 1L]
          held <- on[other]<=on[before]+1e-8 && en[other]>on[after]+1e-8
          neighbors <- (left*right>0 || left == -right) && abs(left) %in% 1:2 && abs(right) %in% 1:2
          endpoints <- all(allowed[abs(pitch[c(before,after)]-pitch[other])%%12+1L])
          if(held && neighbors && endpoints) ok <- TRUE
        }
      }
      if(!ok) add('harmony',t,end,vs,rs,paste0('interval=',int))
      if(o$forbid_crossing && p[1]<p[2]) add('crossing',t,end,vs,rs,'Aanten jarjestys rikkoutuu')
      if(o$forbid_parallels) {
        key <- paste(vs,collapse=',')
        old <- get0(key,parallel_history,inherits=FALSE)
        if(!is.null(old)) {
          movement <- p-old$pitch
          if(t-old$until<=gap_tolerance+1e-8 && all(movement!=0) &&
             prod(movement)>0 && int %in% c(0,7) && old$interval==int)
            add('parallel',t,end,vs,rs,'Rinnakkainen kvintti tai oktaavi (myos lyhyen artikulaatiokatkon yli)')
        }
        assign(key,list(pitch=p,interval=int,until=min(en[rs])),parallel_history)
      }
    }
    if(consonance) {
      w <- raw_times[j+1L]-t
      total <- total+w; good <- good+w*(good_pairs/pair_count)
    }
    if(rules && !pair_only && !is.null(o$allowed_sets)) {
      set <- kaanoni_pc_key(pitch[rows])
      if(!set %in% o$allowed_sets) add('set',t,end,voice[rows],rows,set)
    }
  }
  for(v in seq_len(o$voices)) {
    rs <- by_voice[[as.character(v)]]
    p <- pitch[rs]
    bad <- rs[p<o$ranges[[v]][1] | p>o$ranges[[v]][2]]
    for(r in bad) add('range',on[r],en[r],v,r,'Aaniala')
    for(j in which(abs(diff(p))>o$max_leap)) {
      r <- rs[j+1L]
      add('leap',on[r],en[r],v,rs[c(j,j+1L)],'Liian suuri hyppy')
    }
  }
  # Yhtenainen sama rikkomus lasketaan kerran.
  errors <- .empty_errors
  if(length(err)) {
    chr <- function(key) vapply(err,`[[`,character(1),key)
    num <- function(key) vapply(err,`[[`,numeric(1),key)
    type <- chr('type'); voices <- chr('voices'); detail <- chr('detail')
    start <- num('start'); end <- num('end'); notes <- chr('notes')
    ix <- order(type,voices,detail,start)
    keep <- rep(TRUE,length(ix)); last <- ix[1L]
    for(i in ix[-1L]) {
      if(type[i]!='parallel' && type[i]==type[last] && voices[i]==voices[last] &&
         detail[i]==detail[last] && abs(start[i]-end[last])<1e-8) {
        end[last] <- end[i]
        if(!pair_only) notes[last] <- paste(unique(unlist(strsplit(notes[c(last,i)],',',fixed=TRUE))),collapse=',')
        keep[i] <- FALSE
      } else last <- i
    }
    ix <- ix[keep[ix]]
    if(pair_only) {
      hard <- type[ix]!='harmony'
      first <- if(any(hard)) ix[which(hard)[1L]] else ix[1L]
      ix <- first
    }
    errors <- data.frame(type=type[ix],start=start[ix],end=end[ix],
      voices=voices[ix],notes=notes[ix],detail=detail[ix])
    if(pair_only) return(list(errors=errors,harmony=sum(!hard),hard=sum(hard)))
  }
  if(pair_only) return(list(errors=errors,harmony=0L,hard=0L))
  harmony <- sum(errors$type %in% c('harmony','set')); hard <- nrow(errors)-harmony
  interval_count <- length(counted_pairs); chord_count <- length(counted_chords)
  sonority_mode <- if(is.null(o$sonority_mode) || o$voices==2L) 'intervals' else o$sonority_mode
  minimum <- if(is.null(o$min_sonorities)) 0L else o$min_sonorities
  sonority_count <- if(sonority_mode=='intervals') interval_count else chord_count
  list(errors=errors,harmony=harmony,hard=hard,overlap=overlap,
       interval_count=interval_count,chord_count=chord_count,sonority_count=sonority_count,
       valid=hard==0 && harmony<=o$max_harmony_errors && overlap+1e-8>=o$min_overlap && sonority_count>=minimum,
       consonance=if(total>0) good/total else NA_real_)
}

.repair <- function(n,o,deadline,resume=NULL,batch_nodes=Inf,initial=NULL) {
  n$key <- if(o$repair_mode=='strict') n$source else paste(n$voice,n$source,sep='/')
  keys <- unique(n$key); zero <- setNames(rep(0,length(keys)),keys)
  build <- function(d) { a <- n; a$pitch <- a$base+(if(o$repair_mode=='strict') a$sign else 1)*d[a$key]; a }
  score <- function(e,d) c(e$hard,max(0,e$harmony-o$max_harmony_errors),sum(d!=0),sum(abs(d)),max(abs(d)))
  if(is.null(initial)) initial <- if(is.null(resume)) kaanoni_evaluate(n,o) else resume$initial
  if(is.null(resume)) {
    d <- zero; ev <- initial
      queue <- new.env(parent=emptyenv(),hash=TRUE)
    order_ids <- integer()
    ranks <- matrix(0,ncol=5L,nrow=32L); inserted <- 0L
    seen <- new.env(hash=TRUE,parent=emptyenv())
    assign(paste(d,collapse=','),TRUE,seen); visited <- 0L
  } else {
    queue <- resume$queue; seen <- resume$seen; visited <- resume$visited
    order_ids <- resume$order_ids; ranks <- resume$ranks; inserted <- resume$inserted
  }
  before <- function(a,b) {
    different <- which(ranks[a,]!=ranks[b,])
    if(length(different)) ranks[a,different[1L]]<ranks[b,different[1L]] else a<b
  }
  size <- length(order_ids)
  push <- function(d,e) {
    inserted <<- inserted+1L
    id <- inserted
    if(id>nrow(ranks)) ranks <<- rbind(ranks,matrix(0,ncol=5L,nrow=nrow(ranks)))
    ranks[id,] <<- score(e,d)
    assign(as.character(id),list(d=d,e=e),queue)
    size <<- size+1L
    at <- size
    while(at>1L) {
      parent <- at%/%2L
      if(!before(id,order_ids[parent])) break
      order_ids[at] <<- order_ids[parent]
      at <- parent
    }
    order_ids[at] <<- id
  }
  pop <- function() {
    id <- order_ids[1L]
    last <- order_ids[size]
    size <<- size-1L
    at <- 1L
    while(2L*at<=size) {
      child <- 2L*at
      if(child<size && before(order_ids[child+1L],order_ids[child])) child <- child+1L
      if(!before(order_ids[child],last)) break
      order_ids[at] <<- order_ids[child]
      at <- child
    }
    if(size) order_ids[at] <<- last
    key <- as.character(id)
    state <- get(key,queue,inherits=FALSE)
    rm(list=key,envir=queue)
    state
  }
  batch_end <- visited+batch_nodes
  if(is.null(resume)) push(d,ev)
  while(size && visited<o$repair_nodes && visited<batch_end && proc.time()[3]<deadline) {
    state <- pop()
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
      push(nextd,kaanoni_evaluate(a,o))
    }
  }
  list(notes=n,evaluation=initial,delta=zero,nodes=visited,
       status=if(size) 'repair_limit' else 'repair_exhausted_local',
       resume=list(queue=queue,seen=seen,visited=visited,initial=initial,
         order_ids=head(order_ids,size),ranks=ranks,inserted=inserted))
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

# Sama soiva ratkaisu voi syntyä eri ääninumeroilla tai alkuhiljaisuudella.
.solution_key <- function(notes) {
  relative_onset <- round(notes$onset - min(notes$onset), 8)
  duration <- round(notes$duration, 8)
  voice_keys <- vapply(split(seq_len(nrow(notes)), notes$voice), function(rows) {
    rows <- rows[order(relative_onset[rows], duration[rows], notes$pitch[rows])]
    paste(relative_onset[rows], duration[rows], notes$pitch[rows], sep=',', collapse=';')
  }, character(1))
  paste(sort(voice_keys), collapse='|')
}

kaanoni_generate <- function(themes, options=kaanoni_options(), progress=NULL)
  .generate(themes,options,progress)

.generate <- function(themes,options,progress,checkpoint=NULL,keep_checkpoint=FALSE) {
  o <- options; started <- proc.time()[3]; deadline <- started+o$seconds
  # Varaa korjauksille osa kokonaisajasta, mutta etsi ensin korjaamattomat ratkaisut.
  scan_deadline <- if(o$repair) started+o$seconds*0.6 else deadline
  pending <- list(); repair_attempted <- 0L
  results <- list(); seen <- new.env(hash=TRUE,parent=emptyenv()); tested <- 0L; repair_limited <- 0L
  exhausted <- TRUE
  if(!is.null(progress) && !is.function(progress)) stop('progress on funktio tai NULL.')
  expanded <- 0L; current_theme <- 0L; last_progress <- -Inf
  emit_progress <- function(stage,force=FALSE,now=NULL) {
    if(is.null(progress)) return(invisible(NULL))
    now <- if(is.null(now)) proc.time()[3] else now
    if(!force && now-last_progress<0.2) return(invisible(NULL))
    last_progress <<- now
    progress(list(stage=stage,elapsed=unname(now-started),seconds=o$seconds,
      budget_fraction=min(1,(now-started)/o$seconds),
      theme=current_theme,themes=length(themes),expanded=expanded,
      tested=tested,candidate_limit=o$max_candidates,found=if(stage=='done') found_total else length(results),
      repair_attempted=repair_attempted,repair_candidates=length(pending)))
    invisible(NULL)
  }
  if(!is.null(checkpoint)) {
    results <- checkpoint$results; seen <- checkpoint$seen
    pending <- checkpoint$pending
  }
  emit_progress('start',TRUE)
  add_result <- function(n,ev,delta,settings,status) {
    hash <- .solution_key(n)
    if(exists(hash,seen,inherits=FALSE)) return(invisible(NULL))
    assign(hash,TRUE,seen)
    changed <- if(length(delta)) sum(delta!=0) else 0L
    occurrences <- sum(n$pitch!=n$base)
    movement <- if(length(delta)) sum(abs(delta)) else 0
    results[[length(results)+1L]] <<- list(notes=n,settings=settings,evaluation=ev,
      kind=if(o$mode=='canon' && o$repair_mode=='free' && occurrences>0) 'modified_counterpoint' else o$mode,
      changes=data.frame(source=names(delta)[delta!=0],semitones=unname(delta[delta!=0])),
      changed_sources=changed,changed_occurrences=occurrences,movement=movement,
      consonance=ev$consonance,status=status)
  }
  # Muunnokset muodostetaan laiskasti kerran kutakin aanipaikkaa varten.
  grid <- expand.grid(p=o$transpositions,d=o$delays,a=o$factors,f=o$forms,
    t=seq_along(themes),KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  # Pysaytettava haku kokeilee ensin alkuperaisia, lahella olevia muunnoksia.
  if(isTRUE(o$stop_after_results)) grid <- grid[with(grid,
    order(match(f,c('O','I','R','RI')),abs(log(a)),d,abs(p))),,drop=FALSE]
  if(!is.null(checkpoint)) grid <- checkpoint$grid
  variants <- if(is.null(checkpoint)) new.env(hash=TRUE,parent=emptyenv()) else checkpoint$variants
  pairs <- if(is.null(checkpoint)) new.env(hash=TRUE,parent=emptyenv()) else checkpoint$pairs
  variant_hits <- pair_hits <- temporal_rejects <- 0L
  reason_names <- c('doubling','range','overlap','parallel','crossing','leap','harmony_limit','sonority','set','other')
  rejected <- matrix(0L,nrow=length(themes),ncol=length(reason_names),dimnames=list(vapply(themes,function(t)t$name,character(1)),reason_names))
  examples <- list(); example_counts <- setNames(integer(length(reason_names)),reason_names)
  reject <- function(reason,id,voice,error=NULL) {
    if(!reason %in% reason_names) reason <- 'other'
    rejected[current_theme,reason] <<- rejected[current_theme,reason]+1L
    if(example_counts[reason]>=2L) return(invisible(NULL))
    g <- grid[id,]
    examples[[length(examples)+1L]] <<- data.frame(theme=themes[[current_theme]]$name,
      reason=reason,voice=voice,form=g$f,factor=g$a,delay=g$d,transposition=g$p,
      time=if(is.null(error) || !nrow(error)) NA_real_ else error$start[1],
      detail=if(is.null(error) || !nrow(error)) '' else error$detail[1])
    example_counts[reason] <<- example_counts[reason]+1L
    invisible(NULL)
  }
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
  # Pino sailyttaa tahanastisten aanten yhteiset soivat jaksot.
  intersect_playing <- function(on,off,x) {
    other_on <- x$onset; other_off <- other_on+x$duration
    ts <- sort(unique(c(on,off,other_on,other_off)))
    active <- findInterval(ts,on)-findInterval(ts,off)+
      findInterval(ts,other_on)-findInterval(ts,other_off)==2L
    list(on=ts[active & !c(FALSE,head(active,-1L))],
         off=ts[!active & c(FALSE,head(active,-1L))])
  }
  pair_score <- function(left,right,left_id,right_id,lv,rv,root) {
    key <- paste(root,lv,rv,left_id,right_id,sep='/')
    if(exists(key,pairs,inherits=FALSE)) {
      pair_hits <<- pair_hits+1L
      return(get(key,pairs,inherits=FALSE))
    }
    n <- list(onset=c(left$onset,right$onset),duration=c(left$duration,right$duration),
              pitch=c(left$pitch,right$pitch),voice=c(left$voice,right$voice))
    e <- .evaluate(n,o,pair_only=TRUE)
    value <- c(hard=e$hard,harmony=e$harmony)
    attr(value,'reason') <- if(e$hard>0) e$errors$type[1L] else 'harmony_limit'
    attr(value,'example') <- e$errors
    assign(key,value,pairs); value
  }
  # Jokaisen teeman DFS-pino ja seuraavan muunnoksen indeksi sailyvat hakuvuorojen valilla.
  roots <- if(!is.null(checkpoint)) checkpoint$roots else lapply(seq_along(themes),function(t) {
    n <- .variation(themes[[t]],voice=1)
    settings <- data.frame(voice=1,theme=themes[[t]]$name,form='O',factor=1,delay=0,transposition=0)
    list(stack=list(list(parts=list(n),on=n$onset,off=n$onset+n$duration,settings=settings,ids=0L,next_id=1L)),
      choices=if(o$mode=='canon') which(grid$t==t) else seq_len(nrow(grid)))
  })
  now <- started
  while(now<scan_deadline && any(vapply(roots,function(x) length(x$stack)>0,logical(1)))) {
    if(tested>=o$max_candidates || enough()) break
    for(root in seq_along(roots)) {
      now <- proc.time()[3]
      if(now>=scan_deadline || tested>=o$max_candidates || enough()) break
      if(!length(roots[[root]]$stack)) next
      current_theme <- root
      first_visit <- !root %in% roots_visited
      roots_visited <- union(roots_visited,root)
      if(first_visit) emit_progress('scan',TRUE,now=now)
      o$meter <- themes[[root]]$measures
      slice_end <- min(scan_deadline,now+0.10)
      steps <- 0L
      while(length(roots[[root]]$stack) && steps<100L && tested<o$max_candidates && !enough()) {
        if(steps%%16L==0L) {
          if(steps>0L) now <- proc.time()[3]
          if(now>=slice_end) break
          emit_progress('scan',now=now)
        }
        depth <- length(roots[[root]]$stack)
        frame <- roots[[root]]$stack[[depth]]
        ids <- roots[[root]]$choices
        if(frame$next_id>length(ids)) {
          roots[[root]]$stack <- roots[[root]]$stack[-depth]
          next
        }
        id <- ids[frame$next_id]
        roots[[root]]$stack[[depth]]$next_id <- frame$next_id+1L
        steps <- steps+1L; expanded <- expanded+1L
        voice <- length(frame$ids)+1L
        x <- get_variant(id,voice)
        duplicate <- any(vapply(frame$parts,.same_octave_voice,logical(1),b=x))
        if(duplicate) {reject('doubling',id,voice);next}
        if(!o$repair && any(x$pitch<o$ranges[[voice]][1] | x$pitch>o$ranges[[voice]][2])) {reject('range',id,voice);next}
        playing <- intersect_playing(frame$on,frame$off,x)
        # Lisattava aani voi vain vahentaa kaikkien aanten yhteista soivaa aikaa.
        if(sum(playing$off-playing$on)+1e-8<o$min_overlap) {
          temporal_rejects <- temporal_rejects+1L;reject('overlap',id,voice);next
        }
        if(!o$repair) {
          harmony <- 0L; bad <- FALSE
          for(v in seq_len(voice-1L)) {
            e <- pair_score(frame$parts[[v]],x,frame$ids[v],id,v,voice,root)
            harmony <- harmony+e['harmony']
            if(e['hard']>0 || harmony>o$max_harmony_errors) { bad <- TRUE; break }
          }
          if(bad) {reject(if(e['hard']>0) attr(e,'reason') else 'harmony_limit',id,voice,attr(e,'example'));next}
          # Vanhojen aaniparien virheetkin kuluttavat yhteista virhebudjettia.
          total_harmony <- (if(is.null(frame$harmony)) 0 else frame$harmony)+harmony
          if(total_harmony>o$max_harmony_errors) {reject('harmony_limit',id,voice);next}
        } else total_harmony <- 0
        g <- grid[id,]
        settings <- rbind(frame$settings,data.frame(voice=voice,theme=themes[[g$t]]$name,
          form=g$f,factor=g$a,delay=g$d,transposition=g$p))
        if(voice==o$voices) {
          tested <- tested+1L
          # Lopullinen tarkistus on aina sama kaikissa hakutavoissa.
          combined <- do.call(rbind,c(frame$parts,list(x)))
          ev <- kaanoni_evaluate(combined,o)
          if(ev$sonority_count<o$min_sonorities) {reject('sonority',id,voice);next}
          if(ev$valid) add_result(combined,ev,numeric(),settings,'unchanged') else if(o$repair) {
            pending[[length(pending)+1L]] <- list(notes=combined,evaluation=ev,
              settings=settings,meter=o$meter,resume=NULL)
          } else {
            hard_rows <- which(!ev$errors$type %in% c('harmony','set'))
            harmony_rows <- which(ev$errors$type=='harmony')
            set_rows <- which(ev$errors$type=='set')
            if(length(hard_rows)) {
              row <- hard_rows[1L]; reason <- ev$errors$type[row]
            } else if(length(harmony_rows)>o$max_harmony_errors) {
              row <- harmony_rows[1L]; reason <- 'harmony_limit'
            } else if(length(set_rows)) {
              row <- set_rows[1L]; reason <- 'set'
            } else {
              row <- integer(); reason <- 'other'
            }
            reject(reason,id,voice,ev$errors[row,,drop=FALSE])
          }
        } else {
          roots[[root]]$stack[[depth+1L]] <- list(parts=c(frame$parts,list(x)),on=playing$on,off=playing$off,settings=settings,
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
    active <- which(!vapply(pending,function(x) isTRUE(x$done),logical(1)))
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
                      resume=candidate$resume,batch_nodes=10L,initial=candidate$evaluation)
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
  }
  repair_limited <- sum(!vapply(pending,function(x) isTRUE(x$done),logical(1)))
  stopped_results <- enough()
  emit_progress('ranking',TRUE)
  found_total <- length(results)
  unchanged_found <- sum(vapply(results,function(x) x$status=='unchanged',logical(1)))
  next_checkpoint <- if(keep_checkpoint && (!exhausted || repair_limited>0L))
    list(roots=roots,grid=grid,variants=variants,pairs=pairs,pending=pending,results=results,seen=seen)
  if(length(results)) {
    results <- head(kaanoni_rank(results,if(is.null(o$ranking)) 'balanced' else o$ranking),o$max_results)
  }
  emit_progress('done',TRUE)
  result <- structure(list(elapsed=unname(proc.time()[3]-started),results=results,tested=tested,found_total=found_total,unchanged_found=unchanged_found,repair_limited=repair_limited,
    diagnostics=list(counts=colSums(rejected),by_theme=rejected,
      examples=if(length(examples)) do.call(rbind,examples) else data.frame(),
      termination=if(stopped_results) 'result_limit' else if(!exhausted && tested>=o$max_candidates) 'candidate_limit' else
        if(!exhausted) 'scan_time_limit' else if(repair_limited>0) 'repair_limit' else 'complete',
      repair_pending=length(pending),repair_attempted=repair_attempted,repair_limited=repair_limited),
    stats=list(expanded=expanded,roots_visited=roots_visited,variant_cache_hits=variant_hits,
      pair_cache_hits=pair_hits,temporal_rejects=temporal_rejects),
    status=if(stopped_results) 'result_limit' else if(exhausted && !o$repair) 'exhaustive' else if(exhausted && repair_limited==0) 'geometry_exhausted_heuristic_repair' else 'search_limit',
    repair_attempted=repair_attempted,options=o),class='kaanoni_result')
  if(keep_checkpoint) attr(result,'checkpoint') <- next_checkpoint
  result
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
  all <- list(); tested <- 0L; limited <- FALSE; attempted <- 0L; phrase_debug <- list(); child_debug <- list()
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
  first_complete_elapsed <- NA_real_
  search_phrase <- function(i,p,pass,time_budget,candidate_budget,checkpoint=NULL) {
    local <- options
    local$transpositions <- if(same_transposition) p else options$transpositions
    local$seconds <- max(0.001,min(deadline-proc.time()[3],time_budget))
    local$max_candidates <- candidate_budget
    local$max_results <- if(pass=='first_solution') 1L else options$max_results
    local$stop_after_results <- pass=='first_solution'
    callback <- if(is.null(progress)) NULL else function(event) {
      event$stage <- if(pass=='first_solution') 'phrase_seed' else 'phrase_improve'
      event$theme <- i;event$themes <- length(themes)
      event$elapsed <- unname(proc.time()[3]-started);event$seconds <- options$seconds
      event$budget_fraction <- min(1,event$elapsed/options$seconds)
      event$tested <- tested+event$tested;event$found <- length(all)
      event$transposition <- p;progress(event)
    }
    r <- .generate(themes[i],local,callback,checkpoint,keep_checkpoint=pass=='first_solution')
    attempted <<- attempted+1L;tested <<- tested+r$tested
    child_debug[[attempted]] <<- r$diagnostics
    phrase_debug[[attempted]] <<- data.frame(pass=pass,phrase=i,first_measure=themes[[i]]$first_measure,
      transposition=if(same_transposition) p else NA_real_,tested=r$tested,found=r$found_total,
      status=r$status,termination=r$diagnostics$termination)
    if(r$status=='search_limit') limited <<- TRUE
    r
  }
  for(group_index in seq_along(groups)) {
    p <- groups[group_index]
    if(proc.time()[3]>=deadline || tested>=options$max_candidates) {limited<-TRUE;break}
    pools <- checkpoints <- list(); complete <- TRUE
    # Ensin vain yksi ratkaisu jokaiseen sakeeseen. Jaetaan myos ehdokasraja.
    for(i in seq_along(themes)) {
      remaining <- deadline-proc.time()[3]
      candidates_left <- options$max_candidates-tested
      if(remaining<=0 || candidates_left<=0) {limited<-TRUE;complete<-FALSE;break}
      tasks_left <- (length(groups)-group_index)*length(themes)+length(themes)-i+1L
      r <- search_phrase(i,p,'first_solution',min(sub_budget,remaining),
        max(1L,floor(candidates_left/tasks_left)))
      if(!length(r$results)) {complete<-FALSE;break}
      pools[[i]] <- r$results
      checkpoints[i] <- list(attr(r,'checkpoint'))
    }
    if(!complete) next
    # Kokonainen ketju on jo turvassa ennen lisavaihtoehtojen hakua.
    seed <- combine(lapply(pools,function(pool)pool[[1]]),p)
    if(!.solution_key(seed$notes) %in% vapply(all,function(x) .solution_key(x$notes),character(1)))
      all <- c(all,list(seed))
    if(is.na(first_complete_elapsed)) first_complete_elapsed <- unname(proc.time()[3]-started)
    if(isTRUE(options$stop_after_results) && length(all)>=options$max_results) break
    # Parannuskierros ei saa kuluttaa muiden sakeiden / intervallien koko budjettia.
    for(i in seq_along(themes)) {
      remaining <- deadline-proc.time()[3]
      candidates_left <- options$max_candidates-tested
      if(remaining<=0 || candidates_left<=0) {limited<-TRUE;break}
      tasks_left <- (length(groups)-group_index)*length(themes)+length(themes)-i+1L
      if(is.null(checkpoints[[i]])) next
      quota <- floor(candidates_left/tasks_left)
      if(quota<1L) {limited<-TRUE;next}
      r <- search_phrase(i,p,'improvement',remaining/tasks_left,quota,checkpoints[[i]])
      checkpoints[i] <- list(NULL)
      pool <- c(pools[[i]],r$results)
      keys <- vapply(pool,function(x)paste(x$notes$voice,x$notes$onset,x$notes$duration,x$notes$pitch,collapse=';'),character(1))
      pools[[i]] <- head(kaanoni_rank(pool[!duplicated(keys)],options$ranking),options$max_results)
    }
    # Rajattu yhdistelmavalikoima: parhaat loydetyt kokonaisuudet kasvavat sae kerrallaan.
    paths <- lapply(pools[[1]],function(x)list(x))
    edge_pitch <- function(result,last=FALSE) {
      n <- result$notes
      vapply(1:2,function(v) {
        rows <- which(n$voice==v)
        at <- if(last) which.max(n$onset[rows]) else which.min(n$onset[rows])
        n$pitch[rows[at]]
      },numeric(1))
    }
    last_pitch <- lapply(pools[[1]],edge_pitch,last=TRUE)
    boundary <- numeric(length(paths))
    if(length(pools)>1) for(i in 2:length(pools)) {
      parent <- rep(seq_along(paths),each=length(pools[[i]]))
      choice <- rep(seq_along(pools[[i]]),times=length(paths))
      next_first <- lapply(pools[[i]],edge_pitch)
      next_last <- lapply(pools[[i]],edge_pitch,last=TRUE)
      scores <- lapply(seq_along(parent),function(k) {
        path <- c(paths[[parent[k]]],list(pools[[i]][[choice[k]]]))
        w <- vapply(path,function(r)r$evaluation$overlap,numeric(1))
        cs <- vapply(path,function(r)r$consonance,numeric(1));cs[!is.finite(cs)] <- 0
        list(index=k,evaluation=list(harmony=sum(vapply(path,function(r)r$evaluation$harmony,numeric(1))),overlap=sum(w)),
          changed_sources=sum(vapply(path,function(r)r$changed_sources,numeric(1))),
          movement=sum(vapply(path,function(r)r$movement,numeric(1))),
          consonance=if(sum(w)>0)weighted.mean(cs,w) else 0,
          boundary_movement=boundary[parent[k]]+sum(abs(last_pitch[[parent[k]]]-next_first[[choice[k]]])))
      })
      best <- head(kaanoni_rank(scores,options$ranking),options$max_results)
      keep <- vapply(best,function(x)x$index,integer(1))
      paths <- lapply(keep,function(k)c(paths[[parent[k]]],list(pools[[i]][[choice[k]]])))
      boundary <- vapply(best,function(x)x$boundary_movement,numeric(1))
      last_pitch <- next_last[choice[keep]]
    }
    all <- kaanoni_rank(c(all,lapply(paths,function(path)combine(path,p))),options$ranking)
    hashes <- vapply(all,function(x) .solution_key(x$notes),character(1))
    all <- all[!duplicated(hashes)]
    if(isTRUE(options$stop_after_results) && length(all)>=options$max_results) break
  }
  found <- length(all)
  termination <- if(isTRUE(options$stop_after_results) && found>=options$max_results) 'result_limit' else
    if(tested>=options$max_candidates) 'candidate_limit' else if(proc.time()[3]>=deadline) 'scan_time_limit' else
      if(limited) 'search_limit' else 'complete'
  all <- head(kaanoni_rank(all,options$ranking),options$max_results)
  attempts <- if(length(phrase_debug)) do.call(rbind,phrase_debug) else data.frame()
  latest <- if(nrow(attempts)) child_debug[!duplicated(attempts[c('phrase','transposition')],fromLast=TRUE)] else list()
  repair_attempted <- sum(vapply(child_debug,function(d)d$repair_attempted,numeric(1)))
  repair_pending <- sum(vapply(latest,function(d)d$repair_pending,numeric(1)))
  repair_limited <- sum(vapply(latest,function(d)d$repair_limited,numeric(1)))
  structure(list(results=all,options=options,tested=tested,found_total=found,
    unchanged_found=sum(vapply(all,function(r)r$changed_sources==0,logical(1))),
    repair_attempted=repair_attempted,repair_limited=repair_limited,elapsed=unname(proc.time()[3]-started),
    diagnostics=list(counts=if(length(child_debug)) Reduce('+',lapply(child_debug,function(d)d$counts)) else numeric(),
      phrase_attempts=attempts,
      children=child_debug,first_complete_elapsed=first_complete_elapsed,
      repair_pending=repair_pending,repair_attempted=repair_attempted,repair_limited=repair_limited,
      termination=termination),
    status=if(isTRUE(options$stop_after_results) && found>=options$max_results) 'result_limit' else
      if(termination!='complete') 'search_limit' else 'phrase_search_complete',phrase_count=length(themes),phrase_bars=bars,same_transposition=same_transposition),class='kaanoni_result')
}

kaanoni_debug <- function(result) {
  labels <- c(doubling='Unisono- tai oktaavikaksinnus',range='Aanialan ylitys',
    overlap='Liian lyhyt yhteissointi',parallel='Rinnakkaiset kvintit tai oktaavit',
    crossing='Aanten risteaminen',leap='Liian suuri melodinen hyppy',
    harmony_limit='Harmoniavirherajan ylitys',sonority='Liian vahan yhteissointeja',
    set='Kielletty sointujoukko',other='Muu hylkays')
  terms <- c(result_limit='Pyydetty ratkaisumaara tayttyi',candidate_limit='Ehdokasraja tayttyi',
    scan_time_limit='Muunnoshaun aikabudjetti tayttyi',repair_limit='Korjaushaku jai kesken',
    search_limit='Osa saehauista jai kesken',complete='Valittu haku valmistui')
  d <- result$diagnostics
  if(is.null(d)) return('Tasta vanhasta hakutuloksesta ei ole diagnostiikkaa. Tee uusi haku.')
  lines <- c(paste('Haun tila:',result$status),paste('Lopetussyy:',terms[d$termination]),
    paste('Aikaraja:',result$options$seconds,'s; ehdokasraja:',result$options$max_candidates),
    paste('Virheraja:',result$options$max_harmony_errors,'; suurin hyppy:',result$options$max_leap,'puolisavelaskelta'),
    paste('Yhteissoinnin vahimmaiskesto:',result$options$min_overlap,'neljasosaa; vahimmaismaara:',result$options$min_sonorities),
    paste('Rinnakkaiset kielletty:',result$options$forbid_parallels,'; korjaus:',result$options$repair),
    paste('Tutkittuja kokonaisia ehdokkaita:',result$tested,'; loytyneita:',result$found_total),
    'Hylkaysluvut kuvaavat haun karsittuja haaroja, eivat kaikkia mahdollisia kokonaisratkaisuja.',
    'Kullekin karsinnalle kirjataan ensimmainen ratkaiseva syy; samalla ehdokkaalla voi olla muitakin virheita.')
  if(length(d$counts)) {
    counts <- sort(d$counts[d$counts>0],decreasing=TRUE)
    lines <- c(lines,'',paste(unname(labels[names(counts)]),counts,sep=': '))
  }
  if(!is.null(d$phrase_attempts)) {
    if(length(d$first_complete_elapsed)==1 && is.finite(d$first_complete_elapsed)) lines <- c(lines,paste('Ensimmainen kokonainen ketju:',round(d$first_complete_elapsed,2),'sekunnissa'))
    lines <- c(lines,'','Hakukierros: first_solution = ensimmainen ratkaisu joka sakeelle; improvement = lisavaihtoehdot.',
      'Saekohtaiset haut (NA-transpositio = saekohtaisesti vaihtuva):',
      capture.output(print(d$phrase_attempts,row.names=FALSE)),
      'Yksikin ilman ratkaisua jaanyt sae estaa kokonaisen ketjun muodostamisen.',
      'Jos saehausta tuli search_limit, ratkaisun puuttumista ei ole todistettu. Kaikkia sakeita ei valttamatta ehditty tutkia.')
    ex <- lapply(seq_along(d$children),function(i) {
      e <- d$children[[i]]$examples
      if(!is.null(e) && nrow(e)) {e$phrase <- d$phrase_attempts$phrase[i];e} else NULL
    })
    ex <- Filter(Negate(is.null),ex)
    if(length(ex)) lines <- c(lines,'','Esimerkkeja hylatyista osayhdistelmista (ajat sakeen alusta):',
      capture.output(print(head(do.call(rbind,ex),20),row.names=FALSE)))
  } else if(!is.null(d$examples) && nrow(d$examples)) {
    lines <- c(lines,'','Esimerkkeja hylatyista osayhdistelmista (ajat teeman alusta neljasosina):',
      capture.output(print(d$examples,row.names=FALSE)))
  }
  if(!is.null(result$stats$roots_visited)) lines <- c(lines,'',paste('Kaydyt lahtoteemat:',paste(result$stats$roots_visited,collapse=', ')))
  if(isTRUE(result$options$repair)) lines <- c(lines,paste('Korjausehdokkaita:',d$repair_pending,
    '; kasitelty:',d$repair_attempted,'; kesken:',d$repair_limited))
  if(result$status=='search_limit') lines <- c(lines,'Haku jai kesken: nolla ratkaisua ei tarkoita, ettei ratkaisuja olisi olemassa.')
  lines
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

kaanoni_prepare_export <- function(result,merge_duplicates=TRUE,merge_octaves=FALSE,best_n=NULL) {
  if(!is.null(best_n) && (length(best_n)!=1 || !is.finite(best_n) || best_n<1 || best_n!=floor(best_n)))
    stop('Vietavien ratkaisujen maara on positiivinen kokonaisluku.')
  ranking <- if(is.null(result$options$ranking)) 'balanced' else result$options$ranking
  result$results <- kaanoni_rank(result$results, ranking)
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
  keys <- vapply(result$results,function(r) .solution_key(r$export_notes),character(1))
  result$results <- result$results[!duplicated(keys)]
  if(!is.null(best_n)) result$results <- head(result$results,best_n)
  result
}

kaanoni_write <- function(result,path,gap_bars=1,time=c(4,4),tempo=60,best_n=NULL,
    merge_duplicates=TRUE,merge_octaves=FALSE) {
  if(!length(result$results)) stop('Ei vietavia ratkaisuja.')
  if(length(time)!=2 || any(time<=0) || gap_bars<0 || tempo<=0) stop('Virheelliset vientiasetukset.')
  result <- kaanoni_prepare_export(result,merge_duplicates,merge_octaves,best_n)
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
