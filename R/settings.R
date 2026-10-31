# Kayttajan asetukset sailyvat paikallisessa JSON-tiedostossa, eivat selainistunnossa.
kaanoni_setting_defaults <- function() list(
  source_voices='',single=TRUE,bars=4,mode='canon',voices='2',forms=character(),
  factors='1',delay_min=1,delay_max=8,delay_step=1,
  transpositions='-12,-7,-5,0,5,7,12',low=36,high=96,overlap=1,
  min_sonorities=0,sonority_mode='intervals',allow_weak_steps=FALSE,strong_beat_period=2,metric_mode='auto',parallels=TRUE,crossing=FALSE,leap=12,errors=0,repair=FALSE,
  repair_mode='strict',shift=2,changes=4,nodes=500,seconds=240,
  candidates=10000,results=30,ranking='balanced',export_best=10,stop_after_results=FALSE,out_num=4,out_den='4',tempo=60,gap=1)

kaanoni_settings_path <- function() getOption('kaanoni.settings_file',
  file.path(.kaanoni_root,'kaanoni-asetukset.json'))

kaanoni_clean_settings <- function(values,previous=kaanoni_setting_defaults()) {
  choices <- list(ranking=c('balanced','consonance','overlap'),metric_mode=c('auto','manual'),sonority_mode=c('intervals','chords'),mode=c('canon','counterpoint'),voices=as.character(2:4),
    forms=c('I','R','RI'),factors=c('0.5','1','2'),repair_mode=c('strict','free'),
    out_den=c('2','4','8','16'))
  numeric_min <- c(bars=1,delay_min=0,delay_max=0,delay_step=0.00001,low=0,high=0,
    overlap=0,min_sonorities=0,strong_beat_period=0.00001,leap=0,errors=0,shift=1,changes=1,nodes=1,seconds=1,
    candidates=1,results=1,export_best=1,out_num=1,tempo=10,gap=0)
  numeric_max <- c(low=127,high=127,seconds=600,tempo=300)
  for(id in intersect(names(previous),names(values))) {
    v <- values[[id]]
    if(id %in% c('forms','factors')) {
      v <- as.character(unlist(v)); if(all(v %in% choices[[id]])) previous[[id]] <- v
    } else if(id %in% names(choices)) {
      if(length(v)==1 && !is.na(v) && as.character(v) %in% choices[[id]]) previous[[id]] <- as.character(v)
    } else if(is.logical(previous[[id]])) {
      if(is.logical(v) && length(v)==1 && !is.na(v)) previous[[id]] <- v
    } else if(id %in% names(numeric_min)) {
      if(is.numeric(v) && length(v)==1 && is.finite(v) && v>=numeric_min[id] &&
         (!id %in% names(numeric_max) || v<=numeric_max[id])) previous[[id]] <- as.numeric(v)
    } else if(is.character(v) && length(v)==1 && !is.na(v)) previous[[id]] <- v
  }
  previous
}

kaanoni_load_settings <- function(path=kaanoni_settings_path()) {
  defaults <- kaanoni_setting_defaults()
  if(!file.exists(path)) return(defaults)
  tryCatch({
    data <- jsonlite::fromJSON(path,simplifyVector=TRUE)
    if(!is.list(data) || !identical(data$version,1L) || !is.list(data$inputs)) return(defaults)
    kaanoni_clean_settings(data$inputs,defaults)
  },error=function(e) defaults)
}

kaanoni_save_settings <- function(values,path=kaanoni_settings_path()) {
  values <- kaanoni_clean_settings(values,kaanoni_load_settings(path))
  tmp <- tempfile(pattern='.kaanoni-settings-',tmpdir=dirname(path))
  on.exit(unlink(tmp))
  jsonlite::write_json(list(version=1L,inputs=values),tmp,auto_unbox=TRUE,pretty=TRUE)
  if(!file.rename(tmp,path)) stop('Asetustiedoston tallennus ei onnistunut.')
  invisible(values)
}
