old_settings_path <- getOption('kaanoni.settings_file')
test_settings_path <- tempfile()
options(kaanoni.settings_file=test_settings_path)
source('app.R')
shiny::testServer(server, {
  session$setInputs(file=list(name='demo.musicxml',datapath=normalizePath('examples/demo.musicxml')),
    source_voices='1',single=FALSE,bars=4,mode='canon',voices='2',forms=character(),
    factors='1',delay_min=1,delay_max=2,delay_step=1,
    transpositions='-12,0,12',low=36,high=96,overlap=1,
    errors=0,parallels=TRUE,crossing=FALSE,leap=12,repair=FALSE,
    repair_mode='strict',shift=2,changes=4,nodes=100,seconds=3,candidates=100,results=3)
  stopifnot(nrow(loaded()$notes)>0)
  session$setInputs(run=1)
  r <- result()
  stopifnot(inherits(r,'kaanoni_result'),length(r$results)>0)
  session$setInputs(selected='1')
  stopifnot(selected()$evaluation$valid)
  session$setInputs(gap=1,out_num=4,out_den='4',tempo=60)
  # Aja oikea latauskasittelija: vastauksen on oltava XML:aa, ei HTML-sivu.
  downloaded <- output$xml
  stopifnot(file.exists(downloaded),basename(downloaded)=='kaanoni.musicxml')
  xml <- readLines(downloaded,warn=FALSE)
  stopifnot(any(grepl('<score-partwise',xml,fixed=TRUE)),!any(grepl('<html',xml,fixed=TRUE)))
  stopifnot(nrow(kaanoni_read(downloaded)$notes)>0)
  session$setInputs(save_xml_local=1)
  local_file <- saved_xml()
  stopifnot(file.exists(local_file),tools::file_ext(local_file)=='musicxml')
  stopifnot(nrow(kaanoni_read(local_file)$notes)>0)
  unlink(local_file)
  session$setInputs(export_best=1)
  best_download <- output$xml
  stopifnot(nrow(kaanoni_read(best_download)$notes)==nrow(result()$results[[1]]$notes),
            length(export_result()$results)==1)
  # Virheellinen tai edellisen haun liian suuri valinta ei saa indeksoida listaa.
  for(value in c('99999','0','-1','1.5','bad','')) {
    session$setInputs(selected=value)
    error <- tryCatch(selected(),error=identity)
    stopifnot(inherits(error,'shiny.silent.error'))
  }
  session$setInputs(selected='1')
  stopifnot(selected()$evaluation$valid)
  # Uusi haku palauttaa tyhjan listan, vaikka vanha valinta on yha selaimessa.
  session$setInputs(min_sonorities=100000,run=2)
  stopifnot(length(result()$results)==0)
  error <- tryCatch(selected(),error=identity)
  stopifnot(inherits(error,'shiny.silent.error'),
            !grepl('subscript out of bounds',conditionMessage(error),fixed=TRUE))
  stopifnot(grepl('ei löytynyt',output$status,fixed=TRUE))
  for(id in c('settings','changes','errors_table','piano')) {
    value <- tryCatch(output[[id]],error=identity)
    stopifnot(!inherits(value,'error') || inherits(value,'shiny.silent.error'))
  }
})
cat('Shiny input and search OK\n')

unlink(test_settings_path)
options(kaanoni.settings_file=old_settings_path)
