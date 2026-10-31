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
})
cat('Shiny input and search OK\n')

unlink(test_settings_path)
options(kaanoni.settings_file=old_settings_path)
