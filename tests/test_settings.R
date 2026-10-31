source('app.R')
path <- tempfile(fileext='.json')
old <- getOption('kaanoni.settings_file')
options(kaanoni.settings_file=path)
base <- kaanoni_setting_defaults()
values <- modifyList(base,list(voices='4',forms=c('I','RI'),factors=c('0.5','2'),
  allow_weak_steps=TRUE,strong_beat_period=1.5,delay_step=0.5,transpositions='-12,0,12',repair=TRUE,tempo=92,gap=2))
kaanoni_save_settings(values)
stopifnot(identical(kaanoni_load_settings(),values))
# Uusi UI luetaan levylla olevista arvoista, ei aiemman R-istunnon muistista.
html <- as.character(ui(NULL))
stopifnot(grepl('value="92"',html,fixed=TRUE),grepl('value="0.5"',html,fixed=TRUE))
# Valinnat tallentuvat ilman generointinapin painamista.
shiny::testServer(server, {
  do.call(session$setInputs,values)
  session$setInputs(tempo=110,forms=character(),factors=character(),repair=FALSE)
  persisted <- kaanoni_load_settings()
  stopifnot(persisted$tempo==110,identical(persisted$forms,character()),
    identical(persisted$factors,character()),!persisted$repair)
})
# Istunnon sulkeminen ei muuta juuri tallennettuja arvoja.
stopifnot(kaanoni_load_settings()$tempo==110)
# Toinen istunto ei saa korvata vanhoja arvoja vajavaisella alustuksella.
shiny::testServer(server, {
  session$setInputs(tempo=60)
  stopifnot(kaanoni_load_settings()$tempo==110)
})
# Puuttuvat/uudet/virheelliset kentat eivat riko kaynnistysta.
kaanoni_save_settings(list(unknown='x',tempo=NA_real_,voices='8'))
stopifnot(kaanoni_load_settings()$tempo==110,kaanoni_load_settings()$voices=='4')
writeLines('not json',path)
stopifnot(identical(kaanoni_load_settings(),base))
unlink(path); options(kaanoni.settings_file=old)
cat('Asetusten tallennus- ja palautustestit OK\n')
