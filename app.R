library(shiny)
source('R/kaanoni.R')
source('R/settings.R')
ui <- function(request) {
  saved <- kaanoni_load_settings()
  fluidPage(
  titlePanel('Kaanonit ja kontrapunktit'),
  sidebarLayout(sidebarPanel(
    fileInput('file','MIDI tai MusicXML',accept=c('.mid','.midi','.xml','.musicxml','.mxl')),
    textInput('source_voices','Syötteen äänet (tyhjä = kaikki)',saved$source_voices),
    checkboxInput('single','Koko melodia yhtenä teemana',saved$single),
    conditionalPanel('!input.single',numericInput('bars','Tahteja teemajaksossa',saved$bars,min=1,step=1)),
    selectInput('mode','Haku',c('Kaanon'='canon','Eri teemojen kontrapunkti'='counterpoint'),selected=saved$mode),
    selectInput('voices','Äänimäärä',2:4,selected=saved$voices),
    checkboxGroupInput('forms','Teemavariaatiot (O on aina mukana)',c('I','R','RI'),selected=saved$forms),
    checkboxGroupInput('factors','Rytmikertoimet',c('1/2'='0.5','1'='1','2'='2'),selected=saved$factors),
    numericInput('delay_min','Pienin sisääntuloviive (neljasosia)',saved$delay_min,min=0,step=0.25),
    numericInput('delay_max','Suurin sisääntuloviive',saved$delay_max,min=0,step=0.25),
    numericInput('delay_step','Viiveiden askel',saved$delay_step,min=0.01,step=0.25),
    textInput('transpositions','Transpositiot puolisävelaskelina',saved$transpositions),
    numericInput('low','Äänialan alaraja (MIDI)',saved$low,min=0,max=127),
    numericInput('high','Äänialan yläraja (MIDI)',saved$high,min=0,max=127),
    numericInput('overlap','Kaikkien äänten vähimmäispäällekkäisyys (neljasosia)',saved$overlap,min=0),
    numericInput('min_sonorities','Yhteissointien vähimmäismäärä',saved$min_sonorities,min=0,step=1),
    conditionalPanel('input.voices != 2',
      selectInput('sonority_mode','Yhteissointien laskutapa',
        c('Intervalliparit'='intervals','Kaikkien äänten soinnut'='chords'),selected=saved$sonority_mode)),
    helpText('Kaksiäänisessä lasketaan soivat nuottiparit. Useammassa äänessä voit laskea kaikki intervalliparit tai kaikkien äänten soinnut. Uusi nuotti muodostaa uuden yhteissoinnin myös pidetyn sävelen kanssa. 0 poistaa lukumäärärajan.'),
    checkboxInput('allow_weak_steps','Salli epäiskujen intervallit asteikkoliikkeessä',saved$allow_weak_steps),
    conditionalPanel('input.allow_weak_steps',
      selectInput('metric_mode','Epäiskujen määritys',c('Automaattisesti syötteen tahtilajista'='auto','Oma vahvojen iskujen väli'='manual'),selected=saved$metric_mode),
      conditionalPanel("input.metric_mode == 'manual'",numericInput('strong_beat_period','Vahvojen iskujen väli (neljäsosia)',saved$strong_beat_period,min=0.125,step=0.5)),
      helpText('Automaattisesti: 4/4-tahtilajissa iskut 2 ja 4, 3/4- ja 3/8-tahtilajissa iskut 2 ja 3 ovat epäiskuja. Oma väli 2 vastaa 4/4-tahdin vahvoja iskuja 1 ja 3. Epäiskuilla sallitaan dissonanssi, jos jompikumpi ääni liikkuu säveleen ja siitä pois 1–2 puolisävelaskelta samaan suuntaan ilman taukoja. Rinnakkaisliikkeiden kielto säilyy erillisenä.')),
    checkboxInput('parallels','Kiellä rinnakkaiset kvintit ja oktaavit',saved$parallels),
    checkboxInput('crossing','Kiellä äänten risteäminen (1 = ylin)',saved$crossing),
    numericInput('leap','Suurin melodinen hyppy (puolisävelaskelia)',saved$leap,min=0),
    numericInput('errors','Sallittuja harmoniavirheitä',saved$errors,min=0),
    checkboxInput('repair','Korjaa sävelkorkeuksia',saved$repair),
    conditionalPanel('input.repair',
      selectInput('repair_mode','Korjaustapa',c('Yhteinen lähdeteema: tiukka kaanon'='strict','Äänet erikseen: vapaa kontrapunkti'='free'),selected=saved$repair_mode),
      numericInput('shift','Suurin sävelmuutos (puolisävelaskelia)',saved$shift,min=1),
      numericInput('changes','Enintään muutettuja lähdesäveliä',saved$changes,min=1),
      numericInput('nodes','Korjaushaun tiloja / ehdokas',saved$nodes,min=1)),
    numericInput('seconds','Haun aikaraja (sekuntia)',saved$seconds,min=1,max=600),
    numericInput('candidates','Enintään kokonaisia ehdokkaita',saved$candidates,min=1),
    numericInput('results','Näytä enintään parasta ratkaisua',saved$results,min=1),
    selectInput('ranking','Hyvyysjärjestyksen painotus',
      c('Säilytä teema mahdollisimman tarkasti'='balanced',
        'Suosi konsonoivaa yhteissointia'='consonance',
        'Suosi pitkää kaikkien äänten yhteissointia'='overlap'),selected=saved$ranking),
    helpText('Kaikissa järjestyksissä vähemmän harmoniavirheitä on ensin. Painotus ratkaisee sen jälkeen. Valitse painotus ennen hakua.'),
    checkboxInput('stop_after_results','Pysäytä, kun enimmäismäärä ratkaisuja on löytynyt',saved$stop_after_results),
    helpText('Asetukset tallentuvat automaattisesti ja palautuvat seuraavalla käynnistyskerralla.'),
    actionButton('run','Muodosta ratkaisut',class='btn-primary')
  ),mainPanel(
    p('Aika-arvot mitataan neljasosina. MIDI-sävelkorkeus 60 = keski-C. Alkuperäinen syöte säilyy ennallaan.'),
    h4('Syöte'),verbatimTextOutput('input_summary'),
    h4('Haun tulos'),p('Haun aikana mittari näyttää vaiheen, ajan, nykyisen lähtöteeman, tutkitut ehdokkaat ja ratkaisujen määrän. Palkki kuvaa käytettyä aikabudjettia, ei kaikkien vaihtoehtojen valmistumisprosenttia.'),p('Haku vuorottelee lähtöteemojen välillä ja etsii ensin ilman sävelkorjauksia toimivat vaihtoehdot. Korjausaika jaetaan sen jälkeen ehdokkaiden kesken. Aikaraja voi jättää haun kesken.'),verbatimTextOutput('status'),tableOutput('summary'),
    selectInput('selected','Tarkasteltava ratkaisu',choices=character()),
    tableOutput('settings'),tableOutput('changes'),tableOutput('errors_table'),
    plotOutput('piano',height='350px'),
    h4('Vienti: parhaat ratkaisut hyvyysjärjestyksessä'),
    numericInput('export_best','Vie enintään näin monta parasta ratkaisua',saved$export_best,min=1,step=1),
    textOutput('export_summary'),
    p('Jos haluat parhaat koko tutkitusta joukosta, jätä ratkaisumäärään pysäyttävä valinta pois päältä. Nopeasti pysäytetty haku vertailee vain siihen mennessä löytyneitä ratkaisuja.'),
    numericInput('out_num','Tahtilajin osoittaja',saved$out_num,min=1),
    selectInput('out_den','Tahtilajin nimittaja',c(2,4,8,16),selected=saved$out_den),
    numericInput('tempo','Tempo (neljasosia/min)',saved$tempo,min=10,max=300),
    numericInput('gap','Tauon pituus ratkaisujen välissä (tahteja)',saved$gap,min=0),
    downloadButton('midi','Lataa MIDI'),downloadButton('xml','Lataa MusicXML'),
    downloadButton('report','Lataa raportti'),
    br(),br(),actionButton('save_xml_local','Tallenna MusicXML projektikansioon'),
    textOutput('saved_xml_path'),
    p('Korjaushaku on heuristinen. Aikarajaan päättyminen ei tarkoita, ettei ratkaisua olisi olemassa. MusicXML-vienti käyttää kromaattisesti valittuja sävelnimiä; alkuperäisiä enharmonisia kirjoitusasuja ei vielä säilytetä.')
  ))
)
}
server <- function(input,output,session) {
  # Odota koko lomaketta ennen ensimmaista tallennusta. Tyhja forms/factors on sallittu.
  setting_ids <- names(kaanoni_setting_defaults())
  scalar_ids <- setdiff(setting_ids,c('forms','factors'))
  last_settings <- NULL
  observe({
    values <- setNames(lapply(setting_ids,function(id) input[[id]]),setting_ids)
    if(any(vapply(values[scalar_ids],is.null,logical(1)))) return()
    values$forms <- as.character(values$forms)
    values$factors <- as.character(values$factors)
    if(identical(values,last_settings)) return()
    tryCatch({
      kaanoni_save_settings(values)
      last_settings <<- values
    },error=function(e) showNotification('Asetuksia ei voitu tallentaa levylle.',type='error'))
  },priority=100)

  parse_numbers <- function(s) {
    x <- suppressWarnings(as.numeric(trimws(strsplit(s,',',fixed=TRUE)[[1]])))
    validate(need(length(x)>0 && all(is.finite(x)),'Anna pilkuilla erotetut luvut.')); x
  }
  loaded <- reactive({
    req(input$file)
    # Shiny tallentaa liitteen ilman paatetta: sailyta muoto lukijaa varten.
    ext <- tools::file_ext(input$file$name); p <- tempfile(fileext=paste0('.',ext))
    on.exit(unlink(p)); file.copy(input$file$datapath,p)
    vs <- if(nzchar(trimws(input$source_voices))) parse_numbers(input$source_voices) else NULL
    kaanoni_read(p,vs)
  })
  output$input_summary <- renderPrint({
    a <- loaded(); t <- kaanoni_themes(a,if(input$single) NULL else input$bars)
    cat(nrow(a$notes),'nuottia;',length(t),'teemaa; aanet',paste(unique(a$notes$voice),collapse=', '),'\n')
    cat('Tahtilajit:',paste(unique(paste(a$measures$numerator,a$measures$denominator,sep='/')),collapse=', '),'\n')
    cat('Jaksojen pituudet:',paste(vapply(t,function(x)x$span,numeric(1)),collapse=', '),'neljasosaa\n')
  })
  result <- eventReactive(input$run,{
    validate(need(input$delay_step>0 && input$delay_max>=input$delay_min,'Tarkista viiveiden rajat ja askel.'),
             need(length(input$factors)>0,'Valitse rytmikerroin.'))
    t <- kaanoni_themes(loaded(),if(input$single) NULL else input$bars)
    o <- kaanoni_options(voices=as.integer(input$voices),mode=input$mode,forms=input$forms,
      factors=as.numeric(input$factors),delays=seq(input$delay_min,input$delay_max,by=input$delay_step),
      transpositions=parse_numbers(input$transpositions),ranges=rep(list(c(input$low,input$high)),as.integer(input$voices)),
      min_overlap=input$overlap,min_sonorities=if(is.null(input$min_sonorities)) 0 else input$min_sonorities,
      sonority_mode=if(is.null(input$sonority_mode)) 'intervals' else input$sonority_mode,max_harmony_errors=input$errors,forbid_parallels=input$parallels,
      forbid_crossing=input$crossing,max_leap=input$leap,repair=input$repair,repair_mode=input$repair_mode,
      allow_weak_steps=isTRUE(input$allow_weak_steps),
      metric_mode=if(is.null(input$metric_mode)) 'auto' else input$metric_mode,
      strong_beat_period=if(is.null(input$strong_beat_period)) 2 else input$strong_beat_period,
      max_shift=input$shift,max_changes=input$changes,repair_nodes=input$nodes,
      seconds=input$seconds,max_candidates=input$candidates,max_results=input$results,
      stop_after_results=isTRUE(input$stop_after_results),
      ranking=if(is.null(input$ranking)) 'balanced' else input$ranking)
    r <- withProgress(message='Haku käynnistyy',value=0,{
      kaanoni_generate(t,o,progress=function(p) {
        stages <- c(start='Haku käynnistyy',scan='Teemayhdistelmien haku',
          repair='Sävelkorkeuksien korjaushaku',ranking='Tulosten järjestäminen',done='Haku päättynyt')
        detail <- sprintf('Aika %.0f / %.0f s · teema %d/%d · ehdokkaita %d · ratkaisuja %d',
          p$elapsed,p$seconds,p$theme,p$themes,p$tested,p$found)
        if(p$stage=='repair') detail <- paste0(detail,' · korjaushaussa ',p$repair_attempted,'/',p$repair_candidates)
        setProgress(value=if(p$stage=='done') 1 else min(p$budget_fraction,0.99),
          message=unname(stages[p$stage]),detail=detail)
      })
    })
    choices <- as.character(seq_along(r$results))
    updateSelectInput(session,'selected',choices=choices,
      selected=if(length(choices)>0) choices[1] else character())
    r
  })
  selected <- reactive({
    r <- result()
    # Shiny pitaa numeerista nollaa totena: req(length(...)) ei suojaa tyhjaa listaa.
    req(length(r$results)>0)
    index <- suppressWarnings(as.numeric(input$selected))
    req(length(index)==1 && !is.na(index) && is.finite(index) &&
        index==floor(index) && index>=1 && index<=length(r$results))
    r$results[[as.integer(index)]]
  })
  output$status <- renderPrint({
    r <- result(); print(r)
    if(identical(r$status,'result_limit')) cat('Haku pysäytettiin, koska pyydetty ratkaisumäärä löytyi.\n')
    if(length(r$results)==0) {
      if(r$status=='search_limit')
        cat('Haku jäi kesken aika- tai ehdokasrajan vuoksi. Ratkaisuja ei löytynyt tutkitusta osasta.\n')
      else cat('Valituilla asetuksilla ei löytynyt ratkaisuja.\n')
    }
  })
  output$summary <- renderTable({
    r <- result(); if(!length(r$results)) return(NULL)
    do.call(rbind,lapply(seq_along(r$results),function(i) {
      x <- r$results[[i]]; data.frame(Ratkaisu=i,Lahdesavelmuutokset=x$changed_sources,
      Nuottimuutokset=x$changed_occurrences,Virheet=x$evaluation$harmony,Yhteissointiaika=x$evaluation$overlap,Yhteissointeja=x$evaluation$sonority_count,Konsonanssi=round(x$consonance,3))
    }))
  })
  output$settings <- renderTable(selected()$settings)
  output$changes <- renderTable(selected()$changes)
  output$errors_table <- renderTable(selected()$evaluation$errors)
  output$piano <- renderPlot({
    n <- selected()$notes
    plot(range(c(n$onset,n$onset+n$duration)),range(n$pitch)+c(-2,2),type='n',xlab='Neljäsosia',ylab='MIDI-sävelkorkeus')
    segments(n$onset,n$pitch,n$onset+n$duration,n$pitch,col=n$voice,lwd=4)
    legend('topright',legend=paste('Aani',sort(unique(n$voice))),col=sort(unique(n$voice)),lwd=4)
  })
  export_result <- reactive({
    r <- result()
    count <- input$export_best
    # Vanhojen asiakkaiden ja testien oletus: koko palautettu tuloslista.
    if(is.null(count)) count <- length(r$results)
    req(length(count)==1 && is.finite(count) && count>=1 && count==floor(count))
    r$results <- head(r$results,count)
    r
  })
  output$export_summary <- renderText({
    r <- export_result()
    paste('Tiedostoon viedään',length(r$results),'parasta ratkaisua. Paras on ensimmäisenä.')
  })
  export <- function(file,ext) {
    p <- tempfile(fileext=ext); on.exit(unlink(c(p,paste0(p,'.txt'),paste0(p,'.rds'))))
    kaanoni_write(export_result(),p,gap_bars=input$gap,time=c(input$out_num,as.integer(input$out_den)),tempo=input$tempo)
    if(!file.exists(p) || !file.copy(p,file,overwrite=TRUE)) stop('Vientitiedoston kopiointi epäonnistui.')
  }
  output$midi <- downloadHandler(filename=function()'kaanoni.mid',contentType='audio/midi',content=function(file)export(file,'.mid'))
  output$xml <- downloadHandler(filename=function()'kaanoni.musicxml',contentType='application/vnd.recordare.musicxml+xml',content=function(file)export(file,'.musicxml'))
  output$report <- downloadHandler(filename=function()'kaanoni-raportti.txt',contentType='text/plain; charset=utf-8',content=function(file) {
    r <- export_result(); writeLines(capture.output({print(r); for(x in r$results) {cat('Tyyppi:',x$kind,'Tila:',x$status,'\n');print(x$settings);print(x$changes);print(x$evaluation$errors)}}),file)
  })
  # Rekisteroi lataukset myos silloin, kun painikkeet ovat RStudion nakyman ulkopuolella.
  for(id in c('midi','xml','report')) outputOptions(output,id,suspendWhenHidden=FALSE)
  saved_xml <- reactiveVal('')
  output$saved_xml_path <- renderText(saved_xml())
  observeEvent(input$save_xml_local,{
    req(length(result()$results)>0)
    tryCatch({
      folder <- file.path(.kaanoni_root,'output')
      if(!dir.exists(folder) && !dir.create(folder)) stop('Tulostuskansiota ei voitu luoda.')
      # tempfile antaa yksilollisen nimen: aiempia ratkaisuja ei korvata.
      file <- tempfile(pattern=paste0('kaanoni_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_'),
                       tmpdir=folder,fileext='.musicxml')
      export(file,'.musicxml')
      saved_xml(normalizePath(file))
      showNotification('MusicXML-tiedosto tallennettu projektin output-kansioon.',type='message')
    },error=function(e) showNotification(paste('MusicXML-tallennus epäonnistui:',conditionMessage(e)),type='error'))
  })

}
shinyApp(ui,server)
