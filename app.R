# This script provides the interface and server functions for an web app for calculation
# long COVID prediction score

# data and tools ----

   library(plyr)
   library(tidyverse)
   library(shinyWidgets)

   source('app_tools.R')

# reading the table with the multi-organ phenotype symptoms -----
  
  atypic_sympt_tbl <- read_tsv('./data/atypic_symptoms.txt') ## the list of multi-organ symptoms evolved in the initial phase of the study, hence an external file

# User interface -----

  ui <- fluidPage(
      
     ## Title panel with the logos and names
     
     titlePanel(title =  HTML("<div style = 'font-size:50px; 
                                    color:#5d86bb; 
                                    font-family:Helvetica; 
                                    text-shadow: 2px 2px AliceBlue'>
                                 <strong>ALPS</strong>
                                 <img src = '' width = 1400>
                                 <img src = 'mui_logo.jpg' width = 90><br/>
                                 <strong>A</strong>ustrian 
                                 <strong>L</strong>ong COVID 
                                 <strong>P</strong>rediction
                                 <strong>S</strong>ore<br/><div/>
                                 <hr style = 'height:5px'>"), 
                windowTitle = 'ALPS Calculator'), 
     
    ## Side panel with user's entries.
    ## The symptoms are are grouped by systems: neurological, respiratory, etc.
    ## Note: the symptoms belonging to the multi-organ phenotype are not explicitly 
    ## visible to the user
    
    sidebarLayout(
      
      sidebarPanel(h3('Symptoms during the first 14 days of COVID-19'), 
                   br(), 
                   checkboxGroupInput('resp_sympt', 
                                      'Respiratory symptoms:', 
                                      c('Tachypnea' = 'breath_short', ## German: Kurzatmigkeit
                                        'Dyspnea' = 'dyspnoe', ## German: Atemnot
                                        'Wet cough' = 'wet_cough')), ## German: Husten mit Auswurf
                   checkboxGroupInput('gi_sympt', 
                                      'Gastrointenstinal symptoms:', 
                                      c('Abdominal pain' = 'adbominal_pain', ## Abdominale Schmerzen
                                        'Nausea' = 'nausea', ## Übelkeit
                                        'Vomiting' = 'vomiting', ## Erbrechen
                                        'Diarrhea' = 'diarrhea')), ## Durchfall
                   checkboxGroupInput('cardio_sympt', 
                                      'Cardiological symptoms:', 
                                      c('Tachycardia' = 'tachycardia', ## Herzrasen
                                        'Palpitations' = 'extrasystole')), ## Herzklopfen
                   checkboxGroupInput('neuro_sympt', 
                                      'Neurological/cognitive symptoms:', 
                                      c('Smell impairment or loss' = 'anosmia', ## Beeinträchtigung/Verlust von Geruchsinn
                                        'Confusion' = 'confusion', ## Verwirrung
                                        'Concentration problems' = 'imp_concentration', ## Beeinträchtigtes Konzentrationsvermögen
                                        'Memory problems' = 'forgetfulness', ## Gedächtnisprobleme
                                        'Sleeping problems' = 'insomnia_covid', ## Schlafprobleme/Schlaflossigkeit
                                        'Seizure' = 'epilepsy_covid', ## Krämpfe
                                        'Impaired fine motor skills (binding shoes, tying buttons)' = 'unhandiness_micromotor', ## Beeinträchtigung feinmotorischen Fähigkeiten
                                        'Tingling feet' = 'tingle_feet', ## Kribbeln in den Füßen
                                        'Tingling hands' = 'tingle_hands', ## Kribbeln in den Händen
                                        'Burning feet' = 'ache_feet', ## Brennen in den Füßen
                                        'Burning hands' = 'ache_hands', ## Brennen in den Händen
                                        'Numb feet' = 'numb_feet', ## Taube Füße
                                        'Numb hands' = 'numb_hands', ## Taube Hände
                                        'Difficulties in walking' = 'unhandiness_walk')), ## Gehprobleme
                   checkboxGroupInput('other_sympt', 
                                      'Other symptoms:', 
                                      c('Leg swelling' = 'swelling', ## Geschwollene Füße
                                        'Blue fingers or toes' = 'blue_fingers', ## Blaue Finger/Zechen
                                        'Urticaria' = 'urticaria', ## Nesselausschlag
                                        'Blister rash' = 'blister_rash', ## Blasenausschlag
                                        'Marmorated skin' = 'net_rash', ## Netzausschlag
                                        'Red eyes' = 'red_eyes')), ## Rötung der Augen
                   br(), 
                   h3('Pre-existing conditions'), ## Vorerkrankungen
                   br(), 
                   checkboxGroupInput('comorb_neuropsych', 
                                      'Neurologic/psychiatric comorbidity:', 
                                      c('Epilepsy' = 'epilepsy', 
                                        'Multiple sclerosis' = 'ms', 
                                        'Parkinson disease' = 'parkinson', 
                                        'Dementia' = 'dementia', 
                                        'Tingling/numbness of the hands' = 'tingle_hands', 
                                        'Tingling/numbness of the feets' = 'tingle_feet', 
                                        'Insomnia' = 'insomnia', 
                                        'Sleep apnea' = 'night_dyspnoe', 
                                        'Depression' = 'depression', 
                                        'Burnout' = 'burnout', 
                                        'Bruxism' = 'bruxism')), 
                   br(),
                   em('By using the application you accept', 
                      a('the terms of use and licensing', 
                        href = 'readme.txt')), 
                   br(), 
                   br(), 
                   downloadButton('downloadReport', 
                                  label = 'Download report')),
      
      ## Main panel to hold the dynamic output
      
      mainPanel(h2(strong('Risk of long COVID'), 
                   style = 'color:#5d86bb; font-family:Helvetica'),
                br(), 
                h4('Long COVID is defined as presence of at least one post-acute persistent symptom of COVID-19 for 28 days or longer after symptom onset. 
                   Score development, modeling of long COVID risk and real-life prevalence data are based on the results of the', 
                   a('Health after COVID-19 study', 
                     href = 'https://inneremed2.tirol-kliniken.at/page.cfm?vpath=forschung/gesundheit-nach-covid-19'), 
                   'in Tyrol/Austria.', 
                   style = 'font-size:20px; color: black'),
                hr(), 
                h3(strong('Long COVID Prediction Score calculation', 
                          style = 'color:#5d86bb; font-family:Helvetica')), 
                br(), 
                div(tableOutput('chronic_components'), ## Tabular output of the particular ALPS components
                    style = 'font-size:120%'), 
                br(), 
                h4(textOutput('chronic_score'), ## Text output of the calculated score
                   style = 'font-size:20px; color: black'), 
                hr(), 
                h3(strong('Prediction by modeling'), 
                   style = 'color:#5d86bb; font-family:Helvetica'), 
                br(), 
                h4('Risk estimate obtained by logistic regression (n = 1009).', 
                   style = 'font-size:20px; color: black'), 
                br(), 
                h4(textOutput('chronic_risk'), ## Estimated risk with 95% CI linked to the calculated ALPS, text ouput
                   style = 'font-size:20px; color: black'), 
                br(), 
                plotOutput('chronic_mod_plot', ## graphical representation of the calculated risk and 95% CI
                           width = '60%', 
                           height = '150px'), 
                hr(), 
                h3(strong('Real-life prevalence'), 
                   style = 'color:#5d86bb; font-family:Helvetica'), 
                br(), 
                h4('Long COVID Prediction Score value and prevalence of long COVID in the study cohort.', 
                   style = 'font-size:20px; color: black'), 
                br(), 
                plotOutput('chronic_real_plot', ## prevalence of long COVID associated with the calculated score in the establishment cohort, plot output
                           width = '60%', 
                           height = '600px'), 
                hr(), 
                HTML("<div style =  'text-align: right'>
                      <img src = '' width = 80%>
                      <p>Powered by </p>
                      <img src = 'logo_large.png' width = 60>
                      <img src = '' width = 30>
                      <img src = 'shiny_logo.png' width = 60></div>"))
      
    )
  )

# Define server logic ----

  server <- function(input, output) {
    
   comorb_subsum <- reactive({
      
      if(length(input$comorb_neuropsych) > 0) {1} else {0}
      
    }) ## indicator if there's a pre-existing neurological or psychiatric comorbidity
   
   sympt_vec <- reactive({
     
     c(input$smell_taste, 
       input$resp_sympt, 
       input$gi_sympt, 
       input$cardio_sympt, 
       input$neuro_sympt, 
       input$other_sympt)
     
   }) ## a vector with all ticked symptoms in the app
   
   atypic_sympt_number <- reactive({
     
     sum(sympt_vec() %in% atypic_sympt_tbl$symptom)
     
   }) ## calculation of the number of symptoms belonging to the multi-organ phenotype
   
   ## score calculation
   
   cov_chronic_score <- reactive({
     
     c(atypic_sympt_number(), 
       comorb_subsum(), 
       sum('anosmia' %in% sympt_vec(), 
           'forgetfulness' %in% sympt_vec(), 
           'breath_short' %in% sympt_vec(), 
           'extrasystole' %in% sympt_vec(), 
           'imp_concentration' %in% sympt_vec())) %>% 
       sum
     
   }) ## calculation of the total ALPS
   
   cov_chronic_table <- reactive({
      
      ## displays the score sub-component values in a table
      
      score_table(multi_organ_sum = atypic_sympt_number(), 
                  anosmia_acute = 'anosmia' %in% sympt_vec(), 
                  forgetfulness_acute = 'forgetfulness' %in% sympt_vec(), 
                  breath_short_acute = 'breath_short' %in% sympt_vec(), 
                  comorb_class_neuropsych = comorb_subsum(), 
                  extrasystole_acute = 'extrasystole' %in% sympt_vec(), 
                  imp_concentration_acute = 'imp_concentration' %in% sympt_vec())
      
   }) ## ALPS component in the table
   
   ## prediction of the modeled risk values with CI
   
   cov_chronic_risk <- reactive({
     
     predict_test(score_val = cov_chronic_score(), 
                  response = 'chronic_covid')
     
   })
   
   ## text and table output for the UI: score values, score components, predicted risk values
   
   output$chronic_score <- renderText({
      
      paste('Your chronic COVID prediction score:', cov_chronic_score())
    
   }) ## total ALPS
   
   output$chronic_components <- renderTable({
      
      cov_chronic_table()
      
   }, hover = T, spacing = 'm') ## ALPS component table
   
   output$chronic_risk <- renderText({
     
     mod_values <- cov_chronic_risk()

     paste('Your predicted risk of developing at least one post-acute symptom: ', 
           signif(mod_values$p[1], 2) * 100, 
           '%, [95% CI: ', 
           signif(mod_values$lower_ci[1], 2) * 100, 
           ' to ', 
           signif(mod_values$upper_ci[1], 2) * 100, 
           '%]', sep = '')
          
   }) ## risk associated with the given ALPS value
   
   ## plot output: modeled risk
   
   output$chronic_mod_plot <- renderPlot({
      
      plot_mod_results(score_val = cov_chronic_score(), 
                       response = 'chronic_covid')
      
   })
   
   ## plots: real-life risk in the Tyrol cohort
   
   output$chronic_real_plot <- renderPlot({
      
      plot_score_response(score_val = cov_chronic_score(), 
                          response = 'chronic_covid')
      
   })
   
   ## download report
   
   output$downloadReport <- downloadHandler(
      
      ## defining the filename
      
      filename = function() {
         
         return(paste('@longCOVID_report_', Sys.Date(), '.pdf', sep=''))
         
      },
      
      ## calling the saving function
      
      content = function(con) {
         
         render_report(score_val = cov_chronic_score(), 
                       score_comp_table = cov_chronic_table(), 
                       path_to_save = con)
         
      }
   )
  
  }

# Run the app ----

  shinyApp(ui = ui, server = server)