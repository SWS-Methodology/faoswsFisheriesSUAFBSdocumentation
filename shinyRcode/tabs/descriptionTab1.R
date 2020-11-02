# First tab, Description tab

overviewTab_reac <- reactive({

  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_element_fbs)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_elements_fbs <- as.character(element_input[label == input$btn_element_fbs]$code)
  sel_fbs_groups <- as.character(c(seq(10, 90, by = 10), 99))
  
  # if(CheckDebug()){
  # 
  #   library(faoswsModules)
  #   SETTINGS = ReadSettings("sws.yml")
  # 
  #   ## If you're not on the system, your settings will overwrite any others
  #   R_SWS_SHARE_PATH = SETTINGS[["share"]]
  # 
  #   ## Define where your certificates are stored
  #   SetClientFiles(SETTINGS[["certdir"]])
  # 
  #   ## Get session information from SWS. Token must be obtained from web interface
  #   GetTestEnvironment(baseUrl = SETTINGS[["server"]],
  #                      token = tokenFbs)
  # 
  # }
  # 
  # # R_SWS_SHARE_PATH = "Z:"
  # # SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
  # # GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  # #                      token = tokenFbs)
  #   
  # 
  # KeyFBSfias <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_fbs_fias", dimensions = list(
  #   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
  #   measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
  #                                     keys = sel_elements_fbs), # Or, if all elements, GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredElementSuaFbs" )[,code])
  #   measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
  #                                      keys = sel_fbs_groups),
  #   timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  # 
  # withProgress(message = 'FBS live data loading in progress',
  #              value = 0, {
  #                
  #                Sys.sleep(0.25)
  #                incProgress(0.25)
  #                FBSfias <- GetData(KeyFBSfias)
  #                Sys.sleep(0.75)
  #                incProgress(0.95)
  #              })
  
  validate(
    need(nrow(frozen_data$FBS) > 0, "No frozen FBS data for this country. Please select another country.")
  )
  
  validate(
    need(nrow(live_data$FBS) > 0, "No FBS data for this country. Please select another country.")
  )
  
  FBSfias <- live_data$FBS[measuredElementSuaFbs == sel_elements_fbs  ]
  
  FBSfrozen <- frozen_data$FBS
  
  FBSfrozen <- FBSfrozen[measuredElementSuaFbs == sel_elements_fbs]
  # Now only showing value present both in frozen and live, CHANGE?
  frozenVSlive <- merge(FBSfrozen, FBSfias, 
                        by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                               'measuredElementSuaFbs', 'timePointYears'),
                        suffixes = c('Frozen', 'Live'), all = TRUE)
  
  frozenVSlive <- frozenVSlive[ , c( 'geographicAreaM49_fi', 'measuredItemFaostat_L2',
         'measuredElementSuaFbs', 'timePointYears',
         'flagObservationStatusFrozen', 'flagMethodFrozen', 
         'flagObservationStatusLive', 'flagMethodLive') := list(as.character(geographicAreaM49_fi),
                                                                as.character(measuredItemFaostat_L2),
                                                                as.character(measuredElementSuaFbs),
                                                                as.character(timePointYears),
                                                                as.character(flagObservationStatusFrozen),
                                                                as.character(flagMethodFrozen),
                                                                as.character(flagObservationStatusLive),
                                                                as.character(flagMethodLive))]
  
  frozenVSlive$flagObservationStatusFrozen <- as.character( frozenVSlive$flagObservationStatusFrozen)
  frozenVSlive$flagObservationStatusLive <- as.character( frozenVSlive$flagObservationStatusLive)
  
  frozenVSlive[is.na(ValueFrozen), flagObservationStatusFrozen := 'O']
  frozenVSlive[is.na(ValueLive), flagObservationStatusLive := 'O']
  
  frozenVSlive[is.na(ValueFrozen), flagMethodFrozen := '-']
  frozenVSlive[is.na(ValueLive), flagMethodLive := '-']
  
  frozenVSlive[is.na(ValueFrozen), ValueFrozen := 0]
  frozenVSlive[is.na(ValueLive), ValueLive := 0]
  
  frozen2plot <- frozenVSlive[ , .(geographicAreaM49_fi,
                                   measuredItemFaostat_L2,
                                   measuredElementSuaFbs,
                                   timePointYears,
                                   ValueFrozen)]
  frozen2plot[ , type := 'Frozen']
  
  live2plot <- frozenVSlive[ , .(geographicAreaM49_fi,
                                   measuredItemFaostat_L2,
                                   measuredElementSuaFbs,
                                   timePointYears,
                                   ValueLive)]
  live2plot[ , type := 'Live']
  
  setnames(frozen2plot, 'ValueFrozen', 'Value')
  setnames(live2plot, 'ValueLive', 'Value')
  
  data4plot <- rbind(frozen2plot, live2plot)
  
  frozenVSlive <- frozenVSlive
  
  return(list(tab = frozenVSlive, plot = data4plot))
  
  })


output$fbs_fias_tab1 <- DT::renderDataTable( server = FALSE, {
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_element_fbs)

  fbs_fias_tab_out <- copy(overviewTab_reac()$tab)
  
  if(nrow(fbs_fias_tab_out) > 0){
  grandtotal <- copy(fbs_fias_tab_out)
  grandtotal$flagObservationStatusFrozen <-  factor(grandtotal$flagObservationStatusFrozen, 
                                                    levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                    ordered = TRUE)
  
  grandtotal$flagObservationStatusLive <-  factor(grandtotal$flagObservationStatusLive, 
                                                    levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                    ordered = TRUE)
  
  grandtotal <- grandtotal[ , c('ValueFrozen', 'ValueLive', 'measuredItemFaostat_L2', 
                                'flagObservationStatusFrozen', 'flagMethodFrozen', 
                                'flagObservationStatusLive', 'flagMethodLive') := list(sum(ValueFrozen, na.rm = TRUE), 
                                                                                       sum(ValueLive, na.rm = TRUE),
                                                                                       'Total', 
                                                                                       max(flagObservationStatusFrozen), 's',
                                                                                       max(flagObservationStatusLive), 's'),
                            by = c('geographicAreaM49_fi',
                                   'measuredElementSuaFbs', 
                                   'timePointYears')]
  
  grandtotal$flagObservationStatusFrozen <- as.character(grandtotal$flagObservationStatusFrozen)
  grandtotal$flagObservationStatusLive <- as.character(grandtotal$flagObservationStatusLive)
  
  setkey(grandtotal)
  grandtotal <- grandtotal[!duplicated(grandtotal)]
  
  fbs_fias_tot <- rbind(fbs_fias_tab_out, grandtotal)
  
  setnames(fbs_fias_tot, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                               'measuredElementSuaFbs', 'timePointYears', 
                               'flagObservationStatusFrozen', 'flagMethodFrozen',
                               'flagObservationStatusLive', 'flagMethodLive'),
           c('Country', 'FBSgroup', 'Element', 'Year', 'FlagFrozen1', 'FlagFrozen2',
             'FlagLive1', 'FlagLive2'))
  
  fbs_fias_tot[ , Diff := round(ValueFrozen - ValueLive, 3)]
  DT::datatable(fbs_fias_tot, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf'))) %>%
    formatStyle(columns = c('Diff'), target = 'row',
                color = styleInterval(c(-0.001, 0.001), c('red', ' ', 'red')))
  } else {
    DT::datatable(data.table()) 
  }
})

output$gg_plot_tab1 <- renderPlot({
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_element_fbs)

  fbs_fias_data <- copy(overviewTab_reac()$plot)

  if(nrow(fbs_fias_data) > 0){
    grandtotal <- copy(fbs_fias_data)
    grandtotal <- grandtotal[ , c('Value', 'measuredItemFaostat_L2') := list(sum(Value, na.rm = TRUE), 'Total'),
                              by = c('geographicAreaM49_fi',
                                     'measuredElementSuaFbs', 
                                     'timePointYears', 'type')]
    setkey(grandtotal)
    grandtotal <- grandtotal[!duplicated(grandtotal)]
    
    fbs_fias_tot <- rbind(fbs_fias_data, grandtotal)
  # Make grand total
  
  ggplot(data = fbs_fias_tot, aes(x = timePointYears, y = Value)) + 
    geom_line(aes(group = type, color = type), size = 0.7) +
    facet_wrap( ~ measuredItemFaostat_L2, scales="free") +
    labs(x = 'Year', color = '') +
    theme(text = element_text(size= 15))
  
  } else {
    ggplot(fbs_fias_data, aes(x = timePointYears, y = Value)) + geom_blank()
  }
  
})