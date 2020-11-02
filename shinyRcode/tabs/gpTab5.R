# Fifth tab, Global production by species tab

gpTab_reac <- reactive({
  
  req(input$btn_group_fbs_tab5,
      input$btn_year, input$btn_country, input$btn_start_year)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_group_fbs <- as.character(groups_input[label %in% input$btn_group_fbs_tab5]$code)

  group_sel <- groups_input[ label %in% input$btn_group_fbs_tab5]$code
  # ICS product in the chosen FBS group
  ICSinput_code <- l2l1[code_l1 %in% sel_group_fbs ]$code_l2
  
  map_asfis_filtered <- map_asfis[ICSProd %in% ICSinput_code]
  # -- Get GP data ----
  
  if(nrow(InitialDatasets$GP) == 0){
  keyDim <- c("geographicAreaM49_fi", "fisheriesAsfis", "measuredElement", "timePointYears")
  
  KeyGlobal <- DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
    fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesCatchArea" )[,code]),
    measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years)))
  
  withProgress(message = 'Global production data loading in progress',
               value = 0, {
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 globalProduction <- GetData(KeyGlobal)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  InitialDatasets$GP <- globalProduction
  
  } else {
    
    globalProduction <- InitialDatasets$GP
  }
  # Aggregate by fisheriesCatchArea
  # Convert flags into ordinal factor so that simple aggregation is possible
  # The function aggregateObservationFlag is too slow so flag are transformed into factors
  globalProduction <- globalProduction[fisheriesAsfis %in% map_asfis_filtered$fisheriesAsfis]
  
  globalProduction$flagObservationStatus <- factor(globalProduction$flagObservationStatus, 
                                                   levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                   ordered = TRUE)
  
  globalProduction <- globalProduction[ , list(ValueAggr = sum(Value, na.rm = TRUE), 
                                               flagObservationStatusAggr = max(flagObservationStatus),
                                               flagMethodAggr = "s"),
                                        by=c("geographicAreaM49_fi",
                                             "fisheriesAsfis",
                                             "measuredElement",
                                             "timePointYears")]
  
  setnames(globalProduction, names(globalProduction), c("geographicAreaM49_fi", "fisheriesAsfis",
                                                        "measuredElement", "timePointYears",
                                                        "Value", "flagObservationStatus",
                                                        "flagMethod"))
  # -- Process GP data ----
  # Hard code change from FI_001 to 5510, both are Production in tonnes.
  globalProduction <- globalProduction[ , measuredElement := "5510"]
  
  globalProductionIcs <- merge(globalProduction, map_asfis_filtered, by = c("fisheriesAsfis"))
  
  globalProductionMapping <- merge(globalProductionIcs, l2l1[ , .(code_l1, code_l2)], 
                               by.x = 'ICSProd', by.y = 'code_l2')
  
  # globalProductionMapping[ , c('Ratio', 'Selection') := list(1, TRUE)]
  # gp_map <- ReadDatatable('gp_mapping', where = paste("country = '", sel_country, "'", sep = ''))#data.table(Country = '', Asfis = '', from = '', to = '', start = '', end = '', ratio = '') 
  # 
  # if(nrow(gp_map) == 0 ){
  #   
  #   gp_map <-  data.table(country = sel_country, asfis = '',
  #                          from_code = '', to_code = '', 
  #                          start_year = input$btn_year, end_year = 'LAST', ratio = '1') 
  # }
  
  validate(need(nrow(globalProductionMapping) >0,
                'No GP data to show.'))
  
  return(list(GP = globalProductionMapping))
  
})

output$gp_tab5 <-  DT::renderDataTable( server = FALSE, {
  
  gp_tab <- copy(gpTab_reac()$GP)
  
  validate(need(nrow(gp_tab) >0,
           'No GP data to show.'))
  
  setnames(gp_tab, c("geographicAreaM49_fi", "fisheriesAsfis",
                         "timePointYears",
                         "flagObservationStatus",
                         "flagMethod", "code_l1"),
           c('Country', 'Species', 'Year', 'F1', 'F2', 'FBSgroup'))

  gp_tab_out <- dcast(gp_tab, Country + FBSgroup + ICSProd + description + Species ~ Year, value.var = c("Value"))
  # Deleted: + Selection + Ratio
  # rhandsontable(gp_tab_out, rowHeaders = NULL, width = 'auto', height = 'auto')
  
  DT::datatable(gp_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
})


gpMap_reac <- reactive({
  
  req(input$btn_country)

  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  gp_map <- ReadDatatable('gp_mapping', where = paste("country = '", sel_country, "'", sep = ''))#data.table(Country = '', Asfis = '', from = '', to = '', start = '', end = '', ratio = '') 
  
  updated_mappings$GP <- gp_map
  
  if(nrow(gp_map) == 0 ){
    
    gp_map <-  data.table(country = sel_country, asfis = '',
                          from_code = '', to_code = '', 
                          start_year = input$btn_year, end_year = 'LAST', ratio = '1') 
  }
  
  return(list(newMap = gp_map))

})


output$gp_map_tab5 <-  renderRHandsontable({
  
  gp_map <- gpMap_reac()$newMap
  rhandsontable(gp_map, rowHeaders = NULL, width = 'auto', height = 'auto')
  
})

observeEvent(input$saveGP, {

  sel_country <- country_input[country_input$label == input$btn_country, code]
  new_map <- rhandsontable::hot_to_r(input$gp_map_tab5)
  
  updated_mappings$GP <- new_map
  
  gp_map <- ReadDatatable('gp_mapping', 
                          where = paste("country = '", sel_country, "'", sep = ''), 
                          readOnly = F) 
  changesetGP <- Changeset('gp_mapping')
  # Add rows to delete to changeset
  AddDeletions(changesetGP, gp_map)
  # Send modifications to the server
  Finalise(changesetGP)
  
  # Add new data (twice) to test from original
  AddInsertions(changesetGP, new_map)
  Finalise(changesetGP)
  
  
  showModal(modalDialog(
    title = "GP mapping changed!" ,
    sprintf("The new mapping will be used in the validation tab.")
  ))
})

