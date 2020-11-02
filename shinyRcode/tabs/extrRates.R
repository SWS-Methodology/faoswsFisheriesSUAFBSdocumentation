# Extraction rates tab

extrR_reac <- reactive({
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  
  if(nrow(live_data$SUAb) == 0){
    if(localrun){
      if(CheckDebug()){
        library(faoswsModules)
        SETTINGS = ReadSettings("sws.yml")
        R_SWS_SHARE_PATH = SETTINGS[["share"]]
        SetClientFiles(SETTINGS[["certdir"]])
        GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                           token = tokenSuaB)
      }
    } else {
      R_SWS_SHARE_PATH = "Z:"
      SetClientFiles("/srv/shiny-server/.R/QA/")
      GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                         token = tokenSuaB)
    }

  KeySUAbal <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      keys = '5423'), 
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced","measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  withProgress(message = 'Extraction rate data loading in progress',
               value = 0, {
                 
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 SUAbalEr <- GetData(KeySUAbal)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  } else {
    
    SUAbalEr <- live_data$SUAb
    SUAbalEr <- SUAbalEr[measuredElementSuaFbs == '5423']
    
  }
  
  
  if(nrow(SUAbalEr) == 0){
    
    ny <- length(sel_years)
    
    SUAbalEr <- rbind(SUAbalEr, data.table(geographicAreaM49_fi = rep(sel_country, ny),
                                           measuredElementSuaFbs = rep('5423', ny),
                                           timePointYears = sel_years,
                                           flagObservationStatus = rep('E', ny),
                                           flagMethod = rep('f', ny)),
                       fill = T)
  }
  
  
  return(SUAbalEr)
})

output$extrR <-  renderRHandsontable({
  
  table <- extrR_reac()
  
  rhandsontable(table, rowHeaders = NULL, width = 'auto', height = 'auto', digits = 6) 
})

new_extr_rate <- reactiveValues(eR = data.table())

observeEvent(input$updER, {
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenSuaB)
    }
  
    } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenSuaB)
  }
  
  
  KeySUAbal <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      keys = '5423'), 
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced","measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  SUAbalEr <- GetData(KeySUAbal)
  
  # copy to compare tables with right decimals number

  SUAbalErComp <- copy(SUAbalEr)
  SUAbalErComp$Value <- round(SUAbalErComp$Value,2)   
  updER <- rhandsontable::hot_to_r(input$extrR)
  
  if(nrow(SUAbalErComp) > 0){

  changed <- length(updER[Value != SUAbalErComp$Value, ]$Value)
  
  newValue <- updER[Value != SUAbalErComp$Value, ]

  # Radio button for the update type 
  if(input$radioErUpdt == 1){
    
    eRupd <- merge(SUAbalEr, newValue, by = c('geographicAreaM49_fi', 'measuredElementSuaFbs', 
                                              'measuredItemFaostat_L2', 'timePointYears'),
                   all = TRUE, suffixes = c('Old', 'New'))
    
    eRupd[!is.na(ValueNew) | ValueNew != ValueOld, c('ValueOld',
                                                     'flagObservationStatusOld',
                                                     'flagMethodOld') := list(ValueNew, 'E', 'f')]
    setnames(eRupd, c('ValueOld',
                      'flagObservationStatusOld',
                      'flagMethodOld'),
             c('Value',
               'flagObservationStatus',
               'flagMethod'))
    
    eRupd <- eRupd[ , c('ValueNew',
                        'flagObservationStatusNew',
                        'flagMethodNew') := NULL]
    
    # Not good for the decimal problem
    #eRupd <- SUAbalEr[Value != updER$Value, c("Value", 
    #                                          "flagObservationStatus",
    #                                          "flagMethod"):= list(updER[Value != SUAbalEr$Value, ]$Value, 'E', 'f')]
    
    showModal(modalDialog(
      title = "Extraction rates updated." ,
      sprintf("The new figures have been saved.")
    ))
    
  
    } else if(input$radioErUpdt == 2) {
    
    if(changed == 1){
    eRupd <- SUAbalEr[timePointYears %in% sel_years & 
                        measuredItemFaostat_L2 %in% newValue$measuredItemFaostat_L2, 
                      c('Value', 'flagObservationStatus', 'flagMethod') := list(newValue$Value, 'E', 'f')]
    } else if (changed > 1){
      
      for(i in 1:changed){
        eRupd <- SUAbalEr
        eRupd <- eRupd[timePointYears %in% sel_years & 
                            measuredItemFaostat_L2 %in% newValue$measuredItemFaostat_L2[i], 
                          c('Value', 'flagObservationStatus', 'flagMethod') := list(newValue$Value[i], 'E', 'f')]
      }
      
    }
    
    showModal(modalDialog(
      title = "Extraction rates updated." ,
      sprintf("The new figures have been saved.")
    ))
  
    }
  
  
  } else {
    
    if(input$radioErUpdt == 1){
      eRupd <- updER[!is.na(Value)]
    } else if(input$radioErUpdt == 2) {

      eRupd <-  updER[rep(seq_len(nrow(updER[!is.na(Value)])), each = length(sel_years)), ]
      for(i in unique(eRupd$measuredItemFaostat_L2)){
        eRupd[measuredItemFaostat_L2 == i]$timePointYears <- sel_years
      }
      
    }
    
    showModal(modalDialog(
      title = "Extraction rates updated." ,
      sprintf("The new figures have been saved.")
    ))
  }
  
  
  new_extr_rate$eR <- eRupd[timePointYears %in% sel_years]
  
  # SaveData(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", data = compare)
  
 })