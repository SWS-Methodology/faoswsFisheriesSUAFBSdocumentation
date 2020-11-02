# -- Insert token validated ---- 
output$btn_token1val <- renderUI({
  textInput(inputId = 'btn_token1val', label = "Insert the 'SUA unbalanced validated' session token", value = NA)
})

output$btn_token2val <- renderUI({
  textInput(inputId = 'btn_token2val', label = "Insert the 'SUA balanced validated' session token", value = NA)
})

output$btn_token3val <- renderUI({
  textInput(inputId = 'btn_token3val', label = "Insert the 'FBS FIAS validated' session token", value = NA)
})

output$btn_token4val <- renderUI({
  textInput(inputId = 'btn_token4val', label = "Insert the 'FBS Faostat validated' session token", value = NA)
})

token_val_reac <- reactive({
  
  tokenTabVal <- ReadDatatable('fi_sua_fbs_token_val')
  return(tokenTabVal)
})

output$token_val_tab <- DT::renderDataTable( server = FALSE, {
  tokenOutVal <- token_val_reac()
  DT::datatable(tokenOutVal)
})

observeEvent(input$btn_upd_token_val, {
  
  tokenTabVal <- ReadDatatable('fi_sua_fbs_token_val', readOnly = FALSE)
  
  t1 <- ifelse(is.na(input$btn_token1val), tokenTabVal$token[1], input$btn_token1val)
  t2 <- ifelse(is.na(input$btn_token2val), tokenTabVal$token[2], input$btn_token2val)
  t3 <- ifelse(is.na(input$btn_token3val), tokenTabVal$token[3], input$btn_token3val)
  t4 <- ifelse(is.na(input$btn_token4val), tokenTabVal$token[4], input$btn_token4val)
  date <- as.character(Sys.Date())
  
  tokenTabVal[ , token := c(t1, t2, t3, t4) ]
  tokenTabVal[ , last_upd := date]
  
  changeset <- Changeset('fi_sua_fbs_token_val')
  AddModifications(changeset, tokenTabVal)
  Finalise(changeset)
  
  tokenSuaUval <<- t1
  tokenSuaBval <<- t2
  tokenFbsFiasval <<- t3
  tokenFbsFaostatval <<- t4

  showModal(modalDialog(
    title = "Token updated." ,
    sprintf("The chosen sessions will be updated.")
  ))
  
})

observeEvent(input$update,  {

  sel_country <- country_input[country_input$label == input$btn_country, code]
  # Cancel data from SUA and FBS 
  
  if(input$time2save == 3){
    showModal(modalDialog(
      title = "Missing info!" ,
      sprintf("Please select years to update.")
    ))
  } else{
  
  if(input$time2save == 1){
    sel_years <- input$btn_year
  } else if(input$time2save == 2) {
    sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  } else {
    
  }
  
  # Datasets
    
  if(!is.data.table(updated_data$SUAunbal)){
    
    SUAunbalTot <- reloadDataToken(data = live_data$SUAu, 
                                   keycountry = sel_country, 
                                   minyear = input$btn_start_year, 
                                   maxyear = input$btn_year,
                                   keydomain = domainComm, 
                                   keydataset = datasetSUAUlive,
                                   keytoken = tokenSuaU)
    
    if(!is.null(SUAunbalTot)){
      SUAun <- SUAunbalTot
    } else {
      SUAun <- live_data$SUAu
    } 
    
  } else {
  SUAun <- updated_data$SUAunbal
  }

  SUAun <- SUAun[timePointYears %in% sel_years]
  
  SUAb <- updated_data$SUAbal
  SUAb <- SUAb[timePointYears %in% sel_years]
  
  # SUAbVal <- live_data$SUAbVal
  # SUAbVal <- SUAbVal[timePointYears %in% sel_years]
  # 
  # SUAb <- rbind(SUAb, SUAbVal)
  
  FBSfias <- updated_data$FBSfias 
  FBSfias <- FBSfias[timePointYears %in% sel_years]
  
  FBSfaostat <- updated_data$FBSfaostat
  FBSfaostat <- FBSfaostat[timePointYears %in% sel_years]
  
message('Saving datasets...')
  #-- Save SUA unbalanced validated -----
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenSuaUval)
    }
  } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenSuaUval)
  }

  KeySUAun <- DatasetKey(domain = domainComm, dataset = datasetSUAUval, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  SUAun2blank <- GetData(KeySUAun)
  SUAun2blank <- SUAun2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetSUAUval,
           data = SUAun2blank,
           waitTimeout = Inf)
  
  SUAun$timePointYears <- as.character(SUAun$timePointYears)
  SaveData(domain = domainComm,
           dataset = datasetSUAUval,
           data = SUAun,
           waitTimeout = Inf)
  message('SUA unbalanced validated saved')
  showModal(modalDialog(
    title = "Updating dataset..." ,
    sprintf("1/8")
  ))
  #-- Save SUA unbalanced ---- 
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenSuaU)
    }
  
    } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenSuaU)
  }
  
  KeySUAunLive <- DatasetKey(domain = domainComm, dataset = datasetSUAUlive, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  SUAunlive2blank <- GetData(KeySUAunLive)
  SUAunlive2blank <- SUAunlive2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetSUAUlive,
           data = SUAunlive2blank,
           waitTimeout = Inf)

  SaveData(domain = domainComm,
           dataset = datasetSUAUlive,
           data = SUAun,
           waitTimeout = Inf)
  message('SUA unbalanced live saved')
  showModal(modalDialog(
    title = "Updating dataset..." ,
    sprintf("2/8")
  ))
  
  #-- Save SUA balanced validated ----
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenSuaBval)
    }
  } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenSuaBval)
  }
  
  KeySUAb <- DatasetKey(domain = domainComm, dataset = datasetSUABval, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  SUAb2blank <- GetData(KeySUAb)
  if(nrow(SUAb2blank) > 0){
  SUAb2blank <- SUAb2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetSUABval,
           data = SUAb2blank,
           waitTimeout = Inf)
  }
  SUAb$timePointYears <- as.character(SUAb$timePointYears)
  SaveData(domain = domainComm,
           dataset = datasetSUABval,
           data = SUAb,
           waitTimeout = Inf)
  message('SUA balanced validated saved')
  showModal(modalDialog(
    title = "Updating dataset..." ,
    sprintf("3/8")
  ))
  #-- Save SUA balanced ----
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
  
  KeySUAbLive <- DatasetKey(domain = domainComm, dataset = datasetSUABlive, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  SUAblive2blank <- GetData(KeySUAbLive)
  SUAblive2blank <- SUAblive2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetSUABlive,
           data = SUAblive2blank,
           waitTimeout = Inf)

  SaveData(domain = domainComm,
           dataset = datasetSUABlive,
           data = SUAb,
           waitTimeout = Inf)
  message('SUA balanced live saved')
  showModal(modalDialog(
    title = "Updating dataset..." ,
    sprintf("4/8")
  ))
  #-- Save FBS Fias validated ----
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenFbsFiasval)
    }
  } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenFbsFiasval)
  }

  KeyFbsFias <- DatasetKey(domain = domainComm, dataset = datasetFBSval, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  fbsFias2blank <- GetData(KeyFbsFias)
  fbsFias2blank <- fbsFias2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetFBSval,
           data = fbsFias2blank,
           waitTimeout = Inf)
  
  FBSfias$timePointYears <- as.character(FBSfias$timePointYears)
  SaveData(domain = domainComm,
           dataset = datasetFBSval,
           data = FBSfias,
           waitTimeout = Inf)
  message('FBS Fias validated saved')
  showModal(modalDialog(
    title = "Updating dataset..." ,
    sprintf("5/8")
  ))
  #-- Save FBS Fias ----
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenFbs)
    }
  
    } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenFbs)
  }
  
  KeyFbsFiasLive <- DatasetKey(domain = domainComm, dataset = datasetFBSlive, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  fbsFiaslive2blank <- GetData(KeyFbsFiasLive)
  fbsFiaslive2blank <- fbsFiaslive2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetFBSlive,
           data = fbsFiaslive2blank,
           waitTimeout = Inf)

  FBSfias$timePointYears <- as.character(FBSfias$timePointYears)
  SaveData(domain = domainComm,
           dataset = datasetFBSlive,
           data = FBSfias,
           waitTimeout = Inf)
  message('FBS Fias live saved')
  showModal(modalDialog(
    title = "Updating dataset..." ,
    sprintf("6/8")
  ))
  #-- Save FBS Faostat validated ----
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenFbsFaostatval)
    }
  
    } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenFbsFaostatval)
  }
  
  KeyFbsfaostat <- DatasetKey(domain = domainComm, dataset = datasetFBSfaostatval, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  fbsFaostat2blank <- GetData(KeyFbsfaostat)
  fbsFaostat2blank <- fbsFaostat2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetFBSfaostatval,
           data = fbsFaostat2blank,
           waitTimeout = Inf)
  FBSfaostat$timePointYears <- as.character(FBSfaostat$timePointYears)
  SaveData(domain = domainComm,
           dataset = datasetFBSfaostatval,
           data = FBSfaostat,
           waitTimeout = Inf)
  message('FBS Faostat validated saved')
  showModal(modalDialog(
    title = "Updating dataset..." ,
    sprintf("7/8")
  ))
  #-- Save FBS Faostat -----
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenFbsFaostat)
    }
  
    } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenFbsFaostat)
  }
  
  KeyFbsfaostatlive <- DatasetKey(domain = domainComm, dataset = datasetFBSfaostatlive, dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
  
  fbsFaostatlive2blank <- GetData(KeyFbsfaostat)
  fbsFaostatlive2blank <- fbsFaostatlive2blank[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]
  
  SaveData(domain = domainComm,
           dataset = datasetFBSfaostatlive,
           data = fbsFaostatlive2blank,
           waitTimeout = Inf)

  SaveData(domain = domainComm,
           dataset = datasetFBSfaostatlive,
           data = FBSfaostat,
           waitTimeout = Inf)
  message('FBS Faostat live saved')
  #-- The End ----
  showModal(modalDialog(
    title = "All datasets updated successfully!" ,
    sprintf("Validated data are now saved into the SWS. 
            Please now save data directly into the SWS session.")
  ))
  
  }
  
})