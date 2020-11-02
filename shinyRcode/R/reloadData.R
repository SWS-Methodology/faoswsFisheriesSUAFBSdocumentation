
reloadData <- function(data, keycountry, minyear, maxyear, keydomain, keydataset,
                       keygeo = 'geographicAreaM49_fi', keytime = 'timePointYears',
                       keyelement= 'measuredElementSuaFbs',
                       keyitem = 'measuredItemFaostat_L2'){
  
  sel_years <- as.character(as.numeric(minyear):as.numeric(maxyear))

if(nrow(data) == 0){
  
  Key <- DatasetKey(domain = keydomain, 
                       dataset = keydataset, 
                       dimensions = list(geographicAreaM49_fi = Dimension(name = keygeo, keys = keycountry),
                                         measuredElementSuaFbs = Dimension(name = keyelement, 
                                                                           GetCodeList(keydomain, 
                                                                                       keydataset,
                                                                                       keyelement )[,code]),
                                         measuredItemFaostat_L2 = Dimension(name = keyitem, 
                                                                            GetCodeList(keydomain, 
                                                                                        keydataset,
                                                                                        keyitem )[,code]),
                                         timePointYears = Dimension(name = keytime, keys =  sel_years )))
  
  withProgress(message = 'Data loading in progress',
               value = 0, {
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 dataKey <- GetData(Key)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  
  data <- dataKey
  return(data)
  
} else if(nrow(data) > 0 &
          unique(data$geographicAreaM49_fi) != keycountry |
          min(unique(data$timePointYears)) != minyear |
          max(unique(data$timePointYears)) != maxyear){
  
  Key <- DatasetKey(domain = keydomain, 
                       dataset = keydataset, 
                       dimensions = list(geographicAreaM49_fi = Dimension(name = keygeo, keys = keycountry),
                                         measuredElementSuaFbs = Dimension(name = keyelement, 
                                                                           GetCodeList(keydomain, 
                                                                                       keydataset,
                                                                                       keyelement )[,code]),
                                         measuredItemFaostat_L2 = Dimension(name = keyitem, 
                                                                            GetCodeList(keydomain, 
                                                                                        keydataset,
                                                                                        keyitem )[,code]),
                                         timePointYears = Dimension(name = keytime, keys =  sel_years )))
  
  withProgress(message = 'Data loading in progress',
               value = 0, {
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 dataKey <- GetData(Key)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  
  data <- dataKey
  return(data)
} else {data <- NULL}
  }




reloadDataToken <- function(data, keycountry, minyear, maxyear, keydomain, keydataset, keytoken,
                            keygeo = 'geographicAreaM49_fi', keytime = 'timePointYears',
                            keyelement= 'measuredElementSuaFbs',
                            keyitem = 'measuredItemFaostat_L2'){
  
  sel_years <- as.character(as.numeric(minyear):as.numeric(maxyear))
  
  if(nrow(data) == 0){
    
    if(localrun){
      if(CheckDebug()){
        library(faoswsModules)
        SETTINGS = ReadSettings("sws.yml")
        R_SWS_SHARE_PATH = SETTINGS[["share"]]
        SetClientFiles(SETTINGS[["certdir"]])
        GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                           token = keytoken)
      }
    } else {
      R_SWS_SHARE_PATH = "Z:"
      SetClientFiles("/srv/shiny-server/.R/QA/")
      GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                         token = keytoken)
    }
    
    Key <- DatasetKey(domain = keydomain, 
                      dataset = keydataset, 
                      dimensions = list(geographicAreaM49_fi = Dimension(name = keygeo, keys = keycountry),
                                        measuredElementSuaFbs = Dimension(name = keyelement, 
                                                                          GetCodeList(keydomain, 
                                                                                      keydataset,
                                                                                      keyelement )[,code]),
                                        measuredItemFaostat_L2 = Dimension(name = keyitem, 
                                                                           GetCodeList(keydomain, 
                                                                                       keydataset,
                                                                                       keyitem )[,code]),
                                        timePointYears = Dimension(name = keytime, keys =  sel_years )))
    
    withProgress(message = 'Data loading in progress',
                 value = 0, {
                   Sys.sleep(0.25)
                   incProgress(0.25)
    dataKey <- GetData(Key)
      Sys.sleep(0.75)
      incProgress(0.95)
    })
    
    data <- dataKey
    return(data)
    
  } else if(nrow(data) > 0 &
            unique(data$geographicAreaM49_fi) != keycountry |
            min(unique(data$timePointYears)) != minyear |
            max(unique(data$timePointYears)) != maxyear){
    
    if(localrun){
      if(CheckDebug()){
        library(faoswsModules)
        SETTINGS = ReadSettings("sws.yml")
        R_SWS_SHARE_PATH = SETTINGS[["share"]]
        SetClientFiles(SETTINGS[["certdir"]])
        GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                           token = keytoken)
      }
    } else {
      R_SWS_SHARE_PATH = "Z:"
      SetClientFiles("/srv/shiny-server/.R/QA/")
      GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                         token = keytoken)
    }
    
    Key <- DatasetKey(domain = keydomain, 
                      dataset = keydataset, 
                      dimensions = list(geographicAreaM49_fi = Dimension(name = keygeo, keys = keycountry),
                                        measuredElementSuaFbs = Dimension(name = keyelement, 
                                                                          GetCodeList(keydomain, 
                                                                                      keydataset,
                                                                                      keyelement )[,code]),
                                        measuredItemFaostat_L2 = Dimension(name = keyitem, 
                                                                           GetCodeList(keydomain, 
                                                                                       keydataset,
                                                                                       keyitem )[,code]),
                                        timePointYears = Dimension(name = keytime, keys =  sel_years )))
    
    withProgress(message = 'Data loading in progress',
                 value = 0, {
                   Sys.sleep(0.25)
                   incProgress(0.25)
                   dataKey <- GetData(Key)
                   Sys.sleep(0.75)
                   incProgress(0.95)
                 })
    
    data <- dataKey
    return(data)
  } else {data <- NULL}
}

