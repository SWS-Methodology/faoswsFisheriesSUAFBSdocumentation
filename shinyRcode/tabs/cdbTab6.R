# Sixth tab, trade by commodity tab

cdbTab_reac <- reactive({
  
  req(input$btn_ics_prod_tab6, input$btn_element_cdb_tab6,
      input$btn_country, input$btn_year, input$btn_start_year)
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_ics_prod <- as.character(groups_input[label %in% input$btn_ics_prod_tab6]$code)
  sel_element_cdb <- as.character(sua_element_input[ label %in% input$btn_element_cdb_tab6]$code)

  # ICS product in the chosen FBS group
  map_isscfc_filtered <- map_isscfc[ICSProd %in% sel_ics_prod]
  # -- Get CDB data ----

  if(nrow(InitialDatasets$CDB) == 0){
    KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total", dimensions = list(
      geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
      measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", 
                                     keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
      measuredElement = Dimension(name = "measuredElement", 
                                  keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
      timePointYears = Dimension(name = "timePointYears", keys = sel_years)))

withProgress(message = 'Commodity data loading in progress',
             value = 0, {
               Sys.sleep(0.25)
               incProgress(0.25)
               commodityDB <- GetData(KeyComm)
               Sys.sleep(0.75)
               incProgress(0.95)
             })
commodityDB$flagObservationStatus <- factor(commodityDB$flagObservationStatus,
                                            levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                            ordered = TRUE)

# Re-export in Export (quantity and values)
commodityDB[measuredElement == '5912', measuredElement := '5910'] # quantity
commodityDB[measuredElement == '5923', measuredElement := '5922'] # Value in 1000$
commodityDB[measuredElement == '5931', measuredElement := '5930'] # Unit value $/t


commodityDB <- commodityDB[!measuredElement %in% c('5907', '5937', 
                                                     '5607', '5637',
                                                     '5906', '5940')]

# Isolate prices (not entering all the processing)
ValueElements <- c('5922', '5930', '5622', '5630')
commodityDBValue <- commodityDB[measuredElement %in% ValueElements]
commodityDB <- commodityDB[!measuredElement %in% ValueElements]

InitialDatasets$CDB <- commodityDB
InitialDatasets$CDBVal <- commodityDBValue
  } else {
  
    commodityDB <- InitialDatasets$CDB
    commodityDBValue <- InitialDatasets$CDBVal
  }
  
commodityDB <- rbind(commodityDB, commodityDBValue)  
  
commodityDB <- commodityDB[measuredItemISSCFC %in% map_isscfc_filtered$measuredItemISSCFC &  measuredElement %in% sel_element_cdb] 
commodityDB$flagObservationStatus <- factor(commodityDB$flagObservationStatus,
                                            levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                            ordered = TRUE)


commodityDBIcs <- merge(commodityDB, map_isscfc_filtered, by = "measuredItemISSCFC")
commodityDBIcs$measuredItemISSCFC <- as.character(commodityDBIcs$measuredItemISSCFC)

commodityDBMapping <- merge(commodityDBIcs, l2l1[ , .(code_l1, code_l2)], 
                                 by.x = 'ICSProd', by.y = 'code_l2')

# commodityDBMapping[ , c('Ratio', 'Selection') := list(1, TRUE)]

isscfc_dim <- GetCodeList(domain = 'FisheriesCommodities', dataset = 'commodities_total',
                          dimension = "measuredItemISSCFC", codes = map_isscfc_filtered$measuredItemISSCFC)[ , .(code, description)]

cdbDescr <- merge(commodityDBMapping, isscfc_dim, by.x = 'measuredItemISSCFC', by.y = 'code')
setnames(cdbDescr, c("geographicAreaM49_fi", "measuredItemISSCFC",
                     "timePointYears", "measuredElement",
                     "flagObservationStatus",
                     "flagMethod", "code_l1"),
         c('Country', 'ISSCFC', 'Year', 'Element', 'F1', 'F2', 'FBSgroup'))
# -- Process CDB data ----
# cdb_map <- ReadDatatable('cdb_mapping', where = paste("country = '", sel_country, "'", sep = ''))  #data.table(Country = '', Isscfc = '', Element = '', from = '', to = '', start = '', end = '', ratio = '') 
# 
# if(nrow(cdb_map) == 0 ){
#   
#   cdb_map <-  data.table(country = sel_country, isscfc = '', element = '', 
#                          from_code = '', to_code = '', 
#                          start_year = input$btn_year, end_year = 'LAST', ratio = '1') 
# }
# 
 return(list(CDB = cdbDescr))

})

output$cdb_tab6 <-  DT::renderDataTable( server = FALSE, {
  if(is.null(cdbTab_reac()$CDB)) return(NULL)
  if(is.null(input$btn_ics_prod_tab6)) return(NULL)
  
  cdb_tab <- copy(cdbTab_reac()$CDB)

  cdb_tab <- cdb_tab[ , .(Country, FBSgroup, ICSProd, description, ISSCFC, Element, Year, Value)]
  if(nrow(cdb_tab) > 0){
  cdb_tab_out <- dcast(cdb_tab, Country + FBSgroup + ICSProd + description + ISSCFC + Element ~ Year, value.var = c("Value"))
  } else {cdb_tab_out <- data.table()}
  # Deleted:+ Ratio + Selection
  # rhandsontable(cdb_tab_out, rowHeaders = NULL, width = 'auto', height = 'auto')
  
  DT::datatable(cdb_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
})



cdbMap_reac <- reactive({
  
  req(input$btn_country)
  sel_country <- country_input[country_input$label == input$btn_country, code]
  cdb_map <- ReadDatatable('cdb_mapping', where = paste("country = '", sel_country, "'", sep = ''))  #data.table(Country = '', Isscfc = '', Element = '', from = '', to = '', start = '', end = '', ratio = '') 
  
  if(nrow(cdb_map) == 0 ){
    
    cdb_map <-  data.table(country = sel_country, isscfc = '', element = '', 
                           from_code = '', to_code = '', 
                           start_year = input$btn_year, end_year = 'LAST', ratio = '1') 
  }
  
  return(list(newMapCDB = cdb_map))
  
})

output$cdb_map_tab6 <-  renderRHandsontable({
  
  cdb_map <- copy(cdbMap_reac()$newMapCDB)
  rhandsontable(cdb_map, rowHeaders = NULL, width = 'auto', height = 'auto') 
})

observeEvent(input$cdb_map_tab6 ,{
  
  new_map <- rhandsontable::hot_to_r(input$cdb_map_tab6)
  updated_mappings$CDB <- new_map
  
})

observeEvent(input$saveCDB, {
 
  sel_country <- country_input[country_input$label == input$btn_country, code]
  new_map <- rhandsontable::hot_to_r(input$cdb_map_tab6)
  
  updated_mappings$CDB <- new_map
  
  cdb_map <- ReadDatatable('cdb_mapping', where = paste("country = '", sel_country, "'", sep = ''), readOnly = F) 
 
  changeset <- Changeset('cdb_mapping')
  # Add rows to delete to changeset
  AddDeletions(changeset, cdb_map)
  # Send modifications to the server
  Finalise(changeset)
  
  # Add new data (twice) to test from original
  AddInsertions(changeset, new_map)
  Finalise(changeset)
  
  showModal(modalDialog(
    title = "CDB mapping changed!" ,
    sprintf("The new mapping will be used in the validation tab.")
  ))
  
})
