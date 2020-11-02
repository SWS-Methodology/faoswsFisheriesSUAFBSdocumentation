# Functions 

GPrecalc <- function(GP, map_asfis, new_map_asfis, year = input$btn_year){

  # Map to ICS
  t1 <- Sys.time()
  gpMap <- merge(GP, map_asfis, by = c("fisheriesAsfis"), all.x = TRUE)

  if(nrow(new_map_asfis) > 0){
    
    new_map_asfis[ end_year == 'LAST', end_year := as.character(year)]
    newMapping <- merge(gpMap, new_map_asfis, 
                        by.x = c('geographicAreaM49_fi', 'fisheriesAsfis'),
                        by.y = c('country', 'asfis'), all = TRUE, allow.cartesian = TRUE)
    
    unchanged <- newMapping[is.na(from_code)]
    unchanged <- rbind(unchanged,  newMapping[!is.na(from_code) & timePointYears > end_year | timePointYears < start_year])
    tochange <- newMapping[!is.na(from_code)]
    tochange <- tochange[ , c('timePointYears', 'end_year', 'start_year') := list(as.numeric(timePointYears), as.numeric(end_year), as.numeric(start_year))]
    tochange1 <- tochange[ timePointYears <= end_year & timePointYears >= start_year & ratio == 1]
    tochange1 <- tochange1[, ics := to_code]
    duplicate <- tochange[ timePointYears <= end_year & timePointYears >= start_year & ratio < 1]
    
    # Allow for splitting
    if(nrow(duplicate) > 0){
      duplicate[ timePointYears <= end_year & timePointYears >= start_year & ratio != 1, c('ics', 'Value') := list(to_code, Value * as.numeric(ratio))]
      duplicate[ , total := sum(as.numeric(ratio)), by = c('geographicAreaM49_fi', 'fisheriesAsfis', 'measuredElement', 'timePointYears','from_code', 'start_year', 'end_year')]
      
      if(nrow(duplicate[total < 1]) > 0){
        duplicate[ , diff := (1-total)]
        addMissingQuantities <- duplicate[diff != 0, ]
        addMissingQuantities[ , c('Value', 'ratio') := list(sum(Value), diff), 
                              by = c('geographicAreaM49_fi', 'fisheriesAsfis', 'measuredElement', 
                                     'timePointYears', 'from_code', 'start_year', 'end_year')]
        addMissingQuantities[ , c('Value', 'to_code', 'ics') := list((Value/total)*as.numeric(ratio), from_code, from_code)]
        setkey(addMissingQuantities)
        addMissingQuantities <- unique(addMissingQuantities)
        duplicate <- rbind(duplicate, addMissingQuantities)
        duplicate[ , diff := NULL]
      }
      duplicate[ , total := NULL]
    }
    changed <- rbind(tochange1, duplicate)
    gpMap_new <- rbind(unchanged, changed) 
    gpMap_new[ , c('from_code', 'to_code', 'start_year', 'end_year', 'ratio'):= NULL]
  } else {
    gpMap_new  <- gpMap
  }
  
  globalProductionAggr <- gpMap_new[, list(Value = sum(Value, na.rm = TRUE),
                                           flagObservationStatus = max(flagObservationStatus),
                                           flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                                        timePointYears,
                                                                        measuredElement,
                                                                        ics)]
  
  globalProductionAggr <- globalProductionAggr[!is.na(ics), ]
  
  t2 <- Sys.time()
  message(paste("GP, okay", t2-t1))
  
  return(globalProductionAggr)

}

CDBrecalc <- function(CDB, map_isscfc, new_map_isscfc, year = input$btn_year){
  
  t1 <- Sys.time()
  commodityDBIcs <- merge(CDB, map_isscfc, by = "measuredItemISSCFC")
  commodityDBIcs$measuredItemISSCFC <- as.character(commodityDBIcs$measuredItemISSCFC)
  
  old_map_isscfc <-  ReadDatatable('cdb_mapping', where = paste("country = '", unique(commodityDBIcs$geographicAreaM49_fi), "'", sep = ''))
  
  # If an updated has been done the the new datatable is used, 
  # otherwise if the CDB tab has not even been opened
  # the current SWS datatable is used
  
  if(nrow(new_map_isscfc) > 0){
    
    new_map_isscfc <- new_map_isscfc
    
  } else {
    
    new_map_isscfc <- old_map_isscfc
    
  }
  
  # Account for commodity deviation
  if(nrow(new_map_isscfc) > 0){
    new_map_isscfc[ end_year == 'LAST', end_year := as.character(year)]
    newMappingCDB <- merge(commodityDBIcs, new_map_isscfc,
                           by.x = c('geographicAreaM49_fi', 'measuredElement','measuredItemISSCFC'),
                           by.y = c('country', 'element','isscfc'), all = TRUE, allow.cartesian = TRUE)
    
    unchangedCDB <- newMappingCDB[is.na(from_code)]
    unchangedCDB <- rbind(unchangedCDB,  newMappingCDB[!is.na(from_code) & timePointYears > end_year | timePointYears < start_year])
    tochangeCDB <- newMappingCDB[!is.na(from_code)]
    tochangeCDB <- tochangeCDB[ , c('timePointYears', 'end_year', 'start_year') := list(as.numeric(timePointYears), as.numeric(end_year), as.numeric(start_year))]
    tochangeCDB1 <- tochangeCDB[ timePointYears <= end_year & timePointYears >= start_year & ratio == 1]
    tochangeCDB1 <- tochangeCDB1[, ics := to_code]
    duplicateCDB <- tochangeCDB[ timePointYears <= end_year & timePointYears >= start_year & ratio < 1]
    
    # Allow for splitting
    if(nrow(duplicateCDB) > 0){
      duplicateCDB[ timePointYears <= end_year & timePointYears >= start_year & ratio != 1, c('ics', 'Value') := list(to_code, Value * as.numeric(ratio))]
      duplicateCDB[ , total := sum(ratio), by = c('geographicAreaM49_fi', 'measuredItemISSCFC', 'measuredElement', 'timePointYears','from_code', 'start_year', 'end_year')]
      
      if(nrow(duplicateCDB[total < 1]) > 0){
        duplicateCDB[ , diff := (1-total)]
        addMissingQuantitiesCDB <- duplicateCDB[diff != 0, ]
        addMissingQuantitiesCDB[ , c('Value', 'ratio') := list(sum(Value), diff), 
                                 by = c('geographicAreaM49_fi', 'measuredItemISSCFC', 'measuredElement', 
                                        'timePointYears', 'from_code', 'start_year', 'end_year')]
        addMissingQuantitiesCDB[ , c('Value', 'to_code', 'ics') := list((Value/total)*as.numeric(ratio), from_code, from_code)]
        setkey(addMissingQuantitiesCDB)
        addMissingQuantitiesCDB <- unique(addMissingQuantitiesCDB)
        duplicateCDB <- rbind(duplicateCDB, addMissingQuantitiesCDB)
        duplicateCDB[ , diff := NULL]
      }
      duplicateCDB[ , total := NULL]
    }
    changedCDB <- rbind(tochangeCDB1, duplicateCDB)
    cdbMap_new <- rbind(unchangedCDB, changedCDB) 
    cdbMap_new <- rbind(unchangedCDB, changedCDB[ics != '9999']) # 9999 is a code when the production of the commodity does not have to be considered
    cdbMap_new[ , c('from_code', 'to_code', 'start_year', 'end_year', 'ratio'):= NULL]
    # Sum by ICS, no commodities anymore
    } else {
    cdbMap_new <- commodityDBIcs
  }
  

  # Link table for special period ICS group changes
  link_table <- ReadDatatable("link_table")
  
  ## Checks on link table
  # quantity different from 100% allocated
  link_table[ , check := sum(percentage), by=c("geographic_area_m49","flow","start_year","end_year","from_code")]
  
  linkCorrespondence <- ReadDatatable('link_table_elements')
  setnames(linkCorrespondence, old = 'measuredelement', new = 'measuredElement')
  
  link_table2 <- merge(link_table, linkCorrespondence, by = "flow", allow.cartesian = TRUE)
  
  if(max(as.numeric(cdbMap_new$timePointYears)) == -Inf){
    yearmax<- year
    } else {yearmax<- max(as.numeric(cdbMap_new$timePointYears))}
  
  link_table2$end_year <- ifelse(link_table2$end_year == "LAST", yearmax,
                                 link_table2$end_year)

  link_table2 <- link_table2[end_year >= as.character(start_year)]
  
  years <- expand.grid(as.character(1:nrow(link_table2)), 1948:year)
  years <- as.data.table(years)
  setnames(years, c('Var1','Var2'), c('idx', 'timePointYears'))
  
  link_table2[ , idx := row.names(link_table2) ]
  
  link_table3 <- merge(link_table2, 
                       years, by = 'idx')
  
  link_table3 <- link_table3[timePointYears >= start_year & timePointYears <= end_year]
  link_table3[ , idx := NULL]
  link_table3[ ,timePointYears := as.character(timePointYears)]
  # Change ICS codes
  message('From table to CDB')
  commodityDBLink <- merge(cdbMap_new, link_table3, 
                           by.x = c("geographicAreaM49_fi", "measuredElement", "timePointYears", "ics"),
                           by.y = c("geographic_area_m49", "measuredElement", "timePointYears","from_code"), 
                           all.x = TRUE, allow.cartesian = TRUE)
  
  setkey(commodityDBLink)
  commodityDBLink <- unique(commodityDBLink)
  
  # Avoid NAs for periods
  commodityDBLink$start_year <- ifelse(is.na(commodityDBLink$start_year), "1900", commodityDBLink$start_year)
  commodityDBLink$end_year <- ifelse(is.na(commodityDBLink$end_year), "9999", commodityDBLink$end_year)
  
  # commodityDBLink <- commodityDBLink[timePointYears >= start_year, ]
  # commodityDBLink <- commodityDBLink[timePointYears <= end_year]
  
  # Change ICS for defined periods
  commodityDBLink[!is.na(to_code) & 
                    as.numeric(timePointYears) >= as.numeric(start_year) &
                    as.numeric(timePointYears) <= as.numeric(end_year), ics := to_code]
  
  commodityDBLink[!is.na(percentage) , Value := Value*percentage]
  
  # remove unnecessary dimensions
  commodityDBLink <- commodityDBLink[ , c("flow", "start_year", "end_year", "percentage", "to_code", "check") := NULL]
  
  # Some commodities are not imported for food porpuses (e.g. "ornamental fish").
  # Those flow are deviated to "other utilizations"
  
  otherUses <- ReadDatatable('other_uses')
  
  commodityDBotherUses <- merge(commodityDBLink, otherUses, 
                                by.x = c( "measuredItemISSCFC", "measuredElement", "ics"),
                                by.y = c("isscfc", "measured_element_orig", "ics"))
  
  commodityDBotherUses$measuredElement <- ifelse(is.na(commodityDBotherUses$measured_element_dest),
                                                 commodityDBotherUses$measuredElement,
                                                 commodityDBotherUses$measured_element_dest)
  
  commodityDBotherUses <- commodityDBotherUses[ , c("label", "measured_element_dest", "fias_code") := NULL]
  
  commodityDBdeviated <- rbind(commodityDBLink, commodityDBotherUses)
  
  
  commodityDBAggr <- commodityDBdeviated[ , list(Value = sum(Value, na.rm = TRUE),
                                        flagObservationStatus = max(flagObservationStatus),
                                        flagMethod = "s"),
                                 by = list(geographicAreaM49_fi,
                                           timePointYears,
                                           measuredElement,
                                           ics)]
  
  tradeQ <- commodityDBAggr[measuredElement %in% c('5910', '5610')]
  tradeV <- commodityDBAggr[measuredElement %in% c('5922', '5622')]
  tradeQ[measuredElement == '5910', flow := 'EXP']
  tradeQ[measuredElement == '5610', flow := 'IMP']
  tradeV[measuredElement == '5922', flow := 'EXP']
  tradeV[measuredElement == '5622', flow := 'IMP']
  
  if(nrow(tradeQ) > 0){
  tradeUV <- merge(tradeQ, tradeV, by = c('geographicAreaM49_fi',
                                          'timePointYears',
                                          'flow',
                                          'ics'), all = T,
                   suffixes = c('Q', 'V'))
  
  tradeUV <- tradeUV[, c('Value', 'flagObservationStatus', 'flagMethod') := 
                       list(ValueV/ValueQ, flagObservationStatusV, "c")]
  tradeUV[flow == 'IMP', measuredElement := '5630' ]
  tradeUV[flow == 'EXP', measuredElement := '5930' ]
  tradeUV[is.nan(Value), Value := 0]
  tradeUV[ValueQ == 0, Value := ValueV]
  
  
  commodityDBAggrTot <- rbind(commodityDBAggr[!measuredElement %in% c('5930', '5630')], 
                              tradeUV[,.(geographicAreaM49_fi,
                                         timePointYears,
                                         measuredElement,
                                         ics, Value, 
                                         flagObservationStatus, 
                                         flagMethod)])
  } else {
    commodityDBAggrTot <- commodityDBAggr
  }
  
  t2 <- Sys.time()
  message(paste("CDB, okay", t2-t1))
  return(commodityDBAggrTot)
}

SUAunbalCalc <- function(globalProductionAggr, commodityDBAggr){
t1 <- Sys.time()
  SUA <- rbind(globalProductionAggr, commodityDBAggr)
  yearVals <- unique(SUA$timePointYears)
  setnames(SUA, "ics", "measuredItemFaostat_L2")
  
  SUA <- SUA[ , list(Value = sum(Value, na.rm = TRUE),
                     flagObservationStatus = max(flagObservationStatus),
                     flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                  timePointYears,
                                                  measuredElement,
                                                  measuredItemFaostat_L2)]
  setnames(SUA, 'measuredElement', 'measuredElementSuaFbs')
  SUA <- SUA[!is.na(Value)]
  
  elementSignTable <- ReadDatatable('element_sign_table')
  setnames(elementSignTable, 'measured_element', 'measuredElementSuaFbs')
  
  # Now not considering food processing (as in plugin FP calculated later)
  SUAexpanded <- merge(SUA[measuredElementSuaFbs != "5023"], 
                       elementSignTable[ , .(measuredElementSuaFbs, sign)], 
                       by = "measuredElementSuaFbs", all.x = TRUE)
  
  SUAexpanded[, availability := sum(Value * sign, na.rm = TRUE), 
              by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  initialUnbal <- SUAexpanded[availability < 0]
  t2 <- Sys.time()
  message(paste("SUA unbal, okay", t2-t1))
 return(list(SUAunbal = SUA,
             initialUnbal = initialUnbal))
}

SUAbalCalc <- function(SUA, eR, use){
  t1 <- Sys.time()
  SUAno131 <- SUA[ measuredElementSuaFbs != "5023"]
  SUA131 <- SUA[ measuredElementSuaFbs == "5023"]
  
  elementSignTable <- ReadDatatable('element_sign_table')
  setnames(elementSignTable, 'measured_element', 'measuredElementSuaFbs')
  
  # Now only considering production, import and export to compute availability
  # then after calculations we compare official food processing data with calculations

  SUAexpanded <- merge(SUAno131, elementSignTable[ , .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)

  SUAexpanded[, availability := sum(Value * sign, na.rm = TRUE), 
              by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  # Check no negative primary availability. 
  # Now many production data are missing in the commodity DB in SWS so 
  # there are negative primary availabilities
  map_asfis <- ReadDatatable('map_asfis')
  setnames(map_asfis, c("asfis", "ics"), c("fisheriesAsfis", "measuredItemFaostat_L2"))
  primary <- unique(map_asfis$measuredItemFaostat_L2)
  primaryneg <- SUAexpanded[availability < 0 & measuredItemFaostat_L2 %in% primary]
  
  if(nrow(primaryneg) > 0){
    countriesneg <- unique(primaryneg[ , .(geographicAreaM49_fi, measuredItemFaostat_L2, timePointYears)])
    msgg <- apply(countriesneg,1, paste0, collapse = ', ')
    msg2email4 <- paste0('There are negative primary availabilities. Check (country code, product code, year): ',
                         paste0(msgg, collapse = " and "))
    message(msg2email4)
  } else {
    
    msg2email4 <- ''
  }
  
  rou <- copy(primaryneg)
  rou[ , c('measuredElementSuaFbs', 
           'Value', 
           'flagObservationStatus', 
           'flagMethod', 'sign') := list('5166', availability,
                                 'I', 'i', -1)]
  
  setkey(rou)
  rou <- unique(rou)
  
  SUAexpanded <- rbind(SUAexpanded, rou)
  
  secondaryneg0 <- SUAexpanded[availability < 0 & !measuredItemFaostat_L2 %in% primary]
  setkey(secondaryneg0)
  secondaryneg0 <- unique(secondaryneg0)
  
  yearVals <- as.character(min(unique(as.numeric(as.character(SUAexpanded$timePointYears)))):max(unique(as.numeric(as.character(SUAexpanded$timePointYears)))))
  # Delete old imbalances stored
  imbalance_store <- ReadDatatable('imbalance_tab', readOnly = FALSE)
  if(nrow(imbalance_store[ geographicaream49_fi %in% unique(secondaryneg0$geographicAreaM49_fi) &
                           timepointyears %in% yearVals, ]) > 0){
    changeset <- Changeset('imbalance_tab')
    AddDeletions(changeset, imbalance_store[ geographicaream49_fi %in% unique(secondaryneg0$geographicAreaM49_fi) & timepointyears %in% yearVals, ])
    Finalise(changeset)
  }
  
  # Add new imbalances
  secondarynegCompliant <- copy(secondaryneg0)
  secondarynegCompliant <- secondarynegCompliant[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                      timePointYears, availability)]
  setkey(secondarynegCompliant)
  secondarynegCompliant <- unique(secondarynegCompliant)
  
  setnames(secondarynegCompliant,
           c('geographicAreaM49_fi', 'timePointYears',
             'measuredItemFaostat_L2'),
           c('geographicaream49_fi', 'timepointyears',
             'measureditemfaostat_l2'))
  
  changeset <- Changeset('imbalance_tab')
  AddInsertions(changeset, secondarynegCompliant)
  Finalise(changeset)
  
  if(nrow(secondaryneg0) > 0){
    countriessecneg <- unique(secondaryneg0[ , .(geographicAreaM49_fi, measuredItemFaostat_L2, timePointYears)])
    msgg2 <- apply(countriessecneg,1, paste0, collapse = ', ')
    msg2email5 <- paste0('There are still negative secondary availabilities. Check (country code, product code, year): : ',
                         paste0(msgg2, collapse = " and "))
    message(msg2email5)
  } else {
    
    msg2email5 <- ''
  }
  
  mealCodes <- GetCodeList("FisheriesCommodities",
                           "fi_sua_balanced_legacy",
                           "measuredItemFaostat_L2")[ grepl('meals', description)]$code
  
  if(any(secondaryneg0$measuredItemFaostat_L2 %in% mealCodes)){
    mealsUnbal <- secondaryneg0[measuredItemFaostat_L2 %in% mealCodes]
    message('Unbalance for meal products!')
    secondaryneg <- secondaryneg0[!measuredItemFaostat_L2 %in% mealCodes]
  } else {
    secondaryneg <- secondaryneg0
  }
  
  rouMeals <- copy(secondaryneg0[measuredItemFaostat_L2 %in% mealCodes])
  rouMeals[ , c('measuredElementSuaFbs', 
                'Value', 
                'flagObservationStatus', 
                'flagMethod', 'sign') := list('5166', availability,
                                              'I', 'i', -1)]
  setkey(rouMeals)
  rouMeals <- unique(rouMeals)
  
  if(nrow(secondaryneg) > 0){
    
    # Make sure all production (5510) values have been imputed
    
    icsneg <- unique(secondaryneg$measuredItemFaostat_L2)
    setkey(secondaryneg, geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability)
    prod2add <- unique(secondaryneg[ , .(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability) ])
    
    # add production element with NA values and flags then estimate as in Francesca code with estimation flags
    prod2add[ , ':=' (measuredElementSuaFbs = '5510', Value = - availability,
                      flagObservationStatus = as.factor('I'), flagMethod = 'i', sign = 1)]
    
    # SUA with all production values
    SUAwithProdupd <- merge(secondaryneg, prod2add, by = c('geographicAreaM49_fi',
                                                           'timePointYears',
                                                           'measuredItemFaostat_L2',
                                                           'availability',
                                                           'measuredElementSuaFbs'),
                            suffixes = c('', '_added'), all = TRUE)
    SUAwithProdupd$sign_added <- as.integer(SUAwithProdupd$sign_added)
    
    SUAwithProdupd[measuredElementSuaFbs == '5510' , c("Value", "sign",
                                                       "flagObservationStatus",
                                                       "flagMethod") := list(ifelse(is.na(Value), Value_added, 
                                                                                    Value+Value_added),
                                                                             sign_added,
                                                                             flagObservationStatus_added,
                                                                             flagMethod_added)]
    
    # Putting together values with negative and positive availability which had been separated before
    SUAcomplement <- SUAexpanded[!secondaryneg, on = names(secondaryneg)]
    SUAcomplement <- rbind(SUAcomplement, rouMeals)
    
    # Putting together values with negative and positive availability which had been separated before
    SUAwithProd <- rbind(SUAwithProdupd[ , .(geographicAreaM49_fi, timePointYears,
                                             measuredItemFaostat_L2, availability,
                                             measuredElementSuaFbs, Value,
                                             flagObservationStatus, flagMethod, sign)],
                         SUAcomplement)
  
    # SUAwithProd[ , sign := NULL ]
    } else {
    
    SUAwithProd <- SUAexpanded
    # SUAwithProd[ , sign := NULL ]
    
  }

  SUAwithProd[, availability := sum(Value * sign, na.rm = TRUE), 
              by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  SUAwithProd[ , sign := NULL]
  
  
  tree <- ReadDatatable('fi_commodity_tree')
  treePrim <- copy(tree)
  treePrim <- treePrim[parent %in% primary ]

  yearVals <- as.character(min(unique(as.numeric(as.character(SUAwithProd$timePointYears)))):max(unique(as.numeric(as.character(SUAwithProd$timePointYears)))))
  SUAvalEr <- SUAwithProd[measuredElementSuaFbs == '5423' & timePointYears != max(yearVals)]
  FPproblems <- list()
  
  if(nrow(SUAwithProd[!measuredItemFaostat_L2 %in% primary]) > 0){
  message("fi_SUA-FBS: Calculating extraction rates")

  SUAwithEr <- eRcomputation(data = SUAwithProd, 
                             tree = treePrim, 
                             primary = primary,
                             oldEr = SUAvalEr, years = yearVals)
  
  # If updating extraction rates
  if(use == 1){
SUAnewEr <- merge(SUAwithEr, eR, by = c('geographicAreaM49_fi',
                            'measuredItemFaostat_L2',
                            'measuredElementSuaFbs',
                            'timePointYears'), all.x = TRUE,
                  suffixes = c('','New'))
  
SUAnewEr[measuredElementSuaFbs == '5423', Value := ifelse(!is.na(ValueNew) & Value != ValueNew, ValueNew, Value)]
SUAnewEr[ , c('ValueNew', 'flagObservationStatusNew', 'flagMethodNew'):=NULL]
  } 
  else {
    SUAnewEr <- copy(SUAwithEr)
    
}
  
  message("fi_SUA-FBS: Calculating input element")
  SUAinput <- inputComputation(data = SUAnewEr, primary = primary, use)
  
  newTree <- merge(tree, unique(SUAinput[ measuredElementSuaFbs == '5423' & !is.na(Value),
                                          .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, Value)]), 
                   by.x = 'child', by.y = 'measuredItemFaostat_L2', all.x = TRUE, allow.cartesian = TRUE)
  newTree[ , extraction_rate := Value ]
  newTree[ , Value:= NULL]
  
  #--Food processing ----
  message("fi_SUA-FBS: Calculating food processing")
  
  FPdata_alltest <- foodProcessingComputation(SUAinput = SUAinput, 
                                              treeNewER = newTree, primary = primary)
  FPdatatest <- FPdata_alltest$result
  FPdatatest <- FPdatatest[Value != 0]
  FPproblemstest <- list()
  FPproblemstest <- FPdata_alltest$problems
  ###########
  
  # Change of input to calculate FP for problematic data ----
  
  # Get problematic groups
  if(any(sapply(FPproblemstest, nrow)>0)){
    avoidProblems <- rbindlist(FPproblemstest, fill = TRUE)
    avoidProblems <- unique(avoidProblems[ , .(geographicAreaM49_fi,
                                               timePointYears,
                                               parent_primary)])
    
    # Parent-child tree
    treeneeded0 <- data.table(parent = unique(avoidProblems$parent_primary),
                              child = unique(avoidProblems$parent_primary))
    treeneeded <- unique(newTree[parent %in% unique(avoidProblems$parent_primary), .(parent, child) ])
    treeneeded <- rbind(treeneeded, treeneeded0)
    
    # Get complete structure of problematic groups
    avoidProblems2 <- merge(avoidProblems, treeneeded,
                            by.x = 'parent_primary',
                            by.y = 'parent', all.x = TRUE,
                            allow.cartesian = TRUE)
    setnames(avoidProblems2, 'child', 'measuredItemFaostat_L2')
    
    # The problematic groups with food are recalculated without food
    SUAnoFood0 <- SUAinput[measuredElementSuaFbs != '5141' ]
    
    SUAnoFood <- merge(SUAnoFood0, elementSignTable[ , .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)
    
    SUAnoFood[, availability := sum(Value * sign, na.rm = TRUE), 
                by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
    
    SUAnoFood[ , sign := NULL]
    
    subst <- merge(SUAnoFood, avoidProblems2[ , .(geographicAreaM49_fi,
                                                 timePointYears,
                                                 measuredItemFaostat_L2)],
                   by = c("geographicAreaM49_fi",
                          "timePointYears", 
                          "measuredItemFaostat_L2"))
    
    # The part that was okay with food is recalculated without the problematic part
    cancel <- merge(SUAinput, avoidProblems2[ , .(geographicAreaM49_fi,
                                                 timePointYears,
                                                 measuredItemFaostat_L2)],
                    by = c("geographicAreaM49_fi",
                           "timePointYears", 
                           "measuredItemFaostat_L2"))
    
    SUAinputcan <- SUAinput[!cancel, on = names(SUAinput)]
  
    if(nrow(SUAinputcan) > 0){
    FPdata_all1 <- foodProcessingComputation(SUAinput = SUAinputcan, 
                                             treeNewER = newTree, primary = primary)
    FPdata1 <- FPdata_all1$result
    
    
    FPdata1 <- FPdata1[Value != 0]
    FPproblems1 <- list(primary = data.table(),
                        secondaryTot = data.table(),
                        secondary = data.table(),
                        tertiary = data.table(),
                        quaternary = data.table(),
                        NotCovered = data.table())
    FPproblems1 <- FPdata_all1$problems
    } else {
      FPdata1 <- data.table()
      FPproblems1 <- list(primary = data.table(),
                          secondaryTot = data.table(),
                          secondary = data.table(),
                          tertiary = data.table(),
                          quaternary = data.table(),
                          NotCovered = data.table())
    }
    # FP calculated for problematic elements
    
    FPdata_all2 <- foodProcessingComputation(SUAinput = subst,
                                             treeNewER = newTree,
                                             primary = primary)
    
    FPdata2 <- FPdata_all2$result
    FPdata2 <- FPdata2[Value != 0]
    FPproblems2 <- list(primary = data.table(),
                        secondaryTot = data.table(),
                        secondary = data.table(),
                        tertiary = data.table(),
                        quaternary = data.table(),
                        NotCovered = data.table())
    FPproblems2 <- FPdata_all2$problems
    
    # Put together results and dataset to consider
    FPdata <- rbind(FPdata1, FPdata2)
    FPproblems <- FPproblems2
    SUAnoFP <- rbind(SUAinputcan, subst)
  } else {SUAnoFP <- SUAinput
  FPdata <- FPdatatest
  FPproblems <- list(primary = data.table(),
                      secondaryTot = data.table(),
                      secondary = data.table(),
                      tertiary = data.table(),
                      quaternary = data.table(),
                      NotCovered = data.table())}
  
  message('Food re-processing okay')
  

  FPdata <- FPdata[ , availability := NULL ]
  FPdata[ , c("flagObservationStatus", "flagMethod") := list("E", "i")]
  
  SUAnoFP[ , availability := NULL]

  # If processing value changed in the shiny, i.e. flagged as (E,f)
  # then value flagged (E,f) prevail on the computed one
  
  FPdataupd <- merge(SUA131, FPdata, by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2',
                                            'timePointYears', 'measuredElementSuaFbs'),
                     all = TRUE, suffixes = c('', 'Recalc'))

  # FPdataupd[is.na(Value) | flagMethodRecalc == 'f', c('Value',
  #                                               'flagObservationStatus',
  #                                               'flagMethod') := list(ValueRecalc,
  #                                                                     flagObservationStatusRecalc,
  #                                                                     flagMethodRecalc)]
  
  FPdataupd[!is.na(ValueRecalc), c('Value',
                                   'flagObservationStatus',
                                   'flagMethod') := list(ValueRecalc,
                                                         flagObservationStatusRecalc,
                                                         flagMethodRecalc)]
  
  FPdataupd[ , c('ValueRecalc',
                 'flagObservationStatusRecalc',
                 'flagMethodRecalc') := NULL]
  
  
  SUAunbal <- rbind(SUAnoFP[!is.na(Value), ], FPdataupd[!is.na(Value), ])
  SUAunbal$flagObservationStatus <- as.character(SUAunbal$flagObservationStatus)

  } else {
    SUAunbal <- SUAwithProd[!is.na(Value), ]
  }
  # R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
  # 
  # saveRDS(FPproblems,
  #         file.path(R_SWS_SHARE_PATH, "taglionic", "FPfisheries", "FoodProcessingFeedback.rds")
  # )

  if(length(FPproblems) > 0 & exists('FPproblems$NotCovered')){
    if(nrow(FPproblems$NotCovered) > 0){
    uncovered <- copy(FPproblems$NotCovered)
    uncovered[ , measuredElementSuaFbs := '5023']
    setnames(uncovered, c('parent_primary', 'UncoveredQuantity'),
             c('measuredItemFaostat_L2', 'Value'))

    rouUncovered <-copy(uncovered)
    rouUncovered[ , measuredElementSuaFbs := '5166']
    rouUncovered[ , Value := -Value]

    uncoveredAdjusted <- rbind(uncovered, rouUncovered)
    uncoveredAdjusted[ , c("flagObservationStatus", "flagMethod") := list("E", "i")]
    uncoveredAdjusted[ measuredElementSuaFbs == '5166' , c("flagObservationStatus", "flagMethod") := list("I", "i")]
}
  } else {
    uncoveredAdjusted <- data.table()
  }

  SUAunbal <- rbind(SUAunbal, uncoveredAdjusted)
  SUAunbal$flagObservationStatus <- factor(SUAunbal$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)
  SUAunbal <- SUAunbal[ , list(Value = sum(Value, na.rm = TRUE),
                               flagObservationStatus = max(flagObservationStatus),
                               flagMethod = 's'), 
                        by = c("geographicAreaM49_fi", "timePointYears",
                               "measuredItemFaostat_L2", "measuredElementSuaFbs")]
  
  setkey(SUAunbal)
  SUAunbal <- unique(SUAunbal)
  
  #-- Balancing ----
  
  SUAunbal <-  merge(SUAunbal, elementSignTable[, .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)
  SUAunbal$timePointYears <- as.character(SUAunbal$timePointYears)
  SUAunbal <- SUAunbal[measuredElementSuaFbs != '645']
  if(any(is.na(SUAunbal$sign))){
    message(paste(SUAunbal[is.na(SUAunbal$sign), ], 
                  ' is an element in the SUA not included in the availability calculation.'))
  }
  
  SUAunbal[, availability := sum(Value * sign, na.rm = TRUE), 
           by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  balancingElements <- ReadDatatable('balancing_elements')
  setnames(balancingElements, names(balancingElements), c("geographicAreaM49_fi", 
                                                          "measuredItemFaostat_L2",
                                                          "measuredElementSuaFbs",
                                                          "start_year", "end_year", "share"))
  
  balancingElements[ end_year == "LAST"]$end_year <- as.character(max(unique(as.numeric(SUAunbal$timePointYears))))
  
  balancingValues <- unique(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , measuredItemFaostat_L2, availability) ])
  
  balancing <- merge(balancingElements, 
                     balancingValues, by = c("geographicAreaM49_fi","measuredItemFaostat_L2"),
                     all.y = TRUE)
  setnames(balancing, c("availability"), c("Value"))
  
  if(any(is.na(balancing$measuredElementSuaFbs)) & any(balancing[is.na(measuredElementSuaFbs)]$availability != 0)){
    message('Balancing elements missing!')
    message(balancing[is.na(measuredElementSuaFbs) & availability != 0])
  }
  
  balancing2merge <- balancing[ as.numeric(timePointYears) >= as.numeric(start_year) & as.numeric(timePointYears) <= as.numeric(end_year), Value := Value*share]
  balancing2merge[ , c('start_year', 'end_year', 'share') := NULL]
  balancing2merge[ , c('flagObservationStatus', 'flagMethod') := list('E','b')]
  
  # Balancing cannot be negative
  balancingproblems <- balancing2merge[round(Value,6) < 0,]
  
  balancingproblems_store <- ReadDatatable('balancing_problems_tab', readOnly = FALSE)

  # Store balancing problems
  balancingproblems_store <- ReadDatatable('balancing_problems_tab', readOnly = FALSE)
  if(nrow(balancingproblems_store[ geographicaream49_fi %in% unique(SUAunbal$geographicAreaM49_fi) & 
                                   timepointyears %in% yearVals, ]) > 0){
    changeset <- Changeset('balancing_problems_tab')
    AddDeletions(changeset, balancingproblems_store[ geographicaream49_fi %in% unique(SUAunbal$geographicAreaM49_fi) & timepointyears %in% yearVals, ])
    Finalise(changeset)
  }
  # Add new imbalances
 
  if(length(FPproblems) > 0 & exists('FPproblems$NotCovered')){
    if(nrow(FPproblems$NotCovered) > 0){
    toupload <- copy(FPproblems$NotCovered)
    toupload[ , measuredElementSuaFbs := '5023']
    setnames(toupload, c('parent_primary', 'UncoveredQuantity'),
             c('measuredItemFaostat_L2', 'Value'))
    balancingproblemsCompliant <- rbind(toupload, balancingproblems[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                                         timePointYears, measuredElementSuaFbs, Value)])
    }
    } else {
    balancingproblemsCompliant <- rbind(balancingproblems[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                               timePointYears, measuredElementSuaFbs, Value)])
  }
  
  if(nrow(balancingproblemsCompliant) > 0){
  setkey(balancingproblemsCompliant)
  balancingproblemsCompliant <- unique(balancingproblemsCompliant)
  
  setnames(balancingproblemsCompliant,
           c('geographicAreaM49_fi', 'timePointYears',
             'measuredItemFaostat_L2', 'measuredElementSuaFbs', 'Value'),
           c('geographicaream49_fi', 'timepointyears',
             'measureditemfaostat_l2', 'measuredelementsuafbs', 'value'))
  
  changeset <- Changeset('balancing_problems_tab')
  AddInsertions(changeset, balancingproblemsCompliant)
  Finalise(changeset)
  }
  # SUAbal1 <- rbind(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , 
  #                               measuredItemFaostat_L2, measuredElementSuaFbs, 
  #                               Value, flagObservationStatus, flagMethod)], balancing2merge)
  # # Sum if there is a balancing elements that was already present there
  
  # if negative balancing element then balance imbalance with 5166
  balancingimb <- copy(balancing2merge[Value < 0])
  balancingimb <- balancingimb[Value < 0, c('measuredElementSuaFbs', 
                                            'flagObservationStatus', 
                                            'flagMethod') := list('5166', 'I', 'i')]
  balancingimb[ , Value := Value]
  
  balancingTot <- rbind(balancing2merge[Value > 0], balancingimb)

  SUAbal <- merge(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , 
                                measuredItemFaostat_L2, measuredElementSuaFbs, 
                                Value, flagObservationStatus, flagMethod)], 
                  balancingTot,
                  by = c('geographicAreaM49_fi', 'timePointYears', 
                         'measuredItemFaostat_L2', 'measuredElementSuaFbs'),
                  suffixes = c('','Bal'), 
                  all = TRUE)
  
  SUAbal[is.na(ValueBal), ValueBal := 0 ]
  SUAbal[is.na(Value), Value := 0]
  SUAbal[ , Value := Value + ValueBal ]
  SUAbal$flagObservationStatus <- as.character(SUAbal$flagObservationStatus)
  SUAbal[is.na(flagObservationStatus) , flagObservationStatus := 'E']
  SUAbal[is.na(flagMethod) , flagMethod := 'b']
  SUAbal <- SUAbal[ , c('ValueBal', 
                        'flagObservationStatusBal',
                        'flagMethodBal') := NULL]
  
  tradeQ <- SUAbal[measuredElementSuaFbs %in% c('5910', '5610')]
  tradeV <- SUAbal[measuredElementSuaFbs %in% c('5922', '5622')]
  tradeQ[measuredElementSuaFbs == '5910', flow := 'EXP']
  tradeQ[measuredElementSuaFbs == '5610', flow := 'IMP']
  tradeV[measuredElementSuaFbs == '5922', flow := 'EXP']
  tradeV[measuredElementSuaFbs == '5622', flow := 'IMP']

  if(nrow(tradeQ) > 0){
  tradeUV <- merge(tradeQ, tradeV, by = c('geographicAreaM49_fi',
                                          'timePointYears',
                                          'flow',
                                          'measuredItemFaostat_L2'), all = T,
                   suffixes = c('Q', 'V'))
  
  tradeUV <- tradeUV[, c('Value', 'flagObservationStatus', 'flagMethod') := 
                       list(ValueV/ValueQ, flagObservationStatusV, "c")]
  tradeUV[flow == 'IMP', measuredElementSuaFbs := '5630' ]
  tradeUV[flow == 'EXP', measuredElementSuaFbs := '5930' ]
  tradeUV[is.nan(Value), Value := 0]
  tradeUV[ValueQ == 0, Value := ValueV]

  SUAbal <- rbind(SUAbal[!measuredElementSuaFbs %in% c('5630','5930')], tradeUV[,.(geographicAreaM49_fi,
                                                                                   timePointYears,
                                                                                   measuredElementSuaFbs,
                                                                                   measuredItemFaostat_L2,
                                                                                   Value, 
                                                                                   flagObservationStatus, 
                                                                                   flagMethod)])
  }
  
  
  SUAbalAvail <- merge(SUAbal, elementSignTable[, .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)
  
  SUAbalAvail[, availability := sum(Value * sign, na.rm = TRUE), 
              by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  if(any(round(SUAbalAvail$availability) != 0)){
    msg2email7 <- paste0('Problem with products:', 
                         paste0(unique(SUAbalAvail[round(availability) != 0, ]$measuredItemFaostat_L2), collapse = ", "))
    message(paste("fi_SUA-FBS: Balancing was not successful for some products. ", msg2email7, sep = ''))
  } else {
    msg2email7 <-  ''
  }
  
  SUAbalAvail[, c("sign", "availability"):=NULL]
  t2 <- Sys.time()
  message(paste("SUAbal, okay", t2-t1))
  
  list( NegAv = secondaryneg,
        FPproblems = FPproblems,
        SUA = SUAbalAvail,
        msg = list(msg1 = msg2email4, msg2 = msg2email5, msg3 = msg2email7))
}

SUAnutrCalc <- function(SUAbalAvail, popSWS){
t1 <- Sys.time()
  ## Add NutrientFactors
  nutrientFactors <- ReadDatatable("fishery_nutrient")
  nutrientFactors$calories <- as.numeric(nutrientFactors$calories)
  nutrientFactors$proteins <- as.numeric(nutrientFactors$proteins)
  nutrientFactors$fats <- as.numeric(nutrientFactors$fats)
  nutrientFactors[is.na(proteins), proteins := 0]
  
  SUA_with_nutrient <- merge(SUAbalAvail, nutrientFactors, by.x = "measuredItemFaostat_L2", by.y = "ics", all.x = TRUE)
  
  SUA_with_nutrient[measuredElementSuaFbs=="5141", calories:=Value*calories/100]
  SUA_with_nutrient[measuredElementSuaFbs=="5141", proteins:=Value*proteins/100]
  SUA_with_nutrient[measuredElementSuaFbs=="5141", fats:=Value*fats/100]
  SUA_with_nutrient[measuredElementSuaFbs!="5141",`:=`(c("calories", "proteins", "fats"),list(0,0,0) )]
  
  # Get "calories", "proteins" and "fats" and make them in the dataset format
  SUAnutrients <-  melt.data.table(SUA_with_nutrient[measuredElementSuaFbs=="5141", ],
                                   id.vars = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'timePointYears'),
                                   measure.vars = c('calories', 'proteins','fats'),
                                   variable.name = 'measuredElementSuaFbs', value.name = 'Value')
  SUAnutrients$measuredElementSuaFbs <- as.character(SUAnutrients$measuredElementSuaFbs)
  SUAnutrients$measuredElementSuaFbs <- ifelse(SUAnutrients$measuredElementSuaFbs == 'calories', '261',
                                               ifelse(SUAnutrients$measuredElementSuaFbs == 'proteins', '271',
                                                      ifelse(SUAnutrients$measuredElementSuaFbs == 'fats', '281', SUAnutrients$measuredElementSuaFbs)))
  
  SUAnutrients[ , c('flagObservationStatus', 'flagMethod') := list('E','i')]
  SUAnutrients <- unique(SUAnutrients)
  food <- SUA_with_nutrient[measuredElementSuaFbs=="5141", .(measuredItemFaostat_L2, measuredElementSuaFbs,
                                                             geographicAreaM49_fi, timePointYears, Value,
                                                             flagObservationStatus, flagMethod)]
  
  SUAnutrients <- rbind(SUAnutrients, food)
 
  SUAnutrientCapita <- merge(SUAnutrients, popSWS, by=c("geographicAreaM49_fi","timePointYears"), 
                             suffixes = c("","_pop"))  
  SUAnutrientCapita[measuredElementSuaFbs !="5141" , Value := (Value*1000)/(Value_pop*365)]
  SUAnutrientCapita[measuredElementSuaFbs =="5141" , Value := Value/Value_pop]
  SUAnutrientCapita <- SUAnutrientCapita[ , .(geographicAreaM49_fi,
                                              timePointYears,
                                              measuredItemFaostat_L2,
                                              measuredElementSuaFbs,
                                              Value, flagObservationStatus,
                                              flagMethod)]
  
  SUAnutrientCapita[measuredElementSuaFbs=="261",measuredElementSuaFbs:="264"]
  SUAnutrientCapita[measuredElementSuaFbs=="281",measuredElementSuaFbs:="284"]
  SUAnutrientCapita[measuredElementSuaFbs=="271",measuredElementSuaFbs:="274"]
  SUAnutrientCapita[measuredElementSuaFbs=="5141",measuredElementSuaFbs:="645"]
  
  SUA_with_nutrient[ , c('calories', 'proteins','fats') := NULL] 
  
  
  # bind SUA with "calories", "proteins" and "fats" elements
  SUAallNutr <- rbind(SUAnutrients[measuredElementSuaFbs!="5141"], SUAnutrientCapita)
  SUANoPop <- rbind(SUA_with_nutrient, SUAallNutr)
  Pop2include <- merge(unique(SUANoPop[ , .(measuredItemFaostat_L2,
                                            geographicAreaM49_fi,
                                            timePointYears)]), popSWS, by = c('geographicAreaM49_fi', 
                                                                              'timePointYears'))
  
  SUA2save <- rbind(SUANoPop[measuredElementSuaFbs != '645'], Pop2include)
  
  t2 <- Sys.time()
  message(paste("SUAnut, okay", t2-t1))
  
  return(list(SUA2save = SUA2save, foodPercapita = SUANoPop[measuredElementSuaFbs == '645']))
}

FBScalc <- function(SUA2save, popSWS){
t1 <- Sys.time()
  # get all conversion factors (or extration rates) from commodity tree
  message('Get commodity tree')
  tree <- ReadDatatable('fi_commodity_tree')
  
  primary <- unique(tree[!parent %in% child]$parent)
  
  extrRates <- unique(SUA2save[ measuredElementSuaFbs == '5423', .(measuredItemFaostat_L2, geographicAreaM49_fi, timePointYears, Value)])
  compareEr <- merge(unique(tree[parent %in% primary , .(child, extraction_rate)]), extrRates, by.x = 'child', by.y = 'measuredItemFaostat_L2', all.y = TRUE)
  compareEr[is.na(Value), Value := extraction_rate]
  
  updatedEr <- compareEr[ , .(child, Value, geographicAreaM49_fi, timePointYears)]
  updatedtree <- merge(unique(tree[parent %in% primary , .(parent, child, weight)]), updatedEr, by= 'child', all.y = TRUE, allow.cartesian = TRUE)
  setkey(updatedtree)
  convFact <- unique(updatedtree[weight == TRUE & !is.na(Value) , .(geographicAreaM49_fi, timePointYears, parent, child, Value)])
  
  # For primary product add conversion factor equal 1
  primaryTree1 <- data.table(geographicAreaM49_fi = rep(unique(SUA2save$geographicAreaM49_fi), each = length(unique(SUA2save$timePointYears))), 
                             timePointYears = rep(unique(SUA2save$timePointYears), length(unique(SUA2save$geographicAreaM49_fi))) )
  
  primaryTree2 <- data.table(geographicAreaM49_fi = rep(unique(SUA2save$geographicAreaM49_fi), each = length(unique(primary))), parent = primary)
  primaryTree <- merge(primaryTree1, primaryTree2, by = 'geographicAreaM49_fi', allow.cartesian = TRUE)  
  primaryTree[ , `:=` (child = parent, Value = 1)]
  convFact <- rbind(convFact, primaryTree)
  setnames(convFact, new = 'extraction_rate', old = 'Value')

  # SUA with standardized element, no zero weight elements
  
  SUAstand_prep <- merge(SUA2save, convFact, by.x = c('geographicAreaM49_fi', 'timePointYears', 'measuredItemFaostat_L2'),
                         by.y = c('geographicAreaM49_fi', 'timePointYears', 'child'))
  SUAstand_prep <- SUAstand_prep[!is.na(Value)]
  setkey(SUAstand_prep)
  SUAstand_prep <- unique(SUAstand_prep)
  # Standardised value is Value/eR except from input values which are already in primary equivalent
  SUAstand_prep[! measuredElementSuaFbs %in% c('5302', '261', '264', '271', '274', '281', '284', '511'), Value_stand := Value/extraction_rate]
  SUAstand_prep[measuredElementSuaFbs %in% c('5302', '261', '264', '271', '274', '281', '284', '511'), Value_stand := Value]
  
  # take only all primary elements and for secondary exclude production and input
  SUAstand <-  rbind(SUAstand_prep[measuredItemFaostat_L2 %in% primary, ], 
                     SUAstand_prep[!measuredElementSuaFbs %in% c('5510', '5302') & !measuredItemFaostat_L2 %in% primary, ])
  
  
  
  ##-- FAOSTAT ----
  # Aggregate SUA by parent meals included for FAOSTAT
  
  SUAstandAggr0 <- SUAstand[ , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                 flagObservationStatus, flagMethod, parent, Value_stand)]
  
  
  
  # SUAstandAggr0$flagObservationStatus <- factor(SUAstandAggr0$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)
  
  SUAstandAggr <- SUAstandAggr0[measuredElementSuaFbs != '511' , list(Value = sum(Value_stand, na.rm = TRUE),
                                                                       flagObservationStatusAggr = 'I',
                                                                       flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                                                                                     "timePointYears", "parent")]
  
  setnames(SUAstandAggr, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
           c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))
  # Pop2SUAaggr <- SUAstandAggr0[measuredElementSuaFbs == '511']
  # setkey(Pop2SUAaggr)
  # Pop2SUAaggr <- unique(Pop2SUAaggr)
  # setnames(Pop2SUAaggr, c('Value_stand', 'parent'), c('Value', 'measuredItemFaostat_L2'))
  # SUAstandAggr <- rbind(SUAstandAggr1, Pop2SUAaggr)
  
  IcsGroups <- unique(SUAstandAggr[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)])
  
  faostat_pop2merge <- merge(IcsGroups, popSWS, by=c("geographicAreaM49_fi","timePointYears"), all = TRUE) 
  
  faostatfbsPOP <- rbind(SUAstandAggr, faostat_pop2merge)
  
  # mapp for FBS groups
  sua_fbs_mapping <- ReadDatatable('faostatl2_to_faostatl1')
  setnames(sua_fbs_mapping,  'measureditemfaostat_l2', 'measuredItemFaostat_L2' )
  sua_fbs_mapping[ , label := NULL]
  
  fbsFaostatL1faostat <- merge(faostatfbsPOP, sua_fbs_mapping, by = "measuredItemFaostat_L2")
  fbsFaostatL1faostat[ , measuredItemFaostat_L2 := NULL]
  setnames(fbsFaostatL1faostat, "fbs", "measuredItemFaostat_L2")
  
  #-- FAOSTAT FBS standardization ----
  
  message("Starting Faostat standardization")
  faostatGroups <- ReadDatatable('fi_faostat_standardization_element')
  setnames(faostatGroups, c('measuredelementsuafbs', 'measuredelementfaostat'), c('measuredElementSuaFbs', 'measuredElementFaostat'))
  
  FBSfaostat0 <- merge(fbsFaostatL1faostat[!measuredElementSuaFbs %in% c('261', '271', '281')],
                       faostatGroups, by = "measuredElementSuaFbs")
  FBSfaostat1 <- FBSfaostat0[measuredElementFaostat != '511' , list(Value = sum(Value, na.rm = TRUE),
                                                                    flagObservationStatus = 'I',
                                                                    flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                                                              "timePointYears", "measuredItemFaostat_L2",
                                                                                              "faostat", "measuredElementFaostat")]
  
  FBSfaostat <- rbind(FBSfaostat1, FBSfaostat0[measuredElementFaostat == '511', -'measuredElementSuaFbs', with = FALSE])
  setnames(FBSfaostat, c("faostat", "measuredElementFaostat"), c("element_description", "measuredElementSuaFbs"))

  faostatFBS2save <- FBSfaostat[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2,
                                     measuredElementSuaFbs, Value, flagObservationStatus, flagMethod)]
  faostatFBS2save <- faostatFBS2save[!is.na(Value)]
  
  #-- FIAS ----
  # Aggregate SUA by parent code take away meals
  mealCodes <- GetCodeList("FisheriesCommodities", 
                           "fi_sua_balanced_legacy",
                           "measuredItemFaostat_L2")[ grepl('meals', description)]$code
  
  # SUA with elements and groups to aggregates
  SUAstandAggrFias0 <- SUAstand[ !measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                                                            flagObservationStatus, flagMethod, parent, Value_stand)]
  
  # take only input element 5302
  mealsInput0 <- SUA2save[measuredElementSuaFbs == '5302' & measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                                                                                      flagObservationStatus, flagMethod, 
                                                                                                      measuredItemFaostat_L2, Value)]
  
  mealsInput <- merge(mealsInput0, unique(tree[parent %in% primary, .(parent, child)]), 
                      by.x = 'measuredItemFaostat_L2', by.y = 'child', all.x = TRUE)
  
  mealsInput[ , measuredItemFaostat_L2 := NULL]
  setnames(mealsInput, c("parent"), c("measuredItemFaostat_L2"))
  
  # SUAstandAggrFias0$flagObservationStatus <- factor(SUAstandAggrFias0$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)

  SUAstandAggrFias <- SUAstandAggrFias0[measuredElementSuaFbs != '511' , list(Value = sum(Value_stand, na.rm = TRUE),
                                                                               flagObservationStatusAggr = 'I',
                                                                               flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                                                                                             "timePointYears", "parent")]
  
  setnames(SUAstandAggrFias, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
           c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))
  
  # Pop2SUAaggFias <- SUAstandAggrFias0[measuredElementSuaFbs == '511']
  # setkey(Pop2SUAaggFias)
  # Pop2SUAaggFias <- unique(Pop2SUAaggFias)
  # setnames(Pop2SUAaggFias,c('Value_stand', 'parent'), c('Value', 'measuredItemFaostat_L2'))
  # SUAstandAggrFias <- rbind(SUAstandAggrFias1, Pop2SUAaggFias)
  
  fiasFbsTot <- rbind(SUAstandAggrFias, mealsInput)
  IcsGroups <- unique(fiasFbsTot[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)])
  
  # Introduce population data
  pop2merge <- merge(popSWS, IcsGroups, by = c("geographicAreaM49_fi", "timePointYears"), all = TRUE)
  fiasfbsPOP <- rbind(fiasFbsTot, pop2merge)
  
  # mapp for FBS groups
  sua_fbs_mapping <- ReadDatatable('faostatl2_to_faostatl1')
  setnames(sua_fbs_mapping,  'measureditemfaostat_l2', 'measuredItemFaostat_L2' )
  sua_fbs_mapping[ , label := NULL]
  
  fbsFaostatL1 <- merge(fiasfbsPOP, sua_fbs_mapping, by = "measuredItemFaostat_L2")
  fbsFaostatL1[ , measuredItemFaostat_L2 := NULL]
  setnames(fbsFaostatL1, "fbs", "measuredItemFaostat_L2")
  
  #-- FIAS FBS standardization ---- 
  message("fi_SUA-FBS: Starting Fias standardization")
  fiasGroups <- ReadDatatable('fi_fias_standardization_element')
  setnames(fiasGroups, c('measuredelementsuafbs', 'measuredelementfias'), c('measuredElementSuaFbs', 'measuredElementFias'))
  
  # FBS
  FBSfias0 <- merge(fbsFaostatL1[!measuredElementSuaFbs %in% c('261', '271', '281')], 
                    fiasGroups, by = "measuredElementSuaFbs")
  FBSfias1 <- FBSfias0[measuredElementFias != '511' , list(Value = sum(Value, na.rm = TRUE),
                                                           flagObservationStatus = 'I',
                                                           flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                                                     "timePointYears", "measuredItemFaostat_L2",
                                                                                     "fias", "measuredElementFias")]
  
  FBSfias <- rbind(FBSfias1, FBSfias0[measuredElementFias == '511', -'measuredElementSuaFbs', with = FALSE])
  fiasFBS2save <-  FBSfias[, .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, 
                               measuredElementFias, Value, flagObservationStatus, flagMethod)]
  
  setnames(fiasFBS2save, "measuredElementFias", "measuredElementSuaFbs")
  
  t2 <- Sys.time()
  message(paste("FBS, okay", t2-t1))
  
  #-- To update ----
  
  return(list(faostat = faostatFBS2save, fias =  fiasFBS2save))
}
