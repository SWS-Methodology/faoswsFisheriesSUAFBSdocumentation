# link table recall
linktable_reac <- reactive({
  req(input$btn_year, input$btn_country, input$btn_start_year)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  where <- paste("geographic_area_m49 = '", sel_country, "'", sep = "")
  linktable <- ReadDatatable('link_table', where = where, readOnly = FALSE)
  linktable$percentage <- as.numeric(linktable$percentage)
  
  if(nrow(linktable) == 0){
    linktable <- rbind(linktable, data.table(geographic_area_m49 =sel_country),
                       fill = T)
  }
  
  return(linktable)
})

output$linktable <-  renderRHandsontable({
  
  table <- linktable_reac()
  table$percentage <- as.numeric(table$percentage)

  rhandsontable(table, 
                rowHeaders = NULL, width = 'auto', height = 'auto') %>%
    hot_col(c("__id", "__ts"), colWidths = c(rep(0.1,2),rep(150,7)), readOnly = TRUE)
})

observeEvent(input$updLT, {

  updLinkTable <- rhandsontable::hot_to_r(input$linktable)
  updLinkTable <- as.data.table(updLinkTable)
  
  if(any(!updLinkTable$flow %in% c('EXP', 'IMP', 'PRD', 'ALL', 'TRD'))){
    
    showModal(modalDialog(
      title = "Error!" ,
      sprintf("Flow must belong to one of these options:
              'EXP', 'IMP', 'PRD', 'ALL', 'TRD'")
    ))
    
  } else {
    changeset <- Changeset('link_table')
    AddModifications(changeset, updLinkTable)
    Finalise(changeset)
    
    showModal(modalDialog(
      title = "Link table updated." ,
      sprintf("The new version of the link table is now available on the SWS.")
    ))  
    
  }
  
  
})