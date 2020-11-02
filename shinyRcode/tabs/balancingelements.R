# link table recall

baltable_reac <- reactive({
  req(input$btn_year, input$btn_country, input$btn_start_year)
  sel_country <- country_input[country_input$label == input$btn_country, code]
  where <- paste("geographic_area_m49_fi = '", sel_country, "'", sep = "")
  baltable <- ReadDatatable('balancing_elements', where = where, readOnly = FALSE)
  
  if(nrow(baltable) == 0){
    baltable <- rbind(baltable, data.table(geographic_area_m49_fi =sel_country),
                       fill = T)
  }
  
  return(baltable)
})

output$balancingelements <-  renderRHandsontable({
  
  table <- baltable_reac()
  
  rhandsontable(table, 
                rowHeaders = NULL, width = 'auto', height = 'auto') %>%
    hot_col(c("__id", "__ts"), colWidths = c(rep(0.1,2),rep(150,6)), readOnly = TRUE)
})

observeEvent(input$updBal, {

  updbalTable <- rhandsontable::hot_to_r(input$balancingelements)
  changeset <- Changeset('balancing_elements')
  AddModifications(changeset, updbalTable)
  Finalise(changeset)
  
  showModal(modalDialog(
    title = "Balancing element table updated." ,
    sprintf("The new version of is now available on the SWS.")
  ))
  
})