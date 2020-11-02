# Fourth tab, ICS product by element tab

SUAelemTab_reac <- reactive({
  
  req(input$btn_group_fbs_tab4, input$btn_ics_prod_tab4, input$btn_element_sua_tab4,
      input$btn_year, input$btn_country, input$btn_start_year)

  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_group_fbs <- as.character(groups_input[label == input$btn_group_fbs_tab4]$code)
  sel_ics_prod <- as.character(groups_input[label == input$btn_ics_prod_tab4]$code)
  sel_element_sua <- as.character(sua_element_input[ label %in% input$btn_element_sua_tab4]$code)

  if(length(sel_element_sua) > 0){  
    
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
  

  KeySUAbalElem <- DatasetKey( domain = "FisheriesCommodities", 
                               dataset = "fi_sua_balanced", 
                               dimensions = list(geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", 
                                                                                  keys = sel_country),
                                                 measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                                                                   keys = GetCodeList("FisheriesCommodities", 
                                                                                                      "fi_fbs_fias_control",
                                                                                                      "measuredElementSuaFbs" )[,code]),
                                                 measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                                                                    keys = GetCodeList("FisheriesCommodities",
                                                                                                       "fi_sua_balanced_control",
                                                                                                       "measuredItemFaostat_L2" )[,code]),
                                                 timePointYears = Dimension(name = "timePointYears", keys = sel_years )) )
  
  withProgress(message = 'SUA balanced data loading in progress',
               value = 0, {
                 
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 SUAbalElem <- GetData(KeySUAbalElem)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  ValueElements <- c('5922', '5930', '5622', '5630')
  SUAbalElemVal <- copy(SUAbalElem)
  SUAbalElem <- SUAbalElem[!measuredElementSuaFbs %in% ValueElements]
  
  live_data$SUAb <- SUAbalElem[!measuredElementSuaFbs %in% ValueElements]
  live_data$SUAbVal <- SUAbalElemVal[measuredElementSuaFbs %in% ValueElements]
  
  SUAbalElem <- SUAbalElem[measuredElementSuaFbs %in% sel_element_sua & measuredItemFaostat_L2 %in% sel_ics_prod]
  
  SUAbalElem <- rbind(SUAbalElem, SUAbalElemVal[measuredElementSuaFbs %in% sel_element_sua & measuredItemFaostat_L2 %in% sel_ics_prod])
    
  } else {
 
    SUAbalElem <- live_data$SUAb[measuredElementSuaFbs %in% sel_element_sua & measuredItemFaostat_L2 %in% sel_ics_prod]
    SUAbalElemVal <- live_data$SUAbVal[measuredElementSuaFbs %in% sel_element_sua & measuredItemFaostat_L2 %in% sel_ics_prod]
    SUAbalElem <- rbind(SUAbalElem, SUAbalElemVal)
  }
  
  tab2show <- merge(SUAbalElem, l2l1[ , .(code_l1, code_l2)], 
                    by.x = 'measuredItemFaostat_L2', by.y = 'code_l2')
  
  return(tab2show)
}
  
})

output$sua_elem_tab4 <- DT::renderDataTable( server = FALSE, {
  
  sua_elem_tab_out <- copy(SUAelemTab_reac())
  
  setnames(sua_elem_tab_out, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                              'measuredElementSuaFbs', 'timePointYears', 
                              'flagObservationStatus', 'flagMethod', 'code_l1'),
           c('Country', 'ICSprod', 'Element', 'Year', 'F', 'Fm', 'FBSgroup'))
  setcolorder(sua_elem_tab_out, c('Country', 'FBSgroup', 'ICSprod', 'Element', 'Year', 
                                 'Value', 'F', 'Fm'))
  DT::datatable(sua_elem_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
})

output$gg_plot_tab4 <- renderPlot({
  req(input$btn_group_fbs_tab4, input$btn_ics_prod_tab4,
      input$btn_year, input$btn_country, input$btn_start_year)
  
  suaElem_data <- copy(SUAelemTab_reac())
  
  # suaElem_data[ ,x_pos := as.numeric(input$btn_year)]
  # valuesY <- suaElem_data[timePointYears == as.numeric(input$btn_year), .(measuredElementSuaFbs, Value)]
  # setnames(valuesY, 'Value', 'y_pos')
  # suaElem_data <- merge(suaElem_data, valuesY, by = 'measuredElementSuaFbs')
  
  # Make grand total
  ggplot(data = suaElem_data, aes(x = timePointYears, y = Value)) + 
    geom_line(aes(group = measuredElementSuaFbs, color = measuredElementSuaFbs), size = 0.7) +
    # geom_text(data = suaElem_data[timePointYears == as.numeric(input$btn_year)],
    #           aes(label = measuredElementSuaFbs, color = measuredElementSuaFbs, check_overlap = TRUE),
    #           hjust = 0.7, vjust = 1, show_guide  = F) +
    labs(x = 'Year', color = '') +
    theme(text = element_text(size= 15))
  
})