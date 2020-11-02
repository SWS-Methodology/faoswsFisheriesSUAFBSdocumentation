# Third tab, FBS groups by ICS product tab

SUAicsTab_reac <- reactive({
  
  req(input$btn_group_fbs_tab3, input$btn_ics_prod_tab3,
      input$btn_year, input$btn_country, input$btn_start_year)
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  # sel_group_fbs <- as.character(groups_input[label %in% input$btn_group_fbs_tab3]$code)
  sel_ics_prod <- as.character(groups_input[label %in% input$btn_ics_prod_tab3]$code )
  # sel_element_sua <- as.character(element_input[ label %in% input$btn_element_fbs]$code )

  SUAbalIcs <- reloadDataToken(data = live_data$SUAb, 
                           keycountry = sel_country, 
                           minyear = input$btn_start_year, 
                           maxyear = input$btn_year,
                           keydomain = domainComm, 
                           keydataset = datasetSUABlive,
                           keytoken = tokenSuaB)
  
  if(!is.null(SUAbalIcs)){
    ValueElements <- c('5922', '5930', '5622', '5630')
    SUAbalIcsval <- copy(SUAbalIcs)
    SUAbalIcsval <- SUAbalIcsval[!measuredElementSuaFbs %in% ValueElements]
    SUAbalIcs <- SUAbalIcs[!measuredElementSuaFbs %in% ValueElements]
    live_data$SUAb <- SUAbalIcs
    live_data$SUAbVal <- SUAbalIcsval
  } else {
    SUAbalIcs <- live_data$SUAb
    SUAbalIcsval <- live_data$SUAbVal
  }

   SUAbalIcs <- rbind(SUAbalIcs, SUAbalIcsval)
   SUAbalIcs <- SUAbalIcs[measuredItemFaostat_L2 %in% sel_ics_prod]

  tab2show <- merge(SUAbalIcs, l2l1[ , .(code_l1, code_l2)], 
                    by.x = 'measuredItemFaostat_L2', by.y = 'code_l2')
  
  return(tab2show)
  
})

output$sua_ics_tab3 <- DT::renderDataTable( server = FALSE, {
  
  sua_ics_tab_out <- copy(SUAicsTab_reac())
  
  setnames(sua_ics_tab_out, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                              'measuredElementSuaFbs', 'timePointYears', 
                              'flagObservationStatus', 'flagMethod', 'code_l1'),
           c('Country', 'ICSprod', 'Element', 'Year', 'F1', 'F2', 'FBSgroup'))
  setcolorder(sua_ics_tab_out, c('Country', 'FBSgroup', 'ICSprod', 'Element', 'Year', 
                                 'Value', 'F1', 'F2'))
  DT::datatable(sua_ics_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
})

output$gg_plot_tab3 <- renderPlot({
  req(input$btn_group_fbs_tab3, input$btn_ics_prod_tab3, input$btn_element_fbs,
      input$btn_year, input$btn_country, input$btn_start_year)
  # sel_element_sua <- as.character(element_input[ label %in% input$btn_element_sua]$code )
  sel_elements_fbs <- as.character(element_input[label == input$btn_element_fbs]$code)
  suaIcs_data <- copy(SUAicsTab_reac())
  suaIcs_data <- suaIcs_data[measuredElementSuaFbs == sel_elements_fbs ]
  
  validate(
    need(nrow(suaIcs_data) > 0,
         'The FBS element selected in the upper right part of the screen is not calculated in the SUA. 
       Please choose a different element.'))
    # Make grand total
    ggplot(data = suaIcs_data, aes(x = timePointYears, y = Value)) + 
      geom_line(aes(group = measuredItemFaostat_L2, color = measuredItemFaostat_L2), size = 0.7) +
      # geom_text(data = suaIcs_data[timePointYears == as.numeric(input$btn_year)],
      # aes(label = measuredItemFaostat_L2, color = measuredItemFaostat_L2, check_overlap = TRUE),
      #  hjust = 0.7, vjust = 1, show_guide  = F) +
      labs(x = 'Year', color = '') +
      theme(text = element_text(size= 15))
  
})