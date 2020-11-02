# Create datatable
SUAimbalanceTab_reac <- reactive({

  req(input$btn_country, input$btn_year, input$btn_start_year)

  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  
  imbalance_tab <- ReadDatatable('imbalance_tab')
  imbalance_tab_country <- imbalance_tab[geographicaream49_fi == sel_country & timepointyears %in% sel_years]
  
  
  SUAbal <- reloadDataToken(data = live_data$SUAb, 
                           keycountry = sel_country, 
                           minyear = input$btn_start_year, 
                           maxyear = input$btn_year,
                           keydomain = domainComm, 
                           keydataset = datasetSUABlive,
                           keytoken = tokenSuaB)
  
  if(!is.null(SUAbal)){
    ValueElements <- c('5922', '5930', '5622', '5630')
    SUAbalval <- copy(SUAbal)
    SUAbalval <- SUAbalval[measuredElementSuaFbs %in% ValueElements]
    SUAbal <- SUAbal[!measuredElementSuaFbs %in% ValueElements]
    live_data$SUAbVal <- SUAbalval
    live_data$SUAb <- SUAbal
  } else {
    SUAbalval <- live_data$SUAbVal
    SUAbal <- live_data$SUAb
  }
  
  SUAbal <- live_data$SUAb[measuredElementSuaFbs == '5510']
  SUArou <- SUAbal[measuredElementSuaFbs == '5166']
  
  SUAunbalTot <- reloadDataToken(data = live_data$SUAu, 
                              keycountry = sel_country, 
                              minyear = input$btn_start_year, 
                              maxyear = input$btn_year,
                              keydomain = domainComm, 
                              keydataset = datasetSUAUlive,
                              keytoken = tokenSuaU)

  if(!is.null(SUAunbalTot)){
    ValueElements <- c('5922', '5930', '5622', '5630')
    SUAunbalTot <- SUAunbalTot[!measuredElementSuaFbs %in% ValueElements]
    live_data$SUAu <- SUAunbalTot[!measuredElementSuaFbs %in% ValueElements]
  } else {
    SUAunbalTot <- live_data$SUAu
  }
  
  SUAunbal <- SUAunbalTot[measuredElementSuaFbs == '5510']
  prodCompare <- merge(SUAunbal, SUAbal, by = c("geographicAreaM49_fi","measuredElementSuaFbs", 
                                                "measuredItemFaostat_L2", "timePointYears"),
                       suffixes = c('Unbal', 'Bal'), all = TRUE)
  
  prodCompare[is.na(ValueUnbal) , ValueUnbal := 0]
  
  return(list(imb = imbalance_tab_country, prod = prodCompare, rou = SUArou))
  
})

output$sua_imb_tab2 <- DT::renderDataTable( server = FALSE, {
  
  req(input$btn_country, input$btn_year, input$btn_start_year)
  sua_imb_tab_out <- copy(SUAimbalanceTab_reac()$imb)
  
  validate(
    need(nrow(sua_imb_tab_out) > 0, 
         'No imbalance to show.')
  )
  
  setnames(sua_imb_tab_out, c('geographicaream49_fi', 'measureditemfaostat_l2',
                              'timepointyears'),
           c('Country', 'ICSprod', 'Year'))
  setcolorder(sua_imb_tab_out, c('Country', 'ICSprod', 'Year', 'availability'))
  DT::datatable(sua_imb_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 10,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))

})

output$gg_plot_tab2bis <- renderPlot({
  
  req(input$btn_country, input$btn_year, input$btn_start_year)
  sua_imb_tab_out <- copy(SUAimbalanceTab_reac()$imb)
  
  validate(
    need(nrow(sua_imb_tab_out) > 0, 
         'No imbalance to show.')
  )
  
  ggplot(data = sua_imb_tab_out, aes(x = timepointyears, y = availability, fill = measureditemfaostat_l2)) + 
    # geom_bar(stat = 'identity', position = 'dodge') +
    geom_histogram(stat = 'identity', binwidth = 0.5) +
    facet_wrap( ~ measureditemfaostat_l2, scales="free") +
    labs(x = 'Year') +
    theme(text = element_text(size=15))
  
}) 

output$sua_prod_diff_tab2 <- DT::renderDataTable( server = FALSE, {
  
  req(input$btn_country, input$btn_year, input$btn_start_year)

  prodCompare <- copy(SUAimbalanceTab_reac()$prod)

  setnames(prodCompare, c('geographicAreaM49_fi', 'measuredItemFaostat_L2',
                              'timePointYears', 'measuredElementSuaFbs'),
           c('Country', 'ICSprod', 'Year', 'Element'))
  
  prodCompare[ , Diff := (ValueUnbal - ValueBal)]
  
  DT::datatable(prodCompare, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
})