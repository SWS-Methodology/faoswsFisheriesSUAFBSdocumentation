# Second tab, comparing SUA versions tab

SUAcomparingTab_reac <- reactive({
  
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_group_fbs, input$btn_element_sua)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  
  if(any(input$btn_group_fbs == 'All')){
    sel_group_fbs <- as.character(groups_input$code)
  } else {
  sel_group_fbs <- as.character(groups_input[label %in% input$btn_group_fbs]$code)
  }
  
  sel_element_sua <- as.character(sua_element_input[ label %in% input$btn_element_sua]$code )
  
  l2l1 <- ReadDatatable('ics_l1_2_ics_l2')
  
  ics2select <- l2l1[code_l1 %in% sel_group_fbs ]$code_l2
  
  # if(CheckDebug()){
  # 
  #   library(faoswsModules)
  #   SETTINGS = ReadSettings("sws.yml")
  # 
  #   ## If you're not on the system, your settings will overwrite any others
  #   R_SWS_SHARE_PATH = SETTINGS[["share"]]
  # 
  #   ## Define where your certificates are stored
  #   SetClientFiles(SETTINGS[["certdir"]])
  # 
  #   ## Get session information from SWS. Token must be obtained from web interface
  #   GetTestEnvironment(baseUrl = SETTINGS[["server"]],
  #                      token = tokenSuaB)
  # 
  # }
  # 
  # # R_SWS_SHARE_PATH = "Z:"
  # # SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
  # # GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  # #                      token = tokenSuaB)
  #   
  # 
  # KeySUAbal <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
  #   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
  #   measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
  #                                     keys = sel_element_sua), # Or, if all elements, GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredElementSuaFbs" )[,code])
  #   measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
  #                                      keys = ics2select),
  #   timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  # 
  # withProgress(message = 'SUA balanced data loading in progress',
  #              value = 0, {
  #                
  #                Sys.sleep(0.25)
  #                incProgress(0.25)
  #                SUAbal <- GetData(KeySUAbal)
  #                Sys.sleep(0.75)
  #                incProgress(0.95)
  #              })
  
  validate(
    need(nrow(frozen_data$SUA) > 0, "No frozen SUA data for this country. Please select another country.")
  )
  
  validate(
    need(nrow(live_data$SUAb) > 0, "No SUA data for this country. Please select another country.")
  )
  
  SUAbal <- live_data$SUAb[measuredElementSuaFbs %in% sel_element_sua & measuredItemFaostat_L2 %in% ics2select]
  
  SUAbal <- rbind(SUAbal, live_data$SUAbVal[measuredElementSuaFbs %in% sel_element_sua & measuredItemFaostat_L2 %in% ics2select])
  
  SUAfrozen <- frozen_data$SUA[measuredElementSuaFbs %in% sel_element_sua & measuredItemFaostat_L2 %in% ics2select]

  # Now only showing value present both in frozen and live, CHANGE?
  SUAfrozenVSlive <- merge(SUAfrozen, SUAbal, 
                        by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                               'measuredElementSuaFbs', 'timePointYears'),
                        suffixes = c('Frozen', 'Live'),
                        all = TRUE)
  SUAfrozenVSlive[is.na(ValueFrozen), ValueFrozen := 0]
  SUAfrozenVSlive[is.na(ValueLive), ValueLive := 0]
  
  SUAfrozenVSlive[measuredElementSuaFbs == '5423', ValueLive := ValueLive]
  frozen2show <- SUAfrozenVSlive[measuredElementSuaFbs %in% sel_element_sua]
  
  frozen2show <- merge(frozen2show, l2l1[ , .(code_l1, code_l2)], 
                       by.x = 'measuredItemFaostat_L2', by.y = 'code_l2')
  
  return(frozen2show)
  
})


output$sua_comp_tab2 <- DT::renderDataTable( server = FALSE, {
  
  sua_comp_tab_out <- copy(SUAcomparingTab_reac())
  
  setnames(sua_comp_tab_out, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                               'measuredElementSuaFbs', 'timePointYears', 
                               'flagObservationStatusFrozen', 'flagMethodFrozen',
                               'flagObservationStatusLive', 'flagMethodLive', 'code_l1'),
           c('Country', 'ICSprod', 'Element', 'Year', 'Fr1', 'Fr2',
             'Li1', 'Li2', 'FBSgroup'))
  sua_comp_tab_out[ , Diff := round(ValueFrozen - ValueLive, 3)]
  setcolorder(sua_comp_tab_out, c('Country', 'FBSgroup', 'ICSprod', 'Element', 'Year', 
                                  'ValueFrozen', 'Fr1', 'Fr2',
                                  'ValueLive', 'Li1', 'Li2', 'Diff'))
  DT::datatable(sua_comp_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf'))) %>%
    formatStyle(columns = c('Diff'), target = 'row',
                color = styleInterval(c(-0.001, 0.001), c('red', ' ', 'red')))
                #backgroundColor = styleInterval(0, c(' ', 'red')))
    
})

