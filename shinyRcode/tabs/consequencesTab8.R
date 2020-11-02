# Eighth tab, showing changes consequences

consequenceTab_reac <- reactive({
  
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_element_fbs)
  if(workaround$V != 1) return(NULL)
  if(nrow(updated_data$FBSfias)== 0 ) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_elements_fbs <- as.character(element_input[label == input$btn_element_fbs]$code)
  sel_fbs_groups <- as.character(c(seq(10, 90, by = 10), 99))
  
  fiasFBSupd <- updated_data$FBSfias

  validate(
    need(nrow(fiasFBSupd) != 0, 'No update to show')
         )

  fiasFBSupd <- fiasFBSupd[measuredElementSuaFbs == sel_elements_fbs]

  FBSfrozen <- frozen_data$FBS
  FBSfrozen <- FBSfrozen[measuredElementSuaFbs == sel_elements_fbs]
  # Now only showing value present both in frozen and live, CHANGE?
  frozenVSlive8 <- merge(FBSfrozen, fiasFBSupd, 
                        by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                               'measuredElementSuaFbs', 'timePointYears'),
                        suffixes = c('Frozen', 'Updated'), all = TRUE)

  frozenVSlive8[is.na(ValueFrozen), flagObservationStatusFrozen := 'O']
  frozenVSlive8[is.na(ValueUpdated), flagObservationStatusUpdated := 'O']
  
  frozenVSlive8[is.na(ValueFrozen), flagMethodFrozen := '-']
  frozenVSlive8[is.na(ValueUpdated), flagMethodUpdated := '-']
  
  frozenVSlive8[is.na(ValueFrozen), ValueFrozen := 0]
  frozenVSlive8[is.na(ValueUpdated), ValueUpdated := 0]
  
  frozen2plot <- frozenVSlive8[ , .(geographicAreaM49_fi,
                                   measuredItemFaostat_L2,
                                   measuredElementSuaFbs,
                                   timePointYears,
                                   ValueFrozen)]
  frozen2plot[ , type := 'Frozen']
  
  live2plot <- frozenVSlive8[ , .(geographicAreaM49_fi,
                                 measuredItemFaostat_L2,
                                 measuredElementSuaFbs,
                                 timePointYears,
                                 ValueUpdated)]
  live2plot[ , type := 'Updated']
  
  setnames(frozen2plot, 'ValueFrozen', 'Value')
  setnames(live2plot, 'ValueUpdated', 'Value')
  
  data4plot8 <- rbind(frozen2plot, live2plot)
  
  return(list(tab = frozenVSlive8, plot = data4plot8))
 
  
})

output$fbs_fias_tab8 <- DT::renderDataTable( server = FALSE, {
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_element_fbs)
  if(is.null(consequenceTab_reac()$tab)) return(NULL)
  fbs_fias_tab_upd <- copy(consequenceTab_reac()$tab)
  
 
  grandtotal <- copy(fbs_fias_tab_upd)
  
  grandtotal$flagObservationStatusFrozen <-  factor(grandtotal$flagObservationStatusFrozen, 
                                                    levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                    ordered = TRUE)
  
  grandtotal$flagObservationStatusUpdated <-  factor(grandtotal$flagObservationStatusUpdated, 
                                                  levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                  ordered = TRUE)
  
  grandtotal <- grandtotal[ , c('ValueFrozen', 'ValueUpdated', 'measuredItemFaostat_L2', 
                                'flagObservationStatusFrozen', 'flagMethodFrozen', 
                                'flagObservationStatusUpdated', 'flagMethodUpdated') := list(sum(ValueFrozen, na.rm = TRUE), 
                                                                                       sum(ValueUpdated, na.rm = TRUE),
                                                                                       'Total', 
                                                                                       max(flagObservationStatusFrozen), 's',
                                                                                       max(flagObservationStatusUpdated), 's'),
                            by = c('geographicAreaM49_fi',
                                   'measuredElementSuaFbs', 
                                   'timePointYears')]
  
  setkey(grandtotal)
  grandtotal <- grandtotal[!duplicated(grandtotal)]
  
  fbs_fias_tot_upd <- rbind(fbs_fias_tab_upd, grandtotal)
  
  setnames(fbs_fias_tot_upd, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                               'measuredElementSuaFbs', 'timePointYears', 
                               'flagObservationStatusFrozen', 'flagMethodFrozen',
                               'flagObservationStatusUpdated', 'flagMethodUpdated'),
           c('Country', 'FBSgroup', 'Element', 'Year', 'FlagFrozen1', 'FlagFrozen2',
             'FlagUpdated1', 'FlagUpdated2'))
  fbs_fias_tot_upd[ , Diff := round(ValueFrozen - ValueUpdated, 3)]
  DT::datatable(fbs_fias_tot_upd, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf'))) %>%
    formatStyle(columns = c('Diff'), target = 'row',
                color = styleInterval(c(-0.001, 0.001), c('red', ' ', 'red')))
})

output$gg_plot_tab8 <- renderPlot({
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_element_fbs)
  if(is.null(consequenceTab_reac()$plot)) return(NULL)
  fbs_fias_upd <- copy(consequenceTab_reac()$plot)
  
  grandtotal <- copy(fbs_fias_upd)
  grandtotal <- grandtotal[ , c('Value', 'measuredItemFaostat_L2') := list(sum(Value, na.rm = TRUE), 'Total'),
                            by = c('geographicAreaM49_fi',
                                   'measuredElementSuaFbs', 
                                   'timePointYears', 'type')]
  setkey(grandtotal)
  grandtotal <- grandtotal[!duplicated(grandtotal)]
  
  fbs_fias_tot_upd <- rbind(fbs_fias_upd, grandtotal)
  
  ggplot(data = fbs_fias_tot_upd, aes(x = timePointYears, y = Value)) + 
    geom_line(aes(group = type, color = type), size = 0.7) +
    facet_wrap( ~ measuredItemFaostat_L2, scales="free") +
    labs(x = 'Year', color = '') +
    theme(text = element_text(size= 15))
  
})
