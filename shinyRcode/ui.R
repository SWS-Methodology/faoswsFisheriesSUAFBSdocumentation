ui = fluidPage(
  title = "SUA-FBS Tools",
  br(),
  column(12,
         column(3, 
                selectInput(inputId = "btn_country", 
                            label = 'Country', 
                            choices = country_input$label #, 
                            # selected = 'Chile - 152'
                )       
         ),
         column(1, 
                uiOutput('btn_year')
         ),
         column(1, 
                uiOutput('btn_start_year')       
         ),
         column(3,
                uiOutput('btn_element_fbs')
         )
         
  ),
  tabsetPanel(id = "tabs",
              tabPanel("Token", fluid = TRUE,
                       column(12,
                              column(12,
                                     br(),
                                     h4('Insert new token.'),
                                     h5('The tokens in the table are the last used ones. 
                If the session used has changed please update the tokens through the buttons below. 
                Otherwise, the session used will be the one linked to the token in the table displayed.'),
                                     br()),
                              column(8,
                                     DT::dataTableOutput('token_tab'),
                                     br(),
                                     uiOutput('btn_token1'),
                                     br(),
                                     uiOutput('btn_token2'),
                                     br(),
                                     uiOutput('btn_token3'),
                                     br(),
                                     uiOutput('btn_token4'),
                                     br(),
                                     actionBttn("btn_upd_token", label = "Update token",
                                                color = "primary",
                                                style = "jelly"))
                       )
              ),
              tabPanel("Overview", fluid = TRUE,
                       
                       column(12, 
                              plotOutput('gg_plot_tab1', width = '80%'),
                              DT::dataTableOutput('fbs_fias_tab1')
                       )
              ),
              tabPanel("SUA compare", fluid = TRUE,
                       column(12,
                              column(3, 
                                     uiOutput('btn_group_fbs'),
                                     uiOutput('btn_element_group'),
                                     uiOutput('btn_element_sua')
                              ),
                              column(9,
                                     DT::dataTableOutput('sua_comp_tab2')
                              )
                       )
              ),
              
              tabPanel("SUA imbalances", fluid = TRUE,
                       column(12,
                              column(8,
                                     h4('SUA imbalance before Production increase.'),
                                     br(),
                                     plotOutput('gg_plot_tab2bis', width = '80%')
                              ),
                              column(4,
                                     DT::dataTableOutput('sua_imb_tab2')
                              ),
                              column(12,
                                     h4('SUA Production values before (Unbal) and after (Bal) increase.'),
                                     h5('Please note values after increase are those displayed and used for FBS calculations.'),
                                     DT::dataTableOutput('sua_prod_diff_tab2')
                              )
                       )
              ),
              tabPanel("FBS by ICS", fluid = TRUE,
                       column(12,
                              column(3,
                                     uiOutput('btn_group_fbs_tab3'),
                                     uiOutput('btn_ics_prod_tab3')
                              ),
                              column(9,
                                     br(),
                                     br(),
                                     plotOutput('gg_plot_tab3'),
                                     DT::dataTableOutput('sua_ics_tab3')
                              )
                       )
              ),
              tabPanel("ICS by element", fluid = TRUE,
                       column(12,
                              column(3,
                                     uiOutput('btn_group_fbs_tab4'),
                                     uiOutput('btn_ics_prod_tab4'),
                                     uiOutput('btn_element_group_tab4'),
                                     uiOutput('btn_element_sua_tab4')
                              ),
                              column(9,
                                     plotOutput('gg_plot_tab4'),
                                     DT::dataTableOutput('sua_elem_tab4')
                              )
                       ) 
                       
              ),
              tabPanel("Global Prod", fluid = TRUE,
                       column(12,
                              column(3,
                                     br(),
                                     actionBttn("saveGP", label = "Save",
                                                color = "primary",
                                                style = "gradient"),
                                     br(),
                                     br(),
                                     uiOutput('btn_group_fbs_tab5'),
                                     rHandsontableOutput('gp_map_tab5')
                              ),
                              column(9,
                                     DT::dataTableOutput('gp_tab5') 
                              )
                       )
              ),
              tabPanel("Commodities", fluid = TRUE,
                       column(12,
                              column(3,
                                     br(),
                                     actionBttn("saveCDB", label = "Save",
                                                color = "primary",
                                                style = "gradient"),
                                     br(),
                                     br(),
                                     uiOutput('btn_group_fbs_tab6'),
                                     uiOutput('btn_ics_prod_tab6'),
                                     uiOutput('btn_element_cdb_tab6'),
                                     rHandsontableOutput('cdb_map_tab6')
                              ),
                              column(9,
                                     h4('Note the ICS codes in the table refer to YBKlang file amd applies to ISSCFC codes,
                              i.e. the ICS codes are the default ones before the application of the link table.'),
                                     DT::dataTableOutput('cdb_tab6') 
                              )
                       )
                       
              ),
              tabPanel("Link table", fluid = TRUE,
                       column(12,
                              br(),
                              br(),
                              column(2,
                                     actionBttn("updLT", label = "Update table",
                                                color = "danger",
                                                style = "gradient")),
                              column(10,
                                     rHandsontableOutput('linktable')
                              )
                       )
                       
                       
              ),
              tabPanel("Balancing elements", fluid = TRUE,
                       column(12,
                              br(),
                              br(),
                              column(2,
                                     actionBttn("updBal", label = "Update table",
                                                color = "danger",
                                                style = "gradient")),
                              column(10,
                                     rHandsontableOutput('balancingelements')
                              )
                       )
                       
                       
              ),
              tabPanel("Extraction rates", fluid = TRUE,
                       column(12,
                              br(),
                              br(),
                              column(2,
                                     radioButtons(inputId = "radioErUpdt", 
                                                  label = h4("Update type"),
                                                  choices = list("Single year" = 1, 
                                                                 "Selected time series" = 2 #, 
                                                                 #"Whole time series" = 3
                                                  ), 
                                                  selected = 1),
                                     actionBttn("updER", 
                                                label = "Update",
                                                color = "danger",
                                                style = "gradient")),
                              column(10,
                                     h4("Note this tab is to perform block or single extraction rate updates.
                              Update the series of each product (measuredItemFaostat_L2) only with one value when choosing 'selected series' updates."),
                                     h5('In general, avoid to put different values for different years. Use the data validation tab for this detailed operations.'),
                                     rHandsontableOutput('extrR')
                              )
                       )
                       
                       
              ),
              tabPanel("Data validation", fluid = TRUE,
                       column(12,
                              column(4,
                                     h5('Download SUA table'),
                                     downloadButton('downloadData', 'Save as .csv'),
                                     br(),
                                     br(),
                                     radioButtons(inputId = "csv_online", 
                                                  label = "Update to consider",
                                                  inline = TRUE,
                                                  choices = list("Online modifications" = 1, 
                                                                 "Uploaded .csv file" = 2),
                                                  selected = 1),
                                     h5("Please select if Input or the Extraction rate figures prevail."), 
                                     h5("If no official input is inserted choose 'Extr rate'"),
                                     radioButtons(inputId = "radioErVSinput", 
                                                  label = "Use",
                                                  inline = TRUE,
                                                  choices = list("Extr rates" = 1, 
                                                                 "Input" = 2, 
                                                                 "Null" = 3),
                                                  selected = 3),
                                     radioButtons(inputId = "reprocess", 
                                                  label = "Reprocessing type",
                                                  inline = FALSE,
                                                  choices = list("No calculations wanted" = 'No',
                                                                 "Complete" = 'Complete', 
                                                                 "Only SUA balanced" = 'SUAb',
                                                                 "Since SUA balanced" = 'SUAbTot',
                                                                 "Only Nutrients & FBS" = 'NutFbs'),
                                                  selected = 'No'),
                                     uiOutput('btn_group_fbs_tab7'),
                                     uiOutput('btn_ics_prod_tab7'),
                                     uiOutput('btn_element_group_tab7'),
                                     uiOutput('btn_sua_elem_tab7')
                              ),
                              column(8,
                                     br(),
                                     column(5,
                                            fileInput("updatedSUA", "Upload CSV file",
                                                      multiple = FALSE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv"))),
                                     br(),
                                     column(3,
                                            actionBttn("save", label = "Save & Recalc",
                                                       color = "primary",
                                                       style = "gradient")),
                                     rHandsontableOutput('sua_tab7'),
                                     tableOutput("contents"),
                                     br(),
                                     textOutput('textAv'),
                                     br(),
                                     rHandsontableOutput('availability'),
                                     br(),
                                     h4("Primary availability not covering rank 1 children:"),
                                     br(),
                                     DT::dataTableOutput('FPtab1'),
                                     br(),
                                     h4("Insufficient secondary availability:"),
                                     br(),
                                     DT::dataTableOutput('FPinsuff'),
                                     br(),
                                     h4("Level by level availability problems:"),
                                     br(),
                                     DT::dataTableOutput('FPsecPar'), 
                                     br(),
                                     h4("Total uncovered quantities:"),
                                     br(),
                                     DT::dataTableOutput('FPtabUncov')
                              )
                       ) 
              ),
              tabPanel("Data update", fluid = TRUE,
                       column(12,
                              plotOutput('gg_plot_tab8', width = '80%'),
                              DT::dataTableOutput('fbs_fias_tab8')
                       )
              ),
              tabPanel("Data saving", fluid = TRUE, 
                       column(12,
                              column(4,
                                     br(),
                                     actionBttn("btn_upd_token_val", label = "Update token",
                                                color = "primary",
                                                style = "jelly"),
                                     br(),
                                     radioButtons(inputId = "time2save", 
                                                  label = "Years to save",
                                                  inline = FALSE,
                                                  choices = list("Last year" = 1, 
                                                                 "Selected series" = 2, 
                                                                 "Null" = 3),
                                                  selected = 3
                                     ),
                                     br(),
                                     actionBttn("update", label = "Update SWS",
                                                color = "success",
                                                style = "gradient")
                              ),
                              column(8,
                                     br(),
                                     DT::dataTableOutput('token_val_tab'),
                                     br(),
                                     uiOutput('btn_token1val'),
                                     br(),
                                     uiOutput('btn_token2val'),
                                     br(),
                                     uiOutput('btn_token3val'),
                                     br(),
                                     uiOutput('btn_token4val')
                              )
                       )
              )
  )
)

