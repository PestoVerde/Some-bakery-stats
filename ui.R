library(shiny)
library(plotly)

shinyUI(navbarPage("Snapshot profile",
                   tabPanel("Upload",
                            titlePanel("Uploading Files"),
                            sidebarLayout(
                                sidebarPanel(
                                    fileInput('file1', 'Choose CSV File',
                                              accept=c('text/csv', 
                                                       'text/comma-separated-values,text/plain', 
                                                       '.csv')),
                                    tags$hr(),
                                    checkboxInput('header', 'Header', TRUE),
                                    radioButtons('sep', 'Separator',
                                                 c(Comma=',',
                                                   Semicolon=';',
                                                   Tab='\t'),
                                                 ',')
                                ),
                                mainPanel(
                                    verbatimTextOutput('info_test')
                                )
                            )
                   ),#tabPanel(Upload)
                   
                   tabPanel("Data Set",
                            dataTableOutput("contents")
                   ),#tabPanel(Data Set)
                   
                   tabPanel("Info",
                            verbatimTextOutput("info1"),
                            verbatimTextOutput("info2"),
                            verbatimTextOutput("info3"),
                            helpText('Columns with many zeroes (%% are shown):'),
                            verbatimTextOutput('info5')
                   ),#tabPanel(Info)
                   
                   tabPanel("Corelations",
                            helpText('Corelated pairs (coefficients are shown):'),
                            verbatimTextOutput('corr')
                   ),#tabPanel(Corelations)
                   
                   tabPanel("Plots",
                            titlePanel("Histograms"),
                            sidebarPanel(
                                uiOutput("ColumnSelector"),
                                hr(),
                                helpText("Parameters of my bakery")
                            ), # sidebarPanel
                            
                            mainPanel(
                                tabPanel("Histogram", plotOutput("bakeryHistogramm"))
                                #,verbatimTextOutput("info_test")
                            )# mainPanel
                   ),#tabPanel("Plots")
                   
                   tabPanel("Documentation",
                            p("First of all please upload a file."),
                            p("TO BE CONTINUED"),
                            p("For more help please go to", a("GitHub",
                                                              href="https://github.com/"))
                   )#tabPanel("Documentation")
)#bootstrapPage
)# ShinyUI