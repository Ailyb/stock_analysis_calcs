library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(

  headerPanel("Stock Explorer"),

  sidebarPanel(
    
    helpText("Select a stock to examine. 
      Information will be collected from yahoo finance."),
    
    textInput("symb", "Symbol", "GOOG"),
    
    dateRangeInput("dates", 
      "Compare to historic returns from",
      start = "2018-01-01", end = "2021-06-05"),
    
    actionButton("get", "Get Stock"),
    
    br(),
    br(),
    
    uiOutput("newBox")
    
  ),

  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Main", 
               fluidRow(plotlyOutput("chart"))
               ,br()
               ,fluidRow(DTOutput("chart_dt")))
      ,tabPanel("Signal Analysis",
                fluidRow(DTOutput("returns_dt"))
                ,br()
                ,fluidRow(DTOutput("signals_dt")))
      ,tabPanel("SMA", 
                fluidRow(plotOutput("sma"))
                ,br()
                ,fluidRow(DTOutput("sma_dt")))
      ,tabPanel("PSAR", 
                fluidRow(plotOutput("sar"))
                ,br()
                ,fluidRow(DTOutput("sar_dt")))
      ,tabPanel("CCI", 
                fluidRow(plotOutput("cci"))
                ,br()
                ,fluidRow(DTOutput("cci_dt")))
      ,tabPanel("ROC", 
                fluidRow(plotOutput("roc"))
                ,br()
                ,fluidRow(DTOutput("roc_dt")))
      ,tabPanel("SMI", 
                fluidRow(plotOutput("smi"))
                ,br()
                ,fluidRow(DTOutput("smi_dt")))
      ,tabPanel("WPR", 
                fluidRow(plotOutput("wpr"))
                ,br()
                ,fluidRow(DTOutput("wpr_dt")))
      ,tabPanel("Model", div(h3(textOutput("ks"))), 
               div(h3(textOutput("ksp"))), 
                   plotOutput("hist")) 
      ,tabPanel("VaR", h3(textOutput("text3"))),
      id = "tab"
    )
  )
))