
# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # acquiring data
  dataInput <- reactive({
    if (input$get == 0)
      return(AAPL)

    return(isolate({
      getSymbols(input$symb,src="yahoo", from = input$dates[1], auto.assign = FALSE)
    }))
  })
  
  stock_analysis<-reactive({
    if(input$get ==0)
      return(NULL)
    return(
      Full_analysis(dataInput())
    )
    
  })
  
  datesInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      paste0(input$dates[1], "::",  input$dates[2])
    }))
  })
  
  returns <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    dailyReturn(dataInput())
  })
  
  xs <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    span <- range(returns())
    seq(span[1], span[2], by = diff(span) / 100)
  })
  
  # tab based controls
  output$newBox <- renderUI({
    switch(input$tab,
           "Main" = chartControls,
           "SMA" = chartControls,
           "PSAR" = chartControls,
           "CCI" = chartControls,
           "ROC" = chartControls,
           "SMI" = chartControls,
           "WPR" = chartControls,
           "Model" = modelControls,
           "VaR" = helpText("VaR")
    )
  })
  
  # Charts tab
  chartControls <- div(
    wellPanel(
      selectInput("chart_type",
        label = "Chart type",
        choices = c("Candlestick" = "candlesticks", 
          "Matchstick" = "matchsticks",
          "Bar" = "bars",
          "Line" = "line"),
        selected = "Line"
      ),
      checkboxInput(inputId = "log_y", label = "log y axis", 
                    value = FALSE)
    ),
    
    wellPanel(
      p(strong("Technical Analysis")),
      checkboxInput("ta_vol", label = "Volume", value = FALSE),
      checkboxInput("ta_sma", label = "Simple Moving Average", 
                    value = FALSE),
      checkboxInput("ta_ema", label = "Exponential Moving Average", 
                    value = FALSE),
      checkboxInput("ta_wma", label = "Weighted Moving Average", 
                    value = FALSE),
      checkboxInput("ta_bb", label = "Bolinger Bands", 
                    value = FALSE),
      checkboxInput("ta_momentum", label = "Momentum", 
                    value = FALSE),
      
      br(),
      
      actionButton("chart_act", "Add Technical Analysis")
    )
  )
  
  TAInput <- reactive({
    if (input$chart_act == 0)
      return("NULL")
    
    tas <- isolate({c(input$ta_vol, input$ta_sma, input$ta_ema, 
      input$ta_wma,input$ta_bb, input$ta_momentum)})
    funcs <- c(addVo(), addSMA(), addEMA(), addWMA(), 
      addBBands(), addMomentum())
    
    if (any(tas)) funcs[tas]
    else "NULL"
  })
  
  
  output$chart <-renderPlotly({
    df <- data.frame(Date=index(dataInput()),coredata(dataInput()))
    col_nms<-c("Date", "AAPL.Open",     "AAPL.High",     "AAPL.Low",      "AAPL.Close",   "AAPL.Volume",   "AAPL.Adjusted")
    names(df)<-col_nms
    
    fig <- df %>% plot_ly(x = ~Date, type="ohlc",
                          open = ~AAPL.Open, close = ~AAPL.Close,
                          high = ~AAPL.High, low = ~AAPL.Low) 
    
    fig <- fig %>% layout(title = "Basic OHLC Chart"
                          ,paper_bgcolor = "black"
                          ,plot_bgcolor = "black"
                          ,xaxis = list(rangeslider = list(visible = F)
                            ,rangeselector = list(
                              buttons = list(
                                list(
                                  count = 1,
                                  label = "1 mo",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 3,
                                  label = "3 mo",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 6,
                                  label = "6 mo",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 1,
                                  label = "1 yr",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 1,
                                  label = "YTD",
                                  step = "year",
                                  stepmode = "todate"),
                                list(step = "all"))),
                            
                            rangeslider = list(type = "date", visible=F)),
                          
                          yaxis = list(title = "Price")
                          ,paper_bgcolor = "black"
                          ,plot_bgcolor = "black")
    
    
    fig
    
    
  })
  
  
  output$chart_dt <- renderDT({
    
    
    dt <- data.frame(Date=index(dataInput()),coredata(dataInput()))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) %>%
      formatRound(c(2:length(names(dt))),2)
    p
    
  })
  
  output$signals_dt <- renderDT({
    
    dt <- data.frame(coredata(stock_analysis()[["signals"]]))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) 
    p
    
    
  })
  
  
  output$sma <- renderPlot({
    chartSeries(dataInput(),
                name = input$symb,
                type = 'line',
                subset = datesInput(),
                #log.scale = input$log_y,
                theme = "black",
                TA = c(addSMA(n = 20, col = 'blue'),addSMA(n = 50, col = 'orange')))
                legend('left', col = c('green','blue','orange'),
                       legend = c(input$symb,'SMA20','SMA50'), lty = 1, bty = 'n',
                       text.col = 'white', cex = 0.8)
  })
  
  output$sma_dt<-renderDT({
    
    dt <- data.frame(Date=index(stock_analysis()[["sma_strat"]]),coredata(stock_analysis()[["sma_strat"]]))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) %>% 
      formatCurrency(c(2)) %>% 
      formatRound(c(3:length(names(dt))),2)
    p
    
  })
  
  output$sar <- renderPlot({
    barChart(dataInput(), theme = 'black')
    addSAR(accel = c(0.02, 0.2), col = 'lightblue')
  })
  
  
  output$sar_dt<-renderDT({
    
    dt <- data.frame(Date=index(stock_analysis()[["sar_strat"]]),coredata(stock_analysis()[["sar_strat"]]))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) %>% 
      formatCurrency(c(2)) %>% 
      formatRound(c(3:length(names(dt))),2)
    p
    
  })
  
  
  output$cci <- renderPlot({
    chartSeries(dataInput(),
                name = input$symb,
                type = input$chart_type,
                #log.scale = input$log_y,
                theme = "black",
                TA = addCCI(n = 20, c = 0.015))
  })
  
  
  output$cci_dt<-renderDT({
    
    dt <- data.frame(Date=index(stock_analysis()[["cci_strat"]]),coredata(stock_analysis()[["cci_strat"]]))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) %>% 
      formatCurrency(c(2)) %>% 
      formatRound(c(3:length(names(dt))),2)
    p
    
  })
  
  output$roc <- renderPlot({
    barChart(dataInput(), theme = 'black')
    addROC(n = 25)
    legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
           text.col = 'white', cex = 0.8)
  })
  
  
  output$roc_dt<-renderDT({
    
    dt <- data.frame(Date=index(stock_analysis()[["roc_strat"]]),coredata(stock_analysis()[["roc_strat"]]))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) %>% 
      formatCurrency(c(2)) %>% 
      formatRound(c(3:length(names(dt))),2)
    p
    
  })
  
  output$smi <- renderPlot({
    barChart(dataInput(), theme = 'black')
    addSMI(n = 13, fast = 2, slow = 2, signal = 9)
  })
  
  
  
  output$smi_dt<-renderDT({
    zz<<-stock_analysis()
    dt <- data.frame(Date=index(stock_analysis()[["smi_strat"]]),coredata(stock_analysis()[["smi_strat"]]))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) %>% 
      formatCurrency(c(2)) %>% 
      formatRound(c(3:length(names(dt))),2)
    p
    
  })
  output$wpr <- renderPlot({
    barChart(dataInput(), theme = 'black')
    addWPR(n = 14)
  })
  
  
  output$wpr_dt<-renderDT({
    
    dt <- data.frame(Date=index(stock_analysis()[["wpr_strat"]]),coredata(stock_analysis()[["wpr_strat"]]))
    p<-datatable(dt
                 ,filter = 'top'
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 )) %>% 
      formatCurrency(c(2)) %>% 
      formatRound(c(3:length(names(dt))),2)
    p
    
  })
  
  
  output$returns_dt<-renderDT({
    
    dt <- data.frame(coredata(stock_analysis()[["Returns_Calc"]]))
    p<-datatable(dt
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 ))  %>%
      formatPercentage(c(2:length(names(dt))),2) 
    p
    
  })
  
  output$signals_dt<-renderDT({
    
    dt <- data.frame(coredata(stock_analysis()[["signals"]]))
    p<-datatable(dt
                 ,options = list(
                   order = list(list(1, 'desc'))
                   ,columnDefs = list(list(className = 'dt-center', targets = c(2:length(names(dt)))))
                 ), rownames = FALSE)  
    p
    
  })
  
  
  
  # Model tab
  modelControls <- div(
      br(),
      
      sliderInput("n", "Number of bins in histogram",
        min = 1, max = 250, value = 30
      ),
      
      br(),
    
    wellPanel(
      selectInput("family", "Model returns as",
                choices = c("normal", "double exponential", "t"),
                selected = "normal"
      ),
      
      sliderInput("mu", "Mean",
        min = -1, max = 1, value = 0, step = 0.01
      ), 
      
      sliderInput("sigma", "Standard Deviation",
        min = 0, max = 0.1, value = 0.05, step = 0.001
      ),
      conditionalPanel(condition = "input.family == 't'",
        sliderInput("df", "Degrees of freedom",
          min = 2, max = 1000, value = 10
        )
      )
      
    )
  )
    
  ys <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    switch(input$family,
           "double exponential" = dlaplace(xs(), 
                                location = input$mu, 
                                scale = input$sigma
                                ),
           "normal" = dnorm(xs(), 
                             mean = input$mu, 
                             sd = input$sigma
                             ),
           "t" = dt((xs() - input$mu) / input$sigma,
                    df = input$df) * sqrt(2 * length(returns()))
    )
  })
    
  ks <- reactive({
    switch(input$family,
           "double exponential" = ks.test(returns(), "plaplace", 
                                          input$mu, input$sigma),
           "normal" = ks.test(returns(), "pnorm", 
                              input$mu, input$sigma),
           "t" = ks.test((returns() - input$mu) / input$sigma, "pt", 
                         input$df)
    )
  })
  
  output$hist <- renderPlot({
    hist(returns(), xlab = "returns", freq = FALSE,
      main = paste(input$symb, "Daily Returns:", 
        input$dates[1], "-", input$dates[2], sep = " "),
      breaks = input$n)
   lines(xs(), ys(), col = "red")
  })
  
  
  output$ks <- renderText({
    paste0("Kolmogorv-Smirnoff statistic: ", ks()$statistic)
  })
  output$ksp <- renderText({
    paste0("P-value for model: ", ks()$p.value)
  })
  
  # VaR tab
  output$text3 <- renderText({paste0(input$symb, " 3: ", input$tab)})
                           
})
