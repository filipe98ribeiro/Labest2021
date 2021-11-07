library(shiny)
library(shinyTime)
library(tidyverse)
library(plotly)
library(forecast)
library(quantmod)
library(PerformanceAnalytics)
library(tidyquant)
library(tseries)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  
  dashboardHeader(title = "Analises"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Variaveis",tabName = "var1",icon=icon("database")),
      menuItem("Resultados",tabName = "second",icon=icon("table")),
      menuItem("Retorno Esperado do Potofoilio",tabName = "var3",icon=icon("table"))
    )
  ),
  dashboardBody(
    
    tabItems(
    
      tabItem(tabName = "var1", 
            fluidRow(column(3,
      
        dateInput("date1", "Date:",
                language = "pt",
                startview = "year",
                daysofweekdisabled = c(0,6),
                value = Sys.Date()-10),
        dateInput("date2", "Date:",
                language = "pt",
                value = Sys.Date(),
                daysofweekdisabled = c(0,6)),
      
        selectInput("nomeacao", "Acao:",
                  c("PETR3.SA",
                    "VALE3.SA",
                    "SANB11.SA",
                    "TRPL4.SA",
                    "HYPE3.SA",
                    "SBSP3.SA",
                    "EMBR3.SA",
                    "PGMN3.SA",
                    "ITUB4.SA",
                    "JBSS3.SA",
                    "RDOR3.SA",
                    "^BVSP")),
        selectInput("nomeacao2", "Acao:",
                  c("PETR3.SA",
                    "VALE3.SA",
                    "SANB11.SA",
                    "TRPL4.SA",
                    "HYPE3.SA",
                    "SBSP3.SA",
                    "EMBR3.SA",
                    "PGMN3.SA",
                    "ITUB4.SA",
                    "JBSS3.SA",
                    "RDOR3.SA",
                    "^BVSP")),
      
      
        actionButton("do", " $$$ ")
    ),
    
      column(4,offset = 2,
      textOutput("result1"),
      textOutput("result2"),
      textOutput("result3"),
      textOutput("result4")
     )
  )
),

tabItem(tabName = "second",
        fluidRow(
          plotOutput("plot1"),
          plotOutput("plot11"),
          plotOutput("plot2"),
          plotOutput("plot22"),
          plotOutput("plot3"),
          plotOutput("plot4"),
        )
),

tabItem(tabName = "var3",
        fluidRow(
          verbatimTextOutput("t1")
          # dataTableOutput("t3"),
          # dataTableOutput("t2"),
          # dataTableOutput("t4")
        )
)
)
)
)



server <- function(input, output) {
  
  # reactive({
  #   input$date1
  #   input$date2
  #   input$nomeacao
  # })
  
  
  
  
  
  observeEvent(input$do,{
    
    output$result1 <- renderText({
      paste("Data inicial escolhida:", input$date1)
    })
    output$result2 <- renderText({
      paste("Data final escolhida:", input$date2)
    }) 
    output$result3 <- renderText({
      paste("Açao escolhida:", input$nomeacao)
    })    
    output$result4 <- renderText({
      paste("Açao escolhida:", input$nomeacao2)
    })  
    
    bov <- getSymbols(input$nomeacao, src = "yahoo",
                      from = input$date1, to = input$date2, auto.assign = FALSE)
    bov2 <- getSymbols(input$nomeacao2, src = "yahoo",
                       from = input$date1, to = input$date2, auto.assign = FALSE)
    bov_ret <- Return.calculate(bov)
    bov2_ret <- Return.calculate(bov2)
    bov_ret <- bov_ret[(-1),] %>% Ad()
    bov2_ret <- bov2_ret[(-1),] %>% Ad()
    
    eq_weights <- c(0.5, 0.5)
    rets <- cbind(bov_ret, bov2_ret)
    rets <- rets %>% replace_na(0)
    ######## Create a portfolio using buy and hold
    pf_bh <- Return.portfolio(R = rets, weights = eq_weights, verbose = TRUE)
    ######## Create an optimized portfolio of returns
    opt <- portfolio.optim(rets)
    ## Create pf_weights
    pf_weights <- opt$pw #era bom colocar os pesos nos resultados ??
    ## Assign asset names
    names(pf_weights) <- colnames(rets)
    # ou extractWeights(opt)
    # Print expected portfolio return media dos retornos de cada 
    output$t1 <-renderPrint(opt) 
    # output$t2 <-renderDataTable(mean(pf_bh$returns))
    
    # Print expected sd de cada portfolio
    # output$t3 <- renderDataTable(sd(opt$px))
    # output$t4 <- renderDataTable(sd(pf_bh$returns))
    
    # Calculate the proportion increase in standard deviation acho legal colocar 
    # (sd(opt$px) - sd(pf_bh$returns)) / (sd(pf_bh$returns))
    
    ####Plota os retornos dos portfolios ?
    output$plot3 <- renderPlot({ plot.zoo(pf_bh$returns)})
    output$plot4 <- renderPlot({ plot.zoo(opt$px)})
    
    output$plot1<-renderPlot({
      ggplot(bov, aes(x = index(bov), y = bov[,6])) + geom_line(color = "darkblue") +
        ggtitle(paste("Série de preços"), input$nomeacao) +
        xlab("Data") + ylab("Preço ($)") + theme(plot.title = element_text(hjust = 0.5)) + 
        scale_x_date(date_labels = "%b %y", date_breaks = "6 months")+
        theme_bw()
      
    })
    output$plot11<-renderPlot({
      ggplot(bov, aes(x = index(bov), y = bov2[,6])) + geom_line(color = "darkblue") +
        ggtitle(paste("Série de preços"), input$nomeacao2) +
        xlab("Data") + ylab("Preço ($)") + theme(plot.title = element_text(hjust = 0.5)) + 
        scale_x_date(date_labels = "%b %y", date_breaks = "6 months")+
        theme_bw()
      
    }) 
    
    bov_ret <- diff(log(bov[,6]))
    bov_ret <- bov_ret[-1,]
    
    output$plot2<-renderPlot({
      ggplot(bov_ret, aes(x = index(bov_ret), y = bov_ret)) + geom_line(color = "darkblue") +
        ggtitle(paste("Série de retornos"),input$nomeacao) + xlab("Data") + ylab("Retorno") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_date(date_labels = "%b %y", date_breaks = "3 months")+
        theme_bw()
      
    }) 
    
    bov22_ret <- diff(log(bov2[,6]))
    bov22_ret <- bov22_ret[-1,]
    
    output$plot22<-renderPlot({
      ggplot(bov22_ret, aes(x = index(bov22_ret), y = bov22_ret)) + geom_line(color = "darkblue") +
        ggtitle(paste("Série de retornos"),input$nomeacao2) + xlab("Data") + ylab("Retorno") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_date(date_labels = "%b %y", date_breaks = "3 months")+
        theme_bw()
      
    }) 
    
    
  })
  
  
  
  
  
  
}






shinyApp(ui, server)
