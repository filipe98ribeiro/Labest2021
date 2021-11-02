library(shiny)
library(shinyTime)
library(tidyverse)
library(plotly)
library(forecast)
library(quantmod)


ui <- fluidPage(
  
  titlePanel("Analises"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      dateInput("date1", "Date:",
                language = "pt",
                startview = "year",
                daysofweekdisabled = c(0,6),
                value = Sys.Date()-10),
      dateInput("date2", "Date:",
                language = "pt",
                value = Sys.Date(),
                daysofweekdisabled = c(0,6)),
      
      selectInput("nomeacao", "Acao:",#"Nomeração1"? so fiz a principio com 2 pra ve se roda ,se der pode colocar 3 ações 
                  c("PETR3.SA",
                    "VALE3.SA",
                    "SANB11",
                    "TRPL4",
                    "HYPE3",
                    "SBSP3",
                    "EMBR3",
                    "PGMN3",
                    "ITUB4",
                    "JBSS3",
                    "RDOR3",
                    "^BVSP")),
      # selectInput("nomeacao2", "Acao:",
      #c("PETR3.SA",
       # "VALE3.SA",
        #"SANB11",
       # "TRPL4",
       # "HYPE3",
       # "SBSP3",
       # "EMBR3",
       # "PGMN3",
       # "ITUB4",
       # "JBSS3",
       # "RDOR3",
       # "^BVSP")),
      
      
      actionButton("do", " $$$ ")
    ),
    
    mainPanel(
      
      textOutput("result1"),
      textOutput("result2"),
      textOutput("result3"),
      plotOutput("plot1"),
      plotOutput("plot2")
      
    )
  )
)
server <- function(input, output) {
  
  reactive({
    input$date1
    input$date2
    input$nomeacao
  })
  
  
  
  
  
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
    
    
    bov <- getSymbols(input$nomeacao, src = "yahoo",
                      from = input$date1, to = input$date2, auto.assign = FALSE)
    #bov2 <- getSymbols(input$nomeacao2, src = "yahoo",
    #                   from = input$date1, to = input$date2, auto.assign = FALSE)
    # bov_ret <- Return.calculate(bov)
    # bov2_ret <- Return.calculate(bov2)
    #bov_ret <- bov_ret[(-1),]
    #bov2_ret <- bov2_ret[(-1),]
    
    #eq_weights <- c(0.5, 0.5)
    #rets <- cbind(bov_ret, bov2_ret)
    ######## Create a portfolio using buy and hold
    # pf_bh <- Return.portfolio(R = rets, weights = eq_weights, verbose = TRUE)
    ######## Create an optimized portfolio of returns
    # opt <- portfolio.optim(rets)
    ## Create pf_weights
    #(pf_weights <- opt$pw) era bom colocar os pesos nos resultados ?? 
    ## Assign asset names
    ## names(pf_weights) <- colnames(rets)
    # ou extractWeights(opt)
    # Print expected portfolio return media dos retornos de cada 
    # opt$pm
    #pf_bh$pm
    
    # Print expected portfolio volatility votilidade de cada 
    # opt$ps
    #pf_bh$ps
    
    # Calculate the proportion increase in standard deviation
    # (opt$ps - pf_bh$ps) / (pf_bh$ps)
    
    ####Plota os retornos dos portfolios ?
    #### plot.zoo(pf_bh$returns)
    #### plot.zoo(opt$returns)
    
    output$plot1<-renderPlot({
      ggplot(bov, aes(x = index(bov), y = bov[,6])) + geom_line(color = "darkblue") +
        ggtitle(paste("Série de preços"), input$nomeacao) +
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
    
    })
    
    
    
    


}






shinyApp(ui, server)

