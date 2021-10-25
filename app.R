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
      
      selectInput("nomeacao", "Acao:",
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
      
      
      
      actionButton("do", " $$$ ")
    ),
    
    mainPanel(
      
      textOutput("result1"),
      textOutput("result2"),
      textOutput("result3"),
      plotOutput("plot1")
      
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
    
    output$plot1<-renderPlot({
      ggplot(bov, aes(x = index(bov), y = bov[,6])) + geom_line(color = "darkblue") +
        ggtitle(paste("Série de preços"), input$nomeacao) +
        xlab("Data") + ylab("Preço ($)") + theme(plot.title = element_text(hjust = 0.5)) + 
        scale_x_date(date_labels = "%b %y", date_breaks = "6 months")
    }) 
     ### Acrescenta o grafico dos retornos ??? (Não conseguir fazer aparecer no shinny)
    #bov_ret <- diff(log(bov[,6]))
    #bov_ret <- bov_ret[-1,]
    
    #output$plot2<-renderPlot({
      #ggplot(bov_ret, aes(x = index(bov_ret), y = bov_ret)) + geom_line(color = "darkblue") +
       # ggtitle(paste("Série de retornos"),input$nomeacao), + xlab("Data") + ylab("Retorno") +
        #theme(plot.title = element_text(hjust = 0.5)) +
        #scale_x_date(date_labels = "%b %y", date_breaks = "3 months")
    
    #})
    
    
    
  })
}








shinyApp(ui, server)

