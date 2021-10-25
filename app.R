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
            startview = "decade"),
  dateInput("date2", "Date:",
            language = "pt",
            startview = "decade"),
  numericInput("Valoranterior", label = "Cotacao dia anterior:", NA, min = -10000, max = 10000),
  
  
  numericInput("margem", label = "Margem desejada:", NA, min = -10000, max = 10000),
  
  numericInput("Valordia", label = "Valor Atual:", NA, min = -10000, max = 10000),
  
  actionButton("do", " $$$ ")
    ),
  
  mainPanel(
    verbatimTextOutput("summary1")
  )
  )
)
server <- function(input, output) {
  
  reactive({
    Inicio<-as.date(input$date1)
    Fim<-input$date2
    Valoranterior<-input$Valoranterior
    margem<-input$margem
    VALORDIA<-input$Valordia
    })
  
 
  
  # output$data1 <- renderText({ input$date1 })
  # output$data2 <- renderText({ input$date2 })
  # output$Valoranterior <- renderText({ input$Valoranterior })
  # output$margem <- renderText({ input$margem })
  # output$VALORDIA <- renderText({ input$Valordia })
  
  
  
  observeEvent(input$do, {
    
  bov <- getSymbols("^BVSP", src = "yahoo", 
                    from = Inicio, to = Fim, auto.assign = FALSE)
    
  
  
  bov1 <- as.data.frame(bov) %>% mutate(Data = index(bov)) %>% na.omit()
  
  bov1<- bov1  %>% mutate(Open = as.numeric(BVSP.Open), High = as.numeric(BVSP.High), 
                          Low = as.numeric(BVSP.Low), Close = as.numeric(BVSP.Close), Adj.Close = as.numeric(BVSP.Adjusted),
                          Volume = as.numeric(BVSP.Volume)) %>% 
    select(Data, Open, High, Low, Close, Adj.Close) %>% 
    mutate(GP = Open - Close) %>% mutate( DGP = ifelse(GP< 0, "PERDA","GANHO"))
  bov3 <-bov1 %>% mutate(NextDAYP=0, NextGPP=0,NextDAYG=0, NextGPG=0, NEXTGP=0)
  
  
  for(i in 1:dim(bov3)[1]) {
    if(bov3$GP[i]< 0){
      bov3$NextDAYP[i]=bov3$DGP[i+1]
      bov3$NextGPP[i]=bov3$GP[i+ 1]
      bov3$NEXTGP[i]=bov3$GP[i+ 1]
    } 
    
  }
  for(i in 1:dim(bov3)[1]) {
    if(bov3$GP[i]> 0){
      bov3$NextDAYG[i]=bov3$DGP[i+1]
      bov3$NextGPG[i]=bov3$GP[ i+ 1] 
      bov3$NEXTGP[i]=bov3$GP[i+ 1]
    }
    
  }
  
  bov3gp3<- bov3 %>% mutate(NextDAYP = ifelse(NextDAYP == 0 & NextGPP == 0, NA, NextDAYP), 
                            NextGPP = ifelse(NextDAYP == 0 & NextGPP == 0, NA, NextGPP)) %>% 
    mutate(NextDAYG = ifelse(NextDAYG == 0 & NextGPG == 0, NA, NextDAYG), 
           NextGPG = ifelse(NextDAYG == 0 & NextGPG == 0, NA, NextGPG)) %>% 
    mutate(DGP = as.factor(DGP), NextDAYP = as.factor(NextDAYP), NextDAYG = as.factor(NextDAYG)) %>% 
    mutate( NEXTDGP = ifelse(NEXTGP< 0, "PERDA","GANHO"))
  
  
  
  output$summary1 <- renderPrint(summary(bov3gp3))
  })
}








shinyApp(ui, server)

