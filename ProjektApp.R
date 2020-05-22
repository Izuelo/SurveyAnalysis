library(shiny)
library(ggplot2)


BadaniaF <-
  read.csv(
    "BadanieF1.csv",
    header = TRUE,
    sep = ";",
    encoding = "UTF-8",
    check.names = F
  )
Badania <-
  read.csv(
    "BadanieF1.csv",
    header = TRUE,
    sep = ";",
    encoding = "UTF-8",
    check.names = T
  )
pytania <- colnames(Badania)
nazwy <- colnames(BadaniaF)
names(pytania) <- nazwy



ui <- fluidPage(navbarPage(
  "Ankieta",
  tabPanel(
    "Lęk i Fascynacja",
    
    titlePanel("Wyniki"),
    sidebarLayout(
      sidebarPanel(
        selectInput("cechy", "Wybierz zagadnienie:", pytania[91:92]),
        br(),
        sliderInput(
          inputId = "bins",
          label = "Liczba przedziałów:",
          min = 1,
          max = 25,
          value = 6
        ),
        br(),
        radioButtons(
          inputId = "radio",
          "Kategoria:",
          choices = c(pytania[2], pytania[4], pytania[5], pytania[6])
        )
        
      ),
      mainPanel(plotOutput("his"))
    )
  ),
  tabPanel(
    "Sprawczość i Wspólnotowość",
    
    titlePanel("Wyniki"),
    sidebarLayout(
      sidebarPanel(
        selectInput("cechy2", "Wybierz zagadnienie:", pytania[93:94]),
        br(),
        sliderInput(
          inputId = "bins2",
          label = "Liczba przedziałów:",
          min = 1,
          max = 25,
          value = 12
        ),
        
        
        br(),
        radioButtons(
          inputId = "radio2",
          "Kategoria:",
          choices = c(pytania[2], pytania[4], pytania[5], pytania[6])
        )
      ),
      mainPanel(plotOutput("his2"))
    )
  ),
  tabPanel(
    "Autopromocja i Autodeprecjacja",
    
    titlePanel("Wyniki"),
    sidebarLayout(
      sidebarPanel(
        selectInput("cechy3", "Wybierz zagadnienie:", pytania[95:96]),
        br(),
        sliderInput(
          inputId = "bins3",
          label = "Liczba przedziałów:",
          min = 1,
          max = 25,
          value = 8
        ),
        
        
        br(),
        radioButtons(
          inputId = "radio3",
          "Kategoria:",
          choices = c(pytania[2], pytania[4], pytania[5], pytania[6])
        )
      ),
      mainPanel(plotOutput("his3"))
    )
  ),
  tabPanel(
    "Pytania",
    
    titlePanel("Pytania z ankiety"),
    sidebarLayout(sidebarPanel(
      selectInput("pytanie", "Wybierz pytanie:", pytania[8:90])
    ),
    mainPanel(plotOutput("barchart")))
  )
  
))
server <- function(input, output) {
  output$his <- renderPlot({
    dat <- as.character(input$cechy)
    rad <- as.character(input$radio)
    
    bins <- seq(1, 4, length.out = input$bins + 1)
    
    ggplot(Badania, aes_string(x = dat, color = rad)) +
      geom_histogram(
        fill = "white",
        alpha = 0.5,
        position = "identity",
        breaks = bins
      ) +
      ylab("Liczba osób")
    
  })
  
  output$his2 <- renderPlot({
    dat <- as.character(input$cechy2)
    rad <- as.character(input$radio2)
    
    bins2 <- seq(1, 7, length.out = input$bins2 + 1)
    
    ggplot(Badania, aes_string(x = dat, color = rad)) +
      geom_histogram(
        fill = "white",
        alpha = 0.5,
        position = "identity",
        breaks = bins2
      ) +
      ylab("Liczba osób")
  })
  
  output$his3 <- renderPlot({
    dat <- as.character(input$cechy3)
    rad <- as.character(input$radio3)
    
    bins3 <- seq(1, 5, length.out = input$bins3 + 1)
    
    ggplot(Badania, aes_string(x = dat, color = rad)) +
      geom_histogram(
        fill = "white",
        alpha = 0.5,
        position = "identity",
        breaks = bins3
      ) +
      ylab("Liczba osób")
  })
  
  
  output$barchart <- renderPlot({
    colm <- as.character(input$pytanie)
    
    ggplot(Badania, aes_string(x =
                                 colm)) + geom_bar(stat = "count",
                                                   width = 0.7,
                                                   fill = "steelblue") +
      ylab("Liczba osób") +
      theme_classic()
    
  })
  
  
  
  
}
shinyApp(ui = ui, server = server)