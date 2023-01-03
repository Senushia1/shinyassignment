

library(ggplot2)
library(shiny)
library(shinythemes)
library(readxl)
library(tidyr)

ui <- fluidPage(
  navbarPage( title = "Cumulative Claims",
              position = "static-top",
              theme = shinytheme("united")
  ),
  sidebarPanel(
    h3("Input"),
    fileInput(inputId = "upload", label = "Upload a file"),
    h3("Tail"),
    sliderInput(inputId = "tail", label = "Tail", value = 1.1, min = 0, max = 10, step = 0.1, animate = TRUE)
  ),
  mainPanel(
    plotOutput(outputId = "scatterplot"),
    tableOutput("table"),
  ),
)


server <- function(input,output) {
  
  DF  <- reactive({
    req(input$upload)
    hh <- input$upload$datapath
    jj = read_excel(hh)
    DFWide = reshape(as.data.frame(jj), timevar = "Development Year", idvar = "Loss Year", direction = "wide")
    DFWideC = as.data.frame(t(apply(DFWide[, -1], 1, cumsum)))
    
    count = 0
    
    for (i in 2:ncol(DFWideC)) {
      count = count + 1
      for (k in 1:nrow(DFWideC)){
        if (!is.na(DFWideC[k,i]) == TRUE) 
        {
          next
        }
        h = (sum(DFWideC[c(1:(nrow(DFWideC) - count)), i])/sum(DFWideC[c(1:(nrow(DFWideC) - count)),i - 1]))*DFWideC[k,i-1]
        DFWideC[k,i] = h
      }
    }
    
    DFWideC
    DFWideC$Development_Year_4 <- rev(DFWideC)[1] * input$tail
    DFWideC
    colnames(DFWideC) <- c("Development_Year_1", "Development_Year_2", "Development_Year_3", "Development_Year_4" )
    k <- cbind(DFWide[1],DFWideC[])
    colnames(k)[1] <- "Loss Year"
    p <- as.data.frame(k)
    p
    
  })
  
  output$table <- renderTable({
    DF()
    
  })
  
  output$scatterplot <- renderPlot({
    
    t = p %>% pivot_longer(cols = c(p$Loss_Year), names_to = "Loss_Year", values_to = "Cumulative_Loss_Claims")
    ggplot(data = t, mapping = aes(x = t$Development_Year, y = Cumulative_Loss_Claims, colour = t$Cumulative_Loss_Claims, group = t$Cumulative_Loss_Claims)) +
      geom_point(mapping = aes(x = Development_Year, y = Cumulative_Loss_Claims)) +
      
      labs(x = "Development Year", y = "Cumulative Loss Claims")
    
  })
  
  
}

shinyApp(ui, server)

