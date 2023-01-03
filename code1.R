

library(ggplot2)
library(shiny)
library(shinythemes)
library(readxl)
library(tidyr)

#ui
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

#server
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
    
    DFWideC$Development_Year_4 <- rev(DFWideC)[1] * input$tail
    colnames(DFWideC) <- c("Development_Year_1", "Development_Year_2", "Development_Year_3", "Development_Year_4" )
    table_ <- cbind(DFWide[1],DFWideC[])
    colnames(table_)[1] <- "Loss Year"
    table_
    
  })
  
  output$table <- renderTable({
    DF()
    
  })
  
  data1 <- reactive ({
    
    NewDF <- cbind(gather(table_[2:4], key = "Development_Year", value = "Cumulative", factor_key = T ), p[1])
    NewDF
    
  })
  
  output$scatterplot <- renderPlot({
    
    ggplot(data = data1(), mapping = aes(x = Development_Year, y = Cumulative, colour = NewDF$`Loss Year`, group = NewDF$`Loss Year`), theme = chartTheme("white"), title(main = "Graph of Cumulative Loss Claims"), xlab = "Development_Year", ylab = "Cumulative Loss CLaims") +
      #geom_smooth(mapping = aes(x = NewDF$Development_Year, y = NewDF$Cumulative)) +
      geom_smooth() +
      geom_point(mapping = aes(x = NewDF$Development_Year, y = NewDF$Cumulative))  + labs(title = "Graph of Cumulative Loss Claims", x= "Development Year", y = "Cumulative Loss Claims") 
    #theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(color = "slategray4", size=8, angle=90, vjust=.8, hjust=0.8), plot.background = element_rect(fill = "floralwhite"), panel.background = element_rect(fill = "floralwhite", colour = "black"), 
    #panel.border = element_rect(fill = "transparent",color = 4, size = 2))
    
    
    
  })
  
  
}

#shinyapp
shinyApp(ui, server)

