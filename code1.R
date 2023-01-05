

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
    DFWide
  })
    
    
  DF2 <- reactive({
    call_DF <- DF()
    
    cat("calculate DF2 \n")
    
    DFWideC = as.data.frame(t(apply(call_DF[, -1], 1, cumsum)))
    
    
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
    
    DFWideC$Development_Year_4 <- rev(DFWideC)[[1]] * input$tail
    colnames(DFWideC) <- c("Development_Year_1", "Development_Year_2", "Development_Year_3", "Development_Year_4" )
    table_ <- cbind(as.integer(call_DF[, 1]),DFWideC[])
    colnames(table_)[1] <- "Loss Year"
    table_
    
  })  
  
  #to tabulate the input data
  output$table <- renderTable({
    
    DF2()
    
  })
  
 
  data1 <- reactive ({
    
    dataframe0 <- DF()
    dataframe <- DF2()
    NewDF <- cbind(gather(dataframe[, 2:5], key = "Development_Year", value = "Cumulative", factor_key = T ), dataframe0[1])
    NewDF
    
  })
  
  #to plot the table output 
  output$scatterplot <- renderPlot({
    
    graph_data <- data1()
    
    
    ggplot(data = graph_data, mapping = aes(x = Development_Year, y = Cumulative,  group = factor(round(`Loss Year`,0)), colour = factor(round(`Loss Year`,0)))) + 
    geom_line() +
    geom_point()  + 
    labs(title = "Graph of Cumulative Paid Claims", x= "Development Year", y = "Cumulative Paid Claims", colour = "Loss Year", subtitle = "A Graph of Development Year vs Cumulative Paid Claims", caption = "R Shiny Assignment") +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(color = "slategrey", size=8, vjust=.8, hjust=0.8), plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "snow2", colour = "black"), 
    panel.border = element_rect(fill = "transparent",color = "gray70", size = 1), legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.margin = margin(6, 6, 6, 6), panel.grid.major=element_line(colour="white"),panel.grid.minor=element_line(colour="white")) +
    scale_linetype_manual(values = c("solid","dotted","dashed")) +
    scale_color_manual(values=c("darkorange1","springgreen2","lightskyblue"))
      
   
    
    
  })
  
  
}

#shinyapp
shinyApp(ui, server)

