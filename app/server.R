library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(viridis)

function(input, output, session) {
  
  # PAGE ONE #######################################
  my_range <- reactive({
    cbind(input$range[1],input$range[2])
  })
  output$SliderText <- renderText({my_range()})
  
  # PAGE TWO #######################################
  
  
  # PAGE THREE #######################################
  observe({
    # selectedCrimesMatrix <- df[is.element(df$TYPE.OF.OFFENCE, input$crimes), is.element(df$REGION, input$regions), ]
    
    selectedCrimesMatrix <- filter(df, TYPE.OF.OFFENCE %in% input$crimes, REGION %in% input$regions)
    
    output$heatmap <- renderPlot({
      # crimesScaled <- as.matrix(scale(selectedCrimesMatrix))
      # heatmap(crimesScaled, scale="column")
      ggplot(selectedCrimesMatrix,aes(x=REGION,y=TYPE.OF.OFFENCE,fill=TOTAL))+
        geom_tile(colour="white",size=0.25)+
        scale_fill_distiller(palette = "Spectral")+
        labs(x="",y="")+
        theme_grey(base_size=8)+
        theme(
          legend.text=element_text(face="bold"),
          axis.ticks=element_line(size=0.4),
          plot.background=element_blank(),
          panel.border=element_blank())
    })
  })
  
  # PAGE FOUR #######################################
  observe({
    selectedCrimesMatrix <- denmarkCrimesMatrix[input$crime, ] 
    
    output$treemap <- renderPlot({
      crimesScaled <- as.matrix(scale(selectedCrimesMatrix))
      # heatmap(crimesScaled, Colv = NA, Rowv = NA, scale="column", xaxis.rot=0)
      
    })
  })
}
