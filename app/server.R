

function(input, output, session) {
  # Define content to be displayed by the `message` reactive output
  # `renderText()` is passed a reactive expression
  #my_range <- reactive({
  #  cbind(input$Year[1],input$Year[2])
  #})
  #output$Range <- renderText({my_range()})
  my_legend <- reactive({
    if(length(input$icons) ==0) FALSE
    else  'legend' %in% input$icons
  })
  my_strmap <- reactive({
    if(length(input$icons) ==0) FALSE
    else  'strmap' %in% input$icons
  })
  output$icons<-  renderText({paste(input$icons, collapse = ", ")})
  output$Region <- renderText({input$Region})
  output$map <- renderLeaflet({
    if(input$Region != "All Denmark") municip<-input$Region
    else{ municip<-NULL}
    municipalityDK(paste(input$Year[1]),"REGION",subplot = municip, data = crime_rates,
                   legend = my_legend(),map = my_strmap(), legendtitle = "Crimes per 1k habitant")
    
  })
  output$histoPlot <- renderPlot({
    x  <- seq(2008,2019.5,by = 0.25)
    y  <- as.numeric(crime_rates %>%
                       filter(REGION==paste0(input$Region)) %>%
                       select(-REGION))
    
    plot(x, y, type = "b", col = "red", xlab = "Years by trimester", ylab = "Crimes per 1k habitants")
  })
  
  # PAGE THREE #######################################
  output$crimePlot <- renderPlot({
    
    # Render a barplot
    barplot(crime_types_macroregions[,input$crime], 
            col = "#75AADB",
            names.arg=crime_types_macroregions$REGION,
            main=input$crime,
            ylab="Number of crimes every 10.000 inhabitants",
            xlab="Regions")
  })
  
  # PAGE TWO #######################################
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
  
  #observe({
  #  selectedCrimesMatrix <- denmarkCrimesMatrix[input$crime, ] 
  
  #  output$treemap <- renderPlot({
  #  crimesScaled <- as.matrix(scale(selectedCrimesMatrix))
  ### Treemap pending
  #  })
  #})
}
