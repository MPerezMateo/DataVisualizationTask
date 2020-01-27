function(input, output, session) {
  # Define content to be displayed by the `message` reactive output
  # `renderText()` is passed a reactive expression
  #my_range <- reactive({
  #  cbind(input$Year[1],input$Year[2])
  #})
  observe({
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

    selectedCrimesMatrix <- denmarkCrimesMatrix[input$crimes, input$regions]
    selectedCrimesDF <- filter(denmarkCrimesTotal, TYPE.OF.OFFENCE %in% input$crimes, REGION %in% input$regions)
    
    if (input$plotType == "heatmap") {
      output$plot3 <- renderPlot({
        ggplot(selectedCrimesDF,aes(x=REGION,y=TYPE.OF.OFFENCE,fill=TOTAL))+
          geom_tile(colour="white",size=0.25)+
          scale_fill_distiller(palette = "Spectral")+
          labs(x="",y="")+
          theme_grey(base_size=8)+
          theme(
            text = element_text(size=18),
            legend.text=element_text(face="bold"),
            axis.ticks=element_line(size=0.4),
            plot.background=element_blank(),
            panel.border=element_blank())

      })
    } else {
      output$plot3 <- renderPlot({
        ggplot(data=selectedCrimesDF, aes(x=REGION, y=TOTAL, fill=TYPE.OF.OFFENCE)) +
          geom_bar(stat="identity", color="black", position=position_dodge())+
          theme(
            text = element_text(size=18),
            legend.text=element_text(face="bold"),
            axis.ticks=element_line(size=0.4))+
          scale_fill_brewer(palette="Spectral")
      })
    }
  })
  
  # PAGE FOUR #######################################
  observe({
    y<-input$yeartree
    selectedCrimesDF <- denmarkCrimesByYear[c("REGION", "TYPE.OF.OFFENCE", y)]
    names(selectedCrimesDF)[names(selectedCrimesDF) == y] <- "TOTAL"
    selectedCrimesDF <- filter(selectedCrimesDF, REGION == input$region)
    
    output$treemap <- renderPlot({
      treemap(selectedCrimesDF,
              index=c("TYPE.OF.OFFENCE"),
              vSize="TOTAL",
              type="index",
              palette = "Spectral"
      )
    })
  })
}
