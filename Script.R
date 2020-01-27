install.packages("shiny")
devtools::install_github("56north/leafletDK")
install.packages("tidyverse")
install.packages("data.table")
install.packages("usethis")
install.packages("devtools")
install.packages("leaflet")
install.packages("demogR")

library(shiny)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(viridis)
library(data.table)
library(usethis)
library(devtools)
library(leaflet)
library(demogR)
library(dplyr)
# https://github.com/mikkelkrogsholm/leafletDK
library(leafletDK)

data<-read.csv("./data/crime-statistics-of-denmark/Denmark_Crime_Regionwise.csv",stringsAsFactors = F)
colnames(data)
population<-read.csv("./data/Denmark_Population.csv", stringsAsFactors = F)
trimesters<-colnames(data)[3:(ncol(data))]

match<- function(dataset){
  res<-as.integer(regmatches(dataset, gregexpr("[[:digit:]]+", dataset))[[1]])
  out<-res[1]+(res[2]-1)*0.25
}
matches <-unlist(lapply(trimesters,match))
matchesString<-as.character(matches)
setnames(data, old = trimesters, new = matchesString)
popTrimesters<-colnames(population)[2:ncol(population)]
popMatches <-unlist(lapply(popTrimesters,match))
setnames(population, old = popTrimesters, new = as.character(popMatches))
total_crimes<- data %>%
  dplyr::filter(TYPE.OF.OFFENCE =="Penal Code, total" )
total_crimes[,3:6]<-NULL
total_crimes<-total_crimes[c(2:31,33:49,51:72,74:92,94:104),]
population<-population[c(1,3:32,34:50,52:73,75:93,95:105),-(ncol(population))]

sumation<-colSums(total_crimes[,3:ncol(total_crimes)])
total_crimes[nrow(total_crimes)+1,1:2]<-c("All Denmark","Penal Code, total")
total_crimes[nrow(total_crimes),3:ncol(total_crimes)]<-sumation
total_crimes<-arrange(total_crimes,REGION)
population<-arrange(population,REGION)
crime_rates <- cbind(total_crimes[1],1000*total_crimes[,-(1:2)]/population[,-1])

#######################
#######################
#### Second Part

#the last rows are about unknown municipality so i shouldn't consider them
tail(data$REGION)
nrow(data[data$REGION == "Unknown municipality",])

#adding column sum
crime_types <- mutate(
  data,
  total_number_of_crimes = data %>%
    select(3:ncol(data)) %>%
    rowSums/12
)

#select only the 3 columns I need
crime_types <- crime_types %>%
  select(REGION, TYPE.OF.OFFENCE, total_number_of_crimes)

#the columns are the regions
crime_types <- spread(
  crime_types,
  key = TYPE.OF.OFFENCE,
  value = total_number_of_crimes
) 

#taking only the general types of crimes
crime_types <- crime_types[, c(1,47, 72,21,54,59,75)]


#taking only the rows of the macroregions
crime_types_macroregions <- crime_types[73:77,]

#vector of hinabitants
inhab <- c(1848989,837087,1223894,1326913,590580)

#dividing for number of hinabitants and multipying per 10000
for (i in (1:nrow(crime_types_macroregions)))
{
  for (j in (2:ncol(crime_types_macroregions))) {
    crime_types_macroregions[i,j] <- crime_types_macroregions[i,j]/inhab[i]*10000
  }
}


names(crime_types_macroregions)[2] <- "All crimes"
names(crime_types_macroregions)[3] <- "Sexual offences"
names(crime_types_macroregions)[4] <- "Crimes of violence"
names(crime_types_macroregions)[5] <- "Offences against property"
names(crime_types_macroregions)[6] <- "Other offences"
names(crime_types_macroregions)[6] <- "Special acts"

crime_types_macroregions$REGION <- c("Hovedstaden", "Midtjylland", 
                                     "Nordjylland", "Sjælland","Syddanmark")


#################################
#################################
#########Third and Fourth part
denmarkCrimesByTrimester <- read.csv("data/crime-statistics-of-denmark/Denmark_Crime_Regionwise.csv", header = TRUE, fileEncoding='ISO-8859-1')
denmarkCrimesByTrimester$LAT <- jitter(denmarkCrimesByTrimester$LAT)
denmarkCrimesByTrimester$LONG <- jitter(denmarkCrimesByTrimester$LONG)

# Create DataFrame With Values Per Year

denmarkCrimesByYear <- denmarkCrimesByTrimester

for(i in 0:12) {
  year <- as.character(2007 + i)
  
  idxYearQ1 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q1", sep = ""))
  idxYearQ2 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q2", sep = ""))
  idxYearQ3 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q3", sep = ""))
  idxYearQ4 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q4", sep = ""))
  
  if(year != '2019'){
    denmarkCrimesByYear[year] <- denmarkCrimesByYear[idxYearQ1] + denmarkCrimesByYear[idxYearQ2] + denmarkCrimesByYear[idxYearQ3] + denmarkCrimesByYear[idxYearQ4]
  } else {
    denmarkCrimesByYear[year] <- denmarkCrimesByYear[idxYearQ1] + denmarkCrimesByYear[idxYearQ2] + denmarkCrimesByYear[idxYearQ3]
    
  }
  
  yearColumns <- names(select(denmarkCrimesByYear, contains(paste("X", year, "Q", sep = ""))))
  denmarkCrimesByYear[yearColumns] <- NULL
}

denmarkCrimesTotal <- denmarkCrimesByYear
denmarkCrimesTotal['TOTAL'] <- 0

for(i in 0:12) {
  year <- as.character(2007 + i)
  
  denmarkCrimesTotal['TOTAL'] <- denmarkCrimesTotal['TOTAL'] + denmarkCrimesTotal[year]
  denmarkCrimesTotal[year] <- NULL
}

denmarkCrimesTotal$X <- NULL
denmarkCrimesTotal$LAT <- NULL
denmarkCrimesTotal$LONG <- NULL

denmarkCrimesTotal <- denmarkCrimesTotal[denmarkCrimesTotal$REGION != "Region Hovedstaden", ]

df <- denmarkCrimesTotal

for (crime in unique(df$TYPE.OF.OFFENCE)){
  v1 <- df[df$TYPE.OF.OFFENCE == crime,]$TOTAL
  df[df$TYPE.OF.OFFENCE == crime,]$TOTAL = scales::rescale(v1, to=c(0,1))
}

nameRows <- sort(unique(df$TYPE.OF.OFFENCE))
nameCols <- sort(unique(df$REGION))

denmarkCrimesMatrix <- matrix(0, length(nameRows), length(nameCols), dimnames = list(nameRows, nameCols))

# fill in the matrix with matrix indexing on row and column names 
denmarkCrimesMatrix[as.matrix(df[c("TYPE.OF.OFFENCE", "REGION")])] <- df[["TOTAL"]]


#################################
#################################
##############Shiny Functions


my_server <- function(input, output) {
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
    output$crimePlot <- renderPlot({
      
      # Render a barplot
      barplot(crime_types_macroregions[,input$crime], 
              col = "#75AADB",
              names.arg=crime_types_macroregions$REGION,
              main=input$crime,
              ylab="Number of crimes every 10.000 inhabitants",
              xlab="Regions")
    })
    
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
    #observe({
    #  selectedCrimesMatrix <- denmarkCrimesMatrix[input$crime, ] 
    #  
    #  output$treemap <- renderPlot({
    #    crimesScaled <- as.matrix(scale(selectedCrimesMatrix))
    #    # heatmap(crimesScaled, Colv = NA, Rowv = NA, scale="column", xaxis.rot=0)
    #  })
    #})
}

page_one <- tabPanel(
  titlePanel("Overall Crime by regions"), # show with a displayed title
  
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("Region","Select the desired municipality to isolate",unique(population$REGION),selected="All Denmark"),
      sliderInput("Year","Select the desired range of years",min=2008,max=2019.5,value=c(2008),step=0.25,textOutput("DualSlider")),
      checkboxGroupInput("icons", "Choose options:",
                         choiceNames =
                            list(tags$span(icon("map"),(" Street-like Map")),
                                 tags$span(icon("sign"),(" Legend"))),
                                 #tags$span(icon("clipboard-list"),(" One or two regions"))),
                                 # A optional bool for single year would be cool
                         choiceValues =
                           list("strmap", "legend")# ,"oneortwo")
      ),
    ),
    mainPanel(
      h3("Penalty Crimes commited in each quatrimester, per 1k habitants"),
      leafletOutput("map"),
      plotOutput(outputId = "histoPlot")
      )
  )
)

# Define content for the second page
page_two <- tabPanel(
  titlePanel("Heatmap by Crimes & Regions"), # show with a displayed title
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "regions",
        "Regions (max. 5)",
        unique(denmarkCrimesTotal$REGION),
        multiple = TRUE,
        selected = c("Copenhagen", "Frederiksberg"),
        options = list(maxItems = 5)
      ),
      selectizeInput(
        "crimes",
        "Crimes (max. 10)",
        unique(denmarkCrimesTotal$TYPE.OF.OFFENCE),
        multiple = TRUE,
        options = list(maxItems = 10),
        selected = c("Penal Code, total", "Sexual offenses, total", "Rape, etc.", "Prostitution, etc.", "Homicide", "Common assault")
      ),
      selectInput(
        "year",
        "Years",
        2008:2019,
        selected = 2019)
    ),
    mainPanel(
      h3("Heatmap by Crimes & Regions"),
      plotOutput("heatmap", height = "600px")
    )
  )
)

# Define content for the third page
page_three <- tabPanel(
  titlePanel("Crimes by regions"),
  sidebarLayout(      
    sidebarPanel(
      selectInput("crime", "Crime:", 
                  choices=colnames(crime_types_macroregions[,-c(1)])),
      hr(),
      helpText("Choose the type of crime to see how it is spread in the 5 macroregions of Denmark")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("crimePlot")  
    )
  )
)

page_four <- tabPanel(
  titlePanel("Treemap of Crimes per Region in a Year"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "reg",
        "Region",
        unique(denmarkCrimesTotal$REGION),
        selected = "Copenhagen"
      ),
      selectInput(
        "year",
        "Year",
        2008:2019,
        selected = 2019)
    ),
    mainPanel(
      h3("Heatmap by Crimes & Regions"),
      plotOutput("treemap", height = "600px")
    )
  )
)

# Pass each page to a multi-page layout (`navbarPage`)
ui <- navbarPage(
  "Crimes in Denmark", # application title
  page_one,         # include the first page content
  page_two,         # include the second page content
  page_three       # include the third page content
  # page_four
)

shinyApp(ui = ui, server = my_server)
