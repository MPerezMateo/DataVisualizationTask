install.packages("shiny")
library("shiny")
library(tidyverse)
library(data.table)

data<-read.csv("./data/crime-statistics-of-denmark/Denmark_Crime_Regionwise.csv", stringsAsFactors = F)
regions<-unique(data$REGION)
trimesters<-colnames(data)[3:ncol(data)]

match<- function(dataset){
  res<-as.integer(regmatches(dataset, gregexpr("[[:digit:]]+", dataset))[[1]])
  out<-res[1]+(res[2]-1)*0.25
}
matches <-unlist(lapply(trimesters,match))
matchesString<-as.character(matches)
setnames(data, old = trimesters, new = matchesString)

my_server <- function(input, output) {
  # Define content to be displayed by the `message` reactive output
  # `renderText()` is passed a reactive expression
    my_range <- reactive({
      cbind(input$Year[1],input$Year[2])
    })
    output[["Overall Crime by regions"]] <- renderText({my_range()})

}

page_one <- tabPanel(
  "Overall Crime by regions", # label for the tab in the navbar
  titlePanel("Overall Crime by regions"), # show with a displayed title
  
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("Regions","Select the desired region to isolate",c(regions,"All")),
      sliderInput("Year","Select the desired range of years",min=2007,max=2019.75,value=c(2007,2019.75),step=0.25,textOutput("DualSlider"))
    ),
    mainPanel(
      h3("Primary Content"),
      p("Plots, data tables, etc. would go here")
    )
  )
)

# Define content for the second page
page_two <- tabPanel(
  "Specific crimes", # label for the tab in the navbar
  titlePanel("Specific crimes"), # show with a displayed title
  # ...more content would go here...
)

# Define content for the third page
page_three <- tabPanel(
  "Third Page" # label for the tab in the navbar
  # ...more content would go here...
)

# Pass each page to a multi-page layout (`navbarPage`)
ui <- navbarPage(
  "Crimes in Denmark", # application title
  page_one,         # include the first page content
  page_two,         # include the second page content
  page_three        # include the third page content
)
server <- shinyServer(function(input, output, session){
  my_range <- reactive({
    cbind(input$range[1],input$range[2])
  })
  output$SliderText <- renderText({my_range()})
})
shinyApp(ui = ui, server = my_server)
Who is overall crime evolution in the whole Denmark & in specific municipalities

