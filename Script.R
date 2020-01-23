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
  output$message <- renderText({
    # A render function block can include any code used in a regular function
    my_greeting <- "Hello "
    
    # Because the render function references an `input` value, it will be
    # automatically rerun when that value changes, producing an updated output
    message_str <- paste0(my_greeting, input$username, "!")
    message_str # return the message to be output
  })
}

page_one <- tabPanel(
  "Overall Crime by regions", # label for the tab in the navbar
  titlePanel("Overall Crime by regions"), # show with a displayed title
  
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("Regions","Select the desired region to isolate",c(regions,"All")),
      sliderInput("Year","Select the desired year",min=2007,max=2019.75,value=2007,step=0.25,c(data$`2007`:data$`2019.5`))
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
shinyApp(ui = ui, server = my_server)
Who is overall crime evolution in the whole Denmark & in specific municipalities

