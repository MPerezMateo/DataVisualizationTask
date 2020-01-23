install.packages("shiny")
library("shiny")

data<-read.csv("./data/crime-statistics-of-denmark/Denmark_Crime_Regionwise.csv", stringsAsFactors = F)

regions<-unique(data$REGION)

my_ui <- fluidPage(
  # A static content element: a 2nd level header that displays text
  h2("Greetings from Shiny"),
  
  # A widget: a text input box (save input in the `username` key)
  textInput(inputId = "username", label = "What is your name?"),
  
  # An output element: a text output (for the `message` key)
  textOutput(outputId = "message")
)

my_server <- function(input, output) {
  # Assign a value to the `message` key in the `output` list using
  # the renderText() method, creating a value the UI can display
  output$message <- renderText({
    # This block is like a function that will automatically rerun
    # when a referenced `input` value changes
    
    # Use the `username` key from `input` to create a value
    message_str <- paste0("Hello ", input$username, "!")
    
    # Return the value to be rendered by the UI
    message_str
  })
}

ui1 <- fluidPage(
  sliderInput(
    inputId = "age",           # key this value will be assigned to
    label = "Age of subjects", # label to display alongside the slider
    min = 18,                  # minimum slider value
    max = 80,                  # maximum slider value
    value = 42                 # starting value for the slider
  )
)

ui2 <- fluidPage(  # lay out the passed content fluidly
  sidebarLayout(  # lay out the passed content into two columns
    sidebarPanel( # lay out the passed content inside the "sidebar" column
      p("Sidebar panel content goes here")
    ),
    mainPanel(    # lay out the passed content inside the "main" column
      p("Main panel content goes here"),
      p("Layouts usually include multiple content elements")
    )
  )
)

page_one <- tabPanel(
  "First Page", # label for the tab in the navbar
  titlePanel("Page 1"), # show with a displayed title
  
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "username", label = "What is your name?")
    ),
    mainPanel(
      h3("Primary Content"),
      p("Plots, data tables, etc. would go here")
    )
  )
)

# Define content for the second page
page_two <- tabPanel(
  "Second Page" # label for the tab in the navbar
  # ...more content would go here...
)

# Define content for the third page
page_three <- tabPanel(
  "Third Page" # label for the tab in the navbar
  # ...more content would go here...
)

# Pass each page to a multi-page layout (`navbarPage`)
ui <- navbarPage(
  "My Application", # application title
  page_one,         # include the first page content
  page_two,         # include the second page content
  page_three        # include the third page content
)
shinyApp(ui = ui, server = my_server)

server <- function(input, output) {
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
