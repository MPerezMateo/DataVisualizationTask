
page_one <- tabPanel(
  titlePanel("Overall Crime by municipalities"), # show with a displayed title
  
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
  page_three,       # include the third page content
  page_four
)

