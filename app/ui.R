page_one <- tabPanel(
  "Overall Crime by regions", # label for the tab in the navbar
  titlePanel("Overall Crime by regions"),
  
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("Regions","Select the desired region to isolate",c(regions,"All")),
      sliderInput("Year","Select the desired range of years",min=2007,max=2019.75,value=c(2007,2019.75),step=0.25,textOutput("DualSlider"))
    ),
    mainPanel(
      h3("Primary Content"),
      p("Plots, data tables, etc. would go here"),
      textOutput("Range")
    )
  )
)

# Define content for the second page
page_two <- tabPanel(
  "Specific crimes", # label for the tab in the navbar
  titlePanel("Specific crimes") # show with a displayed title
  # ...more content would go here...
)

# Define content for the third page
page_three <- tabPanel(
  "Heatmap by Crimes & Regions",
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
page_four <- tabPanel(
  "Treemap of Crimes per Region in a Year",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "region",
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

navbarPage(
  "Crimes in Denmark", # application title
  page_one,         # include the first page content
  page_two,         # include the second page content
  page_three,       # include the third page content
  page_four         # include the four page content
)
