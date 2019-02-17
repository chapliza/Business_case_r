fluidPage(
  # Application title
  titlePanel("Word Cloud Air France"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a parameter:",
                  choices = parameters),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 100, value = 50),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 150)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)