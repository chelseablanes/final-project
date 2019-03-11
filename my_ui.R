library("ggplot2")
library("tidyr")
library("DT")
library("dplyr")


first_page <- tabPanel(  # lay out the passed content fluidly
  "Page 1",
  
  sidebarLayout(  # lay out the passed content into two columns
    
    sidebarPanel( # lay out the passed content inside the "sidebar" column
      
      # shows choices of which country's data they can see
      radioButtons(inputId = "category", label = "Which Value?", 
                   choices = c("Happiness Score", "Unemployment Rate"), selected = "Happiness Score")
    ),
    
    mainPanel(    # lay out the passed content inside the "main" column
      
      h3("World by", input$category),
      # generates the plot output made in the server
      plotOutput(outputId = "world_happiness_plot")
   
    )
  )
)


# creates a navbarpage layout for the UI
my_ui <- fluidPage(
  navbarPage("World", first_page)
)


