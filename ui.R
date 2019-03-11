library("dplyr")
library("tidyr")
library("shiny")

source("data.R")

# ui: Comparison Tab
comparison_page <- tabPanel(
  "Comparison",
  titlePanel("Comparison of Contribution Factors Based on Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country1",
        label = "Select a first country of interest:",
        choices = as.vector(unique(happiness$Country)),
        selected = 1
      ),
      selectInput(
        inputId = "country2",
        label = "Select a second country of interest:",
        choices = as.vector(unique(happiness$Country)),
        selected = 1
      )
    ),
    mainPanel(
      h3(textOutput(outputId = "text")),
      # written explanation of data
      p("The results of this data compare the differences in contributors (ex:
        family, GDP, dystopia residual, etc...) to the rankings of happiness, 
        showing the possible correlations between those factors and the outcome 
        of a specific country's ranking."),
      p("This data is potentially insightful for people who are interested
        in how the contributors differ between countries of different 
        rankings within the world happiness data set. It might be worthwhile
        to see how important one contributor is to a country's happiness 
        compared to another country, or if there is a correlation between the 
        contributors and a country's ranking (ex: the more important the family 
        contributor is, the higher the country's ranking)."),
      plotOuput(outputId = "comparison_plot")
      )
  )
)

ui <- fluidPage(
  navbarPage(
    "Happiness and Unemployment Data",
    comparison_page)
)
  
