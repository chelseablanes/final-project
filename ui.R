library("dplyr")
library("tidyr")
library("shiny")
library("ggplot2")
library("maps")
library("Hmisc")

# load unemployment csv
unemployment <- read.csv('data/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_10473697.csv', 
                         stringsAsFactors = FALSE)

unemployment <- unemployment %>% 
  select(Country.Name, Country.Code, X2016)

happiness <- read.csv('data/2016.csv', stringsAsFactors = FALSE)
happiness <- happiness %>% 
  mutate(Country.Code = iso.alpha(happiness$Country, n = 3))


# ui: world map 
intro <- tabPanel(  # lay out the passed content fluidly
  "Introduction",
  sidebarLayout(  # lay out the passed content into two columns
    sidebarPanel( # lay out the passed content inside the "sidebar" column
      # shows choices of which country's data they can see
      radioButtons(inputId = "category", label = "Which Value?", 
                   choices = c("Happiness Score", "Unemployment Rate"), 
                   selected = "Happiness Score")
    ),
    
    mainPanel(    # lay out the passed content inside the "main" column
      # generates the plot output made in the server
      h3(textOutput(outputId = "map_title")),
      plotOutput(outputId = "plot"),
      plotOutput(outputId = "scatterplot"),
      textOutput(outputId = "map_description"),
      textOutput(outputId = "scatterplot_description")
  
    )
  )
)

# ui: contribution tab
contribution_page <- tabPanel(
  "Contribution",
  titlePanel("Largest Contribution of Happiness"),
  sidebarLayout(
    sidebarPanel(
      # creates slider widget so the user can choose a range of years between 2000 and 2016
      textInput("text", label = h3("Type a Country Name"), value = "United States"),
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
    ),
    mainPanel(
            titlePanel("Happiness Pie Chart"),
            textOutput("score"),
            textOutput("large"),
            textOutput("unemployment"),
            plotOutput("pie"),
            p("Even as unemployment rates rise, the economy portion of the happiness chart
              is not affected drastically.")
    )
  )
)



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
        selected = "United States"
      ),
      selectInput(
        inputId = "country2",
        label = "Select a second country of interest:",
        choices = as.vector(unique(happiness$Country)),
        selected = "Canada"
      ),
      radioButtons(
        inputId = "categoryGroup",
        label = "Select a category of interest",
        choices = colnames(happiness)[3:13],
        selected = "Happiness.Rank"
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
      plotOutput(outputId = "comparison_plot")
      )
    )
  )

# ui: Glossary
glossary <- tabPanel(
  "Glossary",
  
  p(strong("Data:"), "This data was sourced from Kaggle and was recorded using a Gallup World Poll in 2016. 
    2,000 to 3,000 people were surveyed for each country."),
  
  p(strong("Happiness Score:"), "A country’s happiness score is based on a survey in 
    which people were asked how they would rate their happiness on a scale of 1 to 10."),
  
  p("The following categories state the extent to which they affected the person’s happiness score:
      Economy, family, health, freedom, trust, generosity, and dystopia residual."),
  
  p(strong("Economy:"), "The economy of that country measured in GDP per capita."),
  
  p(strong("Health:"), "The health of that country in regards to life expectancy."),
  
  p(strong("Trust:"), "The trust the citizens have in their country with regards to their perception of government corruption"),
  
  p(strong("Dystopia Residual:"), "The dystopia residual represents an imaginary country with the world’s least happy people that is 
    used as a benchmark to compare the country’s other scores against and normalize them.")
)

  
  
ui <- fluidPage(
  navbarPage(
    "Happiness and Unemployment Data",
    intro,
    contribution_page,
    comparison_page, glossary)
)
  
