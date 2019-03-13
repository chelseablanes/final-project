library("dplyr")
library("tidyr")
library("shiny")
library("ggplot2")
library("maps")
library("Hmisc")

# load unemployment csv
unemployment <- read.csv('data/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_10473697.csv', 
                         stringsAsFactors = FALSE)

# select only pertinent columns
unemployment <- unemployment %>% 
  select(Country.Name, Country.Code, X2016)

# load happiness csv
happiness <- read.csv('data/2016.csv', stringsAsFactors = FALSE)
# mutate happiness data frame to have 3 letter country code for combining
happiness <- happiness %>% 
  mutate(Country.Code = iso.alpha(happiness$Country, n = 3))

# countries that did not have matches for iso.alpha 3 letter country codes
# manually change these values to avoid NAs for these countries
happiness$Country.Code[happiness$Country == "United States"] <- "USA"
happiness$Country.Code[happiness$Country == "United Kingdom"] <- "GBR"
happiness$Country.Code[happiness$Country == "Congo (Kinshasa)" | 
                         happiness$Country == "Congo (Brazzaville)" ] <- "COD"

# combined happiness and unemployment data frames
happy_unemployed <- left_join(happiness, unemployment, by = "Country.Code")

# url for our data sources
happiness_url <- a("World Happiness Report", href="https://www.kaggle.com/unsdsn/world-happiness")
unemployment_url <- a("World Bank Unemployment Data", 
                      href="https://www.kaggle.com/uddipta/world-bank-unemployment-data-19912017")
# ui: world map 
intro <- tabPanel(  # lay out the passed content fluidly
  "Introduction",
  sidebarLayout(  # lay out the passed content into two columns
    sidebarPanel( # lay out the passed content inside the "sidebar" column
      # shows choices of which data they can see
      radioButtons(inputId = "category", label = "Which Value?", 
                   choices = c("Happiness Score", "Unemployment Rate"), 
                   selected = "Happiness Score")
    ),
    
    mainPanel(    # lay out the passed content inside the "main" column
      # generates the plot output made in the server
      # heading for the Map using rendered text from server
      h3(textOutput(outputId = "map_title")),
      # returns map of either happiness or unemployment
      plotOutput(outputId = "plot"),
      # returns scatterplot
      plotOutput(outputId = "scatterplot"),
      # description of map
      textOutput(outputId = "map_description"),
      # description of scatterplot
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
      # creates title panel for tab
      titlePanel("Happiness Pie Chart"),
      # produces text output
      textOutput("score"),
      # produces text output
      textOutput("large"),
      # produces text output
      textOutput("unemployment"),
      # creates pie chart
      plotOutput("pie"),
      # prints findings from data gathered
      p("Even as unemployment rates rise and fall, the economy portion of the happiness chart
        is not affected drastically. Therefore, unemployment rates do not have huge
        effects on the economy rating of each country's happiness score.")
    )
  )
)



# ui: Comparison Tab
comparison_page <- tabPanel(
  "Comparison: Happiness",
  titlePanel("Comparison of Contribution Factors Based on Country"),
  sidebarLayout(
    sidebarPanel(
      # drop down tab for first country option
      selectInput(
        inputId = "country1",
        label = "Select a first country of interest:",
        choices = as.vector(unique(happiness$Country)),
        selected = "United States"
      ),
      # drop down tab for second country option
      selectInput(
        inputId = "country2",
        label = "Select a second country of interest:",
        choices = as.vector(unique(happiness$Country)),
        selected = "Canada"
      ),
      # lets user choose which value to see in the bar chart
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

# unemployment tab
region <- tabPanel(
  "Comparison: Unemployment",
  titlePanel("Summary Statstics of Regional Unemployment Rates"),
  sidebarLayout(
    sidebarPanel(
      # drop down tab to choose region of interest
      selectInput(
        inputId = "Select_Region",
        label = "Region",
        choices = happiness$Region,
        selected = happiness$Region == "Western Europe"
      )
    ),
    
    # quick explaination and reasoning for boxplot and unemployment
    mainPanel(
      plotOutput(outputId = "boxplot"),
      # written explanation of data
      p("The reason why we would get a visualization for unemployment is to
        make sure users can fully see how unemployment affects a region. Some
        regions here are heavily affected while some are not.
        This is why we did a boxplot for this section. It gives users the 
        full range, median, and inter-quartile ranges for each region. Overall 
        an easy way for users to see all the data into 1 plot.")
      )
  )
    )


# ui: Glossary
# shows definitions and sources of both data frames
glossary <- tabPanel(
  "Glossary",
  
  p(strong("Data:"), "The Happiness data was sourced from Kaggle and was recorded using a Gallup World Poll in 2016. 
    2,000 to 3,000 people were surveyed for each country."),
  
  p(strong("Happiness Score:"), "A country’s happiness score is based on a survey in 
    which people were asked how they would rate their happiness on a scale of 1 to 10."),
  
  p("The following categories state the extent to which they affected the person’s happiness score:
      Economy, family, health, freedom, trust, generosity, and dystopia residual."),
  
  p(strong("Economy:"), "The economy of that country measured in GDP per capita."),
  
  p(strong("Health:"), "The health of that country in regards to life expectancy."),
  
  p(strong("Trust:"), "The trust the citizens have in their country with regards to their perception of government corruption"),
  
  p(strong("Dystopia Residual:"), "The dystopia residual represents an imaginary country with the world’s least happy people that is 
    used as a benchmark to compare the country’s other scores against and normalize them."),
  
  # hyperlink of sources
  tagList("Sources:", happiness_url, "and", unemployment_url, "were both from Kaggle.")
  
)

  
# compelted UI
ui <- fluidPage(
  navbarPage(
    "World Happiness and Unemployment",
    # map and scatterplot
    intro,
    # contributors of happiness per country
    contribution_page,
    # compares happiness/contributor of happiness of one country to another
    comparison_page, 
    # summary statistics of unemployment rates per region
    region,
    # glossary definitions
    glossary)
)
  
