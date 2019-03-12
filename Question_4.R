library("shiny")
library("dplyr")
library("ggplot2")
library("maps")

# first get the unemployment table 
unemployment_table <- read.csv(
  "data/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_10473697.csv", stringsAsFactors = F)

colnames(unemployment_table)

unemployment_2016 <- unemployment_table %>% 
  select(ï..Country.Name, Country.Code, X2016)

happiness_table <- read.csv("data/2016.csv", stringsAsFactors = F) %>% 
  mutate("Country.Code" = iso.alpha(happiness_table$Country, n = 3)) 

happiness_table$Country.Code[happiness_table$Country == "United States"] = "USA"

happiness_unemployment <- left_join(happiness_table, unemployment_2016, 
                                    by = "Country.Code")

happy_unemployment <- happiness_unemployment %>% 
  select(Country, Region, Country.Code, X2016)
  




ui <- fluidPage(
  titlePanel("Unemployment of a Region"), 
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "Select_Region",
        "Region",
        choices = happy_unemployment$Region,
        selected = happy_unemployment$Region == "Western Europe"
        )
    ),
    mainPanel(
      plotOutput("boxplot"),
      
      p("The reason why we would get a visualization for unemployment is to
        make sure users can fully see how unemployment affects a region. Some
        regions here are heavily affected while some are not.
        This is why we did a boxplot for this section. It gives users the 
        full range, median, and inter-quartile ranges for each region. Overall 
        an easy way for users to see all the data into 1 plot. ")
    )
  )
)


happy_unemployment <- happy_unemployment %>% filter(X2016 > 0)

server <- function(input, output) {
  
  output$boxplot <- renderPlot({
    happy_unemployment <- happy_unemployment %>% 
      filter(Region == input$Select_Region)
    
    data_lool <- ggplot(data = happy_unemployment,
                   mapping = aes(
                     x = input$Select_Region,
                     y = X2016))
    
    data_lool + geom_boxplot(fill = "white", colour = "red") + coord_flip() 
  })
}


shinyApp(ui, server)











