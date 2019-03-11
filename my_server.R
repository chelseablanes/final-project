library("ggplot2")
library("tidyr")
library("maps")
library("shiny")

first_page <- tabPanel(  # lay out the passed content fluidly
  "Page 1",
  
  sidebarLayout(  # lay out the passed content into two columns
    
    sidebarPanel( # lay out the passed content inside the "sidebar" column
      
      # shows choices of which country's data they can see
      radioButtons(inputId = "category", label = "Which Value?", 
                   choices = c("Happiness Score", "Unemployment Rate"), 
                   selected = "Happiness Score")
    ),
    
    mainPanel(    # lay out the passed content inside the "main" column
      
      h3("World by"),
      # generates the plot output made in the server
      plotOutput(outputId = "plot")
      
    )
  )
)


# creates a navbarpage layout for the UI
ui <- fluidPage(
  navbarPage("World", first_page)
)

# load unemployment csv
unemployment <- read.csv('data/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_10473697.csv', 
                         stringsAsFactors = FALSE)

unemployment <- unemployment %>% 
  select(Country.Name, Country.Code, X2016)

happiness <- read.csv('data/2016.csv', stringsAsFactors = FALSE)
world <- map_data("world")

world <-  world %>% 
  mutate(Country.Code = iso.alpha(world$region, n = 3))


my_server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    if(input$category == "Unemployment Rate") {
    world_unemployment <- left_join(world, unemployment, by = "Country.Code") %>% 
      select(Country.Code, lat, long, group, X2016)
    
    world_unemployment <- world_unemployment[!is.na(world_unemployment$X2016), ]
    
    world_unemployment_map <- ggplot(data = world_unemployment) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = X2016)) +
      coord_quickmap() +
      labs(
        title = "Unemployment Rates in the World",
        x = NULL,
        y = NULL
      )
    
    world_unemployment_map
    }
    
    else if(input$category == "Happiness Score") {
      happiness <- happiness %>% 
        mutate(Country.Code = iso.alpha(happiness$Country, n = 3))
      
      world_for_happiness <- world %>% 
        filter(Country.Code != "GRL") %>% 
        filter(Country.Code != "ATA")
      
      happiness <- happiness %>% 
        mutate(Country.Code = iso.alpha(happiness$Country, n = 3))
      
      happiness$Country.Code[happiness_2016$Country == "United States"] <- "USA"
      happiness$Country.Code[happiness_2016$Country == "United Kingdom"] <- "GBR"
      happiness$Country.Code[happiness_2016$Country == "Congo (Kinshasa)" | 
                               happiness_2016$Country == "Congo (Brazzaville)" ] <- "COD"
      
      world_happiness <- left_join(world_for_happiness, happiness, by = "Country.Code") %>% 
        select(lat, long, group, Country, Happiness.Score) 
      
      world_happiness_map <- ggplot(data = world_happiness) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = Happiness.Score)) +
        coord_quickmap() +
        labs(
          title = "Happiness Score of Each Country"
        )
      
      world_happiness_map
    }
  
  })
  
  
  output$unemployment_happiness_plot <- renderPlot({
    happy_unemployed <- left_join(happiness_2016, unemployment, by = "Country.Code")
    
    happy_unemployed_plot <- ggplot(data = happy_unemployed) +
      geom_point(mapping = aes(x = X2016, y = Happiness.Score), color = "blue", size = 2, alpha = .8) +
      labs(
        title = "Country's Happiness Score as a Function of Unemployment Rate",
        x = "Unemployment Rate %",
        y = "Happiness Score"
      )
    
    happy_unemployed_plot
    
  })
  
}

shinyApp(ui = ui, my_server)
