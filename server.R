source("ui.R")
library("dplyr")
library("tidyr")
library("shiny")
library("ggplot2")
library("maps")

# combine 2 data frames that will be used in multiple render functions
happy_unemployed <- left_join(happiness, unemployment, by = "Country.Code")

server <- function(input, output) {
  
  # server: world map
  world <- map_data("world")
  
  # mutate world map data to have 3 letter country code
  world <-  world %>% 
    mutate(Country.Code = iso.alpha(world$region, n = 3))
  
  # use of if/else statement: user gets optins of seeing either happiness
  # or unemployment map visualization
  output$plot <- renderPlot({
    # scenario is user chooses to see unemployment rate
    if(input$category == "Unemployment Rate") {
      # combined map data and unemployment data
      world_unemployment <- left_join(world, unemployment, by = "Country.Code") %>% 
        select(Country.Code, lat, long, group, X2016)
      
      # remove NA values since world map data has more countries than unemployment
      world_unemployment <- world_unemployment[!is.na(world_unemployment$X2016), ]
      
      # create map visualization for world unemployment 
      world_unemployment_map <- ggplot(data = world_unemployment) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = X2016)) +
        coord_quickmap() +
        labs(
          title = "Unemployment Rates in the World",
          x = NULL,
          y = NULL,
          fill = "Unemployment Rate"
        ) +
        theme_bw() +
        #eliminates background, gridlines, and chart border
        theme(
          plot.background = element_blank()
          ,panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank()
          ,panel.border = element_blank()
        ) 
      
      # return
      world_unemployment_map
    }
    
    # scenario if user chooses to see Happiness score
    else if(input$category == "Happiness Score") {
      # remove values for Antarctica and Greenland (parts of the world with no data)
      world_for_happiness <- world %>% 
        filter(Country.Code != "GRL") %>% 
        filter(Country.Code != "ATA")
      
      # combine world map data and happiness data 
      world_happiness <- left_join(world_for_happiness, happiness, by = "Country.Code") %>% 
        select(lat, long, group, Country, Happiness.Score) 
      
      # visualization for World Happiness Score
      world_happiness_map <- ggplot(data = world_happiness) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = Happiness.Score)) +
        coord_quickmap() +
        labs(
          title = "Happiness Score of Each Country", 
          x = NULL,
          y = NULL, 
          fill = input$category
        ) +
        theme_bw() +
        #eliminates background, gridlines, and chart border
        theme(
          plot.background = element_blank()
          ,panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank()
          ,panel.border = element_blank()
        ) 
      world_happiness_map
    }
  })
  
  # reactive title for Map 
  output$map_title <- renderText({
    # title changes depending on what the user chooses to look at
    paste("World Map Showing", input$category, "by Country")
  })
  
  # scatterplot that looks at the association of unemployment rate and happiness
  output$scatterplot <- renderPlot ({
    
    # plots combined happiness and unemployment data frame 
    ggplot(data = happy_unemployed) +
      geom_point(mapping = aes(x = X2016, y = Happiness.Score), color = "blue", size = 2, alpha = .8) +
      labs(
        title = "Country's Happiness Score as a Function of Unemployment Rate",
        x = "Unemployment Rate %",
        y = "Happiness Score"
      ) +
      theme_bw() 
    
  })
  
  # reactive description of the map depending on user choice
  output$map_description <- renderText({
    paste("The map shows the", input$category, "across the world.")
  })
  
  # description of scatterplot
  output$scatterplot_description <- renderText({
    paste("The scatterplot shows that there is a loose negative association (happiness 
          decreases when unemployment rates increase) between 
          unemployment rates and overall happiness in the world. This suggests
          that there may be other larger contributors to happiness than employment rates alone.")
  })
  
  # server: contribution tab
  
  # creates happiness data frame without certain columns
  happiness_data <- happiness %>%
    select(-contains("Happiness.Rank"),
           -contains("Lower.Confidence.Interval"),
           -contains("Upper.Confidence.Interval"),
           -contains("Region"))
  
  # creates unemployment data frame
  unemployment_data <- unemployment
  # changes col name to "country"
  colnames(unemployment_data)[1] <- "Country"
  # only selects certain columns in the unemployment df
  unemployment_data <- unemployment_data %>%
    select(contains("Country"), contains("X2016"), -contains("Country.Code"))
  
  # joins happiness and unemployment data frame
  happiness_data <- left_join(happiness_data, unemployment_data, by = "Country")

  # changes column names
  colnames(happiness_data)[3:9] <- c("Economy", "Family", "Life_Expectancy", "Freedom",
                                     "Government_Trust", "Generosity", "Dystopia_Residual")
  
  # adjusts data frame and renders it so that it is able to create a pie chart
  gathered_happiness_data <- gather(happiness_data, "Category", "Data", 3:9) %>%
    mutate(Category = factor(Category, levels = c("Dystopia_Residual", "Generosity", "Government_Trust", "Freedom",
                                                  "Life_Expectancy", "Family", "Economy")),
           cumulative = cumsum(Happiness.Score),
           midpoint = cumulative - Happiness.Score / 2,
           label = paste0(Category, " ", round((Data / Happiness.Score) * 100, digits = 2), "%"))
  
  # creates new column with percentages
  gathered_happiness_data$label <- round((gathered_happiness_data$Data / gathered_happiness_data$Happiness.Score), digits = 3)
  
  # adjusts df from input by user
  happiness_reactive <- reactive ({
    filter(gathered_happiness_data, Country %in% capitalize(input$text))
  })
  
  # renders text for that country's happiness score
  output$score <- renderText({
    if (nrow(happiness_reactive()) == 0) {
      print("")
    } else {
      paste("Happiness Score: ", mean(happiness_reactive()$Happiness.Score))
    }
  })
  
  # renders input for what category had the largest impact on the score
  output$large <- renderText({
    if (nrow(happiness_reactive()) == 0) {
      print("")
    } else {
      num <- max(happiness_reactive()$Data)
      value <- filter(happiness_reactive(), Data == num)
      final <-  as.character(value$Category)
      if (final == "Dystopia_Residual") {
        final <- "Dystopia Residual"
      } else if (final == "Life_Expectancy") {
        final <- "Life Expectancy"
      } else if (final == "Government_Trust"){
        final <- "Government Trust"
      }
      paste("Largest impact on this score is", final, "scored at", num, "points.")
    }
  })
  
  # renders feedback for country's unemployment rate
  output$unemployment <- renderText({
    if (nrow(happiness_reactive()) == 0) {
      print("")
    } else {
      paste0("Unemployment Rate: ", round(mean(happiness_reactive()$X2016)), "%")
    }
    
  })
  
  # renders pie chart
  output$pie <- renderPlot({
    ggplot(happiness_reactive()) +
      geom_bar(mapping = aes(x = "", y = Data, fill = Category), width = 1, 
               stat = "identity") + # creates bar chart
      coord_polar(theta = "y") + # creates pie chart
      theme_void() + # voids the x and y axis
      geom_text(aes(x = 1.4, y = (labPos=cumsum(Data) - (Data / 2)), 
                    label=scales::percent(label)), size = 4) + # creates percentages on chart
      scale_fill_discrete(name = "Category",
                          labels = c("Dystopia Residual",
                                     "Generosity",
                                     "Government Trust",
                                     "Freedom",
                                     "Life Expectancy",
                                     "Family",
                                     "Economy")) # changes legend names
  })
  
  
  # server: comparison tab
  output$text <- renderText ({
    paste("Comparison of Contribution Factors between ", input$country1, " and ",
          input$country2)
  })
  
  # server: plot output
  output$comparison_plot <- renderPlot({
    country_filter <- happiness %>%
      select(-Region) %>%
      # filter dataframe for two countries and category (input)
      filter(Country == input$country1 | Country == input$country2) %>%
      select(Country, input$categoryGroup)
    
    # grouped bar plot where x-axis is category and y-axis is values
    compare_plot <- ggplot(data = country_filter) +
      geom_col(mapping = aes_string(x = "Country", y = input$categoryGroup, fill = "Country")) 
      labs(x = "Country", 
           y = input$categoryGroup)
    compare_plot
  })
  
  output$boxplot <- renderPlot({
    
    # uses combined happiness and unemployment data frame
    happy_unemployed <- happy_unemployed %>% 
      # filters region the user selected
      filter(Region == input$Select_Region)
    
    # boxplot visualization of the specific region
    boxplot <- ggplot(data = happy_unemployed,
                      mapping = aes(
                        x = input$Select_Region,
                        y = X2016)) +
      geom_boxplot(fill = "white", color = "red") +
      labs(
        title = paste("Distribution of Unemployment Rates in", input$Select_Region),
        x = "Region",
        y = "Unemployment Rates"
      ) +
      # flip coordinates to desired orientation
      coord_flip() 
    
    boxplot
  })
}





