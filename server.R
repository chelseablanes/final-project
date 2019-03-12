source("ui.R")
library("dplyr")
library("tidyr")
library("shiny")
library("ggplot2")
library("maps")

happy_unemployed <- left_join(happiness, unemployment, by = "Country.Code")

server <- function(input, output) {
  
  # server: world map
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
      
      world_unemployment_map
    }
    
    else if(input$category == "Happiness Score") {
      world_for_happiness <- world %>% 
        filter(Country.Code != "GRL") %>% 
        filter(Country.Code != "ATA")
      
      world_happiness <- left_join(world_for_happiness, happiness, by = "Country.Code") %>% 
        select(lat, long, group, Country, Happiness.Score) 
      
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
  
  output$map_title <- renderText({
    paste("World Map Showing", input$category, "by Country")
  })
  
  output$scatterplot <- renderPlot ({
    
    ggplot(data = happy_unemployed) +
      geom_point(mapping = aes(x = X2016, y = Happiness.Score), color = "blue", size = 2, alpha = .8) +
      labs(
        title = "Country's Happiness Score as a Function of Unemployment Rate",
        x = "Unemployment Rate %",
        y = "Happiness Score"
      ) +
      theme_bw() 
    
  })
  
  output$map_description <- renderText({
    paste("The map shows the", input$category, "across the world.")
  })
  
  output$scatterplot_description <- renderText({
    paste("The scatterplot shows that there is a loose negative association (happiness 
          decreases when unemployment rates increase) between 
          unemployment rates and overall happiness in the world. This suggests
          that there may be other larger contributors to happiness than employment rates alone.")
  })
  
  # server: contribution tab
  happiness_data <- happiness %>%
    select(-contains("Happiness.Rank"),
           -contains("Lower.Confidence.Interval"),
           -contains("Upper.Confidence.Interval"),
           -contains("Region"))
  
  unemployment_data <- unemployment
  colnames(unemployment_data)[1] <- "Country"
  unemployment_data <- unemployment_data %>%
    select(contains("Country"), contains("X2016"), -contains("Country.Code"))
  
  happiness_data <- left_join(happiness_data, unemployment_data, by = "Country")
  
  colnames(happiness_data)[3:9] <- c("Economy", "Family", "Life_Expectancy", "Freedom",
                                     "Government_Trust", "Generosity", "Dystopia_Residual")
  
  gathered_happiness_data <- gather(happiness_data, "Category", "Data", 3:9) %>%
    mutate(Category = factor(Category, levels = c("Dystopia_Residual", "Generosity", "Government_Trust", "Freedom",
                                                  "Life_Expectancy", "Family", "Economy")),
           cumulative = cumsum(Happiness.Score),
           midpoint = cumulative - Happiness.Score / 2,
           label = paste0(Category, " ", round((Data / Happiness.Score) * 100, digits = 2), "%"))
  
  gathered_happiness_data$label <- round((gathered_happiness_data$Data / gathered_happiness_data$Happiness.Score), digits = 3)
  happiness_reactive <- reactive ({
    filter(gathered_happiness_data, Country %in% capitalize(input$text))
  })
  
  
  output$score <- renderText({
    if (nrow(happiness_reactive()) == 0) {
      print("")
    } else {
      paste("Happiness Score: ", mean(happiness_reactive()$Happiness.Score))
    }
  })
  
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
      paste("Largest Impact on this score is", final, "scored at", num, "points.")
    }
  })
  
  output$unemployment <- renderText({
    if (nrow(happiness_reactive()) == 0) {
      print("")
    } else {
      paste0("Unemployment Rate: ", round(mean(happiness_reactive()$X2016)), "%")
    }
    
  })
  
  output$pie <- renderPlot({
    ggplot(happiness_reactive()) +
      geom_bar(mapping = aes(x = "", y = Data, fill = Category), width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(x = 1.4, y = (labPos=cumsum(Data) - (Data / 2)), label=scales::percent(label)), size = 4) +
      scale_fill_discrete(name = "Country Name",
                          labels = c("Dystopia Residual",
                                     "Generosity",
                                     "Government Trust",
                                     "Freedom",
                                     "Life Expectancy",
                                     "Family",
                                     "Economy"))
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
    
    happy_unemployed <- happy_unemployed %>% 
      filter(Region == input$Select_Region)
    
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
      coord_flip() 
    
    boxplot
  })
}





