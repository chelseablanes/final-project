
# ui: Comparison Tab
comparison_page <- tabPanel(
  "Comparison",
  titlePanel("Comparison of Contribution Factors Based on Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country1",
        label = "Select a first country of interest:",
        choices = as.vector(unique(forest$Country.Name)),
        selected = 1
      ),
      selectInput(
        inputId = "country2",
        label = "Select a second country of interest:",
        choices = as.vector(unique(forest$Country.Name)),
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


# server: text output
output$text <- renderText ({
  paste("Comparison of Contribution Factors between ", input$country1, " and ",
        input$country2)
})

# server: plot output
output$comparison_plot <- renderPlot({
  country_filter <- happiness_df %>% 
    # filter dataframe for two countries (input)
    filter(Country %in% input$country1, input$country2) %>% 
    # long dataframe where columns are categories, country, values
    gather(key = "category", value = "value", -country) %>% 
    spread(key = country, value = value) 
  
  # grouped bar plot where x-axis is category and y-axis is values
  compare_plot <- ggplot(data = country_filter) +
    geom_bar(mapping = aes(x = category, y = value, fill = country))
})