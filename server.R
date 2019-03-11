

# server for comparison page
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