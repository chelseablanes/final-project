source("ui.R")
library("ggplot2")

my_server <- function(input, output) {
  
  output$comparison_plot <- renderPlot({
    country_filter <- happiness %>% 
      filter(Country == input$country1 | 
             Country == input$country2) %>% 
      gather(key = "category", value = "value", -Country) %>% 
      spread(key - Country, value = value)
    
    compare_plot <- ggplot(data = country_filter) +
      geom_bar(mapping = aes(x = category, y = value, fill = Country))
    
    compare_plot
  })
  
  
}



