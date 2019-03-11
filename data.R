library("maps")
library("dplyr")
library("ggplot2")

# World Map
world <- map_data("world")

world <-  world %>% 
  mutate(Country.Code = iso.alpha(world$region, n = 3))

# World Unemployment

unemployment <- read.csv('data/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_10473697.csv', stringsAsFactors = FALSE)

world_unemployment <- left_join(world, unemployment, by = "Country.Code") %>% 
  select(Country.Code, lat, long, group, X2016)

world_unemployment <- world_unemployment[!is.na(world_unemployment$X2016), ]

world_unemployment_map <- ggplot(data = world_unemployment) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = X2016)) +
  coord_quickmap() 

world_unemployment_map

# World Happiness

happiness_2016 <- read.csv('data/2016.csv', stringsAsFactors = FALSE)

happiness_2016 <- happiness_2016 %>% 
  mutate(Country.Code = iso.alpha(happiness_2016$Country, n = 3))

world_for_happiness <- world %>% 
  filter(Country.Code != "GRL") %>% 
  filter(Country.Code != "ATA")

happiness_2016 <- happiness_2016 %>% 
  mutate(Country.Code = iso.alpha(happiness_2016$Country, n = 3))

happiness_2016$Country.Code[happiness_2016$Country == "United States"] <- "USA"
happiness_2016$Country.Code[happiness_2016$Country == "United Kingdom"] <- "GBR"
happiness_2016$Country.Code[happiness_2016$Country == "Congo (Kinshasa)" | 
                            happiness_2016$Country == "Congo (Brazzaville)" ] <- "COD"

world_happiness <- left_join(world_for_happiness, happiness_2016, by = "Country.Code") %>% 
  select(lat, long, group, Country, Happiness.Score) 

world_happiness_map <- ggplot(data = world_happiness) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Happiness.Score)) +
  coord_quickmap()

world_happiness_map

# Unemployment and Happiness

View(happiness_2016)
View(unemployment)

happy_unemployed <- left_join(happiness_2016, unemployment, by = "Country.Code")
View(happy_unemployed)

happy_unemployed_plot <- ggplot(data = happy_unemployed) +
  geom_point(mapping = aes(x = X2016, y = Happiness.Score), color = "blue", size = 2, alpha = .8) +
  labs(
    title = "Country's Happiness Score as a Function of Unemployment Rate",
    x = "Unemployment Rate %",
    y = "Happiness Score"
  )

happy_unemployed_plot








