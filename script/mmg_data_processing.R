# set up ----
# load packages
library(here)
library(tidyverse)
library(ggthemes)

# load data to environment
# already prepared table for UAZCC
food_insecurity_table <- read_rds("data/tidy/food_insecurity.rds")

# data provided by MMG for counties in USA
food_insecure_counties <- read_rds("data/tidy/food_insecure_usa_counties.rds")

# data provided by MMG for states in USA
food_insecure_states <- read_rds("data/tidy/food_insecure_usa_states.rds")

# plot of food insecurity for AZ counties, in comparison to USA, AZ, and catchment
food_insecurity_table %>%
  ggplot(mapping = aes(x = reorder(NAME, prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .25) +
  geom_text(aes(label = round(prop, digits = 3), hjust = -.25)) +
  labs(title = "Food Insecurity in Arizona",
       subtitle = "For Arizona counties in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020 \n U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates")
  
glimpse(food_insecure_counties)

# explore the county dataset 
food_insecure_az_counties <- food_insecure_counties %>%
  filter(State == "AZ")

# use stringr to clean up county name
# and save
food_insecure_az_counties <- food_insecure_az_counties %>%
  mutate(`County, State` = str_replace(food_insecure_az_counties$`County, State`, " County, Arizona", ""))

# proportion of food insecure above poverty threshold 
food_insecure_az_counties %>% 
  ggplot(mapping = aes(x = reorder(`County, State`, `% FI > High Threshold`), y = `% FI > High Threshold`)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .5) +
  geom_text(aes(label = round(`% FI > High Threshold`, digits = 3), hjust = -.25))  +
  labs(title = "% of Food Insecure > Threshold in Arizona",
       subtitle = "Proportion of food insecure households above poverty \nthreshold for Arizona counties in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020")

# proportion of children living in food insecure households
food_insecure_az_counties %>% 
  ggplot(mapping = aes(x = reorder(`County, State`, `2018 Child food insecurity rate`), y = `2018 Child food insecurity rate`)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .5) +
  geom_text(aes(label = round(`2018 Child food insecurity rate`, digits = 3), hjust = -.25))  +
  labs(title = "% of Children Living in Food Insecure Households in Arizona",
       subtitle = "Proportion of children living in food insecure households \nfor Arizona counties in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020")

# food insecurity rate by cost per meal 
food_insecure_az_counties %>% 
  ggplot(mapping = aes(y = `2018 Food Insecurity Rate`, x = `2018 Cost Per Meal`)) +
  geom_point() +
  geom_text(aes(label = `County, State`)) +
  theme_clean() 
