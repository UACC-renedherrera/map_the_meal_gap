# set up ----
# load packages
library(here) 
library(tidyverse)
library(readxl)
library(tidycensus)

# load variables of US Census acs1 into environment
acs1_2018 <- load_variables(2018, "acs1", cache = TRUE)

# to get the denominator in order to calculate rates
# get from acs1 total population of USA ----
population_us <- get_acs(
  geography = "us",
  variables = c("population" = "B01003_001"),
  year = 2018,
  survey = "acs1"
)

# get from acs1 total population of AZ ----
population_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c("population" = "B01003_001"),
  year = 2018,
  survey = "acs1"
)

# get from acs5 total population of UAZCC catchment ----
# acs five year estimate because
# The one-year ACS provides data for geographies with populations of 65,000 and greater.
population_az_counties <- get_acs(
  geography = "county",
  state = "az",
  variables = c("population" = "B01003_001"),
  year = 2018,
  survey = "acs5"
)

# save catchment counties to value
counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# use stringr to clean up county name
# and save
population_az_counties <- population_az_counties %>%
  mutate(NAME = str_replace(population_az_counties$NAME, " County, Arizona", ""))

# combine all population into one dataframe
population_tbl <- bind_rows(population_us, 
          population_az, 
          population_az_counties)

# population estimates for each area 
population_tbl

# total population
population_catchment <- population_az_counties %>%
  filter(NAME %in% counties) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(NAME = "Catchment")

# total population in catchment
population_catchment

population_tbl <- bind_rows(population_tbl,
          population_catchment)

# data import 
# food insecurity 
# state data for the map the meal gap ----
mmg2020_state <- read_xlsx("data/raw/MMG2020_2018Data_ToShare.xlsx",
  sheet = "2018 State",
  range = "A2:R53",
  col_names = TRUE,
  na = "",
  trim_ws = TRUE,
  skip = 1
)

glimpse(mmg2020_state)

# display column names
names(mmg2020_state)

# food insecurity rate usa ----
food_insecure_us <-  mmg2020_state %>%
  select(
    `State Name`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = population_us$estimate) %>%
  mutate(prop = food_insecure / total,
         NAME = "United States")

# calculated food insecurity rate usa
food_insecure_us

# food insecurity rate az ----
food_insecure_az <- mmg2020_state %>%
  filter(State == "AZ") %>%
  select(
    `State Name`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = population_az$estimate) %>%
  mutate(prop = food_insecure / total,
         NAME = "Arizona")

# calculated food insecurity rate az
food_insecure_az

# county data for the map the meal gap ----
mmg2020_county <- read_xlsx("data/raw/MMG2020_2018Data_ToShare.xlsx",
  sheet = "2018 County",
  range = "A2:R3144",
  col_names = TRUE,
  na = "",
  trim_ws = TRUE,
  skip = 1
)

glimpse(mmg2020_county)

# filter to AZ only
mmg2020_county <- mmg2020_county %>%
  filter(State == "AZ")

# use stringr to clean up county name
mmg2020_county <- mmg2020_county %>%
  mutate(`County, State` = str_replace(mmg2020_county$`County, State`, " County, Arizona", ""))

# food insecurity az counties
food_insecure_az_counties <- mmg2020_county %>%
  select(
    State,
    NAME = `County, State`,
    food_insecure= "# of Food Insecure Persons in 2018"
  ) %>%
  mutate(total = population_az_counties$estimate) %>%
  mutate(prop = food_insecure / total) %>%
  select(!(State))

# food insecurity shown for all az counties
food_insecure_az_counties

# catchment food insecurity rate ---- 
food_insecure_catch <- mmg2020_county %>%
  filter(`County, State` %in% counties) %>%
  select(
    State,
    `County, State`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = population_catchment$estimate) %>%
  mutate(prop = food_insecure / total,
         NAME = "Catchment") 

# food insecurity for catchment altogether
food_insecure_catch

# combine all food insecurity values to one
food_insecurity <- bind_rows(
  food_insecure_us,
  food_insecure_az,
  food_insecure_catch,
  food_insecure_az_counties
)

# food insecurity shown for all areas
food_insecurity <- food_insecurity %>%
  select(NAME,
         food_insecure,
         total,
         prop)

# print food insecurity for all areas
food_insecurity

# save 
write_rds(food_insecurity,
          "data/tidy/food_insecurity.rds")
