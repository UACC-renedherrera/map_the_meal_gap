# set up ----
library(here)
library(tidyverse)
library(readxl)
library(tidycensus)

# load variables of US Census acs1 into environment
acs1_2018 <- load_variables(2018, "acs1", cache = TRUE)

# display total population of USA ----
us_population_total <- get_acs(
  geography = "us",
  variables = c("us_estimate" = "B01003_001"),
  year = 2018,
  survey = "acs1"
)

# display total population of AZ ----
az_population_total <- get_acs(
  geography = "state",
  state = "az",
  variables = c("us_estimate" = "B01003_001"),
  year = 2018,
  survey = "acs1"
)

# display total population of UAZCC catchment ----
# acs five year estimate because
# The one-year ACS provides data for geographies with populations of 65,000 and greater.
catch_population_total <- get_acs(
  geography = "county",
  state = "az",
  variables = c("us_estimate" = "B01003_001"),
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

# catchment counties, save for later 
catch_population <- catch_population_total

# use stringr to clean up county name
# and save
catch_population_total <- catch_population_total %>%
  mutate(NAME = str_replace(catch_population_total$NAME, " County, Arizona", ""))

# total population
catch_population_total <- catch_population_total %>%
  filter(NAME %in% counties) %>%
  summarise(estimate = sum(estimate))

# data import 
# state data for the map the meal gap ----
mmg2020_state <- read_xlsx("data/raw/MMG2020_2018Data_ToShare.xlsx",
  sheet = "2018 State",
  range = "A2:R53",
  col_names = TRUE,
  na = "",
  trim_ws = TRUE,
  skip = 1
)

# display column names
names(mmg2020_state)

# food insecurity rate usa ----
mmg2020_state %>%
  select(
    `State Name`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = us_population_total$estimate) %>%
  mutate(prop = food_insecure / total)

# food insecurity rate az ----
mmg2020_state %>%
  filter(State == "AZ") %>%
  select(
    `State Name`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = az_population_total$estimate) %>%
  mutate(prop = food_insecure / total)

# county data for the map the meal gap ----
mmg2020_county <- read_xlsx("data/raw/MMG2020_2018Data_ToShare.xlsx",
  sheet = "2018 County",
  range = "A2:R3144",
  col_names = TRUE,
  na = "",
  trim_ws = TRUE,
  skip = 1
)

# filter to AZ only
mmg2020_county <- mmg2020_county %>%
  filter(State == "AZ")

# use stringr to clean up county name
mmg2020_county <- mmg2020_county %>%
  mutate(`County, State` = str_replace(mmg2020_county$`County, State`, " County, Arizona", ""))

# catchment food insecurity rate ---- 
mmg2020_county %>%
  filter(`County, State` %in% counties) %>%
  select(
    State,
    `County, State`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = catch_population_total$estimate) %>%
  mutate(prop = food_insecure / total)

# county level insecurity rate ---- 
mmg2020_county_totals <- mmg2020_county %>%
  filter(`County, State` %in% counties) %>%
  select(
    State,
    `County, State`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  select(NAME = "County, State",
       estimate = "# of Food Insecure Persons in 2018")

catch_population_totals <- catch_population %>%
  mutate(NAME = str_replace(catch_population$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, total = estimate)

food_insec_catch <- full_join(mmg2020_county_totals, catch_population_totals)

food_insec_catch %>%
  mutate(prop = estimate / total)
