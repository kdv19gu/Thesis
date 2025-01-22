library(tidyverse)
library(dplyr)
library(here)

### Loading county ACS data

load(here("Processed/county_acs.RData"))

### Loading municipality ACS data

load(here("Processed/city_acs.RData"))

### State minimum wage data

state_minwage <- read_csv("Supplemental Data/state_minwages.csv") |> 
  filter(`State or other` != "Federal (FLSA)") |> 
  select(!`2020`) |> 
  rename(state_fips = statefips)

# Formatting state min wage data to panel data

state_minwage_panel <- state_minwage |> 
  pivot_longer(cols = c(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, 
                        `2018`, `2019`, `2021`, `2022`, `2023`), 
               names_to = "year",
               values_to = "minwage") |> 
  mutate(year = as.numeric(year))

### County minimum wage data

county_minwage <- read_csv("Supplemental Data/county_minwages.csv") |> 
  rename(name = county)

county_minwage <- county_minwage |> 
  left_join(acs_binded_counties |> 
              select(name, statecounty_fips),
            by = "name") |> 
  mutate(state_fips = substr(statecounty_fips, 1, 2)) |> 
  select(name, state_fips, statecounty_fips, everything()) |> 
  distinct() |> 
  rename(county = name)

# Reformatting to panel data

county_minwage_panel <- county_minwage |> 
  pivot_longer(cols = c(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, 
                        `2018`, `2019`, `2021`, `2022`, `2023`), 
               names_to = "year",
               values_to = "minwage") |> 
  mutate(year = as.numeric(year))

# Filling in missing min wage to state min wage

county_minwage_panel <- county_minwage_panel |> 
  left_join(state_minwage_panel, by = c("state_fips", "year")) |> 
  mutate(minwage = coalesce(minwage.x, minwage.y)) |> 
  select(-minwage.x, -minwage.y)

# Adding state and county minimum wage data to ACS data

acs_binded_counties <- acs_binded_counties |> 
  mutate(state_fips = substr(statecounty_fips, 1, 2),
         year = as.numeric(year)) |> 
  select(state_fips, everything())

county_acs_minwage <- acs_binded_counties |> 
  left_join(state_minwage_panel |> 
              select(state_fips, year, minwage), 
            by = c("state_fips", "year")) |> 
  rename(state_minwage = minwage) |> 
  left_join(county_minwage_panel |> 
              select(statecounty_fips, year, minwage),
            by = c("statecounty_fips", "year")) |> 
  rename(county_minwage = minwage) |> 
  mutate(county_minwage = if_else(is.na(county_minwage), state_minwage, county_minwage),
         prevailing_minwage = if_else(county_minwage >= state_minwage, county_minwage, state_minwage)) # prevailing minimum wage is the highest between the state and county wages

# Implementing lagged variables and higher-than-fed indicator var

county_acs_minwage <- county_acs_minwage |> 
  group_by(statecounty_fips) |> 
  arrange(year) |> 
  mutate(minwage_over_fed = if_else(prevailing_minwage > 7.25, 1, 0),
         minwage_over_state = if_else(prevailing_minwage > state_minwage, 1, 0),
         prevailing_minwage_diff = prevailing_minwage - lag(prevailing_minwage),
         minwage_lag1 = lag(prevailing_minwage),
         minwage_lag2 = lag(prevailing_minwage, 2),
         minwage_lag3 = lag(prevailing_minwage, 3)) |> 
  ungroup()

### City minimum wage data

city_minwage <- read_csv("Supplemental Data/city_minwages.csv") |> 
  rename(name = locality)

city_minwage <- city_minwage |> 
  left_join(acs_binded_cities |> 
              select(name, statecountycity_fips, statecounty_fips),
            by = "name") |> 
  mutate(state_fips = substr(statecounty_fips, 1, 2)) |> 
  select(name, state_fips, statecounty_fips, statecountycity_fips, everything()) |> 
  distinct() |> 
  rename(locality = name) |> 
  filter(state_fips != "NA")

# Reformatting to panel data

city_minwage_panel <- city_minwage |> 
  pivot_longer(cols = c(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, 
                        `2018`, `2019`, `2021`, `2022`, `2023`), 
               names_to = "year",
               values_to = "minwage")

# Adding city minwage data to ACS data

acs_binded_cities_minwage <- acs_binded_cities |> 
  mutate(year = as.numeric(year)) |> 
  left_join(city_minwage_panel |> 
              select(statecountycity_fips, year, minwage) |> 
              mutate(year = as.numeric(year)),
            by = c("statecountycity_fips", "year")) |> 
  rename(city_minwage = minwage)

# Adding state minwage data to cities missing proprietary minwage for cities

acs_binded_cities_minwage <- acs_binded_cities_minwage |> 
  left_join(state_minwage_panel |> 
              select(state_fips, year, minwage),
            by = c("state_fips", "year")) |> 
  rename(state_minwage = minwage) |> 
  mutate(minwage_2 = if_else(city_minwage > state_minwage, city_minwage, state_minwage),
         minwage_2 = if_else(is.na(minwage_2), state_minwage, minwage_2),
         prevailing_minwage = if_else(minwage_2 > state_minwage, minwage_2, state_minwage)) |> 
  select(-minwage_2)

# Calculating year-to-year difference and lagged minwage vars

city_acs_minwage <- acs_binded_cities_minwage |> 
  group_by(statecountycity_fips) |> 
  arrange(year) |> 
  mutate(minwage_over_fed = if_else(prevailing_minwage > 7.25, 1, 0),
         minwage_over_state = if_else(prevailing_minwage > state_minwage, 1, 0),
         prevailing_minwage_diff = prevailing_minwage - lag(prevailing_minwage),
         minwage_lag1 = lag(prevailing_minwage),
         minwage_lag2 = lag(prevailing_minwage, 2),
         minwage_lag3 = lag(prevailing_minwage, 3)) |> 
  ungroup()

### Removing uneccesary dataframes

rm(list = setdiff(ls(), c("county_acs_minwage", "city_acs_minwage")))

### Saving dataframes

save.image(here("Processed/city_county_acs_minwage.RData"))





