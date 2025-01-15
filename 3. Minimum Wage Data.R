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

counties_acs_minwage <- acs_binded_counties |> 
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

counties_acs_minwage <- counties_acs_minwage |> 
  group_by(statecounty_fips) |> 
  arrange(year) |> 
  mutate(minwage_over_fed = if_else(prevailing_minwage > 7.25, 1, 0),
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

# Calculating year-to-year difference and lagged minwage vars

# think about doing these modifications once the entire panel dataset is completed........

state_minwage_panel <- state_minwage_panel |> 
  group_by(statefips) |> 
  arrange(year) |> 
  mutate(minwage_over_fed = ifelse(minwage > 7.25, 1, 0),
         # minwage_yearly_difference = minwage - lag(minwage), # add this one later
         # minwage_change_indicator = ifelse(minwage_yearly_difference > 0, 1, 0), # add this one later
         minwage_lag1 = lag(minwage),
         minwage_lag2 = lag(minwage, 2),
         minwage_lag3 = lag(minwage, 3)) |> 
  ungroup()



























### Add later

rename(statecounty_fips = geoid) |> 
  mutate(statecounty_fips = sub("^0+", "", statecounty_fips),
         statecounty_fips = as.numeric(statecounty_fips))

acs_binded_counties |> 
  ggplot() +
  geom_point(mapping = aes(x = year, y = prop_foreign_born_undocumented))

#

mutate(statecountycity_fips = sub("^0+", "", statecountycity_fips),
       statecounty_fips = sub("^0+", "", statecounty_fips),
       statecountycity_fips = as.numeric(statecountycity_fips),
       statecounty_fips = as.numeric(statecounty_fips))

acs_binded_cities_filtered <- acs_binded_cities |> 
  filter(name %in% municipalities)