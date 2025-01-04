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
  select(!`2020`)

# Formatting state min wage data to panel data

state_minwage_panel <- state_minwage |> 
  pivot_longer(cols = c(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, 
                        `2018`, `2019`, `2021`, `2022`, `2023`), 
               names_to = "year",
               values_to = "minwage")

### County minimum wage data

### City minimum wage data

city_minwage <- read_csv("Supplemental Data/city_minwages.csv")

# Filling in NA data to county min wage

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