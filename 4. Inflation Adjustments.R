library(tidyverse)
library(dplyr)
library(here)
library(haven)

### Loading city and county panel datasets with min wage

load(here("Processed/city_county_acs_minwage.RData"))

### Loading state regions

regions <- read_csv("Supplemental Data/states_regions.csv") |> 
  rename(state_fips = `State (FIPS)`,
         region = Name) |> 
  select(-Region)

### Loading CPIs by region

# Northeast

northeast_cpi <- read_csv("Supplemental Data/cpi_series_northeast.csv")

northeast_cpi <- northeast_cpi |> 
  select(Year, Annual) |> 
  mutate(region = "Northeast")

# South

south_cpi <- read_csv("Supplemental Data/cpi_series_south.csv")

south_cpi <- south_cpi |> 
  select(Year, Annual) |> 
  mutate(region = "South")

# Midwest

midwest_cpi <- read_csv("Supplemental Data/cpi_series_midwest.csv")

midwest_cpi <- midwest_cpi |> 
  select(Year, Annual) |> 
  mutate(region = "Midwest")

# West 

west_cpi <- read_csv("Supplemental Data/cpi_series_west.csv")

west_cpi <- west_cpi |> 
  select(Year, Annual) |> 
  mutate(region = "West")

# Combining all regions and CPI 

cpi <- rbind(northeast_cpi, south_cpi, midwest_cpi, west_cpi)

### Merging regions and CPI with state FIPS code

cpi_regions_states <- cpi |> 
  left_join(regions,
            by = "region") |> 
  rename(year = Year)

### Adjusting figures by inflation for county panel using 2010 as base

county_acs_adj_minwage <- county_acs_minwage |> 
  left_join(cpi_regions_states,
            by = c("state_fips", "year")) |> 
  rename(cpi = Annual) |> 
  mutate(r_prevailing_minwage = prevailing_minwage * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_lowest_quintile = lowest_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_second_quintile = second_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_third_quintile = third_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_fourth_quintile = fourth_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_highest_quintile = highest_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_top_five_percent = top_five_percent * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_median_housing_costs = median_housing_costs * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    p50_p10 = r_third_quintile - r_lowest_quintile,
    p90_p50 = r_highest_quintile - r_third_quintile,
    p90_p10 = r_highest_quintile - r_lowest_quintile) |> 
  group_by(statecounty_fips) |> 
  arrange(year) |> 
  mutate(r_prevailing_minwage_diff = r_prevailing_minwage - lag(r_prevailing_minwage),
         r_minwage_lag1 = lag(r_prevailing_minwage),
         r_minwage_lag2 = lag(r_prevailing_minwage, 2),
         r_minwage_lag3 = lag(r_prevailing_minwage, 3),
         minwage_change_indicator = case_when(
           prevailing_minwage_diff == NA ~ 0,
           prevailing_minwage_diff == 0 ~ 0,
           prevailing_minwage_diff > 0 ~ 1),
         minwage_change_interaction = minwage_change_indicator * prevailing_minwage_diff) |> 
  ungroup()
  
### Adjusting figures by inflation for city panel using 2010 as base

city_acs_adj_minwage <- city_acs_minwage |> 
  left_join(cpi_regions_states,
            by = c("state_fips", "year")) |> 
  rename(cpi = Annual) |> 
  mutate(r_prevailing_minwage = prevailing_minwage * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_lowest_quintile = lowest_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_second_quintile = second_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_third_quintile = third_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_fourth_quintile = fourth_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_highest_quintile = highest_quintile * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_top_five_percent = top_five_percent * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    r_median_housing_costs = median_housing_costs * (case_when(
      region == "South" ~ 211.338,
      region == "Northeast" ~ 233.868,
      region == "West" ~ 221.202,
      region == "Midwest" ~ 208.046) / cpi),
    p50_p10 = r_third_quintile - r_lowest_quintile,
    p90_p50 = r_highest_quintile - r_third_quintile,
    p90_p10 = r_highest_quintile - r_lowest_quintile) |> 
  group_by(statecounty_fips) |> 
  arrange(year) |> 
  mutate(r_prevailing_minwage_diff = r_prevailing_minwage - lag(r_prevailing_minwage),
         r_minwage_lag1 = lag(r_prevailing_minwage),
         r_minwage_lag2 = lag(r_prevailing_minwage, 2),
         r_minwage_lag3 = lag(r_prevailing_minwage, 3),
         minwage_change_indicator = case_when(
           prevailing_minwage_diff == NA ~ 0,
           prevailing_minwage_diff == 0 ~ 0,
           prevailing_minwage_diff > 0 ~ 1),
         minwage_change_interaction = minwage_change_indicator * prevailing_minwage_diff) |> 
  ungroup()

### Removing unnecessary dataframes

rm(list = setdiff(ls(), c("county_acs_adj_minwage", "city_acs_adj_minwage")))

### Final adjustments

# County

county_acs_adj_minwage <- county_acs_adj_minwage |> 
  mutate(state_fips = as.numeric(state_fips),
         statecounty_fips = as.numeric(statecounty_fips)) |> 
  filter(state_fips != 72) # Removing Puerto Rico

county_acs_adj_minwage %>%
  select(-c(minwage_lag1, minwage_lag2, minwage_lag3, 
            r_minwage_lag1, r_minwage_lag2, r_minwage_lag3)) %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_count_{.col}")) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0)

# City

city_acs_adj_minwage <- city_acs_adj_minwage |> 
  mutate(state_fips = as.numeric(state_fips),
         statecounty_fips = as.numeric(statecounty_fips),
         statecountycity_fips = as.numeric(statecountycity_fips)) |> 
  filter(state_fips != 72) # Removing Puerto Rico

city_acs_adj_minwage %>%
  select(-c(minwage_lag1, minwage_lag2, minwage_lag3, 
            r_minwage_lag1, r_minwage_lag2, r_minwage_lag3)) %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_count_{.col}")) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) |> 
  view()

### Saving as STATA files

county_acs_adj_minwage |> 
  write_dta("STATA Files/county_acs_adj_minwage.dta")

city_acs_adj_minwage |> 
  write_dta("STATA Files/city_acs_adj_minwage.dta")
