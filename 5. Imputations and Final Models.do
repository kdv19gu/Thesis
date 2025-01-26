*****************
*** 5. Data Imputation and Final Models - County
*****************

clear
set more off

use "county_acs_adj_minwage.dta"

***** Inputing missing data for race proportions

*** Imputing values for prop_white 

mi set wide

mi register imp prop_white

mi impute pmm prop_white gini r_lowest_quintile r_second_quintile r_third_quintile r_fourth_quintile r_highest_quintile r_top_five_percent prop_less_than_hs prop_hs_grad prop_some_college_associates prop_bachelors_higher prop_18_to_24 prop_24_to_34 prop_35_to_44 prop_45_to_64 prop_65_years_older labor_force_part_rate unemployment_rate r_median_housing_costs prop_foreign_born prop_foreign_born_undocumented pop_density state_minwage r_prevailing_minwage cpi, add(20) knn(3) force

mi estimate: reg prop_white gini r_lowest_quintile r_second_quintile r_third_quintile r_fourth_quintile r_highest_quintile r_top_five_percent prop_less_than_hs prop_hs_grad prop_some_college_associates prop_bachelors_higher prop_18_to_24 prop_24_to_34 prop_35_to_44 prop_45_to_64 prop_65_years_older labor_force_part_rate unemployment_rate r_median_housing_costs prop_foreign_born prop_foreign_born_undocumented pop_density state_minwage r_prevailing_minwage cpi

mi xeq: correlate prop_white gini r_lowest_quintile r_second_quintile r_third_quintile r_fourth_quintile r_highest_quintile r_top_five_percent prop_less_than_hs prop_hs_grad prop_some_college_associates prop_bachelors_higher prop_18_to_24 prop_24_to_34 prop_35_to_44 prop_45_to_64 prop_65_years_older labor_force_part_rate unemployment_rate r_median_housing_costs prop_foreign_born prop_foreign_born_undocumented pop_density state_minwage r_prevailing_minwage cpi

* Final imputation model after considering regression coefficients and correlation

mi impute pmm prop_white gini r_lowest_quintile r_highest_quintile r_top_five_percent prop_24_to_34 prop_35_to_44 prop_45_to_64 prop_65_years_older unemployment_rate r_median_housing_costs prop_foreign_born prop_foreign_born_undocumented pop_density state_minwage cpi, add(20) knn (3) force

* Replacing prop_white with a column imputed

replace prop_white = _1_prop_white if prop_white == .

*** Imputing values for prop_black

mi update

mi set wide

mi register imp prop_black

mi impute pmm prop_black gini r_lowest_quintile r_second_quintile r_third_quintile r_fourth_quintile r_highest_quintile r_top_five_percent prop_less_than_hs prop_hs_grad prop_some_college_associates prop_bachelors_higher prop_18_to_24 prop_24_to_34 prop_35_to_44 prop_45_to_64 prop_65_years_older labor_force_part_rate unemployment_rate r_median_housing_costs prop_foreign_born prop_foreign_born_undocumented pop_density state_minwage r_prevailing_minwage cpi, add(20) knn(3) force

mi estimate: reg prop_black gini r_lowest_quintile r_second_quintile r_third_quintile r_fourth_quintile r_highest_quintile r_top_five_percent prop_less_than_hs prop_hs_grad prop_some_college_associates prop_bachelors_higher prop_18_to_24 prop_24_to_34 prop_35_to_44 prop_45_to_64 prop_65_years_older labor_force_part_rate unemployment_rate r_median_housing_costs prop_foreign_born prop_foreign_born_undocumented pop_density state_minwage r_prevailing_minwage cpi

mi xeq: correlate prop_black gini r_lowest_quintile r_second_quintile r_fourth_quintile r_highest_quintile r_top_five_percent prop_18_to_24 prop_24_to_34 prop_35_to_44 prop_45_to_64 prop_65_years_older labor_force_part_rate unemployment_rate r_median_housing_costs prop_foreign_born prop_foreign_born_undocumented pop_density state_minwage r_prevailing_minwage cpi

* Final imputation model after considering regression coefficients and correlation

mi impute pmm prop_black gini r_lowest_quintile r_second_quintile r_fourth_quintile prop_24_to_34 prop_65_years_older unemployment_rate prop_foreign_born_undocumented pop_density state_minwage r_prevailing_minwage, add(20) knn(3) force

replace prop_black = _1_prop_black if prop_black == . 

drop _*
