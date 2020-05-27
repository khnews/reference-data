
library(censusapi)
library(dplyr)

###########################################################################
# Poverty rates and counts with MOE by state
###########################################################################
poverty_state <- getCensus(
	name = "timeseries/poverty/saipe",
	vars = c("GEOID", "STABREV", "NAME", 
					 # Poverty rate, all ages
					 "SAEPOVRTALL_PT", "SAEPOVRTALL_MOE", 
					 # Poverty count, all ages 
					 "SAEPOVALL_PT", "SAEPOVALL_MOE",
					 # Poverty universe
					 "SAEPOVU_ALL",
					 # Median ousehold income, all ages
					 "SAEMHI_PT", "SAEMHI_MOE"),
	region = "state:*",
	time = "from 1989 to 2019")

head(poverty_state)

# Clean up
poverty_state <- poverty_state %>%
	rename(year = time, fips_state = state, state_code = STABREV, state_name = NAME,
				 poverty_rate = SAEPOVRTALL_PT, poverty_rate_moe = SAEPOVRTALL_MOE,
				 poverty_count = SAEPOVALL_PT, poverty_count_moe = SAEPOVALL_MOE,
				 poverty_universe = SAEPOVU_ALL,
				 median_hh_income = SAEMHI_PT, median_hh_income_moe = SAEMHI_MOE) %>%
	mutate_at(vars(starts_with("poverty") | starts_with("median")), as.numeric) %>%
	select(-GEOID) %>%
	arrange(fips_state, year)

write.csv(poverty_state, "population/state-poverty.csv", row.names = F, na ="")

###########################################################################
# Poverty rates and counts with MOE by county
###########################################################################
poverty_county <- getCensus(
	name = "timeseries/poverty/saipe",
	vars = c("GEOID", "STABREV", "NAME", 
					 # Poverty rate, all ages
					 "SAEPOVRTALL_PT", "SAEPOVRTALL_MOE", 
					 # Poverty count, all ages 
					 "SAEPOVALL_PT", "SAEPOVALL_MOE",
					 # Poverty universe
					 "SAEPOVU_ALL",
					 # Median ousehold income, all ages
					 "SAEMHI_PT", "SAEMHI_MOE"),
	region = "county:*",
	time = "from 1989 to 2019")

head(poverty_county)

# Clean up
poverty_county <- poverty_county %>% mutate(fips_county = paste0(state, county)) %>%
	rename(year = time, state_code = STABREV, state_name = NAME,
				 poverty_rate = SAEPOVRTALL_PT, poverty_rate_moe = SAEPOVRTALL_MOE,
				 poverty_count = SAEPOVALL_PT, poverty_count_moe = SAEPOVALL_MOE,
				 poverty_universe = SAEPOVU_ALL,
				 median_hh_income = SAEMHI_PT, median_hh_income_moe = SAEMHI_MOE) %>%
	mutate_at(vars(starts_with("poverty") | starts_with("median")), as.numeric) %>%
	select(-GEOID, -state) %>%
	select(year, fips_county, everything()) %>%
	arrange(fips_county, year)

# Note: 1996 is missing for all but DC
write.csv(poverty_county, "population/county-poverty.csv", row.names = F, na ="")
