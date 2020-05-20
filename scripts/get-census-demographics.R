# Get Census population, poverty rates etc

library(censusapi)
library(dplyr)
library(tidyr)
library(stringr)

fips_states <- read.csv("geography-codes/fips-states.csv", stringsAsFactors = F, colClasses = "character")

###########################################################################
# State population via API
# Variable and endpoint names aren't totally consistent across vintages
###########################################################################
# Latest years
population_vars <- listCensusMetadata(name = "pep/population", vintage = 2019)
state_pop <- getCensus(
	name = "pep/population",
	vintage = 2019,
	vars = c("POP", "DATE_CODE", "DATE_DESC"),
	region = "state:*")

state_pop <- state_pop %>% 
	mutate(population = as.numeric(POP),
				 DATE_CODE = as.numeric(DATE_CODE)) %>%
	# Use July estimates not decennial Census
	filter(DATE_CODE > 2) %>% 
	mutate(year = as.numeric(str_replace_all(DATE_DESC, "7/1/| population estimate", ""))) %>%
	select(fips_state = state, year, population) %>%
	arrange(fips_state, year)

# 2000-2010 intercensals
state_pop_2000 <- getCensus(
	name = "pep/int_population",
	vintage = 2000,
	vars = c("POP", "DATE_DESC", "DATE_"),
	region = "state:*")

state_pop_2000 <- state_population_2000 %>%
	mutate(population = as.numeric(POP),
				 DATE_ = as.numeric(DATE_)) %>%
	# Use July estimates not decennial Census
	filter(DATE_ >1 & DATE_ < 12) %>% 
	mutate(year = as.numeric(str_replace_all(DATE_DESC, "7/1/| population estimate", ""))) %>%
	select(fips_state = state, year, population) %>%
	arrange(fips_state, year)

###########################################################################
# 1990-2000 Intercensals totals aren't in data.census.gov, only county-level with demographics in API
# Scraped PDF from:
# https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/co-est2001-12-00.pdf
###########################################################################
state_pop_1990 <- read.csv("data-original/population/tabula-co-est2001-12-00.csv", stringsAsFactors = F)
state_pop_1990 <- state_pop_1990 %>% gather(year, population, -Geography) %>%
	rename(state_name = Geography) %>%
	filter(!(year %in% c("April.1..1990", "April.1..2000.Census"))) %>%
	mutate(year = as.numeric(str_replace_all(year, "July.1..", ""))) %>%
	arrange(state_name, year) %>%
	mutate(population = as.numeric(str_replace_all(population, ",", ""))) %>%
	filter(state_name != "USA")

# Add fips codes
state_pop_1990 <- left_join(state_pop_1990, fips_states, by = "state_name") %>%
	select(-state_code, -state_name)

###########################################################################
# 1900-1990 via 
# https://www.census.gov/data/tables/time-series/demo/popest/pre-1980-state.html
###########################################################################

state_pop_1980_a <- read.table("data-original/population/st8090ts.txt",
									 skip = 10, nrows = 52, header = F, stringsAsFactors = F)

state_pop_1980_b <- read.table("data-original/population/st8090ts.txt",
												 skip = 69, nrows = 52, header = F, stringsAsFactors = F)
colnames(state_pop_1980_a) <- c("state_code", "4/80cen", "7/81", "7/82", "7/83", "7/84")
colnames(state_pop_1980_b) <- c("state_code", "7/85" ,"7/86", "7/87", "7/88", "7/89", "4/90cen")

# Join
state_pop_1980 <- left_join(state_pop_1980_a, state_pop_1980_b, by = "state_code")
state_pop_1980 <- state_pop_1980 %>% gather(year, population, -state_code) %>%
	filter(year != "4/90cen") %>%
	mutate(year = 1900 + as.numeric(str_replace_all(year, "7/|4/|cen", ""))) %>%
	filter(state_code != "US")

# Add fips codes
state_pop_1980 <- left_join(state_pop_1980, fips_states, by = "state_code") %>%
	select(-state_code, -state_name)

###########################################################################
# Join years
###########################################################################
state_population <- bind_rows(state_pop_1980, state_pop_1990, state_pop_2000, state_pop)
state_population <- left_join(state_population, fips_states, by = "fips_state")
state_population <- state_population %>% select(year, fips_state, state_code, state_name, population) %>%
	arrange(fips_state, year)

write.csv(state_population, "population/state-population.csv", row.names = F, na ="")
