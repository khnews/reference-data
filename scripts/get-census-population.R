# Get Census population, poverty rates etc

library(censusapi)
library(dplyr)
library(tidyr)
library(stringr)
library(vroom)

fips_states <- read.csv("geography-codes/fips-states.csv", 
												colClasses = "character")
fips_counties <- read.csv("geography-codes/fips-counties.csv", 
												colClasses = "character", encoding = "UTF8")

###########################################################################
# Latest estimates not available via API
###########################################################################
pop_2020s_full <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/co-est2022-alldata.csv",
															 colClasses = c("STATE" = "character", "COUNTY" = "character"))

# Counties
county_latest <- pop_2020s_full %>% select(STATE, COUNTY, CTYNAME, starts_with("POPESTIMATE")) %>%
	mutate(fips_county = paste0(STATE, COUNTY)) %>%
	filter(COUNTY != "000") %>%
	select(fips_county, starts_with("POPESTIMATE")) %>%
	pivot_longer(cols = starts_with("POPESTIMATE"), names_to = "year", values_to = "population") %>% 
	mutate(year = as.numeric(str_replace_all(year, "POPESTIMATE", "")))

# State version
state_latest <- pop_2020s_full %>% select(STATE, COUNTY, starts_with("POPESTIMATE")) %>%
	filter(COUNTY == "000") %>%
	select(fips_state = STATE, starts_with("POPESTIMATE")) %>%
	pivot_longer(cols = starts_with("POPESTIMATE"), names_to = "year", values_to = "population") %>%
	mutate(year = as.numeric(str_replace_all(year, "POPESTIMATE", "")))

###########################################################################
# State population via API
# Variable and endpoint names aren't totally consistent across vintages
###########################################################################
# Latest years
population_vars <- listCensusMetadata(name = "pep/population", vintage = 2019)
state_pop_2010 <- getCensus(
	name = "pep/population",
	vintage = 2019,
	vars = c("POP", "DATE_CODE", "DATE_DESC"),
	region = "state:*")

state_pop_2010 <- state_pop_2010 %>% 
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

state_pop_2000 <- state_pop_2000 %>%
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

# 1980s
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

# 1970s
state_pop_1970_a <- read.table("data-original/population/st7080ts.txt",
															 skip = 14, nrows = 51, header = F, stringsAsFactors = F)

state_pop_1970_b <- read.table("data-original/population/st7080ts.txt",
															 skip = 67, nrows = 51, header = F, stringsAsFactors = F)
colnames(state_pop_1970_a) <- c("fips_state", "state_code", "4/70", "7/71", "7/72", "7/73", "7/74", "7/75")
colnames(state_pop_1970_b) <- c("fips_state", "state_code", "7/76", "7/77", "7/78", "7/79", "4/80cen")

# Join
state_pop_1970 <- left_join(state_pop_1970_a, state_pop_1970_b, by = c("state_code", "fips_state"))
state_pop_1970 <- state_pop_1970 %>% gather(year, population, -state_code, -fips_state) %>%
	filter(year != "4/80cen") %>%
	mutate(year = 1900 + as.numeric(str_replace_all(year, "7/|4/|cen", ""))) %>%
	filter(state_code != "US") %>%
	select(-state_code) %>%
	mutate(fips_state = sprintf("%02s", fips_state))

###########################################################################
# Join years
###########################################################################
state_population <- bind_rows(state_pop_1970, state_pop_1980, state_pop_1990, state_pop_2000, state_pop_2010, state_latest)
state_population <- left_join(state_population, fips_states, by = "fips_state")
state_population <- state_population %>% select(year, fips_state, state_code, state_name, population) %>%
	arrange(fips_state, year)

table(state_population$year)

write.csv(state_population, "population/state-population.csv", row.names = F, na ="")


###########################################################################
# County population via API
###########################################################################
county_pop_2010 <- getCensus(
	name = "pep/population",
	vintage = 2019,
	vars = c("POP", "DATE_CODE", "DATE_DESC"),
	region = "county:*")

county_pop_2010 <- county_pop_2010 %>% 
	mutate(population = as.numeric(POP),
				 DATE_CODE = as.numeric(DATE_CODE)) %>%
	# Use July estimates not decennial Census
	filter(DATE_CODE > 2) %>% 
	mutate(year = as.numeric(str_replace_all(DATE_DESC, "7/1/| population estimate", ""))) %>%
	mutate(fips_county = paste0(state, county)) %>%
	select(fips_county, year, population) %>%
	arrange(fips_county, year)

# 2000-2010 intercensals
county_pop_2000 <- getCensus(
	name = "pep/int_population",
	vintage = 2000,
	vars = c("POP", "DATE_DESC", "DATE_"),
	region = "county:*")

county_pop_2000 <- county_pop_2000 %>%
	mutate(population = as.numeric(POP),
				 DATE_ = as.numeric(DATE_)) %>%
	# Use July estimates not decennial Census
	filter(DATE_ >1 & DATE_ < 12) %>% 
	mutate(year = as.numeric(str_replace_all(DATE_DESC, "7/1/| population estimate", ""))) %>%
	mutate(fips_county = paste0(state, county)) %>%
	select(fips_county, year, population) %>%
	arrange(fips_county, year)

###########################################################################
# County pops pre-2000s from txt files
# 1990s https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-1990-2000-state-and-county-characteristics.html
# 1980s https://www.census.gov/data/datasets/time-series/demo/popest/1980s-county.html
# 1970s https://www.census.gov/data/datasets/time-series/demo/popest/1970s-county.html
###########################################################################

# Header described in a separate txt file yargh
pop_cols <- c("year", "fips_county", "group", "age1", "age2", "age3", "age4", "age5",
															 "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", "age15",
															 "age16", "age17", "age18")

county_pop_1970 <- read.csv("data-original/population/co-asr-7079.csv", stringsAsFactors = F, header = F)
colnames(county_pop_1970) <- pop_cols
county_pop_1970$group <- as.character(county_pop_1970$group)

county_pop_1980 <- read.csv("data-original/population/pe-02.csv", stringsAsFactors = F, header = F, skip = 7)
colnames(county_pop_1980) <- pop_cols

county_pop_older <- bind_rows(county_pop_1970, county_pop_1980)

county_pop_older  <- county_pop_older %>%
	mutate(population = select(., starts_with("age")) %>% rowSums(na.rm = TRUE)) %>%
	group_by(year, fips_county) %>%
	summarize(population = sum(population)) %>%
	mutate(fips_county = sprintf("%05s", fips_county))

# 1990 files are longer not the same layout, 1 file per year
files_1990 <- fs::dir_ls("data-original/population/1990-counties/", glob = "*txt")
files_1990
county_pop_1990 <- vroom_fwf(files_1990, trim_ws = T, 
														 col_positions = fwf_widths(c(2, 7, 3, 2, 2, 7)))
head(county_pop_1990)
colnames(county_pop_1990) <- c("year", "fips_county", "group_age", "group_race_sex", "group_ethnicity", "population")

county_pop_1990 <- county_pop_1990 %>%
	group_by(year, fips_county) %>%
	summarize(population = sum(population)) %>%
	ungroup() %>%
	mutate(year = year + 1900) %>%
	arrange(year, fips_county)

###########################################################################
# Join years
###########################################################################
county_population <- bind_rows(county_pop_older, county_pop_1990, county_pop_2000, county_pop_2010, county_latest)
county_population <- left_join(county_population, fips_counties, by = "fips_county")
county_population <- county_population %>% arrange(fips_county, year) %>%
	select(year, fips_county, state_code, county_name, population)

county_population <- as.data.frame(county_population)

write.csv(county_population, "population/county-population.csv", row.names = F, na ="")

###########################################################################
# NHGIS from decennial Censuses
###########################################################################
nhgis <- read.csv("data-original/population/nhgis0001_csv/nhgis0001_ts_nominal_county.csv", stringsAsFactors = F,
									colClasses = c("STATEFP" = "character","STATENH" = "character",
																 "COUNTYFP" = "character","COUNTYNH" = "character"))
