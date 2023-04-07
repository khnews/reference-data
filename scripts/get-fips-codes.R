# Make master county fips code file, including legacy codes from past years
# Source: Census Gazetteer files
# https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.2019.html

library(dplyr)
library(tidyr)
library(stringr)

###########################################################################
# Read in txt files
###########################################################################
# 2022 file - Connecticut change from counties to planning region
gaz_2022 <- read.delim("data-original/fips/2022_Gaz_counties_national.txt", 
											 sep = "\t", stringsAsFactors = F,
											 colClasses = c("GEOID" = "character"),
											 encoding = "UTF8")

fips_2022 <- gaz_2022 %>% select(
	fips_county = GEOID,
	state_code = USPS,
	county_name = NAME) %>%
	mutate(fips_county = str_trim(fips_county))

# 2019 file
gaz_2019 <- read.delim("data-original/fips/2019_Gaz_counties_national.txt", 
											 sep = "\t", stringsAsFactors = F,
											 colClasses = c("GEOID" = "character"),
											 encoding = "UTF8")

fips_2019 <- gaz_2019 %>% select(
	fips_county = GEOID,
	state_code = USPS,
	county_name = NAME) %>%
	mutate(fips_county = str_trim(fips_county))

gaz_2010 <- read.delim("data-original/fips/2010-Gaz_counties_national.txt", 
												sep = "\t", stringsAsFactors = F,
												colClasses = c("GEOID" = "character"),
											 encoding = "UTF8")
fips_2010 <- gaz_2010 %>% select(
	fips_county = GEOID,
	state_code = USPS,
	county_name = NAME) %>%
	mutate(fips_county = str_trim(fips_county))

# 2000 and 1990 formatting is garbage, opened in Excel to export to delimited
gaz_2000 <- read.csv("data-original/fips/2000-county2k.csv", 
										 header = F, colClasses = "character",
										 encoding = "UTF8")
fips_2000 <- gaz_2000 %>% select(
	fips_county = V2,
	state_code = V1,
	county_name = V3) %>%
	mutate(fips_county = str_trim(fips_county))

gaz_1990 <- read.csv("data-original/fips/1990-counties.csv", 
										 header = F, colClasses = "character",
										 encoding = "UTF8")
gaz_1990 <- gaz_1990 %>% mutate(fips_county = paste0(V1, V2))
fips_1990 <- gaz_1990 %>% select(
	fips_county,
	state_code = V4,
	county_name = V3) %>%
	mutate(fips_county = str_trim(fips_county))

###########################################################################
# Add in older counties that aren't present anymore
# Someday add in 1970s additions
###########################################################################
fips_2019_add <- fips_2019 %>% filter(!(fips_county %in% fips_2022$fips_county))

fips_2010_add <- fips_2010 %>% filter(!(fips_county %in% fips_2022$fips_county) & 
																				!fips_county %in% fips_2019_add$fips_county)

fips_2000_add <- fips_2000 %>% filter(!(fips_county %in% fips_2022$fips_county) & 
																				!fips_county %in% fips_2019_add$fips_county & 
																				!fips_county %in% fips_2010_add$fips_county)

fips_1990_add <- fips_1990 %>% filter(!(fips_county %in% fips_2022$fips_county) &
																				!fips_county %in% fips_2019_add$fips_county & 
																				!fips_county %in% fips_2010_add$fips_county &
																				!fips_county %in% fips_2000_add$fips_county)

fips_add <- bind_rows(fips_1990_add, fips_2000_add, fips_2010_add, fips_2019_add)

# Full dataset
fips_counties <- bind_rows(fips_add, fips_2022) %>%
	select(fips_county, state_code, county_name) %>%
	arrange(fips_county)

write.csv(fips_counties, "geography-codes/fips-counties.csv", row.names = F, na ="")
