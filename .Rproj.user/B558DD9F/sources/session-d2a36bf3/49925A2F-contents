# August 2025 analysis ----

#### Load Packages 
library(readr)
library(dplyr)

## EWorkers + SES Indicators ----
### Starting dataframe containing US Census tracts, essential worker information, and socio-demographic characteristics
#### Socio-demo info: NaNDA Essential Workers - ISR\freshish\ICPSR_38528-Socio_Demo\DS0005-16-20_ZCTA
# One value for each year. 2016-2020 essential workers (average of ACS data over the time period)
# How to characterize the neighborhood context at a specific point in time
# Environment at the time of the pandemic - 2020.


df <- read.csv("everything.csv", colClasses = c("TRACT_FIPS20" = "character"))
df_quintiles <- df %>%
  mutate(EWRKR_quintile = ntile(EWRKR_PROP, 5))

df <- df_quintiles

#### Pollution Sites ----

##### Questions for Philippa re: pollution sites
###### What is the best way to do data that is per year as opposed to the EW dataset that has one number per the 16-20 RANGE?

# 2020 - assumption of documentation of - error within 2019-2020


pollution_sites <- read_tsv("DS00030-Polluting_sites/38597-0003-Data.tsv")

pollution_sites <- pollution_sites %>%
  filter(YEAR == 2020)


pollution_sites %>%
  group_by(TRACT_FIPS20) %>%
  summarise(AVG_Facilities = mean(COUNT_TRI_FACILITIES)) %>%
  arrange(desc(AVG_Facilities))

head(pollution_sites)


#### Grocery Stores ----

grocery_stores <- read.csv("nanda_grocery_Tract20_1990-2021_01P.csv", colClasses = c("tract_fips20" = "character"))
grocery_stores <- grocery_stores %>%
  filter(year == 2020)

head(grocery_stores)

#### Health Care Services ----
# capture ambulatory health care services ("walk-in")
healthcare_services <- read.csv("healthcare_services.csv", colClasses = c("tract_fips20" = "character"))
healthcare_services <- healthcare_services %>%
  filter(year == 2020)

head(healthcare_services)

#### Parks ----

parks <- read_tsv("ICPSR_38586-Parks/DS0003/38586-0003-Data.tsv")
head(parks)

#### Urbanicity ----
##### Question: there seems to be only 2010 data. Can we use this (I feel like no as they won't match up)
# 2010 -- that 
# Population density - as a proxy for urban-rural 
# rural-urban consideration: look at 2010-urban rural dataset and link it up with the popden of 2010 and get 4 category RUCA codes)

#### Convenience Stores ----

convenience_stores <- read.csv("convenience_stores.csv", colClasses = c("tract_fips20" = "character"))
convenience_stores <- convenience_stores %>%
  filter(year == 2020)

#### Broadband Access ----

# broadband availability -- whether broadband is provided in that neighborhood
# we are after the provider that is providing these services

# internet access is number of people who are actually using it.


broadband_access <- read_tsv("ICPSR_38567-Broadband/DS0001/38567-0001-Data.tsv")
broadband_access <- broadband_access %>%
  filter(YEAR == 2020)

#### Dollar Stores ----
dollar_stores <- read.csv("dollar_stores.csv", colClasses = c("tract_fips20" = "character"))
dollar_stores <- dollar_stores %>%
  filter(year == 2020)

#### MERGE ----

df_merge <- df %>%
  left_join(pollution_sites, by = "TRACT_FIPS20") %>%
  left_join(grocery_stores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(healthcare_services, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(parks, by = "TRACT_FIPS20") %>%
  left_join(convenience_stores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  # left_join(broadband_access, by = "TRACT_FIPS20") %>%
  left_join(dollar_stores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  distinct(TRACT_FIPS20, .keep_all = TRUE)

# Variables currently in merged dataset
info <- data.frame(
  column = names(df_merge),
  class  = sapply(df_merge, class)
)

write.csv(info, "df_structure.csv", row.names = FALSE)


# Basic Descriptives
# in the sample: essential workers divided by quintiles
# Which quintile that census tract is in?

# for each quintile, do descriptive of each variable we talked about.
# Mean Median Stdev

