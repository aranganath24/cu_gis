# Script to Extract Census Data for Workshop

library(tidyverse)
library(tidycensus)
census_api_key("API KEY HERE")

# Variable List for 2015 ACS

acs5<-load_variables(2015, "acs5")
View(acs5)

# Define and name variables for census API call

my_vars<-c(total_pop="B01003_001",
           black_totalpop="B01001B_001",
           black_men_u5="B01001B_003",
           black_men_5to9="B01001B_004",
           black_men_10to14="B01001B_005",
           black_men_15to17="B01001B_006",
           black_women_u5="B01001B_018",
           black_women_5to9="B01001B_019",
           black_women_10to14="B01001B_020",
           black_women_15to17="B01001B_021",
           totalpop_men_u5="B01001_003",
           totalpop_men_5to9="B01001_004",
           totalpop_men_10to14="B01001_005",
           totalpop_men_15to17="B01001_006",
           totalpop_women_u5="B01001_027",
           totalpop_women_5to9="B01001_028",
           totalpop_women_10to14="B01001_029",
           totalpop_women_15to17="B01001_030")

# Issue Call to Census API 
co_counties_race<-get_acs(
  geography="county", 
  variables=my_vars,
  state="CO",
  survey="acs5",
  output="wide",
  year=2015,
  geometry=FALSE)

# remove county from name field
co_counties_race<-co_counties_race %>% separate(col=NAME, c("County", "x"), sep=",") %>% 
  select(-x)

# List variables
names(co_counties_race)

# Create new variables 

# Create variable for total over 17 population
co_counties_race<-co_counties_race %>% mutate(total_pop_over17=total_popE-totalpop_men_u5E-totalpop_men_5to9E-
                                                totalpop_men_10to14E-totalpop_men_15to17E-totalpop_women_u5E-
                                                totalpop_women_5to9E-totalpop_women_10to14E-totalpop_women_15to17E)

# Create variable for total over-17 black population
co_counties_race<-co_counties_race %>% mutate(total_black_pop_over17=black_totalpopE-black_men_u5E-black_men_5to9E-
                                                black_men_10to14E-black_men_15to17E-black_women_u5E-black_women_5to9E-
                                                black_women_10to14E-black_women_15to17E)
# Select relevant variables
co_counties_race<-co_counties_race %>% select(GEOID, County, total_popE, total_black_pop_over17, 
                                              total_pop_over17)


# Write out data as CSV
write_csv(co_counties_race, "co_county_census.csv")




