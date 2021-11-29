library(tidyverse)
library(tidycensus)
census_api_key("INSERT HERE")

# Variable list for 2010 Decennial
dec_2010_variables<-load_variables(2010, "sf1")
View(dec_2010_variables)

# Define and name variables for census API call

my_vars<-c(total_pop="P001001",
           totalpop_men_u5="P012003",
           totalpop_men_5to9="P012004",
           totalpop_men_10to14="P012005",
           totalpop_men_15to17="P012006",
           totalpop_women_u5="P012027",
           totalpop_women_5to9="P012028",
           totalpop_women_10to14="P012029",
           totalpop_women_15to17="P012030",
           black_totalpop="PCT012B001",
           black_men_u1="PCT012B003",
           black_men_1="PCT012B004",
           black_men_2="PCT012B005",
           black_men_3="PCT012B006",
           black_men_4="PCT012B007",
           black_men_5="PCT012B008",
           black_men_6="PCT012B009",
           black_men_7="PCT012B010",
           black_men_8="PCT012B011",
           black_men_9="PCT012B012",
           black_men_10="PCT012B013",
           black_men_11="PCT012B014",
           black_men_12="PCT012B015",
           black_men_13="PCT012B016",
           black_men_14="PCT012B017",
           black_men_15="PCT012B018",
           black_men_16="PCT012B019",
           black_men_17="PCT012B020",
           black_women_u1="PCT012B107",
           black_women_1="PCT012B108",
           black_women_2="PCT012B109",
           black_women_3="PCT012B110",
           black_women_4="PCT012B111",
           black_women_5="PCT012B112",
           black_women_6="PCT012B113",
           black_women_7="PCT012B114",
           black_women_8="PCT012B115",
           black_women_9="PCT012B116",
           black_women_10="PCT012B117",
           black_women_11="PCT012B118",
           black_women_12="PCT012B119",
           black_women_13="PCT012B120",
           black_women_14="PCT012B121",
           black_women_15="PCT012B122",
           black_women_16="PCT012B123",
           black_women_17="PCT012B124")
           
# Issue call to Census API
co_counties_race<-get_decennial(
  geography="county", 
  variables=my_vars,
  state="CO",
  survey="sf1",
  output="wide",
  year=2010,
  geometry=FALSE)           

# Remove state name from name field
co_counties_race<-co_counties_race %>% separate(col=NAME, c("County", "x"), sep=",") %>% 
                  select(-x)           


# List variables
names(co_counties_race)

# Create new variables

# Create variable for total over-17 population

co_counties_race<-co_counties_race %>% mutate(total_pop_over17=total_pop-totalpop_men_u5-totalpop_men_5to9-
                                                totalpop_men_10to14-totalpop_men_15to17-totalpop_women_u5-
                                                totalpop_women_5to9-totalpop_women_10to14-totalpop_women_15to17)

# Create variable for total over--17 black population

co_counties_race<-co_counties_race %>% mutate(total_black_pop_over17=black_totalpop-black_men_u1-black_men_1-
                                                black_men_2-black_men_3-black_men_4-black_men_5-black_men_6-
                                                black_men_7-black_men_8-black_men_9-black_men_10-black_men_11-
                                                black_men_12-black_men_13-black_men_14-black_men_15-black_men_16-
                                                black_men_17-black_women_u1-black_women_1-black_women_2-black_women_3-
                                                black_women_4-black_women_5-black_women_6-black_women_7-black_women_8-
                                                black_women_9-black_women_10-black_women_11-black_women_12-
                                                black_women_13-black_women_14-black_women_15-black_women_16-
                                                black_women_17)

# Select relevant variables
co_counties_race_final<-co_counties_race %>% select(GEOID, County, total_pop, total_black_pop_over17, 
                                              total_pop_over17)

           
           
           
           
           
           
           
           
           
           
           
           
           
           