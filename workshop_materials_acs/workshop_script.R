# Load libraries
library(tidyverse)
library(sf)
library(tmap)

# Set working directory
setwd("/Users/adra7980/Documents/CU_workshops/gis/workshop_materials/data")

# Load and View Traffic Stops Data
co_stops<-read_csv("co_statewide_2020_04_01.csv")
View(co_stops)

# Generate Year variable
co_stops<-co_stops %>% mutate(Year=substr(co_stops$date, 1,4))
View(co_stops)

# Create dataset of 2015 observations
co_stops_2015<-co_stops %>% filter(Year==2015)

# Compute county-level count of traffic stops by race

co_county_summary<-co_stops_2015 %>% 
                      group_by(county_name) %>% 
                      count(subject_race) 

View(co_county_summary)

# Reshape "co_county_summary" to make racial categories into separate fields
co_county_summary<-co_county_summary %>% 
                  spread(subject_race, n)

View(co_county_summary)

# Generate field containing information on total number of stops
co_county_summary<-co_county_summary %>% 
                        rowwise() %>% 
                        mutate(total_stops=sum(c_across(where(is.integer)), na.rm=TRUE)) 

View(co_county_summary)


# Make a new dataset based on "co_county_summary" that selects only the "county_name", 
# "black", and "total_stops" variables, then renames "black" as "black_stops" (to avoid 
#confusion with census measures), and then deletes the record in which "county_name" is "NA"

co_county_summary_black_stops<-co_county_summary %>% select(county_name, black, total_stops) %>% 
                                                     rename(black_stops=black) %>% 
                                                     filter(county_name!="NA")

View(co_county_summary_black_stops)


# Read in relevant county-level census data (2015 ACS)

co_counties_census<-read_csv("co_county_census.csv") 
View(co_counties_census)

# Join traffic stops dataset and census dataset
co_counties_census_trafficstops<-full_join(co_county_summary_black_stops, co_counties_census,
                                           by=c("county_name"="County"))


# Creates variables for black share of total stops, black share of over-17 population, and difference between
# black share of total stops and black share of over-17 population (if the difference is >0, it indicates excess
# relative to population share and therefore possible bias)

co_counties_census_trafficstops<-co_counties_census_trafficstops %>% 
                                  mutate(black_stop_pct=((black_stops/total_stops)*100),
                                         black_pop_pct=((total_black_pop_over17/total_pop_over17)*100),
                                         excess_stops_index=black_stop_pct-black_pop_pct)
                                


# Read in CO County Shapefile 
co_counties_shapefile<-st_read("tl_2019_08_county.shp")

# View shapefile metadata
co_counties_shapefile

# View shapefile attribute table
View(co_counties_shapefile) 

# Render shapefile

tm_shape(co_counties_shapefile)+
  tm_polygons()

# Assign shapefile to object

CO_counties_map<-tm_shape(co_counties_shapefile)+
                   tm_polygons()+
                   tm_layout(frame=FALSE)

CO_counties_map

# Join tabular dataset with census and traffic stops information ("co_counties_census_trafficstops" 
# to shapefile)

county_shapefile_joined<-full_join(co_counties_shapefile, co_counties_census_trafficstops, 
                                   by=c("NAMELSAD"="county_name"))


# Make preliminary map

traffic_stops_bias<-tm_shape(county_shapefile_joined)+
                      tm_polygons(col="excess_stops_index", 
                                  palette="YlOrRd", 
                                  textNA="No Data", 
                                  n=4, 
                                  style="jenks")+
                      tm_layout(frame=FALSE, 
                                legend.outside=TRUE)

traffic_stops_bias


# Map with custom breaks

my_colors<-c("white", "peachpuff", "red1", "red4")
traffic_stops_bias<-tm_shape(county_shapefile_joined)+
                      tm_polygons(col="excess_stops_index", 
                                  palette=my_colors, 
                                  textNA="No Data", 
                                  n=4, 
                                  breaks=c(-10,-5, 0, 2.5, 5))+
                      tm_layout(frame=FALSE, 
                                legend.outside=TRUE)
traffic_stops_bias

# Map with dichotomous categories

Labels<-c("No Apparent Bias", "Apparent Bias")
traffic_map_race_dichotomous<-tm_shape(county_shapefile_joined)+
                                tm_polygons(col="excess_stops_index", 
                                            title="", pal=c("white", "orangered1"), 
                                            labels=Labels, 
                                            breaks=c(-10, 0,10), 
                                            textNA="No Data")+
                              tm_layout(legend.outside=TRUE, 
                                        main.title="Disproportionate Traffice Stops of Black Drivers in Colorado, by County\n(Using Black Share of Over-17 County Population Share as Baseline) ",
                                        main.title.size=1,
                                        main.title.position="left",
                                        frame=FALSE, 
                                        legend.outside.position = "right", 
                                        attr.outside=TRUE)+
                                        tm_text("NAME", size=0.38)+
                              tm_credits("Map Author: Aditya Ranganath\nData Sources: 2015 ACS, Stanford Open Policing Project ",
                                          position=c(0,0),
                                          size=0.38)    

traffic_map_race_dichotomous

# View interactive map
tm_mode("view")
categorical_map






