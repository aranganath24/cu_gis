library(tidyverse)
library(sf)
library(tmap)

# Load and View Data
View(co_stops)

# Generate Year variable
co_stops<-co_stops %>% mutate(Year=substr(co_stops$date, 1,4))
View(co_stops)

# Create dataset of 2015 observations
co_stops_2015<-co_stops %>% filter(Year==2015)

# Count of stops and racial breakdown, by county

co_county_summary<-co_stops_2015 %>% 
                    group_by(county_name) %>% 
                    count(subject_race) %>% 
                    spread(subject_race, n) %>%
                    rowwise() %>% 
                    mutate(total_stops=sum(c_across(where(is.integer)), na.rm=TRUE)) 

View(co_county_summary)

# Select relevant observations, rename "black" to "black_stops" for clarity, and delete NA county
co_county_summary_black_stops<-co_county_summary %>% select(county_name, black, total_stops) %>% 
                                                     rename(black_stops=black) %>% 
                                                     filter(county_name!="NA")

# Read in relevant county-level census data (2015 ACS)
co_counties_census<-read_csv("co_county_census.csv") 
View(co_counties_census)

# Join traffic stops dataset and census dataset

co_counties_census_trafficstops<-full_join(co_county_summary_black_stops, co_counties_census,
                                           by=c("county_name"="County"))


co_counties_census_trafficstops<-co_counties_census_trafficstops %>% 
                                  mutate(black_stop_pct=((black_stops/total_stops)*100),
                                         black_pop_pct=((total_black_pop_over17/total_pop_over17)*100),
                                         excess_stops_index=black_stop_pct-black_pop_pct)

# Read in CO County Shapefile 
  
co_counties_shapefile<-st_read("/Users/adra7980/Documents/CU_workshops/gis/data/tl_2019_08_county/tl_2019_08_county.shp")

# Render Colorado Map
tm_shape(co_counties_shapefile)+
  tm_polygons()

tm_shape(co_counties_shapefile)+
  tm_polygons()+
  tm_layout(frame=FALSE)

# View attribute table
View(co_counties_shapefile)

# Join tabular dataset to shapefile

county_shapefile_joined<-full_join(co_counties_shapefile, co_counties_census_trafficstops, 
                                   by=c("NAMELSAD"="county_name"))


                                                                                             
# Make map

policing_map<-tm_shape(county_shapefile_joined)+
                tm_polygons(col="excess_stops_index", palette="BuGn", textNA="No Data")+
                  tm_layout(frame=FALSE, legend.outside=TRUE)



# Categorical variables map

# Make new categorical variable
county_shapefile_joined<-county_shapefile_joined %>% 
  mutate(apparent_bias=ifelse(excess_stops_index>0, "Apparent Bias", "No Apparent Bias"))

# Map based on categorical variable 

categorical_map<-tm_shape(county_shapefile_joined)+
                    tm_polygons(col="apparent_bias", title="", pal=c("orangered1", "white"), textNA="No Data")+
                    tm_layout(legend.outside=TRUE, 
                              main.title="Disproportionate Traffice Stops of Black Drivers in Colorado, by County\n(Using Black Share of Over-17 County Population Share as Baseline) ",
                              main.title.size=1,
                              main.title.position="left",
                              frame=FALSE, 
                              legend.outside.position = "right", 
                              attr.outside=TRUE)+
                    tm_text("NAME", size=0.38)+
                    tm_credits("Map Author: Aditya Ranganath\nData Sources: 2015 ACS, Stanford Open Policing Project ", # Sets text for map credits
                    position=c(0,0), # Specifies location of map credits
                    size=0.38)    

# switch to view mode
tmap_mode("view")

# view interactive map
categorical_map 

