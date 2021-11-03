library(tidyverse)
library(sf)
library(tmap)

# Load and View Data
co_stops<-read_csv("/Users/adra7980/Documents/CU_workshops/gis/co_statewide_2020_04_01.csv")
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

bias<-county_shapefile_joined$excess_stops_index>0

county_shapefile_joined<-cbind(county_shapefile_joined, bias)
        
tm_shape(county_shapefile_joined)+
  tm_polygons(col="bias", style="cat", pal=c("white", "skyblue"))

tm_shape(county_shapefile_joined)+
  tm_polygons(col="bias", pal=c("white", "skyblue"))

# Removes legend title 
tm_shape(county_shapefile_joined)+
  tm_polygons(col="bias", title="", pal=c("white", "skyblue"))

# change legend order 

bias2<-county_shapefile_joined$excess_stops_index>0

bias2<-factor(bia

county_shapefile_joined<-factor(county_shapefile_joined$bias, levels=c("TRUE", "FALSE", "NA"))
st_as_sf(county_shapefile_joined)




