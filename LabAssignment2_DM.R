# Author: Deidre Mensah
# Date: 05/02/2020

library(sf); library(rgdal); library(sp); require(magrittr)
library(raster); library(dplyr); library(rgeos); library(ggplot2); library(tidyverse)

#read in csv
covid_4_19 <- read.csv("tests-by-zcta_2020_04_19.csv")
covid_4_12 <- read.csv("tests-by-zcta_2020_04_12.csv")

#read in shapefile annd then converts it to sf object.
zipsf<- sf::st_read("ZIP_CODE_040114.shp") #sf class

#read in census tract shapefile
census_tracts <- sf::st_read("geo_export_1dc7b645-647b-4806-b9a0-7b79660f120a.shp") #it's now an sf object. no worries

#1) - Join COIVD data to NYC zipcodes data
zip_covid_merged <- base::merge(zipsf, covid_4_19, by.x = "ZIPCODE", by.y = "MODZCTA")
names(zip_covid_merged)

#2) - Filter food retail and aggregate by zipcode
#read in shapfile
food_stores <- sf::st_read("nycFoodStore.shp")

#filter for only food retail
food_retail <- food_stores %>% filter(Estbl_T %in% 'A')

#check CRS for food retail
st_crs(food_retail)
st_crs(zip_covid_merged)

#transform the zipcodes to match crs of food retail
zipcode_trans <- sf::st_transform(zip_covid_merged, st_crs(food_retail))
st_crs(zipcode_trans)

#aggregates retail food by zipcode
zip_food <- zipcode_trans %>% 
  #spatial join
  st_join(food_retail) %>%
  #column to be aggregated by
  group_by(ZIPCODE) %>% 
  #creats new column with summarized data
  summarize(Stores=n())

#3 - Aggregrate NYC health facilities to zip code data
#reads csv into data frame
nyc_health_df <- read.csv("NYS_Health_Facility.csv")

#pipe data to omit NA features
nyc_health_df <-nyc_health_df %>% 
  filter(!is.na(Facility.Latitude))

#converts data frame to sf object
nyc_health_df <- st_as_sf(nyc_health_df, coords=c("Facility.Longitude", "Facility.Latitude")) 

#check class of health facilty dataset - it was a dataframe before. I tried piping it to get an sf object in the previous statement, but it didn't work for some reason
class(nyc_health_df)

#check coordinate systeme of health facilities now that it's an sf object
st_crs(nyc_health_df)

#assign WGS84 projection - note: you can't transform for an sf object that's missing a coordinate system entirely
st_crs(nyc_health_df) <- 4326

#check coordinate sytem again to be safe
st_crs(nyc_health_df)

#aggregates facilties by zipcode
zip_health <- zipcode_trans %>% 
  st_join(nyc_health_df) %>%
  group_by(ZIPCODE) %>% 
  summarize(Facilties =n())

#4 - Join ACS census data to NYC Planning Data
#read in csv
acs_data <- read_csv("ACSDP5Y2018.DP05_data_with_overlays_2020-04-22T132935.csv")
#select main demographic columns and ID column
acs_data_clean <- acs_data %>%
select(GEO_ID,totPop = DP05_0001E, elderlyPop = DP05_0024E,
       malePop = DP05_0002E, femalePop = DP05_0003E,  
       whitePop = DP05_0037E, blackPop = DP05_0038E,
       asianPop = DP05_0067E, hispanicPop = DP05_0071E,
       adultPop = DP05_0021E, citizenAdult = DP05_0087E) %>%  
  #create new column and select the last 9 characters of the field so you can properly join it to the census tract data
  dplyr::mutate(censustract = stringr::str_sub(GEO_ID, -9,-1))

#create new column in dataframe that reclassifies borough code column
census_tracts %<>% dplyr::mutate(cntyFIPS = case_when(
  boro_name == 'Bronx' ~'005',
  boro_name == 'Brooklyn' ~ '047',
  boro_name == 'Manhattan' ~ '061',
  boro_name == 'Queens' ~ '081',
  boro_name == 'Staten Island' ~ '085'),
  #create new column that merges both fields together so we can make our own census tract field
  tractFIPS = paste(cntyFIPS, ct2010, sep='')
) 

# Merge (JOIN) ACS data to the census tracts
popData <- merge(census_tracts, acs_data_clean, by.x ='tractFIPS', by.y = 'censustract')


#5 - Aggregate ACS census data to zip code area

#just checking the class. it's sf, so we need to do a spatial join
class(popData) 

#transform the zipcodes to match crs of food retail
popData_trans <- sf::st_transform(popData, st_crs(census_tracts))
zipcode_trans <- sf::st_transform(zip_health, st_crs(census_tracts))

# Error message said I had NA, but I checked and got no NA records returned
zipcode_trans %>% dplyr::filter(is.na(ZIPCODE))

#join zip code layer to population census data
covidPopZipNYC <- sf::st_join(zipcode_trans, 
                              #convert population data to centroid points
                              popData_trans %>% sf::st_centroid(), 
                              #collect points to each zipcode
                              join = st_contains) %>% 
  #aggregate population data by zipcode
  group_by(ZIPCODE) %>%
  #summarize demographic variables per zip code
  summarise(totPop = sum(totPop),
            malePctg = sum(femalePop)/totPop*100,
            asianPop = sum(asianPop),
            blackPop = sum(blackPop),
            hispanicPop = sum(hispanicPop),
            whitePop = sum(whitePop),
            citizenAdult = sum(citizenAdult),
            elderlyPop = sum(elderlyPop),
            totPop = sum(totPop))

#create maps of new aggregated zipcode data
plot(covidPopZipNYC["totPop"], breaks='jenks')
plot(covidPopZipNYC["elderlyPop"], breaks='jenks')
plot(covidPopZipNYC["citizenAdult"], breaks='jenks')
plot(covidPopZipNYC["blackPop"], breaks='jenks')
plot(covidPopZipNYC["asianPop"], breaks='jenks')
plot(covidPopZipNYC["hispanicPop"], breaks='jenks')
plot(covidPopZipNYC["whitePop"], breaks='jenks')