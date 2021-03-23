# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~ TWITTER CONFERENCES ~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#### initial set-up ####

# clear the environment and set working directory
rm(list=ls())
# load libraries
library(CoordinateCleaner)
library(sp)


#### parameters ####

# source: http://www.tweetfarts.com/
tweet_CO2 = 0.02/1000 # divide by 1000 to get kg as unit

# source: https://github.com/milankl/CarbonFootprintAGU 
railETC = 60/1000
flightSHORT = 200/1000
flightLONG = 250/1000
flightSLONG = 300/1000

flight_lim = 400

short_lim = 1500
long_lim = 8000


#### how far do you get with emissions corresponding to one tweet? ####
tweet_CO2/railETC*1000
tweet_CO2/flightSHORT*1000
tweet_CO2/flightLONG*1000
tweet_CO2/flightSLONG*1000


#### loading country location data ####
data(countryref)
countryref = countryref[countryref$type == "country",]
countryref = countryref[!duplicated(countryref$name),] # need to check what the different coordinates are?
countryref = countryref[, c("name", "centroid.lon", "centroid.lat", "capital", "capital.lon", "capital.lat")]
names(countryref) = c("country", "lon", "lat", "capital", "capital.lon", "capital.lat")


#### tweets per person ####

# WSTC5 #
WSTC5 = read.csv("WSTC5.csv", sep = ";")
tweetsWSTC5 = mean(aggregate(WSTC5$tweets, by = list(WSTC5$id), sum)$x, na.rm = T)

# BOU17 #
BOU17TC = read.csv("BOU17TC.csv", sep = ";")
BOU17TC = BOU17TC[!is.na(BOU17TC$id),]
tweetsBOU17TC = mean(aggregate(BOU17TC$tweets, by = list(BOU17TC$id), sum)$x, na.rm = T)

# BOU18 #
BOU18TC = read.csv("BOU18TC.csv", sep = ";")
tweetsBOU18TC = mean(aggregate(BOU18TC$tweets, by = list(BOU18TC$id), sum)$x, na.rm = T)




#### TWITTER CONFERENCES ####

#### loading data ####

# load coference data 

TCs = c("WSTC5", "BOU17TC", "BOU18TC")

sum.data.TC = data.frame(tc = TCs,
                      total_travel = rep(NA, length(TCs)),
                      travel_per_person = rep(NA, length(TCs)),
                      total_twitter = rep(NA, length(TCs))
)

for(tc in TCs){

  df = read.csv(paste0(tc, ".csv"), sep = ";", header = T) # presenter countries
  df = df[!duplicated(df$id),]
  
  df = merge(df, countryref, all.x = T) # add information on lat long of country centroid 
  
  
  #### calculate distances between locations and set capital that minimises total distance as host capital ####
  
  total_dist = numeric()
  
  for(c in 1:nrow(countryref)){
    
    if(!is.na(countryref$capital.lat[c]) & !is.na(countryref$capital.lon[c])){
      centre_lat = countryref$capital.lat[c]
      centre_long = countryref$capital.lon[c]
      
      locs = SpatialPoints(coords = cbind(df$lon, df$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
      C = SpatialPoints(coords = cbind(centre_long, centre_lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
      total_dist[c] = sum(spDistsN1(locs, C, longlat = T)) 
    }
  }
  
  conf_country = countryref$country[which.min(total_dist)]
  print(c(tc, conf_country))
  
  
  #### calculate travel distances ####
  
  # assume that everyone is travelling in a straight line from centroid of country to capital of host country
  
  capital_long = countryref$capital.lon[countryref$country == conf_country]
  capital_lat = countryref$capital.lat[countryref$country == conf_country]
  
  locs = SpatialPoints(coords = cbind(df$lon, df$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
  C = SpatialPoints(coords = cbind(capital_long, capital_lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
  df$travel_dist = spDistsN1(locs, C, longlat = T) 
  
  
  
  #### travel emissions ####
  df$travel_emissions = 
    df$travel_dist*flightSLONG
  df$travel_emissions[df$travel_dist <= long_lim] = 
    df$travel_dist[df$travel_dist <= long_lim]*flightLONG
  df$travel_emissions[df$travel_dist <= short_lim] = 
    df$travel_dist[df$travel_dist <= short_lim]*flightSHORT
  df$travel_emissions[df$travel_dist <= flight_lim] = 
    df$travel_dist[df$travel_dist <= flight_lim]*railETC
  
  
  #### multiply by two as people go back and forth ####
  
  df$travel_emissions = df$travel_emissions*2
  
  #### twitter emissions #### 
  
  if(tc == "WSTC5") no_tweets = tweetsWSTC5
  if(tc == "BOU17TC") no_tweets = tweetsBOU17TC
  if(tc == "BOU18TC") no_tweets = tweetsBOU18TC
  
  df$twitter_emissions = no_tweets*tweet_CO2
  
  
  #### comparison ####
  
  sum.data.TC$total_travel[sum.data.TC$tc == tc] = sum(df$travel_emissions)
  sum.data.TC$travel_per_person[sum.data.TC$tc == tc] = sum(df$travel_emissions)/nrow(df)
  sum.data.TC$total_twitter[sum.data.TC$tc == tc] = sum(df$twitter_emissions)
  
}

sum.data.TC$total_travel/sum.data.TC$total_twitter



#### actual conferences ####

ACs = c("WSC2", "BOU17", "BOU18")
AC_lat = c(-33.914861, 52.378597, 52.936087)
AC_long = c(18.429411, -1.561288, -1.205867)


sum.data.AC = data.frame(ac = ACs,
                      total_travel = rep(NA, length(ACs)),
                      travel_per_person = rep(NA, length(ACs)),
                      total_twitter = rep(NA, length(ACs))
)

for(i in 1:length(ACs)){
  
  ac = ACs[i]
  
  df = read.csv(paste0(ac, ".csv"), sep = ";")
  
  df = merge(df, countryref, by = "country", all.x = T)
  
  # location of conference
  conf_lat = AC_lat[i]
  conf_long = AC_long[i]
  
  df = df[!is.na(df$lat),]
  
  locs = SpatialPoints(coords = cbind( df$lon, df$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
  C = SpatialPoints(coords = cbind(conf_long, conf_lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
  df$travel_dist = spDistsN1(locs, C, longlat = T) 
  
  
  
  #### travel emissions ####
  df$travel_emissions = 
    df$travel_dist*flightSLONG
  df$travel_emissions[df$travel_dist <= long_lim] = 
    df$travel_dist[df$travel_dist <= long_lim]*flightLONG
  df$travel_emissions[df$travel_dist <= short_lim] = 
    df$travel_dist[df$travel_dist <= short_lim]*flightSHORT
  df$travel_emissions[df$travel_dist <= flight_lim] = 
    df$travel_dist[df$travel_dist <= flight_lim]*railETC
  
  
  #### multiply by two as people go back and forth ####
  
  df$travel_emissions = df$travel_emissions*2
  
  
  #### twitter emissions #### 
  
  if(tc == "WSC2") no_tweets = tweetsWSTC5
  if(tc == "BOU17") no_tweets = tweetsBOU17TC
  if(tc == "BOU18") no_tweets = tweetsBOU18TC
  
  
  df$twitter_emissions = no_tweets*tweet_CO2
  
  
  #### comparison ####
  sum.data.AC$total_travel[i] = sum(df$travel_emissions)
  sum.data.AC$travel_per_person[i] = sum(df$travel_emissions)/nrow(df)
  sum.data.AC$total_twitter[i] = sum(df$twitter_emissions)
  
}

sum.data.AC$total_travel/sum.data.AC$total_twitter


