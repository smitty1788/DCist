# Analysis of Car2Go Data in Washington D.C.
# For: DCist.com
# Author: Randy Smith


library(tidyverse)
library(rgdal)
library(lubridate)
library(ggmap)
library(gmapsdistance)
library(stringr)
library(ggthemes)
library(hrbrthemes)
library(gcookbook)



setwd("G:/DC Policy Center/Car2Go/Data/Tab")

# file list
files <- list.files(pattern = "*.csv")

#file names
names <- c('d1', 'd2', 'd3','d4', 'd5','d6','d8','d9', 'd10', 'd11', 'd13',
           'd14', 'd15', 'd16', 'd17',
           'd18', 'd19', 'd20', 'd21',
           'd22', 'd23', 'd24', 'd25', 'd26', 'd27')

# Read in data
for (i in 1:length(files)){
  assign(names[i], 
         read.csv(files[i], stringsAsFactors = FALSE)
  )}

setwd("G:/DC Policy Center/Car2Go/Data")

# Bind all dataframes. Set Unique IDs
time_df <- rbind(d1, d2, d3, d4, d5, 
                 d6, d8, d9, d10, d11, 
                 d13, d14, d15, d16, 
                 d17, d18, d19, d20, d21,
                 d22, d23, d24, d25, d26, d27) 

# Remove from global environment
rm(d1, d2, d3, d4, d5, d6, d8, d9, d10, d11, d13,
   d14, d15, d16, d17,
   d18, d19, d20, d21,
   d22, d23, d24, d25, d26, d27)
#-------------------------------------------------------------------------------------------------------------

# Remove Duplicates, Identify first appearance in location
car_first_app <- subset(time_df, !duplicated(time_df[, c(-1,-15)], fromLast = F))
car_first_app <- car_first_app[duplicated(car_first_app[, 8]) | duplicated(car_first_app[, 8], fromLast = T),]

# Remove Duplicates, Identify last appearance in location
car_last_app <- subset(time_df, !duplicated(time_df[, c(-1,-15)], fromLast = T))
car_last_app <- car_last_app[duplicated(car_last_app[, 8]) | duplicated(car_last_app[, 8], fromLast = T),]

# Bind, format time, arrange by date/time
car2go <- rbind(car_first_app, car_last_app) %>%
  mutate(Time = ymd_hms(Time)) %>% 
  group_by(name) %>%
  arrange(name, Time) %>%
  filter(row_number() != 1,
         row_number() != n())

# Add individual Trip IDs
tripID <- car2go %>% 
  group_by(name) %>% 
  mutate(trip_id  = (1 + seq_along(address)) %/% 2,
         from_to  = (seq_along(address) %% 2))

# seprate into from and to
df_from <- tripID %>% 
  filter(from_to %% 2 == 1) %>% 
  select(-from_to)
df_to   <- tripID %>% 
  filter(from_to %% 2 == 0) %>% 
  select(-from_to)

# join the result
car2go_trips <- inner_join(df_from, df_to, by = c("name", "trip_id")) %>% 
  select(-charging.x, -engineType.x,-smartPhoneRequired.x, 
         -X.y, -charging.y, -engineType.y, -smartPhoneRequired.y, 
         -vin.y, -Altitude.x, - Altitude.y)

# rename columns of both combined datasets
colnames(car2go_trips) <- c("UID","pAddress","pExterior","pFuel", "pInterior","name","vin","pCity",
                    "pLon","pLat","pTime","carTripID","dAddress", "dExterior","dFuel","dInterior",
                    "dCity","dLon","dLat","dTime")

# calculate day and hour
car2go_trips <- car2go_trips %>%
  mutate(pDay = day(pTime),
         pDOW = wday(pTime),
         pDoW = wday(pTime, label = TRUE),
         pHR = hour(pTime),
         dDay = day(dTime),
         dHR = hour(dTime)) %>% 
  ungroup(name)
#-------------------------------------------------------------------------------------------------------------

pick <- car2go_trips %>% 
  select(pLon, pLat, UID)

drop <- car2go_trips %>% 
  select(dLon, dLat, UID)


hood <- readOGR(dsn = "Spatial/dc_arlington_hoods.GeoJSON", layer = "OGRGeoJSON")

# Add pickup/dropff points to Spatial points
addAll <- SpatialPoints(pick, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
dropAll <- SpatialPoints(drop, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Perfom Spatial Join
puHood <- over(addAll, hood)
doHood <- over(dropAll, hood)

puH <- cbind(pick, puHood)
doH <- cbind(drop, doHood)


trip <- cbind(puH, doH)

colnames(trip) <- c("pLon","pLat", "UID", "pHood", "pQuad", "pNID",
                    "dLon", "dLat", "dTrip", "dHood", "dQuad", "dNID")

trip <- trip %>% 
  select(-pLon, -pLat, -dLon, -dLat, -dTrip)


c2g_trips <- merge(x = car2go_trips, y = trip, by = "UID", all.x = TRUE, all.y = TRUE) %>% 
  
  mutate(pQuad = as.character(pQuad),
         pHood = as.character(pHood),
         dQuad = as.character(dQuad),
         dHood = as.character(dHood)) %>% 
  
  select(UID, carTripID, name, vin, 
         pFuel, pExterior, pInterior, pAddress, pCity, 
         pNID, pQuad, pHood, pLon, pLat, pTime, pDOW, pDoW, pDay, pHR, 
         dFuel, dExterior, dInterior, dAddress, dCity, 
         dNID, dQuad, dHood, dLon, dLat, dTime, dDay, dHR) %>% 
  na.omit()




#-------------------------------------------------------------------------------------------------------------

route <- gmapsdistance(origin = str_c(c2g_trips$pLat, c2g_trips$pLon, sep = ","), 
                       destination = str_c(c2g_trips$dLat, c2g_trips$dLon, sep = ","), 
                       combinations = 'pairwise', 
                       mode = 'driving',
                       key = 'AIzaSyCLDsY7fgqGlQffr917Rqx7NUL42lGT_lM')

route <- data.frame(route$Distance) 

c2g_trips$distance <- route$Distance

c2g_trips <- c2g_trips %>% 
  mutate(time_taken = time_length(interval(pTime,dTime), "seconds"),
         time_taken_min = time_taken / 60)

#-------------------------------------------------------------------------------------------------------------
# Summaries and Vignettes of Data

# Summary By neighborhood
hood_sum <- c2g_trips %>% 
  group_by(pHood, dHood) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  ungroup() %>% 
  mutate(percent = count / sum(count) * 100)

# Vingette of Petworth
petworth <- c2g_trips %>%
  filter(pHood == "Petworth") %>% 
  group_by(pHood, dHood) %>% 
  summarise(count = n(),
            dNID = median(dNID)) %>% 
  arrange(desc(count)) %>%
  ungroup() %>% 
  mutate(percent = round(count / sum(count) * 100, digits = 3))

# Vingette of Aurora Highlands
aurora_highlands <- c2g_trips %>%
  filter(pHood == "Aurora Highlands") %>% 
  group_by(pHood, dHood) %>% 
  summarise(count = n(),
            dNID = median(dNID)) %>% 
  arrange(desc(count)) %>%
  ungroup() %>% 
  mutate(percent = round(count / sum(count) * 100, digits = 3))
  

# Summary by quadrant
quad_sum <- c2g_trips %>%
  count(pQuad, dQuad) %>% 
  arrange(desc(n)) %>% 
  filter(pQuad != dQuad)


# Write summaries to csv for map/graohic creation
write.csv(petworth, "Tab/Tidy/petworth.csv", row.names = FALSE)
write.csv(aurora_highlands, "Tab/Tidy/aurora_highlands.csv", row.names = FALSE)
write.csv(quad_sum, "Tab/Tidy/quad_sum.csv", row.names = FALSE)


#-------------------------------------------------------------------------------------------------------------
# Routes Data

# Subset of route data, June 6, 2017
route_data <- c2g_trips %>% 
  filter(dDay == 6,
         between(dHR, 6, 23)) %>%
  head(2000) %>% 
  mutate(start = as.character(str_c(pLat, pLon, sep = ",")),
         dest = as.character(str_c(dLat, dLon, sep = ",")))

# Create new dataframe with only start/destination  
start<-c(route_data[1:2000, 35])
dest<-c(route_data[1:2000,36])

routes <- tibble(
  start,
  dest)

# Calculate route function
calculationroute <- function(startingpoint, stoppoint) {
  Sys.sleep(1)
  route(from = startingpoint,
        to = stoppoint,
        mode = "driving",
        structure = "route",
        output = "all")}

decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}

# Calculate detailed routes for every trip in dataframe
calculatedroutes <- mapply(calculationroute,
                           startingpoint = routes$start,
                           stoppoint = routes$dest,
                           SIMPLIFY = FALSE)

# Convert List to dataframe
do.call(rbind.data.frame, lapply(names(calculatedroutes), function(x) {
  cbind.data.frame(route = x, decodeLine(calculatedroutes[[x]]$routes[[1]]$overview_polyline$points), 
                   stringsAsFactors=FALSE)
})) -> long_routes

#-------------------------------------------------------------------------------------------------------------
# Graphics 


# Categorical Color Scheme
group_colors <- c(
  'Sun' = '#FF6F00', 'Mon' = '#C71000',
  'Tues' = '#008EA0', 'Wed' = '#8A4198',
  'Thurs' = '#5A9599', 'Fri' = '#84D7E1', 
  'Sat' = '#FF95A8')

# Histogram of trip times
ggplot(filter(c2g_trips, time_taken_min <= 200), aes(time_taken_min, y = (..count..)/sum(..count..))) +
  geom_histogram(aes( fill = ..count..), binwidth = 5, 
                 color = "black", show.legend = FALSE,
                 closed = 'right', center = 0, na.rm = TRUE) +
  scale_fill_gradient("Frequency", low = "#FFFFFF", high = "#00A0E1") +
  labs(x = "Minutes", y = "Frquency of Trips",
       title = "Plurality of Car2Go Trips last between 20min - 40min",
       subtitle = "Frequency of Car2Go total trip time during May 22nd to June 14th",
       caption = "Source: Car2Go") +
  theme(plot.title = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(5, 150, by = 10), limits = c(0, 160)) +
  scale_y_percent(breaks = seq(0, 0.14, by = 0.02), limits = c(0, 0.14)) +
  theme_fivethirtyeight()

# Frequency of Trips by day/hour
c2g_trips %>% 
  count(pDoW, pHR) %>% 
ggplot(aes(pHR, n/sum(n), color = pDoW)) +
  geom_freqpoly(stat = 'identity', size = 1, linejoin = "round") +
  labs(x = "Hour of Day", y = "Frquency of Trips",
       title = "Car2Go usage follows typical workday commute patterns",
       subtitle = "Frequency of Car2Go Trips by Hour during May 22nd to June 14th",
       caption = "Source: Car2Go, 2017") +
  theme(plot.title = element_text(size = 18),
        plot.caption = element_text(margin = margin(b = -20))) +
  scale_color_manual("Day of Week", values = group_colors) +
  scale_x_continuous(breaks = seq(0, 23, by = 2), limits = c(0, 23)) +
  scale_y_percent(breaks = seq(0, 0.020, by = 0.002), limits = c(0, 0.020)) +
  theme_fivethirtyeight()

# Map of route frquency for June 6, 2017
ggplot(long_routes, aes(x=lon, lat)) +
  geom_path(data=long_routes, aes(x=lon, y=lat, group = route), color = "white", size = .5, alpha = .1) + 
  scale_color_gradient(low = "#bfbfbf", high = "#cc00cc", trans = "log") +
  theme(legend.position = "none",
        panel.background = element_rect(fill="black", color="black"),
        plot.background = element_rect(fill="black", color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(family = "Helvetica", size = 18, 
                                  face = "bold", color = "#bfbfbf",
                                  margin = margin(t = 15,r = -10)),
        plot.subtitle = element_text(family = "Helvetica", size = 10, 
                                  face = "bold", color = "#bfbfbf",
                                  margin = margin(t = 2)),
        plot.caption = element_text(family = "Helvetica", size = 10,
                                    color = "#bfbfbf", margin = margin(b = 15, l = -20))) +
  labs(title="Most frequent Car2Go routes",
       subtitle = "Individual trips taken on June 6, 2017",
       caption="Source: Car2Go, 2017\nRandy Smith\nwww.rhsmithjr.com") +
  coord_map(projection = 'mercator')
