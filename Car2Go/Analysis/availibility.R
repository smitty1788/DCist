# Analysis of Car2Go Data in Washington D.C.
# For: DCist.com
# Author: Randy Smith

library(rgdal)
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(gcookbook)
library(ggsci)
library(artyfarty)

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

# Bind all dataframes. Set Unique IDs
time_df <- rbind(d1, d2, d3, d4, d5, 
                 d6, d8, d9, d10, d11, 
                 d13, d14, d15, d16, 
                 d17, d18, d19, d20, d21,
                 d22, d23, d24, d25, d26, d27) %>% 
  rename(UID = X) %>% 
  mutate(UID = row_number())

rm(d1, d2, d3, d4, d5, d6, d8, d9, d10, d11, d13,
   d14, d15, d16, d17,
   d18, d19, d20, d21,
   d22, d23, d24, d25, d26, d27)

#-----------------------------------------------------------------------------------------------------------------
# Spatial Join

setwd("G:/DC Policy Center/Car2Go/Data")

# Subset Longitude/Latitude
neighbor <- time_df %>%
  select(Longitude, Latitude)

# read neighborhood GeoJSON file
hood <- readOGR(dsn = "Spatial/dc_arlington_hoods.GeoJSON", layer = "OGRGeoJSON")

# Add pickup/dropff points to Spatial points
addAll <- SpatialPoints(neighbor, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Perfom Spatial Join
Hood <- over(addAll, hood)

# Bind to neighborhood info to original df
Hoods <- cbind(time_df, Hood) %>% 
  mutate(subhood = as.character(subhood),
         shortdate = as.Date(Time),
         dow = wday(Time, label = TRUE),
         hour = hour(Time))


#-----------------------------------------------------------------------------------------------------------------
# Calculate Statistics

# Average Cars Availible, use in map
neigh_availibilty <- Hoods %>% 
  group_by(subhood, id, shortdate, hour) %>% 
  summarise(count = n_distinct(name)) %>% 
  group_by(subhood, id) %>% 
  summarise(daily_avg = mean(count),
            daily_median = median(count),
            daily_max = max(count),
            daily_min = min(count),
            daily_sd = sd(count)) %>% 
  arrange(desc(daily_avg)) %>% 
  na.omit()
write.csv(neigh_availibilty, "Tab/Tidy/neigh_availibility.csv", row.names = FALSE)


# Cars Availible by hour animation
animation <- Hoods %>% 
  filter(shortdate == "2017-06-07") %>% 
  select(Longitude, Latitude, Time)

animation <- animation %>% 
  mutate(Time = ymd_hms(Time, tz = "America/New_York"),
         # Carto subtracts 4 hours, force time 4 hours ahead to compensate
         Time = with_tz(Time, tzone = "Atlantic/Azores"))
write.csv(animation, "Tab/Tidy/Car2Go_locations.csv", row.names = FALSE)


# Adams Morgan Car Availibility
admo_avg <- Hoods %>% 
  filter(subhood %in% c('Reed-Cooke', 'Washington Heights', 'Kalorama Triangle')) %>% 
  mutate(name = as.factor(name)) %>% 
  group_by(shortdate, hour) %>% 
  summarise(n_cars = n_distinct(name)) %>% 
  mutate(dow = wday(shortdate, label = TRUE)) %>% 
  group_by(dow, hour) %>% 
  summarise(cars_avg = mean(n_cars),
            cars_median = median(n_cars),
            cars_min = min(n_cars),
            cars_max = max(n_cars),
            sample = n())


#------------------------------------------------------------------------------------------------------------------------
# Graphics

# Categorical Color Scheme
group_colors <- c(
  'Sun' = '#FF6F00', 'Mon' = '#C71000',
  'Tues' = '#008EA0', 'Wed' = '#8A4198',
  'Thurs' = '#5A9599', 
  'Fri' = '#84D7E1', 'Sat' = '#FF95A8')

# Plot by Average per day of week and hour
ggplot(admo_avg) +
  geom_line(aes(hour, cars_median, color = dow), size = 1) +
  labs(x = "Hour of Day", y = "Cars Available",
       title = "The Car2Go 'hole' in Adams Morgan",
       subtitle = "Average hourly car availibility in Adams Morgan",
       caption = "Source: Car2Go") +
  scale_color_manual("Day of Week", values = group_colors) +
  scale_x_continuous(breaks = seq(0,23, by = 2), limits = c(0, 23)) +
  scale_y_continuous(breaks = seq(0,22, by = 5), limits = c(0, 21)) +
  theme(plot.title = element_text(size = 18)) +
  theme_fivethirtyeight()



#-------------------------------------------------------------------------------------------------------------------------
# Altering fivethirtyeight theme from ggthemes package
#' @include ggthemes-package.R
#' @include ggthemes-data.R
NULL

#' Theme inspired by fivethirtyeight.com plots
#'
#' Theme inspired by the plots on
#' \href{fivethirtyeight.com}{http://fivethirtyeight.com}.
#'
#' @inheritParams ggplot2::theme_grey
#' @family themes fivethirtyeight
#' @export
#' @example inst/examples/ex-theme_fivethirtyeight.R
theme_fivethirtyeight <- function(base_size = 12, base_family = "sans") {
  (theme_foundation(base_size = base_size, base_family = base_family)
   + theme(
     line = element_line(colour = "black"),
     rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"],
                         linetype = 0, colour = NA),
     text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]),
     axis.title = element_text(size = 10),
     axis.text = element_text(),
     axis.ticks = element_blank(),
     axis.line = element_blank(),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     panel.grid = element_line(colour = NULL),
     panel.grid.major =
       element_line(colour = ggthemes_data$fivethirtyeight["medgray"]),
     panel.grid.minor = element_blank(),
     # unfortunately, can't mimic subtitles
     plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.background = element_rect()))
}

#' fivethirtyeight.com color palette
#'
#' The standard fivethirtyeight.com palette for line plots is blue, red, green.
#'
#' @family colour fivethirtyeight
#' @export
#' @example inst/examples/ex-fivethirtyeight_pal.R
fivethirtyeight_pal <- function() {
  function(n) {
    colors <- ggthemes_data$fivethirtyeight[c("blue", "red", "green")]
    unname(colors[seq_len(n)])
  }
}

#' fivethirtyeight.com color scales
#'
#' Color scales using the colors in the fivethirtyeight graphics.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour fivethirtyeight
#' @rdname scale_fivethirtyeight
#' @seealso \code{\link{theme_fivethirtyeight}} for examples.
#' @export
scale_colour_fivethirtyeight <- function(...) {
  discrete_scale("colour", "economist", fivethirtyeight_pal(), ...)
}

#' @rdname scale_fivethirtyeight
#' @export
scale_color_fivethirtyeight <- scale_colour_fivethirtyeight

#' @rdname scale_fivethirtyeight
#' @export
scale_fill_fivethirtyeight <- function(...) {
  discrete_scale("fill", "economist", fivethirtyeight_pal(), ...)
}



