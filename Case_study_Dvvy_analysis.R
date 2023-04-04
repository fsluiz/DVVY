### Divvy_Case_Study_Full_Year_Analysis_2020_to_2021 ###
#This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). 
####### Import packages##########
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(sunburstR) #helps to visualize data
library(leaflet) # help to visulaize maps
library(htmlwidgets) #save maps
library(sf) # read geojson
library(readr) # help to read sf archive
library(treemap) #create a dataframe hierachical
library(ggplotify) #create sunbusrt grafic
library(datasets)
library(data.table)
library(plotly) #help to create a interative plots.
getwd() #displays your working directory
setwd("/home/fabricio/learning/Google_data_analytic/Case_study_01/raw_data") #sets your working directory to simplify calls to data

#==========================
# Collect the data
#=========================
#Upload Divvy datasets
dez_2020 <- read.csv("202012-divvy-tripdata.csv")
jan_2021 <- read.csv("202101-divvy-tripdata.csv")
fev_2021 <- read.csv("202102-divvy-tripdata.csv")
mar_2021 <- read.csv("202103-divvy-tripdata.csv")
abr_2021 <- read.csv("202104-divvy-tripdata.csv")
mai_2021 <- read.csv("202105-divvy-tripdata.csv")
jun_2021 <- read.csv("202106-divvy-tripdata.csv")
jul_2021 <- read.csv("202107-divvy-tripdata.csv")
ago_2021 <- read.csv("202108-divvy-tripdata.csv")
set_2021 <- read.csv("202109-divvy-tripdata.csv")
out_2021 <- read.csv("202110-divvy-tripdata.csv")
nov_2021 <- read.csv("202111-divvy-tripdata.csv")

#======================================
#Wrangle Data and Combine into a Single File
#=====================================
#Compare column names each of the files, all.equal it will return True if is same name and False if is diffenent
all.equal(colnames(dez_2020) , colnames(jan_2021))
all.equal(colnames(jan_2021) , colnames(fev_2021))
all.equal(colnames(fev_2021) , colnames(mar_2021))
all.equal(colnames(mar_2021) , colnames(abr_2021))
all.equal(colnames(abr_2021) , colnames(mai_2021))
all.equal(colnames(mai_2021) , colnames(jun_2021))
all.equal(colnames(jun_2021) , colnames(jul_2021))
all.equal(colnames(jul_2021) , colnames(ago_2021))
all.equal(colnames(ago_2021) , colnames(set_2021))
all.equal(colnames(set_2021) , colnames(out_2021))
all.equal(colnames(out_2021) , colnames(nov_2021))
#===============================
# Inspect the dataframes and look for incongruencies
str(dez_2020)
str(jan_2021)
str(fev_2021)
str(mar_2021)
str(abr_2021)
str(jun_2021)
str(jul_2021)
str(ago_2021)
str(set_2021)
str(out_2021)
str(nov_2021)
# The started_at and ende_at are in chr after join we will need to loock that, and are serveral start_station_name, id, and end_station_name, id its missiing 
#Stack individual month data frame into one big data frame of role year
year_trips <-bind_rows(dez_2020, jan_2021, fev_2021, mar_2021, abr_2021, mai_2021, jun_2021, jul_2021, ago_2021, set_2021, out_2021, nov_2021)
#================================================
#Clean up anda prepare for analusis
#===============================================
#inspect the new dataset
colnames(year_trips)
nrow(year_trips)
dim(year_trips)
head(year_trips)
str(year_trips)
summary(year_trips)

#How many observaton fall under each usertype
table(year_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year 
year_trips$date <- as.Date(year_trips$started_at)# Defalt formart yyyy-mm-dd
year_trips$month <- format(as.Date(year_trips$date),"%m")
year_trips$day <- format(as.Date(year_trips$date),"%d")
year_trips$year <- format(as.Date(year_trips$date),"%Y")
year_trips$day_of_week <- format(as.Date(year_trips$date),"%A")

#Add a "ride_length" calculation to year_trips (in seconds)
year_trips$ride_length <-difftime(year_trips$ended_at, year_trips$started_at)

# Inspect the structure of the columns
str(year_trips)
is.difftime(year_trips$ride_length)
#Convert "ride_length" to numeric so we can run calculations on the data
year_trips$ride_length <- as.numeric(year_trips$ride_length)
is.numeric(year_trips$ride_length)
#Verify if is ride_length negative
subset(year_trips,ride_length<0)
#The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative we will remove this data
year_trips <- year_trips[!(year_trips$ride_length<0),]

#The columns start_station_name and start_station_id are Na values to make sure which columns are empty
colSums(is.na(year_trips))
# Checks for duplicate values in ride_id
unique(duplicated(year_trips$ride_id))
#after removing the negative ride_lengths, there is 4736 rows if end_lat and en_lhg empty. We could delet this rows
year_trips <- year_trips[complete.cases(year_trips),]
#======================================================
#Conduct Descriptive Analysis
#=====================================================
#Descriptive analysis on ride_length (in seconds)
mean(year_trips$ride_length) #Average
median(year_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(year_trips$ride_length) #longest ride
min(year_trips$ride_length)#shortest ride

# Condensing the four lines using sumary() on the specific attribute
summary(year_trips$ride_length)

# Compare members and casual users
aggregate(year_trips$ride_length ~ year_trips$member_casual, FUN = mean)
aggregate(year_trips$ride_length ~ year_trips$member_casual, FUN = median)
aggregate(year_trips$ride_length ~ year_trips$member_casual, FUN = max)
aggregate(year_trips$ride_length ~ year_trips$member_casual, FUN = min)

#See the averange ride time by each day for members vs casual users
aggregate(year_trips$ride_length ~ year_trips$member_casual + year_trips$day_of_week, FUN = mean)
#Notice that the days of week are out of order
year_trips$day_of_week <-ordered(year_trips$day_of_week,levels = c('domingo', 'segunda', 'terça', 'quarta', 'quinta', 'sexta', 'sábado'))
# let's the average ride time by each day for members vs casual users
aggregate(year_trips$ride_length ~ year_trips$member_casual + year_trips$day_of_week, FUN = mean)
# Analyze ridership data by type and weekday
#creates weekday fields using wday()
#groups by usertype and weekday
#calculate the number of rides and average duration
nride_avduration_weekday <-year_trips %>% 
  mutate(weekday = lubridate::wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), averange_duration = mean(ride_length) ) %>%  
  arrange(member_casual, weekday)
head(year_trips)
#===============================================================================
#Plots number of rides in funcion of the hour the day
year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  ggplot(aes(x=hora, y= number_of_rides, colour = member_casual))+ geom_line() + 
  annotate("rect", xmin = 6, xmax = 8, ymin = 0, ymax = Inf, alpha = 0.2, fill = "orange")+
  annotate("text", x = 7, y = 3e+05, label = "Rush", size = 3)+
  annotate("rect", xmin = 16, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.2, fill = "orange")+
  annotate("text", x = 17, y = 1e+05, label = "Rush", size = 3)+
  labs(title = "Number of Rides per day hours", subtitle = "Comparison between casual and members consumers", x = "Hour", y = "Number of Rides", fill = "Consumer")
#Plot average duration per hour of the day
year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  ggplot(aes(x=hora, y= average_duration, colour = member_casual))+ geom_line() + 
  annotate("rect", xmin = 6, xmax = 8, ymin = 0, ymax = Inf, alpha = 0.2, fill = "orange")+
  annotate("text", x = 7, y = 500, label = "Rush", size = 3)+
  annotate("rect", xmin = 16, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.2, fill = "orange")+
  annotate("text", x = 17, y = 500, label = "Rush", size = 3)+
  labs(title = "Average duration per day hours", subtitle = "Comparison between casual and members consumers", x = "Hour", y = "Averange duration", fill = "Consumer")
#Filter average time between midnight and five am maximal average valor
year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  filter(hora>=0 & hora<=5) %>% 
  filter(average_duration==max(average_duration))
#Filter average time between midnight and five am minimal average valor
year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  filter(hora>=0 & hora<=5) %>% 
  filter(average_duration==min(average_duration))
#filter after 20:00 hours max average 
year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  filter(hora>=20) %>% 
  filter(average_duration==max(average_duration))
#filter after 20 h min average
year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  filter(hora>=20) %>% 
  filter(average_duration==min(average_duration))
# filter by work days in  day of week 
work_days <- filter(year_trips, day_of_week %in% c('segunda','terça','quarta','quinta','sexta'))
work_days %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  ggplot(aes(x=hora, y= number_of_rides, colour = member_casual))+ geom_line() + 
  annotate("rect", xmin = 6, xmax = 8, ymin = 0, ymax = Inf, alpha = 0.2, fill = "orange")+
  annotate("text", x = 7, y = 3e+05, label = "Rush", size = 3)+
  annotate("rect", xmin = 16, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.2, fill = "orange")+
  annotate("text", x = 17, y = 1e+05, label = "Rush", size = 3)+
  labs(title = "Number of Rides per day hours, workday", subtitle = "Comparison between casual and members consumers", x = "Hour", y = "Number of Rides", fill = "Consumer")
#filter by weekend
weekend_days <- filter(year_trips, day_of_week %in% c('domingo','sábado'))
weekend_days%>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora) %>% 
  ggplot(aes(x=hora, y= number_of_rides, colour = member_casual))+ geom_line() + 
  labs(title = "Number of Rides per day hours Weekend", subtitle = "Comparison between casual and members consumers", x = "Hour", y = "Number of Rides", fill = "Consumer")
colnames(year_trips)
# Number of rides per hour in the weekend for casual consumers
weekend_days%>% filter(member_casual=='casual') %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(rideable_type,member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type,member_casual, hora) %>% 
  ggplot(aes(x=hora, y= number_of_rides, colour = rideable_type))+ geom_line() + 
  labs(title = "Number of Rides per day hours Weekend", subtitle = "Comparison between casual and members consumers", x = "Hour", y = "Number of Rides", fill = "Consumer")
# Number of rides per hour in the wook day for casual consumers
work_days%>% filter(member_casual=='casual') %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(rideable_type,member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type,member_casual, hora) %>% 
  ggplot(aes(x=hora, y= number_of_rides, colour = rideable_type))+ geom_line() + 
  labs(title = "Casual Number of Rides per day hours work day", subtitle = "For different rideable types, in work day (Monday to Friday)", x = "Hour", y = "Number of Rides", color = "Rideble Type")

# Averagne duration the weekend for casual consumers
weekend_days%>% filter(member_casual=='casual') %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(rideable_type,member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type,member_casual, hora) %>% 
  ggplot(aes(x=hora, y= average_duration, colour = rideable_type))+ geom_line() + 
  labs(title = "Average duration per day hours Weekend", subtitle = "For different rideable types, in weekend", x = "Hour", y = "Average Duration", color = "Rideable Type")
# Averange durateions per hour in the wook day for casual consumers
work_days%>% filter(member_casual=='casual') %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(rideable_type,member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type,member_casual, hora) %>% 
  ggplot(aes(x=hora, y= average_duration, colour = rideable_type))+ geom_line() + 
  labs(title = "Average duration per day hours work day", subtitle = "For different rideable types, in work day (Monday to Friday)", x = "Hour", y = "Number of Rides", color = "Rideble Type")


#defi function to use a sunburs plot see https://stackoverflow.com/questions/12926779/how-to-make-a-sunburst-plot-in-r-or-python
as.sunburstDF <- function(DF, valueCol = NULL){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  DT[, root := names(DF)[1]]
  colNamesDT <- names(DT)
  
  if(is.null(valueCol)){
    setcolorder(DT, c("root", colNamesDF))
  } else {
    setnames(DT, valueCol, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
  }
  
  hierarchyCols <- setdiff(colNamesDT, "values")
  hierarchyList <- list()
  
  for(i in seq_along(hierarchyCols)){
    currentCols <- colNamesDT[1:i]
    if(is.null(valueCol)){
      currentDT <- unique(DT[, ..currentCols][, values := .N, by = currentCols], by = currentCols)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=currentCols, .SDcols = "values"]
    }
    setnames(currentDT, length(currentCols), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parentCols]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parentCols) := NULL]
  return(hierarchyDT)
}
#====================================================
# number rides weekday 
nride_weekday <-year_trips %>% 
  mutate(weekday = lubridate::wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>%  
  arrange(member_casual, weekday)

DF <- as.data.table(nride_weekday)
setcolorder(DF, c("member_casual", "weekday", "number_of_rides"))
sunburstDF <- as.sunburstDF(DF, valueCol = "number_of_rides")


plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total') %>% 
  layout(title = "Comsumer Number of Rides per Weekday")


# Visualize the number of rides by rider type for weekday
year_trips %>% 
  mutate(weekday = lubridate::wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y= number_of_rides, fill = member_casual))+ geom_col(position='dodge') + 
  scale_x_discrete(labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  labs(title = "Weekday vs Number of Rides", subtitle = "Comparison between casual consumers and members", x = "Weekday", y = "Number of Rides", fill = "Consumer")

#Create a map of chicago with gggplot see https://thisisdaryn.netlify.app/post/intro-to-making-maps-with-ggplot2/
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") 

sample_of <- year_trips[sample(nrow(year_trips),10000),] 
# Create a data set with weekday, member_casual start_lng, end_lng, start_lat, end_lat, number of rides and average duration
data_graph = year_trips %>% 
  mutate(weekday = lubridate::wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday, start_lng, start_lat, end_lng, end_lat) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(desc(number_of_rides))
#Filter by casual rider
casual_data_graph <- data_graph %>% filter(member_casual=="casual")
#Filter by member rider
member_data_graph <-  data_graph %>% filter(member_casual=="member")
# take top location in number of rider
top_location_casual <- casual_data_graph[1:200, ]
top_location_member <- member_data_graph[1:200, ]
# Map with top initial location for casual
ggplot() + 
  geom_sf(data = chi_map, fill = 'ivory', colour = 'ivory')+
  geom_point(data = top_location_casual, aes(x = start_lng, y = start_lat,size = number_of_rides)) +
  labs(title = "Top start location for casual riders", subtitle = "Number of Riders for casual rides in top start locations", x = "Longitude", y = "Latitude")
# Map with top final location for casual
ggplot() + 
  geom_sf(data = chi_map, fill = 'ivory', colour = 'ivory')+
  geom_point(data = top_location_casual, aes(x = end_lng, y = end_lat, colour = member_casual, size = number_of_rides))+
  labs(title = "Top end location for casual riders", subtitle = "Number of Riders for casual rides in top end locations", x = "Longitude", y = "Latitude")

# Map with top initial location for member
ggplot() + 
  geom_sf(data = chi_map, fill = 'ivory', colour = 'ivory')+
  geom_point(data = top_location_member, aes(x = start_lng, y = start_lat,size = number_of_rides)) +
  labs(title = "Top start location for member riders", subtitle = "Number of Riders for member rides in top start locations", x = "Longitude", y = "Latitude")
# Map with top final location for member
ggplot() + 
  geom_sf(data = chi_map, fill = 'ivory', colour = 'ivory')+
  geom_point(data = top_location_member, aes(x = end_lng, y = end_lat, colour = member_casual, size = number_of_rides))+
  labs(title = "Top end location for member riders", subtitle = "Number of Riders for member rides in top end locations", x = "Longitude", y = "Latitude")
  
#Create a visualization for average duration for weekday
year_trips %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + 
  scale_x_discrete(labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  labs(title = "Weekday vs Average Duration in Seconds", subtitle = "Comparison between casual consumers and members", x = "Weekday", y = "Average Duration in Seconds", fill = "Consumer")

#Visualization average duration for month comapre by costumer
year_trips %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + 
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  labs(title = "Month vs Average Duration in Seconds", subtitle = "Comparison between casual consumers and members", x = "Month", y = "Average Duration in Seconds", fill = "Consumer")

#Visualization number of ride for month comapre by costumer
year_trips %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  labs(title = "Month vs Number of Rides", subtitle = "Comparison between casual consumers and members", x = "Month", y = "Number of Rides", fill = "Consumer")

#Create a visulaization for average duration by readiable_type
year_trips %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + 
  scale_x_discrete(labels=c("Classic Bike", "Docked Bike", "Elecric Bike")) +
  labs(title = "Rideable Type vs Average Duration in Seconds", subtitle = "Comparison between casual consumers and members", x = "Rideable Type", y = "Average Duration in Seconds", fill = "Consumer")

#Create a visualization for number of ride
year_trips %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  scale_x_discrete(labels=c("Classic Bike", "Docked Bike", "Elecric Bike")) +
  labs(title = "Rideable Type vs Number fo Rides", subtitle = "Comparison between casual consumers and members", x = "Rideable Type", y = "Number of Rides", fill = "Consumer")

#Start ponints more comum

#======================================================================
#Export summary file for further analysis
#=====================================================================
#Create a csv file that we will visualize in SpreadSheet, Tableau, etc
counts <- aggregate(year_trips$ride_length ~ year_trips$member_casual + year_trips$day_of_week, FUN = mean)
write.csv(counts, file = '/home/fabricio/learning/Google_data_analytic/Case_study_01/avg_ride_length.csv')
#Create a csv file that we will visualize number of riders per hour
riders_per_hour <- year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hora)
write.csv(riders_per_hour, file = '/home/fabricio/learning/Google_data_analytic/Case_study_01/ride_length_per_hour.csv')
#Create a csv file that we visualize number of riders per hours depends rideable type for members
rideable_type_member_hour <- year_trips %>% 
  filter(member_casual=='member') %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(rideable_type, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type, hora)
write.csv(rideable_type_member_hour, file = '/home/fabricio/learning/Google_data_analytic/Case_study_01/rideable_type_member_hour.csv')
#Create a csv file that we visualize number of riders per hours depends rideable type for members
rideable_type_casual_hour <- year_trips %>% 
  mutate(hora = lubridate::hour(started_at)) %>% 
  group_by(rideable_type, member_casual, hora) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  filter(member_casual=='casual') %>% 
  arrange(rideable_type, hora)
write.csv(rideable_type_casual_hour, file = '/home/fabricio/learning/Google_data_analytic/Case_study_01/rideable_type_casual_hour.csv')
rideable_type_casual_hour

library

