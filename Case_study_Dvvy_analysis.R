### Divvy_Case_Study_Full_Year_Analysis_2020_to_2021 ###
#This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). 
####### Import packages##########
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
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
unique(duplicated(year_trips$ride_id))# Checks for duplicate values in ride_id

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
year_trips %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), averange_duration = mean(ride_length) ) %>%  
  arrange(member_casual, weekday)
  
# Visualize the number of rides by rider type
year_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y= number_of_rides, fill = member_casual))+ geom_col(position='dodge')

#Create a visualization for average duration
year_trips %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")

#======================================================================
#Export summary file for further analysis
#=====================================================================
#Create a csv file that we will visualize in SpreadSheet, Tableau, etc
counts <- aggregate(year_trips$ride_length ~ year_trips$member_casual + year_trips$day_of_week, FUN = mean)
write.csv(counts, file = '/home/fabricio/learning/Google_data_analytic/Case_study_01/avg_ride_length.csv')