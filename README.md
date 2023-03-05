## Divvy Case Study Full Year Analysis 2020 to 2021

This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by [Kevin Hartman](https://artscience.blog/home/divvy-dataviz-case-study).

This repository contains an R analysis file that examines Divvy Trip data, including:

1. Collecting the Divvy Trip data and combining it into a single file.
2. Cleaning and preparing data for analysis.
3. Data analysis

### Required Packages
This R script requires the installation of the following packages:

* tidyverse - Helps with data manipulation.
* lubridate - Helps with handling date attributes
* ggplot2 - Helps with data visualization

## Data Collection
Divvy Trip data was collected from the following CSV [files](https://divvy-tripdata.s3.amazonaws.com/index.html):

* 202012-divvy-tripdata.csv
* 202101-divvy-tripdata.csv
* 202102-divvy-tripdata.csv
* 202103-divvy-tripdata.csv
* 202104-divvy-tripdata.csv
* 202105-divvy-tripdata.csv
* 202106-divvy-tripdata.csv
* 202107-divvy-tripdata.csv
* 202108-divvy-tripdata.csv
* 202109-divvy-tripdata.csv
* 202110-divvy-tripdata.csv
* 202111-divvy-tripdata.csv

### Data cleaning and preparation
The data was cleaned and prepared as follows:

+ Data from all months has been combined into a single file.
+ Additional columns have been added for each trip's date, month, day, and year.
+ Travel time was calculated in seconds and added as a new column.
+ Data were examined for duplicate or missing values.

### Data analysis
The data were analyzed as follows:

* The number of trips made by each type of user (member or casual) was analyzed.
* Patterns of bicycle use were examined, including use by day of week, month, and time of day.
* The average travel time per user was examined.
* Graphs of bike usage by time of day, day of the week, and month have been created.

