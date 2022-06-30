# Assignment 4 
# Special Topics in Biomedical Communications
# Archana Jeyagaran 
# June 28, 2022

library(tidyr)
library(dplyr)
library(stringr)


# Read the data into a dataframe (check to make sure that column names do not have spaces in them)
# set workding directory 
wd <- setwd("~/MBiotech Year 1/MSC 2011H Special Topics in Biomedical Communications/Class 6")
# get wd
getwd()
df_original <- read.csv("ufo.csv")
# makesure df is a dataframe
is.data.frame(df)
# replace all blanks/unknowns with NA
df_original[df_original == "" | df_original == " " | df_original == "unknown"] <- NA

### Clean up the rows that do not have Country or Shape information ###
# duplicate dataframe
df_duplicate <- df_original %>%
  # remove rows that do not have country or shape information
  filter(!is.na(shape)) %>% # drop_na(shape) %>% 
  filter(!is.na(country)) %>%  # drop_na(country) %>% # alternatives to what you did 
  ### Convert Datetime and Date_posted columns into appropriate formats ###
  mutate(datetime = as.Date(datetime, tryFormats = ("%Y-%m-%d"))) %>%
  mutate(date.posted = as.Date(date.posted, tryFormats = ("%Y-%m-%d")))

### NUFORC officials comment on sightings that may be hoax. Figure out a way ###
### (go through the Comments and decide how a proper filter should look like) and ###
### remove these sightings from the dataset ###
#string <- c("Hoax", "HOAX", "hoax")
df_duplicate<-df_duplicate[!grepl("hoax|HOAX|Hoax", df_duplicate$comments),]

# Could have also made this apart of the dplyr sequence above by doing 
#filter(!grepl("HOAX"| "hoax"|"Hoax", comments)) %>%

### Add another column to the dataset (report_delay) and populate with the time difference ###
### in days, between the date of the sighting and the date it was reported. ###
#df3 %>% mutate(report_delay = date.posted - datetime )
df_new_duplicate <- df_duplicate %>%
  mutate(report_delay = date.posted - datetime) %>%
  # mutate(report_delay = difftime(datetime, date.posted, units = "days")) %>%
  # could use this function so r knows you are dealing with time as well as set the until to days 
  ### Filter out the rows where the sighting was reported before it happened. ###
  filter(report_delay != -1)

### Create a table with the average report_delay per country. ###
my_df <- data.frame(country=df_new_duplicate$country, report_delay=df_new_duplicate$report_delay)
all_countries <- unique(my_df$country)


average_time_df <- data.frame(country=character(), average_report_delay=double())

for (country_name in all_countries) {
  my_new_df <- my_df %>% filter(country == country_name)
  average_report_delay <- mean(my_new_df$report_delay)
  typeof(average_report_delay)
  print(nrow(average_time_df))
  average_time_df[nrow(average_time_df) + 1,] <- c(country_name,average_report_delay)
}

# this is my solution below :)
# ufo_cleaned %>%
# group_by(country) %>%
# summarise(Avg_report_delay = mean(report_delay))


### Check the data quality (missingness, format, range etc) of the duration(seconds) column. ###
### Explain what kinds of problems you have identified and how you chose to deal with them, ###
### in your comments
range(df_new_duplicate$duration..seconds.)
# the range is from 0.02 to 52623200, which is very large 
sum(is.na(df_new_duplicate$duration..seconds.))
# no missing values in the data 


### Create a histogram using the duration(seconds) column
duration <- df_new_duplicate$duration..seconds. # unneccessary i think 
hist(log(duration), main="Log of Duration of UFO Sighting", xlab = "Log of Duration (secs)")

# Good job :) :) - added some comments to help simplify your code 
