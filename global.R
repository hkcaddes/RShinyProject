## global.R code ##

library(data.table)
library(dplyr)
library(shiny)

# read cleaned college scoreboard data
schools.df = fread("data/SchoolsCleaned.csv")
schools.df[schools.df == "NULL"] <- NA

# read cleaned college scoreboard data dictionary
myDict = fread("data/schoolDict.csv")


# jitter the location data
schools.df$lat = jitter(schools.df$lat)
schools.df$long = jitter(schools.df$long)






