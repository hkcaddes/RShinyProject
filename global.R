## global.R code ##

library(data.table)
library(dplyr)
library(shiny)

# read cleaned college scoreboard data
schools.df = fread("data/SchoolsCleaned.csv")
schools.df[schools.df == "NULL"] <- NA
schools.df$size = as.numeric(schools.df$size)
schools.df$attendance.academic_year = as.numeric(schools.df$attendance.academic_year)
schools.df$admission_rate.overall = as.numeric(schools.df$admission_rate.overall)
schools.df$sat_scores.25th_percentile.critical_reading = as.numeric(schools.df$sat_scores.25th_percentile.critical_reading)
schools.df$sat_scores.25th_percentile.math = as.numeric(schools.df$sat_scores.25th_percentile.math)
schools.df$act_scores.25th_percentile.cumulative = as.numeric(schools.df$act_scores.25th_percentile.cumulative)
schools.df$median_debt.completers.overall = as.numeric(schools.df$median_debt.completers.overall)
schools.df$six_years_after_entry.median = as.numeric(schools.df$six_years_after_entry.median)
schools.df$tuition.in_state = as.numeric(schools.df$tuition.in_state)
schools.df$tuition.out_of_state = as.numeric(schools.df$tuition.out_of_state)
schools.df$completion_rate_4yr_150nt = as.numeric(schools.df$completion_rate_4yr_150nt)
schools.df$federal_loan_rate = as.numeric(schools.df$federal_loan_rate)
#schools.df$religious_affiliation = ifelse(schools.df$religious_affiliation %in% c(22, 33, 36, 37, 38, 39, 102))

# read cleaned college scoreboard data dictionary
myDict = fread("data/schoolDict.csv")


# jitter the location data
schools.df$lat = jitter(schools.df$lat)
schools.df$long = jitter(schools.df$long)

locales <- c("Large")
locale_values <- myDict %>% filter(variable_name == "locale") %>% select(value)





