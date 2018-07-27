## global.R code ##

library(data.table)
library(tidyverse)
library(shiny)
library(Quandl)

# read cleaned college scoreboard data
schools.df = fread("./data/SchoolsCleaned.csv")
schools.df[schools.df == "NULL"] <- NA
schools.df$admissions = as.numeric(schools.df$admission_rate.overall)



# read cleaned college scoreboard data dictionary
myDict = fread("./data/schoolDict.csv")


# jitter the location data
schools.df$lat = jitter(schools.df$lat)
schools.df$long = jitter(schools.df$long)

# read wage data
wages = full_join(Quandl("FRED/LEU0252917300Q", api_key="p3mtdWs2NjoX3cMkpyVs") %>% rename(hs_wage = Value),
                  Quandl("FRED/LEU0252918500Q", api_key="p3mtdWs2NjoX3cMkpyVs") %>% rename(bach_wage = Value),
                  by = "Date")

# read cpi data
cpi = full_join(Quandl("BLSI/CUSR0000SEEB01", api_key="p3mtdWs2NjoX3cMkpyVs") %>% select(Date = Date, tuition = Value), 
                Quandl("BLSI/CUSR0000SA0", api_key="p3mtdWs2NjoX3cMkpyVs") %>% rename(total = Value),
                by = "Date")

# read debt data
debt = Quandl("FRBNY/HDC_STLOAN", api_key="p3mtdWs2NjoX3cMkpyVs") %>% select(Date = Date, debt_per_capita = `United States`)


# read major data
majors = fread("./data/WagesByMajor2012-2016.csv") %>% select(group = Group, subgroup = Subgroup, degree = Degree, income = `Average PERNP`) %>% arrange(subgroup, group)




