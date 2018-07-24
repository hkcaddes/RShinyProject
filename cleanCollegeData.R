# clean college dataset
library(tidyverse)
library(data.table)
library(openxlsx)


# read file
allData <- fread("./data/Most-Recent-Cohorts-All-Data-Elements.csv")

# read datadictionary
dataDict <- read.xlsx("./data/CollegeScorecardDataDictionary.xlsx", sheet = 4)

# 1. Filter out schools that we don't want ####

# filter out schools that don't award bachelors degrees and that are distance only
  # PREDDEG == 3 : predominantly bachelor's degree granting
  # DISTANCEONLY == 0 : schools that are not distance only
data1 <- allData %>% filter(PREDDEG == 3, DISTANCEONLY == 0) %>% arrange(INSTNM) # now we only have 2082 schools

# filter out schools that are not the main campus
data2 <- data1 %>% filter(MAIN == 1) %>% arrange(INSTNM)




# 2. create new data frame that we'll add each column we want to ####

# we'll start with data1 and see if necessary to filter out non-main campus schools


# 2.1 start with school info columns

  # OPEID: 8-digit OPE ID for institution
  # OPEID6: 6-digit OPE ID for institution (1 per school)
  # INSTNM: institution name
  # CITY: city
  # STABBR: state abbreviation
  # INSTURL: URL for institution's homepage
  # MAIN: flag for main campus (MAIN == 1 for main camups)
  # NUMBRANCH: number of branches of the campus
  # CONTROL: control of instituion (0 = Public, 1 = private nonprofit, 2 = private forprofit)
  # LATITUDE
  # LONGITUDE

schools.df <- data.frame(ope8_id = data1$OPEID, ope6_id = data1$OPEID6,
                         name = data1$INSTNM, city = data1$CITY,
                         state = data1$STABBR, school_url = data1$INSTURL,
                         main_campus = data1$MAIN, branches = data1$NUMBRANCH,
                         lat = data1$LATITUDE, long = data1$LONGITUDE,
                         ownership = data1$CONTROL) %>% arrange(name)

# 2.2 add more location data
  # REGION: see dataDict
  # LOCALE: locale of institution

schools.df$region_id = data1$REGION
schools.df$locale = data1$LOCALE

# 2.3 add size of schools

schools.df$size = data1$UGDS

# 2.4 add cost of tuition & fees

# average cost of attendance (academic year institutions)
schools.df$attendance.academic_year = data1$COSTT4_A

# in and out of state tuition and fees
schools.df$tuition.in_state = data1$TUITIONFEE_IN
schools.df$tuition.out_of_state = data1$TUITIONFEE_OUT

# 2.5 add admissions and test score data (25th percentiles)

schools.df$admission_rate.overall = data1$ADM_RATE

schools.df$sat_scores.25th_percentile.critical_reading = data1$SATVR25
schools.df$sat_scores.25th_percentile.math = data1$SATMT25
schools.df$sat_scores.25th_percentile.writing = data1$SATWR25

schools.df$act_scores.25th_percentile.cumulative = data1$ACTCM25

# 2.5 add completion and retention rate data

schools.df$completion_rate_4yr_150nt = data1$C150_4
schools.df$retention_rate.four_year.full_time = data1$RET_FT4

# 2.6 add debt and loan data

schools.df$median_debt.completers.overall = data1$GRAD_DEBT_MDN
schools.df$federal_loan_rate = data1$PCTFLOAN

# 2.7 add specialized interest (i.e. historically black college)

schools.df$minority_serving.historically_black = data1$HBCU
schools.df$minority_serving.tribal = data1$TRIBAL
schools.df$men_only = data1$MENONLY
schools.df$women_only = data1$WOMENONLY

# 2.8 religious affiliation

schools.df$religious_affiliation = data1$RELAFFIL

# 2.9 demographics

schools.df$demographics.race_ethnicity.white = data1$UGDS_WHITE
schools.df$demographics.race_ethnicity.black = data1$UGDS_BLACK
schools.df$demographics.race_ethnicity.hispanic = data1$UGDS_HISP
schools.df$demographics.race_ethnicity.asian = data1$UGDS_ASIAN
schools.df$demographics.race_ethnicity.aian = data1$UGDS_AIAN
schools.df$demographics.race_ethnicity.nhpi = data1$UGDS_NHPI

schools.df$demographics.men = data1$UGDS_MEN
schools.df$demographics.women = data1$UGDS_WOMEN

# part-time
schools.df$part_time_share = data1$PPTUG_EF

# 2.10 Earnings

schools.df$six_years_after_entry.median = data1$MD_EARN_WNE_P6
schools.df$six_years_after_entry.working_not_enrolled.earnings_percentile.10 = data1$PCT10_EARN_WNE_P6
schools.df$six_years_after_entry.working_not_enrolled.earnings_percentile.25 = data1$PCT25_EARN_WNE_P6
schools.df$six_years_after_entry.working_not_enrolled.earnings_percentile.75 = data1$PCT75_EARN_WNE_P6
schools.df$six_years_after_entry.working_not_enrolled.earnings_percentile.90 = data1$PCT90_EARN_WNE_P6
schools.df$six_years_after_entry.working_not_enrolled.std_dev = data1$SD_EARN_WNE_P6

# 2.11 Bachelor's Degree programs offered

schools.df$program.bachelors.agriculture = data1$CIP01BACHL
schools.df$program.bachelors.architecture = data1$CIP04BACHL
schools.df$program.bachelors.ethnic_cultural_gender = data1$CIP05BACHL
schools.df$program.bachelors.communication = data1$CIP09BACHL
schools.df$program.bachelors.communications_technology = data1$CIP10BACHL
schools.df$program.bachelors.computer = data1$CIP11BACHL
schools.df$program.bachelors.personal_culinary = data1$CIP12BACHL
schools.df$program.bachelors.education = data1$CIP13BACHL
schools.df$program.bachelors.engineering = data1$CIP14BACHL
schools.df$program.bachelors.engineering_technology = data1$CIP15BACHL
schools.df$program.bachelors.language = data1$CIP16BACHL
schools.df$program.bachelors.legal = data1$CIP22BACHL
schools.df$program.bachelors.english = data1$CIP23BACHL
schools.df$program.bachelors.humanities = data1$CIP24BACHL
schools.df$program.bachelors.biological = data1$CIP26BACHL
schools.df$program.bachelors.mathematics = data1$CIP27BACHL
schools.df$program.bachelors.military = data1$CIP29BACHL
schools.df$program.bachelors.multidiscipline = data1$CIP30BACHL
schools.df$program.bachelors.philosophy_religious = data1$CIP38BACHL
schools.df$program.bachelors.theology_religious_vocation = data1$CIP39BACHL
schools.df$program.bachelors.physical_science = data1$CIP40BACHL
schools.df$program.bachelors.science_technology = data1$CIP41BACHL
schools.df$program.bachelors.psychology = data1$CIP42BACHL
schools.df$program.bachelors.public_administration_social_service = data1$CIP44BACHL
schools.df$program.bachelors.social_science = data1$CIP45BACHL
schools.df$program.bachelors.construction = data1$CIP46BACHL
schools.df$program.bachelors.transportation = data1$CIP49BACHL
schools.df$program.bachelors.visual_performing = data1$CIP50BACHL
schools.df$program.bachelors.health = data1$CIP51BACHL
schools.df$program.bachelors.business_marketing = data1$CIP52BACHL
schools.df$program.bachelors.history = data1$CIP54BACHL


# Clean up the numbers
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

# 3. Write into a csv ####
fwrite(schools.df, "./data/SchoolsCleaned.csv")



# 4. customize the data dictionary for my data

library(zoo)

# replace na valuse with value above it

myDict <- data.frame(element_name = na.locf(dataDict$NAME.OF.DATA.ELEMENT),
                     variable_name = na.locf(dataDict$`developer-friendly.name`),
                     data_type = na.locf(dataDict$API.data.type),
                     value = dataDict$VALUE,
                     label = dataDict$LABEL,
                     source = dataDict$SOURCE,
                     notes = dataDict$NOTES) 

# change some variable names to my convention for easy filtering

myDict$variable_name <- gsub("6_yrs", "six_years", myDict$variable_name)
myDict$variable_name <- gsub("location\\.","", myDict$variable_name)
myDict$variable_name <- gsub("lon","long", myDict$variable_name)

# now filter out any variable_names that aren't in names(schools.df)

myDict <- myDict %>% filter(variable_name %in% names(schools.df))

# save csv file for later

fwrite(myDict, "./data/schoolDict.csv")

# Now we're ready to build the map!