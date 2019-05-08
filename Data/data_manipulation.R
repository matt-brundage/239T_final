# 1. Setup ----

# Clear, set directory, and load packages
rm(list=ls())
setwd("/Users/mattsair/Desktop/Projects/Refugee project/Data")

library(pacman)
p_load(dplyr, knitr, kableExtra, ggplot2, tidyr)








# 2. Refugee data cleaning ----

# 2a. Clean and merge separate datasets ----

# The process for obtaining datasets is as follows:
# Dowload data from http://ireports.wrapsnet.org
# Grab data in two-year increments for all nationalities and destinations
# Data should be ordered "by destination" and downloaded as .csv
# Locally, open each .csv in excel, then save again as .csv
# Otherwise, for some reason, R cannot read the .csv

# The following function cleans each two-year dataset before they are merged into one

data_cleaner <- function(file_name) {
  data <- read.csv(file_name)
  data <- as.data.frame(data)
  colnames(data) <- c("start",
                      "end",
                      "state",
                      "year",
                      "total_state_refugees",
                      "nationality",
                      "total_state_refugees_of_nationality",
                      "city",
                      "arrivals",
                      "total_us_refugees")
  
  # "arrivals" provides the most granular level of data on refugees:
  # total arrivals of refugees of nationality N in city C, state S, in year Y
  
  
  
  # Column cleaning
  
  # Delete all columns that provide no additional information. These include:
  # "start" is the start date of counting. 1/1/2002 for 2002-2003, 1/1/2004 for 2004-2005
  # "end" is the end date of counting. 12/31/2003 for 2002-2003, 12/31/2005 for 2004-2005
  # "total_state_refugees" can be calculated: sum "arrivals" across all cities in state
  # "total_state_refugees_of_nationality" can similarly be calculated using "arrivals"
  # "total_us_refugees" can similarly be calculated using "arrivals"
  
  # The following code keeps only the relevant columns
  
  data <- dplyr::select(data, c("state",
                                "year",
                                "nationality",
                                "city",
                                "arrivals"))
  
  
  
  
  # Row cleaning
  
  # Delete the wholly irrelevant rows
  
  # Irrelevant rows are patterned in the same way in each data set
  # The first set are at the top, identified by a blank row,
  # followed by a row of unhelpful header names, which have been replaced by helpful colnames
  # The second set are at the middle through the end, identified by a blank row,
  # followed by rows of data organized by nationality of origin,
  # rather than by state of destination
  # These data offer no additional information from what is already provided in prior rows
  
  
  # First, store the row numbers with empty rows
  # Delete the rows in the second empty set and below
  empty_row_numbers <- which(data$state == "")
  data <- data[-(empty_row_numbers[2]:nrow(data)), , 
                                   drop = FALSE]
  # Delete the rows in the first empty set, plus one below and all above
  data <- data[-(1:empty_row_numbers[1]), , 
                                   drop = FALSE]
  data <- data[-(1:1), , 
                                   drop = FALSE]
  
  
  # Delete the rows with zero arrivals
  
  # These rows are in the dataset because if there are arrivals from one nationality
  # in Y2 but not Y1 or vice versa, a "0" row is added
  # They are deleted because they cause an inconsistency
  # Example: Data are downloaded in sets: 2002-2003, 2004-2005, etc.
  # If there are arrivals from one nationality in 2005 but not 2004, 2003, 2002,
  # then there will be a 0 row for 2004 but no such row for 2002 or 2003
  # If we ultimately want these 0-rows for all years where there are no arrivals in that
  # city, but there were arrivals in that city in other years, we can add them in later
  
  data <- data[!(data$arrivals==0),]
  

  
  
  # Output is a cleaned dataframe
  
  return(data)
  
}

# Run the function on all datasets

setwd("/Users/mattsair/Desktop/Projects/Refugee project/Data/refugee_data_raw")

data_2002_2003 <- data_cleaner("city_nationality_2002_2003.csv")
data_2004_2005 <- data_cleaner("city_nationality_2004_2005.csv")
data_2006_2007 <- data_cleaner("city_nationality_2006_2007.csv")
data_2008_2009 <- data_cleaner("city_nationality_2008_2009.csv")
data_2010_2011 <- data_cleaner("city_nationality_2010_2011.csv")
data_2012_2013 <- data_cleaner("city_nationality_2012_2013.csv")
data_2014_2015 <- data_cleaner("city_nationality_2014_2015.csv")
data_2016_2017 <- data_cleaner("city_nationality_2016_2017.csv")
data_2018_2019 <- data_cleaner("city_nationality_2018_2019.csv")

# Merge datasets

data <- rbind(data_2002_2003,
              data_2004_2005,
              data_2006_2007,
              data_2008_2009,
              data_2010_2011,
              data_2012_2013,
              data_2014_2015,
              data_2016_2017,
              data_2018_2019)

# Remove the pre-merge two-year datasets now that they've been merged into one

rm(data_2002_2003,
   data_2004_2005,
   data_2006_2007,
   data_2008_2009,
   data_2010_2011,
   data_2012_2013,
   data_2014_2015,
   data_2016_2017,
   data_2018_2019)





# 2b. Post-merge

# Additional cleaning and reorganizing

# Note that additional data "cleaning" is performed outside the data_cleaner function
# so that it is easier to edit.
# Steps taken in the cleaner_function are generally those that are better to address
# before merging.



# Converting objects to their appropriate class

# convert state to character
data$state <- as.character(data$state)
# convert city to character
data$city <- as.character(data$city)
# convert year to numeric
data$year <- gsub('CY ', '', data$year)
data$year <- as.numeric(as.character(data$year))
# convert nationality to character
data$nationality <- as.character(data$nationality)
# convert arrivals to numeric
data$arrivals <- gsub(',', '', data$arrivals)
data$arrivals <- as.numeric(as.character(data$arrivals))



# Ordering and sorting the data

# change order of columns
data <- data[,c("state", "city", "year", "nationality", "arrivals")]
# change sorting to reflect column order
data <- data[with(data, order(state, city, year, nationality)),]

# this is just a stylistic choice for viewing the data in R; another way to get a good sense of
# interesting trends is to order data by number of arrivals (descending)
# data <- data[with(data, order(-arrivals)),]

# rename data refugee_data
refugee_data <- data
rm(data)










# 3. City population data cleaning ----

# Population estimates by city are part of the "Places" data collected by the US Census
# These "places" are determined by state; they often cross county borders,
# and they include a broader set of cities than other Census categories
# This broader set is needed to capture all refugee placement cities
# (some of which are quite small)

setwd("/Users/mattsair/Desktop/Projects/Refugee project/Data/census_data_raw/population/city_2010_2017")
pop_city <- read.csv("PEP_2017_PEPANNRES_with_ann.csv")
# population estimates are given for July 1 of each calendar year

pop_city <- as.data.frame(pop_city)
colnames(pop_city) <- c("geo_id_0",
                    "geo_id",
                    "city_state",
                    "2010_april_census",
                    "2010_april_estimate",
                    "pop_2010",
                    "pop_2011",
                    "pop_2012",
                    "pop_2013",
                    "pop_2014",
                    "pop_2015",
                    "pop_2016",
                    "pop_2017")

# Column cleaning

# drop geo_id_0 and the two 2010 april population columns

pop_city <- dplyr::select(pop_city, 
                                    c("geo_id",
                                      "city_state",
                                      "pop_2010",
                                      "pop_2011",
                                      "pop_2012",
                                      "pop_2013",
                                      "pop_2014",
                                      "pop_2015",
                                      "pop_2016",
                                      "pop_2017"))

# Delete superfluous row
pop_city <- pop_city[-(1:1), , drop = FALSE]

# convert geo_id to numeric
pop_city$geo_id <- as.numeric(as.character(pop_city$geo_id))

# convert city_state to character
pop_city$city_state <- as.character(pop_city$city_state)

# correct city names with tilde n
pop_city[1404,"city_state"] <- "La Canada Flintridge city, California"
pop_city[1717,"city_state"] <- "Canon City city, Colorado"
pop_city[11259,"city_state"] <- "Espanola city, New Mexico"

# split city and state into two columns
pop_city <- separate(data = pop_city, 
                               col = city_state, into = c("city", "state"), sep = "\\,")

# remove the space at the beginning of each state name
pop_city$state <- trimws(pop_city$state)

# convert each population year to numeric
pop_city$pop_2010 <- as.numeric(as.character(pop_city$pop_2010))
pop_city$pop_2011 <- as.numeric(as.character(pop_city$pop_2011))
pop_city$pop_2012 <- as.numeric(as.character(pop_city$pop_2012))
pop_city$pop_2013 <- as.numeric(as.character(pop_city$pop_2013))
pop_city$pop_2014 <- as.numeric(as.character(pop_city$pop_2014))
pop_city$pop_2015 <- as.numeric(as.character(pop_city$pop_2015))
pop_city$pop_2016 <- as.numeric(as.character(pop_city$pop_2016))
pop_city$pop_2017 <- as.numeric(as.character(pop_city$pop_2017))

# create new column for compound annual growth rate
# to be used as a multiplier to estimate city population in 2002-2009; 2018-2019
pop_city$pop_cagr <- (pop_city$pop_2017 / 
                                    pop_city$pop_2010)^(1/7)

# create new columns for these population estimates
# round to the nearest person and establish a floor at 1 person

#2018
pop_city$pop_2018 <- 
  ifelse(
    round(pop_city$pop_2017*(pop_city$pop_cagr)^1,0) > 1, 
    round(pop_city$pop_2017*(pop_city$pop_cagr)^1,0), 
    1)

#2019
pop_city$pop_2019 <- 
  ifelse(
    round(pop_city$pop_2017*(pop_city$pop_cagr)^2,0) > 1, 
    round(pop_city$pop_2017*(pop_city$pop_cagr)^2,0), 
    1)

#2009
pop_city$pop_2009 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-1),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-1),0), 
    1)

#2008
pop_city$pop_2008 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-2),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-2),0), 
    1)

#2007
pop_city$pop_2007 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-3),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-3),0), 
    1)

#2006
pop_city$pop_2006 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-4),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-4),0), 
    1)

#2005
pop_city$pop_2005 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-5),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-5),0), 
    1)

#2004
pop_city$pop_2004 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-6),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-6),0), 
    1)

#2003
pop_city$pop_2003 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-7),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-7),0), 
    1)

#2002
pop_city$pop_2002 <- 
  ifelse(
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-8),0) > 1, 
    round(pop_city$pop_2010*(pop_city$pop_cagr)^(-8),0), 
    1)

# Ordering and sorting the pop_city data

# change order of columns
pop_city <- 
  pop_city[,
                     c("city", "state", "geo_id", 
                       "pop_2002", 
                       "pop_2003",
                       "pop_2004", 
                       "pop_2005",
                       "pop_2006", 
                       "pop_2007",
                       "pop_2008", 
                       "pop_2009",
                       "pop_2010", 
                       "pop_2011",
                       "pop_2012", 
                       "pop_2013",
                       "pop_2014", 
                       "pop_2015",
                       "pop_2016", 
                       "pop_2017",
                       "pop_2018", 
                       "pop_2019",
                       "pop_cagr"
                       )]







# 4. State population data cleaning ----

# 4.a. 2000-2010 ----

setwd("/Users/mattsair/Desktop/Projects/Refugee project/Data/census_data_raw/population/state_2000_2010")
pop_state_2000 <- read.csv("st-est00int-01.csv")

# population estimates are given for July 1 of each calendar year
pop_state_2000 <- as.data.frame(pop_state_2000)
colnames(pop_state_2000) <- c("state",
                              "2000_april_estimate",
                              "pop_2000",
                              "pop_2001",
                              "pop_2002",
                              "pop_2003",
                              "pop_2004",
                              "pop_2005",
                              "pop_2006",
                              "pop_2007",
                              "pop_2008",
                              "pop_2009",
                              "2010_april_estimate",
                              "pop_2010")

# Column cleaning

# drop geo_id_0 and the two 2010 april population columns

pop_state_2000 <- dplyr::select(pop_state_2000, 
                                c("state",
                                  "pop_2000",
                                  "pop_2001",
                                  "pop_2002",
                                  "pop_2003",
                                  "pop_2004",
                                  "pop_2005",
                                  "pop_2006",
                                  "pop_2007",
                                  "pop_2008",
                                  "pop_2009",
                                  "pop_2010"))

# Delete superfluous rows
pop_state_2000 <- pop_state_2000[-(62:69), , drop = FALSE]
pop_state_2000 <- pop_state_2000[-(60:60), , drop = FALSE]
pop_state_2000 <- pop_state_2000[-(1:8), , drop = FALSE]

# convert state to character; remove "." in state name
pop_state_2000$state <- as.character(pop_state_2000$state)
pop_state_2000$state <- gsub('[.]', '', pop_state_2000$state)

# convert populations to numeric
pop_state_2000$pop_2000 <- gsub(',', '', pop_state_2000$pop_2000)
pop_state_2000$pop_2001 <- gsub(',', '', pop_state_2000$pop_2001)
pop_state_2000$pop_2002 <- gsub(',', '', pop_state_2000$pop_2002)
pop_state_2000$pop_2003 <- gsub(',', '', pop_state_2000$pop_2003)
pop_state_2000$pop_2004 <- gsub(',', '', pop_state_2000$pop_2004)
pop_state_2000$pop_2005 <- gsub(',', '', pop_state_2000$pop_2005)
pop_state_2000$pop_2006 <- gsub(',', '', pop_state_2000$pop_2006)
pop_state_2000$pop_2007 <- gsub(',', '', pop_state_2000$pop_2007)
pop_state_2000$pop_2008 <- gsub(',', '', pop_state_2000$pop_2008)
pop_state_2000$pop_2009 <- gsub(',', '', pop_state_2000$pop_2009)
pop_state_2000$pop_2010 <- gsub(',', '', pop_state_2000$pop_2010)

pop_state_2000$pop_2000 <- as.numeric(as.character(pop_state_2000$pop_2000))
pop_state_2000$pop_2001 <- as.numeric(as.character(pop_state_2000$pop_2001))
pop_state_2000$pop_2002 <- as.numeric(as.character(pop_state_2000$pop_2002))
pop_state_2000$pop_2003 <- as.numeric(as.character(pop_state_2000$pop_2003))
pop_state_2000$pop_2004 <- as.numeric(as.character(pop_state_2000$pop_2004))
pop_state_2000$pop_2005 <- as.numeric(as.character(pop_state_2000$pop_2005))
pop_state_2000$pop_2006 <- as.numeric(as.character(pop_state_2000$pop_2006))
pop_state_2000$pop_2007 <- as.numeric(as.character(pop_state_2000$pop_2007))
pop_state_2000$pop_2008 <- as.numeric(as.character(pop_state_2000$pop_2008))
pop_state_2000$pop_2009 <- as.numeric(as.character(pop_state_2000$pop_2009))
pop_state_2000$pop_2010 <- as.numeric(as.character(pop_state_2000$pop_2010))




# 4.b. 2011-2019 ----

# data include 2010, but this column is dropped because it is included already in the previous dataset
# 2019 data are projected

setwd("/Users/mattsair/Desktop/Projects/Refugee project/Data/census_data_raw/population/state_2010_2018")
pop_state_2011 <- read.csv("nst-est2018-alldata.csv")

# population estimates are given for July 1 of each calendar year
pop_state_2011 <- as.data.frame(pop_state_2011)

# delete superfluous columns and rows
pop_state_2011 <- pop_state_2011[, -(17:136), drop = FALSE]
pop_state_2011 <- pop_state_2011[, -(6:8), drop = FALSE]
pop_state_2011 <- pop_state_2011[, -(1:4), drop = FALSE]
pop_state_2011 <- pop_state_2011[-(1:5), , drop = FALSE]

#rename columns
colnames(pop_state_2011) <- c("state",
                              "pop_2011",
                              "pop_2012",
                              "pop_2013",
                              "pop_2014",
                              "pop_2015",
                              "pop_2016",
                              "pop_2017",
                              "pop_2018")

# Column cleaning

# convert state to character; remove "." in state name
pop_state_2011$state <- as.character(pop_state_2011$state)

# convert populations to numeric
pop_state_2011$pop_2011 <- gsub(',', '', pop_state_2011$pop_2011)
pop_state_2011$pop_2012 <- gsub(',', '', pop_state_2011$pop_2012)
pop_state_2011$pop_2013 <- gsub(',', '', pop_state_2011$pop_2013)
pop_state_2011$pop_2014 <- gsub(',', '', pop_state_2011$pop_2014)
pop_state_2011$pop_2015 <- gsub(',', '', pop_state_2011$pop_2015)
pop_state_2011$pop_2016 <- gsub(',', '', pop_state_2011$pop_2016)
pop_state_2011$pop_2017 <- gsub(',', '', pop_state_2011$pop_2017)
pop_state_2011$pop_2018 <- gsub(',', '', pop_state_2011$pop_2018)

pop_state_2011$pop_2011 <- as.numeric(as.character(pop_state_2011$pop_2011))
pop_state_2011$pop_2012 <- as.numeric(as.character(pop_state_2011$pop_2012))
pop_state_2011$pop_2013 <- as.numeric(as.character(pop_state_2011$pop_2013))
pop_state_2011$pop_2014 <- as.numeric(as.character(pop_state_2011$pop_2014))
pop_state_2011$pop_2015 <- as.numeric(as.character(pop_state_2011$pop_2015))
pop_state_2011$pop_2016 <- as.numeric(as.character(pop_state_2011$pop_2016))
pop_state_2011$pop_2017 <- as.numeric(as.character(pop_state_2011$pop_2017))
pop_state_2011$pop_2018 <- as.numeric(as.character(pop_state_2011$pop_2018))

# create new column for compound annual growth rate 
# to be used as a multiplier to estimate state population in 2019
pop_state_2011$pop_cagr <- (pop_state_2011$pop_2018 / 
                        pop_state_2011$pop_2011)^(1/7)

# create new columns for these population estimates
# round to the nearest person and establish a floor at 1 person

#2019
pop_state_2011$pop_2019 <- 
  ifelse(
    round(pop_state_2011$pop_2018*(pop_state_2011$pop_cagr)^1,0) > 1, 
    round(pop_state_2011$pop_2018*(pop_state_2011$pop_cagr)^1,0), 
    1)




# 4.c. Combine into 2000-2019 ----

pop_state <- merge(pop_state_2000, pop_state_2011, 
                   by=c("state"), 
                   all.x = TRUE)

rm(pop_state_2000, pop_state_2011)















# 5. Refugee-population merging ----

# 5.a. Merging state population data ----

refugee_data <- merge(refugee_data, pop_state, 
                   by=c("state"), 
                   all.x = TRUE)

# change order of columns
refugee_data <- 
  refugee_data[,
           c("state", "city", "year", "nationality", "arrivals",
             "pop_2000",
             "pop_2001",
             "pop_2002", 
             "pop_2003",
             "pop_2004", 
             "pop_2005",
             "pop_2006", 
             "pop_2007",
             "pop_2008", 
             "pop_2009",
             "pop_2010", 
             "pop_2011",
             "pop_2012", 
             "pop_2013",
             "pop_2014", 
             "pop_2015",
             "pop_2016", 
             "pop_2017",
             "pop_2018", 
             "pop_2019",
             "pop_cagr"
           )]

# create a column that reflects state population in the corresponding year for each row of arrivals
state_pop <- rep(NA, nrow(refugee_data))
state_pop <- as.data.frame(state_pop)

for (i in 1:nrow(refugee_data)){
  col_number = refugee_data[i,3] - 2000 + 6 # column number for matching to correct population year
  state_pop[i,1]<- refugee_data[i,col_number]
}

refugee_data <- cbind(refugee_data,state_pop)

# create a column for average state pop, 2002-2019

refugee_data$state_pop_avg <- round((refugee_data$pop_2002 +
  refugee_data$pop_2003 +
  refugee_data$pop_2004 +
  refugee_data$pop_2005 +
  refugee_data$pop_2006 +
  refugee_data$pop_2007 +
  refugee_data$pop_2008 +
  refugee_data$pop_2009 +
  refugee_data$pop_2010 +
  refugee_data$pop_2011 +
  refugee_data$pop_2012 +
  refugee_data$pop_2013 +
  refugee_data$pop_2014 +
  refugee_data$pop_2015 +
  refugee_data$pop_2016 +
  refugee_data$pop_2017 +
  refugee_data$pop_2018 +
  refugee_data$pop_2019) / 18, 0)



# delete all other population columns
refugee_data <- refugee_data[,c("state", "city", "year", "nationality", 
                                "arrivals", "state_pop", "state_pop_avg")]

#delete datasets no longer needed
rm(state_pop, pop_state)






# 5.b. Merging city population data ----

# 5.b.i. City name adjustments ----

# City population data can only be merged by city name, for which there are inconsistencies
# in naming between the census data and the refugees data
# There are no geocodes provided with the refugee data, so we can only merge by name
# Some manual cleaning and judgement calls on which names are which are needed


# make data and pop_city mergeable by city name
# establish consistency in naming
refugee_data$city <- gsub('St. ', 'Saint ', refugee_data$city)
pop_city$city <- gsub('St. ', 'Saint ', pop_city$city)

# remove "city", "town", "village" from the census names; this takes care of >80% of matching problems
# make other similar removals
pop_city$city <- gsub(' city', '', pop_city$city)
pop_city$city <- gsub(' town', '', pop_city$city)
pop_city$city <- gsub(' village', '', pop_city$city)
pop_city$city <- gsub(' unified', '', pop_city$city)
pop_city$city <- gsub(' consolidated', '', pop_city$city)
pop_city$city <- gsub(' metropolitan', '', pop_city$city)
pop_city$city <- gsub(' metro', '', pop_city$city)
pop_city$city <- gsub(' government', '', pop_city$city)
# trim blank space on either end of city names
pop_city$city <- trimws(pop_city$city)


# to address the remaining issues, find the refugee data rows with "NA"s for population in the merge
# there are too many cases to manually address all (at least for now); but:
# Manual adjustments cities with at least one year with 50+ arrivals of at least one nationality:

test_data <- merge(refugee_data, pop_city, 
                   by=c("state","city"), 
                   all.x = TRUE)

test_data <- test_data[is.na(test_data$pop_2002),]
test_data <- test_data[test_data$arrivals>49,]
unmatched_cities <- as.data.frame(unique(test_data$city))

# for the time being, I only change names where I am near-certain that the refugee_data city name and
# the pop_city city name refer to the same place, based on corroborating evidence from other sources

# Other code is left in comments to be addressed at a later time
# I prioritized the cities with the largest absolute populations

pop_city$city[pop_city$city == "Anchorage municipality" & pop_city$state == "Alaska"] <-
  "Anchorage"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Antelope"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Carmichael"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Granada Hills"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "La Crescenta"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "North Highlands"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Northridge"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Reseda"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Spring Valley"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Sunland"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <- 
#  "Tarzana"

#pop_city$city[pop_city$city == "" & pop_city$state == "California"] <-
#  "Tujunga"

#pop_city$city[pop_city$city == "Opa-locka" & pop_city$state == "Florida"] <-
#  "Opa-Locka"

pop_city$city[pop_city$city == "Boise City" & pop_city$state == "Idaho"] <-
  "Boise"

pop_city$city[pop_city$city == "Indianapolis (balance)" & pop_city$state == "Indiana"] <-
  "Indianapolis"

pop_city$city[pop_city$city == "Lexington-Fayette urbanounty" & pop_city$state == "Kentucky"] <-
  "Lexington"

pop_city$city[pop_city$city == "Louisville/Jefferson County (balance)" & pop_city$state == "Kentucky"] <-
  "Louisville"

#pop_city$city[pop_city$city == "" & pop_city$state == ""] <-
#  "Lanham"

pop_city$city[pop_city$city == "Riverdale Park" & pop_city$state == "Maryland"] <-
  "Riverdale"

#pop_city$city[pop_city$city == "" & pop_city$state == ""] <-
#  "Silver Spring"

#pop_city$city[pop_city$city == "" & pop_city$state == ""] <-
#  "Dorchester"

#pop_city$city[pop_city$city == "" & pop_city$state == ""] <-
#  "South Boston"

pop_city$city[pop_city$city == "West Springfield Town" & pop_city$state == "Massachusetts"] <-
  "West Springfield"

#pop_city$city[pop_city$city == "" & pop_city$state == "Michigan"] <-
#  "Clinton Township"

#pop_city$city[pop_city$city == "" & pop_city$state == "Michigan"] <-
#  "Shelby Township"

#pop_city$city[pop_city$city == "" & pop_city$state == "Michigan"] <-
#  "West Bloomfield"

#pop_city$city[pop_city$city == "" & pop_city$state == "New York"] <-
#  "Bronx"

#pop_city$city[pop_city$city == "" & pop_city$state == "New York"] <-
#  "Brooklyn"

#pop_city$city[pop_city$city == "" & pop_city$state == "New York"] <-
#  "Rego Park"

#pop_city$city[pop_city$city == "" & pop_city$state == "New York"] <-
#  "Staten Island"

#pop_city$city[pop_city$city == "" & pop_city$state == "Tennessee"] <-
#  "Antioch"

pop_city$city[pop_city$city == "Nashville-Davidson (balance)" & pop_city$state == "Tennessee"] <-
  "Nashville"

#pop_city$city[pop_city$city == "" & pop_city$state == ""] <-
#  "Colchester"

#pop_city$city[pop_city$city == "" & pop_city$state == ""] <-
#  "Henrico"









# 5.b.ii. Merging ----

refugee_data <- merge(refugee_data, pop_city, 
                      by=c("state", "city"), 
                      all.x = TRUE)

# change order of columns
refugee_data <- 
  refugee_data[,
               c("state", "city", "year", "nationality", "arrivals",
                 "pop_2002", 
                 "pop_2003",
                 "pop_2004", 
                 "pop_2005",
                 "pop_2006", 
                 "pop_2007",
                 "pop_2008", 
                 "pop_2009",
                 "pop_2010", 
                 "pop_2011",
                 "pop_2012", 
                 "pop_2013",
                 "pop_2014", 
                 "pop_2015",
                 "pop_2016", 
                 "pop_2017",
                 "pop_2018", 
                 "pop_2019",
                 "pop_cagr",
                 "geo_id",
                 "state_pop",
                 "state_pop_avg")]

# create a column that reflects city population in the corresponding year for each row of arrivals
city_pop <- rep(NA, nrow(refugee_data))
city_pop <- as.data.frame(city_pop)

for (i in 1:nrow(refugee_data)){
  col_number = refugee_data[i,3] - 2002 + 6 # column number for matching to correct population year
  city_pop[i,1]<- refugee_data[i,col_number]
}

refugee_data <- cbind(refugee_data,city_pop)

# create a column for average city pop, 2002-2019

refugee_data$city_pop_avg <- round((refugee_data$pop_2002 +
                                       refugee_data$pop_2003 +
                                       refugee_data$pop_2004 +
                                       refugee_data$pop_2005 +
                                       refugee_data$pop_2006 +
                                       refugee_data$pop_2007 +
                                       refugee_data$pop_2008 +
                                       refugee_data$pop_2009 +
                                       refugee_data$pop_2010 +
                                       refugee_data$pop_2011 +
                                       refugee_data$pop_2012 +
                                       refugee_data$pop_2013 +
                                       refugee_data$pop_2014 +
                                       refugee_data$pop_2015 +
                                       refugee_data$pop_2016 +
                                       refugee_data$pop_2017 +
                                       refugee_data$pop_2018 +
                                       refugee_data$pop_2019) / 18, 0)


# delete all other population columns
refugee_data <- refugee_data[,c("state", "city", "year", "nationality", "arrivals", 
                                "city_pop",
                                "city_pop_avg",
                                "state_pop",
                                "state_pop_avg")]

#delete datasets no longer needed
rm(city_pop, pop_city)













# 6. City latitude and longitude ----

# City latitude and longitude data come from https://simplemaps.com/data/us-cities
# I am not extremely confident in the data (e.g., they assign each city to only one county,
# and we know that cities can and often do span multiple counties)

# However, for the immediate purposes of mapping, they seem sufficiently reliable
# The threat of misidentifying the lat/long of some cities does not threaten empirical analysis
# One measure of the data being reasonably reliable: it's used by Fortune 500 companies

setwd("/Users/mattsair/Desktop/Projects/Refugee project/Data/census_data_raw/population/cities_lat_long")
city_lat_lng <- read.csv("uscitiesv1.5.csv")

# change colnames for merge and keep only relevant columns
colnames(city_lat_lng)[colnames(city_lat_lng)=="state_name"] <- "state"
city_lat_lng <- city_lat_lng[,c("state", "city", "lat", "lng")]

# compile list of cities without a successful lat and lng merge
test_data_2 <- merge(refugee_data, city_lat_lng,
                     by=c("state", "city"),
                     all.x = TRUE)

test_data_2 <- test_data_2[is.na(test_data_2$lat.x),]
test_data_2 <- test_data_2[test_data_2$arrivals>49,]
unmatched_cities_2 <- as.data.frame(unique(test_data_2$city))

# to address the remaining issues, find the refugee data rows with "NA"s for population in the merge
# there are too many cases to manually address all (at least for now); but:
# Manual adjustments cities with at least one year with 50+ arrivals of at least one nationality:

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "California"] <-
#  "Granada Hills"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "California"] <-
#  "Northridge"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "California"] <-
#  "Reseda"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "California"] <-
#  "Sunland"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "California"] <-
#  "Tarzana"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "California"] <-
#  "Tujunga"

city_lat_lng$city[city_lat_lng$city == "Opa-locka" & city_lat_lng$state == "Florida"] <-
  "Opa-Locka"

city_lat_lng$city[city_lat_lng$city == "Riverdale Park" & city_lat_lng$state == "Maryland"] <-
  "Riverdale"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "Massachusetts"] <-
#  "Dorchester"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "Massachusetts"] <-
#  "South Boston"

city_lat_lng$city[city_lat_lng$city == "Clinton" & city_lat_lng$state == "Michigan"] <-
  "Clinton Township"

city_lat_lng$city[city_lat_lng$city == "Shelby" & city_lat_lng$state == "Michigan"] <-
  "Shelby Township"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "Michigan"] <-
#  "West Bloomfield"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "New York"] <-
#  "Rego Park"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "Tennessee"] <-
#  "Antioch"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "Vermont"] <-
#  "Colchester"

#city_lat_lng$city[city_lat_lng$city == "" & city_lat_lng$state == "Virginia"] <-
#  "Henrico"




# merge

# merge by state and city
refugee_data <- merge(refugee_data, city_lat_lng, 
                      by=c("state", "city"), 
                      all.x = TRUE)

rm(city_lat_lng, test_data, test_data_2, unmatched_cities, unmatched_cities_2)







# Export dataset ----

write.csv(refugee_data, 
          "/Users/mattsair/Desktop/Projects/Refugee project/Data/refugee_data_clean/refugee_data_clean.csv")

write.table(refugee_data, 
            "/Users/mattsair/Desktop/Projects/Refugee project/Data/refugee_data_clean/refugee_data_clean.txt", sep="\t")







