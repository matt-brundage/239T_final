# Setup ----

# Clear, set directory
rm(list=ls())
setwd("/Users/mattsair/Desktop/Projects/Refugee project/Data/refugee_data_clean")

refugee_data <- 
  read.delim("~/Desktop/Projects/Refugee project/Data/refugee_data_clean/refugee_data_clean.txt")

# Convert objects to their appropriate class
refugee_data$state <- as.character(refugee_data$state)
refugee_data$city <- as.character(refugee_data$city)
refugee_data$year <- as.numeric(refugee_data$year)
refugee_data$nationality <- as.character(refugee_data$nationality)
refugee_data$arrivals <- as.numeric(refugee_data$arrivals)
refugee_data$city_pop <- as.numeric(refugee_data$city_pop)
refugee_data$city_pop_avg <- as.numeric(refugee_data$city_pop_avg)
refugee_data$state_pop <- as.numeric(refugee_data$state_pop)
refugee_data$state_pop_avg <- as.numeric(refugee_data$state_pop_avg)
refugee_data$lat <- as.numeric(refugee_data$lat)
refugee_data$lng <- as.numeric(refugee_data$lng)

# Load relevant packages
library(pacman)
p_load(dplyr, knitr, kableExtra, ggplot2, tidyr, scales)




# 1. New columns ----

# Add columns for refugees of nationality X
# (will be helpful later if interested in the most common nationality of refugee arrivals by location)

nationalities <- unique(refugee_data$nationality)

for(i in 1:length(nationalities)) {
  refugee_data <- mutate(refugee_data, total_country = ifelse(nationality==nationalities[i], arrivals, 0))
  colnames(refugee_data)[length(refugee_data)] <- c(paste0(nationalities[i], "_arrivals"))
}




# 2. Function for creating any subset of interest ----

# Overview of subset options
# Refugee arrivals:
# a. Total or of nationality X?
# b. By city or by state?
# c. In what year or years from 2002 to 2019?
# d. As an absolute amount or as a percentage of the city/state population?
# e. What is the most common nationality of refugee arrivals in this location and year?
# f. What is the minimum city population for a city to be eligble to be mapped?
# g. What is the total number of cities when mapping the top N cities by (subset of) refugee arrivals?




# function for creating desired subset

# the function has default values for all arguments

subset_maker <- function(natlty_input = "",         # e.g. "Iran"
                         city_or_state = "City",    # "City" or "State"
                         year_start = 2002,         # year start
                         year_end = year_end,     # year end (if you want multiple years)
                         abs_or_pct = "Abs",        # "Abs" or "Pct": arrivals as absolute number, or
                                                    # as a percentage of the population?
                         most_common_natlty = 1,    # 1 if you want to calc mcn; 0 otherwise
                         city_pop_min = 5000,       # number input if city (useful if we're looking at
                                                    # top N cities of refugees as pct of city pop)
                         number_cities = 100)       # number input if city
  {
  
  subset_data <- refugee_data
  
  # 1. Nationality
  
  # filter by nationality if one is given
  if (nchar(natlty_input) > 0) {
    subset_data <- filter(subset_data, nationality == natlty_input)
  }
  
  
  # 2. City or state
  
  # CITY
  # aggregate data at the state-city-year level
  # if you filtered by nationality, this step will elicit no further aggregation
  # if you did not filter by nationality and are instead looking for total arrivals,
  # this step aggregates across all nationalities
  vars_for_grouping <- c("state", "city", "year", "city_pop", "state_pop", "lat", "lng")
  # include all "arrivals" (total & total from nationality 1,2,..n) as variables to sum
  variables <- colnames(subset_data[,c(5, 12:133)])
  subset_data <- subset_data %<>%group_by(.dots = vars_for_grouping) %>%
    summarise_at(variables, sum)
  # use city_pop as relevant pop variable
  subset_data$pop <- subset_data$city_pop
  # STATE
  # if state, subset further by state; use state_pop as relevant pop variable
  if (city_or_state == "State"){
    state_vars_for_grouping <- c("state", "year", "state_pop")
    subset_data <- subset_data %<>%group_by(.dots = state_vars_for_grouping) %>%
      summarise_at(variables, sum)
    subset_data$pop <- subset_data$state_pop
  }
  
  
  # 3. Year input
  
  # filter by year if only looking at one year
  if (year_start == year_end){
    subset_data <- filter(subset_data, year == year_start)
  }
  
  # if looking at multiple years, subset as follows
  if (year_start < year_end){
    
    # filter down to relevant years
    subset_data <- filter(subset_data, year >= year_start & year <= year_end)
    # add pop as another variable to summarize; will ultimately calculate mean, but
    # first sum like the others
    # recall that the current list includes all "arrivals" columns
    # recall that "pop" in state pop for state subsets and city pop for city subsets
    variables <- c(variables, "pop")
    
    # calculation is summarized at different levels depending on city vs. state decision
    vars_for_grouping <- c("state", "city", "lat", "lng")
    if (city_or_state == "State"){
      vars_for_grouping <- c("state")
    }

    subset_data <- subset_data %<>%group_by(.dots = vars_for_grouping) %>%
      summarise_at(variables, sum)
    # when looking at multiple years, the mean population across these years 
    # (at the geographic level of interest) replaces the annual pop estimate
    subset_data$pop <- round(subset_data$pop / (year_end - year_start + 1), 0)
  }
  
  
  # 4. Absolute or percent
  
  # Create a common variable of interest, "outcome", which captures the relevant outcome for each subset
  # total arrivals, total arrivals by nationality, absolute or percent arrivals...
  
  subset_data$outcome <- subset_data$arrivals
  # if percent is the outcome of interest, calculate as a fraction of the population of interest
  if (abs_or_pct == "Pct"){
    subset_data$outcome <- subset_data$arrivals / subset_data$pop
  }
  subset_data<- subset(subset_data, select=-c(arrivals))
  
  
  # 5. Most common nationality
  
  # Calculate the most common nationality of refugee arrivals at the level you've aggregated to
  # Sometimes, this is uninteresting (if aggregating to Iran arrivals only, Iran will be most common)
  # But it's an interesting / useful metric if looking at all arrivals
  
  if (most_common_natlty == 1){
    a <- which( colnames(subset_data) == "Somalia_arrivals" )
    b <- a + length(nationalities) - 1
    subset_data$most_common_natlty <- colnames(subset_data[a:b])[max.col(subset_data[a:b],
                                                                              ties.method="first")]
    subset_data$most_common_natlty <- gsub("_arrivals", "", subset_data$most_common_natlty)
  }
  
  
  # 6. City population minimum &
  # 7. Number of cities 
  
  subset_data <- subset_data[with(subset_data, order(-outcome)),]
  if (city_or_state == "City"){
    subset_data <- subset(subset_data, subset_data$pop > city_pop_min)
    if (nrow(subset_data) > number_cities) {
      subset_data <- subset_data[1:number_cities,]
    }
  }
  
  
  # Clean and reorder columns for optimal viewing in R
  # These are now superfluous
  if (exists("state_pop") == TRUE){
    subset_data<- subset(subset_data, select=-c(state_pop))
  }
  if (exists("city_pop") == TRUE){
    subset_data<- subset(subset_data, select=-c(city_pop))
  }
  # Reordering to get the most interesting info to the front
  a <- which( colnames(subset_data) == "Somalia_arrivals" ) - 1
  refcols <- colnames(subset_data[1:a])
  refcols <- c(refcols, "pop", "outcome")
  subset_data <- subset_data[, c(refcols, setdiff(names(subset_data), refcols))]
  
  # Return data
  return(subset_data)
}




# 3. City bubble maps ----

# Download all maps packages that may ultimately prove useful down the line

library(pacman)
p_load(dplyr, tidyverse, data.table, rmarkdown, knitr, tinytex, magrittr,
       gplots,
       viridis, RColorBrewer, scales,
       grDevices, graphics,
       mapproj, ggmap,
       rgdal, tmap, 
       maptools, tmaptools,
       tidycensus, 
       censusapi,
       plotly
)


# USER INPUT: Determine what to map ----

# Conceptual overview of variables

# a. Total or of nationality X?
# b. By city or by state?
# c. In what year or years from 2002 to 2019?
# d. As an absolute amount or as a percentage of the city/state population?
# e. What is the most common nationality of refugee arrivals in this location and year?
# f. What is the minimum city population for a city to be eligble to be mapped?
# g. What is the total number of cities when mapping the top N cities by (subset of) refugee arrivals?


# Variable input instructions

# natlty_input: Enter national, e.g. "Iran." Enter "" if total refugees, irrespective of nationality
# city_or_state: Enter "City" or "State." For Bubble maps, only "City" will work.
# year_start: Enter any year from 2002 to 2019.
# year_end: Enter any year from 2002 to 2019. Can be the same as year_start if you only want one year.
# abs_or_pct: Enter "Abs" or "Pct". Arrivals as absolute number, or as a percentage of the population
# most_common_natlty: 1 if this is of interest; 0 otherwise
# city_pop_min: number input
# number_cities: number input

# once you have completed user input, run R from here down

natlty_input        <- "Syria"
city_or_state       <- "City"
year_start          <- 2016
year_end            <- 2016
abs_or_pct          <- "Abs"
most_common_natlty  <- 1
city_pop_min        <- 50000
number_cities       <- 100

# run subset_maker function with user input data
df <- subset_maker(natlty_input = natlty_input,
                       city_or_state = city_or_state,
                       year_start = year_start,
                       year_end = year_end,
                       abs_or_pct = abs_or_pct,
                       most_common_natlty = most_common_natlty,
                       city_pop_min = city_pop_min,
                       number_cities = number_cities)



# MAPPING the USER INPUT data ----

data_mapper <- function(){
  # create quantiles for colors in bubble maps
  df$o_ranks <- rank(df$outcome, ties.method = "first")
  df$q <- with(df, cut(o_ranks, quantile(o_ranks)))
  levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th"), "Quantile")
  df$q <- as.ordered(df$q)
  
  # establish US map background
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray85"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white")
  )
  
  # create title
  title_abs_or_pct <- ifelse(abs_or_pct == "Abs", "Total", "Percent")
  title_natlty <- ifelse (natlty_input == "", "", natlty_input)
  title_year <- paste0(year_start)
  if (year_start<year_end){
    title_year <- paste0(year_start, "-", year_end)
  }
  
  # create title, as a function of user data inputs
  title <- paste0("Top Cities by ", 
                  title_abs_or_pct, 
                  " ", 
                  "<br />",
                  title_natlty, 
                  " Refugee Arrivals: ",
                  "<br />",
                  title_year)
  
  # move legend to ideal location
  l <- list(
    x = 1, 
    y = 0.5
  )
  
  # Make outcomes on map read as percentages if relevant
  df$outcome_map <- df$outcome
  if (mean(df$outcome) < 1){
    df$outcome_map <- percent(df$outcome)
  }
  
  # Create plot
  
  p <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 25)) %>%
    add_markers(
      x = ~lng, y = ~lat, size = ~outcome, color = ~q,
      hoverinfo = "text", text = ~paste(df$city, ",", df$state, ":", "<br />", 
                                        df$outcome_map, "refugees")) %>%
    layout(title = title, geo = g, legend = l)
    
  p
}

data_mapper()

