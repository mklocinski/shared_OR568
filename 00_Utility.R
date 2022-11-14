# ---------------------------------------------------------------------------------- #
# ----------------------------------- Description ---------------------------------- #
# ---------------------------------------------------------------------------------- #
# This utility script loads all libraries, imports all data sets, and creates all
# user-defined functions that will be used by the rest of the scripts in this project. 
# It also runs all of the other scripts in the project. 
# 
# Note that many of the functions and objects have the prefix "ut_." This identifies
# them as significant to the project scripts and prevents them from being deleted
# during the clean-up section of each scripts (which removes helper lists and 
# variables to keep things neat).



# ---------------------------------------------------------------------------------- #
# --------------------------------- Load Libraries --------------------------------- #
# ---------------------------------------------------------------------------------- #
library(tidyverse)
library(lubridate)
library(stringi)
library(gridExtra)
library(caret)
library(glmnet)
library(corrplot)
library(RColorBrewer)
library(boot)
library(broom)
library(fastDummies)
library(zoo)
library(DescTools)
library(car)
library(randomForest)

# ---------------------------------------------------------------------------------- #
# ------------------------------ User-defined Functions ---------------------------- #
# ---------------------------------------------------------------------------------- #

# -------------------------------- For Data Cleaning ------------------------------- #
# - Clean column names for easier lookup in data dictionary
ut_clean_columns_names = function(column_name){
  
  # Replace parentheses and dashes with periods
  c = stri_replace_all_coll(trimws(column_name), c('(', ')', '-'), '.', vectorize_all = F)
  # Add X before column names starting with a number
  c = ifelse(grepl('[0-9]', substring(c, 1, 1)), paste0('X', c), c)
  # Add underscores
  c = gsub(' ', '_', c)
  return(c)
  
}

# - Read in all sheets from data dictionary file
ut_read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sapply(sheets, ut_clean_columns_names)
  x
}

# - Update column names to align with data dictionary for easier look up
ut_update_names = function(df, lookup_table){
  
  n = names(df)
  to_rename = sapply(names(lookup_table), 
                     function(x) sapply(x, 
                                        function(i) n[grepl(i, n)], USE.NAMES = F))
  
  to_rename = to_rename[lapply(to_rename, length) > 0]
  
  print(paste("Number renamed:", length(to_rename)))
  return(df %>% 
           rename_at(vars(names(df[unlist(to_rename, use.names = F)])), ~ names(to_rename)))
  
}

# - Look up categorical variable's code value in data dictionary
ut_lookup_value = function(lookup_table, column, value){
  
  if(value == -1){
    
    val = 'Null'
    
  } else{
    
    val = filter(lookup_table[[column]], code == value) %>%
      select(label)
    
  }
  
  
  return(val[[1]])
  
}


# --------------------------- General Data Management ----------------------------- #
# - Identify date variables
ut_get_date_vars = function(data, expected_date_format){
  
  check = sapply(names(data), 
                 function(x) is.na(as.Date(as.character(data[1, x]), 
                                           paste(guess_formats(data[1, x], 
                                                               expected_date_format), 
                                                 collapse = "|"))))
  return(names(data)[check])  
}

# - Identify time variables
# do this judgmentally within Descriptive Statistics.r

# - Identify categorical variables
ut_get_cat_vars = function(data, lookup_table){
  
  init_check = intersect(names(data), names(lookup_table))
  check = sapply(init_check, function(x) !grepl("[A-Za-z]", data[1, x]))
  return(init_check[check])
}

# - Identify location variables
# do this judgmentally within Descriptive Statistics.r

# - Identify numeric variables
ut_get_numeric_vars = function(data, cat_var_list, location_var_list){
  
  no_cats = setdiff(names(data), cat_var_list)
  no_cats_locs = setdiff(no_cats, location_var_list)
  check = sapply(no_cats_locs, function(x) ifelse(class(data[, x]) %in% c('numeric', 'integer'), 
                                                  TRUE, FALSE))
  
  return(no_cats_locs[check])
}

# - Identify ID variables
ut_get_id_vars = function(data, date_var_list, time_var_list) {
  
  no_dates = setdiff(names(data), date_var_list)
  no_dates_time = setdiff(no_dates, time_var_list)
  check = sapply(no_dates_time, function(x) ifelse(class(data[, x]) %in% c('factor'), 
                                                   TRUE, FALSE))
  return(no_dates_time[check])
}

# --------------------------- Model Pipeline Management ---------------------------- #


# ------------------------------- Data Visualization ------------------------------- #
# - Plot completeness for each column
ut_completeness_plot = function(df){
  
  
  accident_completeness = df
  accident_completeness[accident_completeness == -1] = NA
  
  complete_percents = accident_completeness  %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
    summarise(across(everything(), ~ round(.x/nrow(accident_completeness), 4))) 
  
  data.frame(Column = names(complete_percents), Missing = unlist(unname(complete_percents))) %>%
    ggplot(aes(x = reorder(Column, Missing), y = Missing)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    coord_flip() +
    geom_text(aes(label = scales::percent(Missing)), size = 4, hjust = -0.25, colour = "darkgrey") +
    labs(x = '', title = 'Percent missing in "accidents" dataset, by column')
  
}

# - Plot uniqueness for each column
ut_uniqueness_plot = function(df){
  
  unique_percents = df %>%
    summarise(across(everything(), ~ length(unique(.x)))) %>% 
    summarise(across(everything(), ~ round(.x/nrow(df), 4))) 
  
  
  data.frame(Column = names(unique_percents), Unique = unlist(unname(unique_percents))) %>%
    ggplot(aes(x = reorder(Column, Unique), y = Unique)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    coord_flip() +
    geom_text(aes(label = scales::percent(Unique)), size = 4, hjust = -0.25, colour = "darkgrey") +
    labs(x = '', title = 'Percent unique in "accidents" dataset, by column')
  
  
}

# - Plot boxplots for numeric columns
ut_create_boxplot = function(df, column){
  
  ggplot(data = df, aes(y = .data[[column]])) +
    geom_boxplot() +
    labs(title = column) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank())
}

# - Calculate optimal number of bins using Sturges' Rule
ut_sturges = function(dataset){
  
  sr = ceiling(log2(nrow(dataset)) + 1)
  
  return(sr)
  
}

# - Plot histograms for numeric columns
ut_create_histogram = function(df, column){
  
  ggplot(data = df, aes(x = .data[[column]])) +
    geom_histogram(bins = ut_sturges(df)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = column)
}

# - Plot bar charts for numeric and categorical columns
ut_create_barchart = function(df, column){
  
  grpd =  df %>%
    group_by_(var = column) %>%
    summarize(Count = n()) %>%
    mutate(lab = sapply(var, function(x) ut_lookup_value(ut_data_lookup, column, trimws(x))))  %>% 
    ungroup()
  
  ggplot(data = grpd, aes(x = reorder(lab, Count), y = Count)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    labs(x = '', y = '', title = column)
  
  
}

# - Plot bar charts for numeric and categorical columns using response as y-value
ut_create_barchart_with_response = function(df, column, response){
  
  grpd =  df %>%
    group_by_(var = column) %>%
    summarize(Average= mean(!!sym(response))) %>%
    mutate(lab = sapply(var, function(x) ut_lookup_value(ut_data_lookup, column, trimws(x))))  %>% 
    ungroup()
  
  ggplot(data = grpd, aes(x = reorder(lab, Average), y = Average)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    labs(x = '', y = '', title = paste(column, 'by Average Weighted Severity'))
  
}

# - Plot line charts of time columns
ut_time_line_chart = function(df, column){
  
  grpd = df %>%
    group_by(var = hour(!!sym(column))) %>%
    summarize(Count = n()) %>%
    ungroup()
  
  ggplot(data = grpd, aes(x = var, y = Count)) +
    geom_line() +
    geom_vline(xintercept  = 7, linetype = 'longdash', size =2) +
    geom_vline(xintercept  = 9, linetype = 'longdash', size =2) +
    geom_vline(xintercept  = 16, linetype = 'longdash', size =2) +
    geom_vline(xintercept  = 18, linetype = 'longdash', size =2) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = 'Hour of Day', title = 'Count of accidents by hour of day') 
  
}

# - Plot the line charts of date columns
ut_date_line_chart = function(df, column, monthview = T){
  
  
  if(monthview == T){
    
    grpd = df %>%
      mutate(var = month(!!sym(column))) %>%
      group_by(var) %>%
      summarize(Count = n()) %>%
      ungroup()
    
    
    ggplot(data = grpd, aes(x = var, y = Count)) +
      geom_line() +
      scale_x_continuous(breaks = seq_along(month.name), 
                         labels = month.name) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = 'Month of Year', title = 'Count of accidents by month of year') 
    
    
  } else{
    
    grpd = df %>%
      mutate(var = as.yearmon(!!sym(column))) %>%
      group_by(var) %>%
      summarize(Count = n()) %>%
      ungroup()
    
    
    ggplot(data = grpd, aes(x = var, y = Count)) +
      geom_line() +
      scale_x_yearmon() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = '', title = 'Count of accidents over time') 
    
  }

}

# - Plot line charts of time columns using response as y-value
ut_time_line_chart_with_response = function(df, column, response){
  
  grpd = df %>%
    group_by(var = hour(!!sym(column))) %>%
    summarize(Average = mean(!!sym(response))) %>%
    ungroup()
  
  ggplot(data = grpd, aes(x = var, y = Average)) +
    geom_line() +
    geom_vline(xintercept  = 7, linetype = 'longdash', size =2) +
    geom_vline(xintercept  = 9, linetype = 'longdash', size =2) +
    geom_vline(xintercept  = 16, linetype = 'longdash', size =2) +
    geom_vline(xintercept  = 18, linetype = 'longdash', size =2) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = 'Hour of Day', y = '', title = 'Average Weighted Severity by hour of day') 
  
}

# - Plot the line charts of date columns
ut_date_line_chart_with_response = function(df, column, response, monthview = T){
  
  
  if(monthview == T){
    
    grpd = df %>%
      mutate(var = month(!!sym(column))) %>%
      group_by(var) %>%
      summarize(Average = mean(!!sym(response))) %>%
      ungroup()
    
    
    ggplot(data = grpd, aes(x = var, y = Average)) +
      geom_line() +
      scale_x_continuous(breaks = seq_along(month.name), 
                         labels = month.name) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = 'Month of Year', title = 'Average Weighted Severity by month of year') 
    
    
  } else{
    
    grpd = df %>%
      mutate(var = as.yearmon(!!sym(column))) %>%
      group_by(var) %>%
      summarize(Average = mean(!!sym(response))) %>%
      ungroup()
    
    
    ggplot(data = grpd, aes(x = var, y = Average)) +
      geom_line() +
      scale_x_yearmon() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = '', title = 'Average Weighted Severity over time') 
    
  }
  
}



# ---------------------------------------------------------------------------------- #
# ----------------------------------- Import Data ---------------------------------- #
# ---------------------------------------------------------------------------------- #

ut_accidents = read.csv('../Data/Accidents0514.csv', fileEncoding = 'UTF-8-BOM')

ut_vehicles = read.csv('../Data/Vehicles0514.csv', fileEncoding = 'UTF-8-BOM')
# ut_vehicles = ut_vehicles %>% sample_frac(0.25)

ut_casualties = read.csv('../Data/Casualties0514.csv', fileEncoding = 'UTF-8-BOM')
# ut_casualties = ut_casualties %>% sample_frac(0.25)

ut_data_lookup = ut_read_excel_allsheets('../Data/Road-Accident-Safety-Data-Guide.xls')



# ---------------------------------------------------------------------------------- #
# ------------------------------------ Clean Data ---------------------------------- #
# ---------------------------------------------------------------------------------- #
# - Rename columns in accidents, vehicles, and casualties to allow for simple
#   lookup of their categorical variables' code labels
names(ut_data_lookup)[c(13, 14, 20)] = c('Pedestrian_Crossing.Human_Control', 
                             'Pedestrian_Crossing.Physical_Facilities', 
                             'Urban_or_Rural_Area')


print("Rename 'accidents' dataset --")
ut_accidents = ut_update_names(ut_accidents, ut_data_lookup)
print("Rename 'casualties' dataset --")
ut_casualties = ut_update_names(ut_casualties, ut_data_lookup)
print("Rename 'vehicles' dataset --")
ut_vehicles = ut_update_names(ut_vehicles, ut_data_lookup)


# Convert string Time to hm time
ut_accidents$Time = hm(ut_accidents$Time)

# Convert string Date to lubridate
ut_accidents$Date = dmy(ut_accidents$Date) 



# ---------------------------------------------------------------------------------- #
# ---------------------------- Create Response Variable ---------------------------- #
# ---------------------------------------------------------------------------------- #
ut_accidents = ut_accidents %>%
                left_join(ut_casualties %>% select(Accident_Index, Casualty_Severity), 
                          on = 'Accident_Index') %>%
                mutate(Weighted_Casualty_Severity = Casualty_Severity/Number_of_Casualties, 
                       Weighted_Severity = Accident_Severity + Weighted_Casualty_Severity)

histogram(ut_accidents$Weighted_Severity)

rm(ut_casualties)
rm(ut_vehicles)

set.seed(1469)
ut_accidents = ut_accidents %>% sample_frac(0.25)

# ---------------------------------------------------------------------------------- #
# -------------------------------- Variable Grouping ------------------------------- #
# ---------------------------------------------------------------------------------- #
# Possible determinants of crash likelihood
# + Traffic volume
# > For information on what "road class" means, see: https://www.gov.uk/government/publications/guidance-on-road-classification-and-the-primary-route-network/guidance-on-road-classification-and-the-primary-route-network#chapter3
# > Time variables are included here because traffic volume usually corresponds with time
#   and day of week
# > Based on the completeness graphs from 00_Descriptive Statistics.r X2nd_Road_Class and
#   Junction_Control are being excluded due to their high number of nulls. There will
#   not be too much information loss since there are other columns with similar info
ut_var_type_1 = c('X1st_Road_Class', 'Urban_or_Rural_Area', 
                  'Date', 'Day_of_Week', 'Time')
# + Navigational Complexity (curves, restricted sight lines, road surface)
ut_var_type_2 = c('Junction_Detail', 'Pedestrian_Crossing.Human_Control',
                  'Pedestrian_Crossing.Physical_Facilities', 'Special_Conditions_at_Site', 
                  'Carriageway_Hazards', 'Speed_limit', 'Road_Type', 'Road_Surface')
# + Visibility (lighting, inclement weather)
ut_var_type_3 = c('Light_Conditions', 'Weather')
# + Driver factors (demographics, impairment, distractions, malfunctioning vehicle)
ut_var_type_4 = c()
# + Accident Outcomes
ut_var_type_5 = c('Accident_Severity', 'Number_of_Vehicles', 'Number_of_Casualties', 
                  'Casualty_Severity', 'Weighted_Casualty_Severity', 'Weighted_Severity')
# + Location-specific factors (things that may contribute to an environment that is 
#                             more or less conducive to accidents, such as expectations
#                             around traffic law enforcement, regional culture, etc.)
ut_var_type_6 = c('Location_Easting_OSGR', 'Location_Northing_OSGR', 
                  'Longitude', 'Latitude', 'Police_Force', 'Local_Authority_.District.',
                  'Local_Authority_.Highway.', 'LSOA_of_Accident_Location', 'Police_Officer_Attend', 
                  'X1st_Road_Number')
# + ID and index info
ut_var_type_7 = c('Accident_Index')


# Does this account for all ut_accident variables?
ncol(ut_accidents) == sum(length(ut_var_type_1), length(ut_var_type_2), length(ut_var_type_3), length(ut_var_type_4), 
                          length(ut_var_type_5), length(ut_var_type_6), length(ut_var_type_7))

# ---------------------------------------------------------------------------------- #
# ----------------------------------- Run Scripts ---------------------------------- #
# ---------------------------------------------------------------------------------- #
# start = Sys.time()
# print("> Running descriptive statistics --")
# source('00_Descriptive Statistics.r')
# print(paste(">>", Sys.time() - start))

#start = Sys.time()
#print("> Running basic regression --")
#source('00_Linear Regression.r')
#print(paste(">>", Sys.time() - start))

# start = Sys.time()
# print("> Running LASSO, Ridge, and Elastic Net --")
# source('00_Regression Models.r')
# print(paste(">>", Sys.time() - start))

#start = Sys.time()
#print("> Running regression model comparisons --")
#source('00_Regression Comparisons.Rmd')
#print(paste(">>", Sys.time() - start))


# ---------------------------------------------------------------------------------- #
# ----------------------------------- Clean Up ------------------------------------- #
# ---------------------------------------------------------------------------------- #
