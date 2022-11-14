# ---------------------------------------------------------------------------------- #
# ------------------------------------ Description --------------------------------- #
# ---------------------------------------------------------------------------------- #
# This script generates box, histogram, series, autocorrelation, and correlation plots 
# for all non-ID and coordinate variables in the 'ut_accidents' data set. It also 
# conducts some data quality checks, specifically for completeness and uniqueness. 

# ---------------------------------------------------------------------------------- #
# -------------------------------- Variable Grouping ------------------------------- #
# ---------------------------------------------------------------------------------- #
ut_time_vars = c('Time')
ut_location_vars = c('Latitude', 'Longitude', 'Location_Easting_OSGR', 'Location_Northing_OSGR')
ut_cat_vars = ut_get_cat_vars(ut_accidents, ut_data_lookup)
ut_date_vars = ut_get_date_vars(ut_accidents, 'Ymd')
ut_id_vars = ut_get_id_vars(ut_accidents, ut_date_vars, ut_time_vars)
ut_numeric_vars = ut_get_numeric_vars(ut_accidents, ut_cat_vars, ut_location_vars)


# ---------------------------------------------------------------------------------- #
# ----------------------------------- Data Quality --------------------------------- #
# ---------------------------------------------------------------------------------- #
# - ut_accidents: Completeness
ut_completeness_plot(ut_accidents)
ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Completeness.png")

# - ut_accidents: Uniqueness
ut_uniqueness_plot(ut_accidents)
ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Uniqueness.png")


# ---------------------------------------------------------------------------------- #
# ------------------------------------- Box Plots ---------------------------------- #
# ---------------------------------------------------------------------------------- #
# - Boxplots were created for all numeric variables
boxplot_list = lapply(ut_numeric_vars, function(x) ut_create_boxplot(ut_accidents, x))
boxplots = grid.arrange(grobs = boxplot_list, ncol=2)

for(i in boxplot_list){
  
  ggsave(file = paste0("../Data/Output/Images/Descriptive Analysis/Accidents - Box Plots - ",
                       i$labels$title,
                       ".png"), 
         i)
  
}



# ---------------------------------------------------------------------------------- #
# ------------------------------------ Histograms ---------------------------------- #
# ---------------------------------------------------------------------------------- #
histogram_list = lapply(ut_numeric_vars, function(x) ut_create_histogram(ut_accidents, x))
histograms = grid.arrange(grobs = histogram_list, ncol=2)

for(i in histogram_list){
  
  ggsave(file = paste0("../Data/Output/Images/Descriptive Analysis/Accidents - Histograms - ",
                       i$labels$title,
                       ".png"), 
         i)
  
}
# ---------------------------------------------------------------------------------- #
# ------------------------------------ Bar Charts ---------------------------------- #
# ---------------------------------------------------------------------------------- #
barchart_list = lapply(ut_cat_vars, function(x) ut_create_barchart(ut_accidents, x))
barcharts = grid.arrange(grobs = barchart_list, ncol=3)
names(barchart_list) = ut_cat_vars

for(i in barchart_list){
  
  ggsave(file = paste0("../Data/Output/Images/Descriptive Analysis/Accidents - Bar Charts (Frequency) - ",
                       i$labels$title,
                       ".png"), 
         i)

}

# ---------------------------------------------------------------------------------- #
# ----------------------------- Bar Charts With Response --------------------------- #
# ---------------------------------------------------------------------------------- #
barchart_resp_list = lapply(ut_cat_vars, 
                            function(x) ut_create_barchart_with_response(ut_accidents, 
                                                                         x, 
                                                                         'Weighted_Severity'))
barcharts_resp = grid.arrange(grobs = barchart_resp_list, ncol=2)
names(barchart_list) = ut_cat_vars

for(i in barchart_resp_list){
  
  ggsave(file = paste0("../Data/Output/Images/Descriptive Analysis/Accidents - Bar Charts (Weighted Severity) - ",
                       i$labels$title,
                       ".png"), 
         i)
  
}
# ---------------------------------------------------------------------------------- #
# ---------------------------------- Time Charts ----------------------------------- #
# ---------------------------------------------------------------------------------- #
timechart_list = lapply(ut_time_vars, function(x) ut_time_line_chart(ut_accidents, x))
timecharts = grid.arrange(grobs = timechart_list, ncol=2)
ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Frequency by Hour of Day.png", timecharts)


# ---------------------------------------------------------------------------------- #
# -------------------------- Time Charts with Response ----------------------------- #
# ---------------------------------------------------------------------------------- #
timechart_resp_list = lapply(ut_time_vars, 
                             function(x) ut_time_line_chart_with_response(ut_accidents, 
                                                                          x, 
                                                                          'Weighted_Severity'))
timecharts_resp = grid.arrange(grobs = timechart_resp_list, ncol=2)

ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Weighted Severity by Hour of Day.png", timecharts_resp)


# ---------------------------------------------------------------------------------- #
# --------------------------- Date Charts by Month --------------------------------- #
# ---------------------------------------------------------------------------------- #
datechart_list = lapply(ut_date_vars, function(x) ut_date_line_chart(ut_accidents, x))
datecharts = grid.arrange(grobs = datechart_list, ncol=2)

ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Frequency by Month.png", datecharts)


# ---------------------------------------------------------------------------------- #
# --------------------------- Date Charts Over Time -------------------------------- #
# ---------------------------------------------------------------------------------- #
datechart_list = lapply(ut_date_vars, function(x) ut_date_line_chart(ut_accidents, x, 
                                                                     monthview = F))
datecharts = grid.arrange(grobs = datechart_list, ncol=2)

ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Frequency Over Time.png", datecharts)



# ---------------------------------------------------------------------------------- #
# --------------------- Date Charts by Month with Response ------------------------- #
# ---------------------------------------------------------------------------------- #
datechart_resp_list = lapply(ut_date_vars, 
                             function(x) ut_date_line_chart_with_response(ut_accidents, 
                                                            x, 
                                                            'Weighted_Severity'))
datecharts_resp = grid.arrange(grobs = datechart_resp_list, ncol=2)

ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Weighted Severity by Month.png", datecharts)


# ---------------------------------------------------------------------------------- #
# --------------------- Date Charts Over Time with Response ------------------------ #
# ---------------------------------------------------------------------------------- #
datechart_resp_list = lapply(ut_date_vars, 
                             function(x) ut_date_line_chart_with_response(ut_accidents, 
                                                                          x, 
                                                                          'Weighted_Severity', 
                                                                          monthview = F))
datecharts_resp = grid.arrange(grobs = datechart_resp_list, ncol=2)

ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents - Weighted Severity Over Time.png", datecharts)




# ---------------------------------------------------------------------------------- #
# ------------------------------- Autocorrelation ---------------------------------- #
# ---------------------------------------------------------------------------------- #
ts = data.frame(All_Dates = seq(min(ut_accidents$Date),max(ut_accidents$Date), by = '1 day')) %>%
  left_join(ut_accidents %>%
              group_by(Date) %>%
              summarize(Count = n()), by = c('All_Dates' = 'Date')) %>%
  select(Count) %>%
  acf(., by = '1 day', pl = F)

daily = data.frame(Period = seq(0, nrow(ts$acf) - 1, 1), 
                   AC = ts$acf) %>%
  ggplot(aes(x = Period, y = AC)) + 
  geom_bar(stat = 'identity', width = 0.1) +
  labs(y = '', title = 'Accidents Autocorrelation')


ggsave(file = "../Data/Output/Images/Descriptive Analysis/Accidents Autocorrelation.png", daily)



# ---------------------------------------------------------------------------------- #
# ------------------------------------ Corr Plots ---------------------------------- #
# ---------------------------------------------------------------------------------- #

# ------------------------------------- Numeric ------------------------------------ #

num_mat = ut_accidents %>%
  select(Number_of_Vehicles, Number_of_Casualties, Accident_Severity, Speed_limit)
  
num_cor = corrplot(cor(num_mat), method = 'number', 
                   title = '\nCorrelation Plot - Numeric Variables (Pearson)')

ggsave(file = "../Data/Output/Images/Descriptive Analysis/Correlation Plot - Numeric.png", num_cor)


# ----------------------------------- Categorical ---------------------------------- #

cat_mat = ut_accidents %>%
  mutate_at(ut_cat_vars, as.factor) %>%
  select(all_of(ut_cat_vars))

M = PairApply(cat_mat, CramerV)
cat_cor = corrplot(M, method = 'number', 
                   title = '\nCorrelation Plot - Categorical Variables (Cramer)') 

ggsave(file = "../Data/Output/Images/Descriptive Analysis/Correlation Plot - Categorical.png", cat_cor)




# ---------------------------------------------------------------------------------- #
# ------------------------------------- Clean up ----------------------------------- #
# ---------------------------------------------------------------------------------- #
to_remove = sapply(ls(), function(x) !grepl('^ut_', x))
rm(list = ls()[to_remove])







