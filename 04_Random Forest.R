# ---------------------------------------------------------------------------------- #
# ------------------------------------ Description --------------------------------- #
# ---------------------------------------------------------------------------------- #
# This script constructs random forest models
# 



# ---------------------------------------------------------------------------------- #
# --------------------------------- Pre-processing --------------------------------- #
# ---------------------------------------------------------------------------------- #
# Filter ut_accidents to the explanatory variables that were sorted into variable
# types 1, 2, or 3 (see 00_Variable Analysis.r) and response variable Weighted_Severity.
mod_dat = ut_accidents %>%
  sample_frac(0.25) %>%
  select(Weighted_Severity, all_of(ut_var_type_1), all_of(ut_var_type_2), 
         all_of(ut_var_type_3)) %>%
  select(-Time, -Date) %>%
  # Remove all rows with null values
  na_if(-1) %>%
  na.omit() %>%
  # Convert categorical columns to factor for encoding
  mutate_at(intersect(c(ut_var_type_1, ut_var_type_2, ut_var_type_3), ut_cat_vars), as.factor) 
  # Convert categorical columns to dummy
  #dummy_columns() %>%
  # Remove unencoded categorical columns
 # select_if(is.numeric)

mod_mat_dims = dim(mod_dat)

set.seed(9372)
split_index = createDataPartition(mod_dat$Weighted_Severity, p = .2, 
                                  list = FALSE, 
                                  times = 1)
train = mod_dat[ split_index,]
test = mod_dat[-split_index,]



# ---------------------------------------------------------------------------------- #
# ---------------------------------- Random Forest --------------------------------- #
# ---------------------------------------------------------------------------------- #
# Model with more specifications (like asking it to include var importance)
rf2 = randomForest(Weighted_Severity~., data=train, ntree=1000,
                   importance=TRUE, proximity=TRUE)

print(rf2)

# Tree with lowest MSE
which.min(rf2$mse)
rf2$mse[which.min(rf2$mse)]

error_red = data.frame(Trees = seq(1, length(rf2$mse), 1), MSE = rf2$mse) %>%
  ggplot(aes(x = Trees, y = MSE)) +
  geom_line() +
  labs(x ='# Trees', x = 'MSE', title = 'Model 2: Random Forest', subtitle = 'MSE versus # Trees')
  
ggsave(file = "../Data/Output/Images/Random Forest/Model 2_MSE vs Number of Trees.png",error_red)


# Based on https://hackernoon.com/random-forest-regression-in-r-code-and-interpretation
# Get variable importance from the model fit
varImpPlot(rf2)

ImpData <- as.data.frame(importance(rf2))
ImpData$Var.Names <- row.names(ImpData)


var_imp = ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  labs(x ='', x = 'MSE', title = 'Model 2: Random Forest', subtitle = 'First Pass, no transformations') +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(file = "../Data/Output/Images/Random Forest/Model 2_Variable Importance.png", var_imp)


# ----------------------------------- Tuning rf2 ----------------------------------- #
model_tuned <- tuneRF(
  x=mod_dat[,-1], 
  # choosing 4 starting parameters because of the results of the variable importance
  # plot: Day_of_Week, Junction_Detail, X1st_Road_Class, and Weather all had relatively
  # high node purity scores and low MSEs
  mtryStart=4, 
  # Choosing 100 trees 1) to reduce time spent on these initial runs and 2) because
  # when looking at the MSE vs number of trees chart it looks like an "elbow" occurs
  # at around 50. 
  ntreeTry=100,
  # selected because it's the default I see in examples
  stepFactor=1.5,
  # selected because it's the default I see in examples
  improve=0.01,
  trace=FALSE #don't show real-time progress
)
# ---------------------------------------------------------------------------------- #
# --------------------------------- Model Assessment ------------------------------- #
# ---------------------------------------------------------------------------------- #

# ---------------------------------- Test on test ---------------------------------- #

rf2_pred = predict(rf2, test)


# ---------------------------------------------------------------------------------- #
# ------------------------------------ Export Data --------------------------------- #
# ---------------------------------------------------------------------------------- #
write.csv(summary(rf2), '../Data/Output/Model 2_Random Forest_First Pass.csv')
write.csv(ImpData, '../Data/Output/Model 2_Random Forest_Variable Importance.csv')


# ---------------------------------------------------------------------------------- #
# ------------------------------------- Clean up ----------------------------------- #
# ---------------------------------------------------------------------------------- #
to_remove = sapply(ls(), function(x) !grepl('^ut_', x))
rm(list = ls()[to_remove])








