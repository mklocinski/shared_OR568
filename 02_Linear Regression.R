# ---------------------------------------------------------------------------------- #
# ------------------------------------ Description --------------------------------- #
# ---------------------------------------------------------------------------------- #
# This script constructs basic linear regressions for exploratory purposes.
# 



# ---------------------------------------------------------------------------------- #
# --------------------------------- Pre-processing --------------------------------- #
# ---------------------------------------------------------------------------------- #
# Filter ut_accidents to the explanatory variables that were sorted into variable
# types 1, 2, or 3 (see 00_Variable Analysis.r) and response variable Weighted_Severity.
mod_dat = ut_accidents %>%
  select(Weighted_Severity, all_of(ut_var_type_1), all_of(ut_var_type_2), 
        all_of(ut_var_type_3)) %>%
  select(-Time, -Date) %>%
  # Remove all rows with null values
        na_if(-1) %>%
        na.omit() %>%
  # Convert categorical columns to factor for encoding
        mutate_at(intersect(c(ut_var_type_1, ut_var_type_2, ut_var_type_3), ut_cat_vars), as.factor) %>%
  # Convert categorical columns to dummy
        dummy_columns() %>%
  # Remove unencoded categorical columns
        select_if(is.numeric)
  
mod_mat_dims = dim(mod_dat)

# ---------------------------------------------------------------------------------- #
# -------------------------------- Linear Regression ------------------------------- #
# ---------------------------------------------------------------------------------- #
lm1 = lm(Weighted_Severity ~., data = mod_dat)

# View summary
summary(lm1)

# Take a look at p-values
data.frame(Predictors = names(summary(lm1)$coefficients[,4]), 
           P_Values = unname(summary(lm1)$coefficients[,4])) %>%
  filter(P_Values < 0.05) %>%
  ggplot(aes(x = reorder(Predictors, P_Values), y = P_Values)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = P_Values), hjust = -0.2) +
  labs(x = 'P-Values', y = '', title = 'Predictors with P-Values less than 0.05')
ggsave("../Data/Output/Images/Linear Regression/Model 1 - P-Values.png")



# ---------------------------------------------------------------------------------- #
# --------------------------------- Model Assessment ------------------------------- #
# ---------------------------------------------------------------------------------- #

# ----------------------------------- Independence --------------------------------- #
# Using 35 as the max lag periods because that was the number used in the 
# autocorrelation plot in Descriptive Analysis
acf(lm1$residuals, lag.max = 35)

# ------------------------------------ Linearity ----------------------------------- #
res_fit = plot(lm1,1)
ggsave("../Data/Output/Images/Linear Regression/Model 1_Residuals vs Fitted.png", res_fit)

act_fit = plot(lm1$fitted.values, lm1$model$Weighted_Severity)
ggsave("../Data/Output/Images/Linear Regression/Model 1_Actuals vs Fitted.png", act_fit)


# -------------------------------- Multi-collinearity ------------------------------ #
vif(lm1)
alias(lm1)

# --------------------------------- Homoscedasticity ------------------------------- #
scale_loc = plot(lm1, 3)
ggsave("../Data/Output/Images/Linear Regression/Model 1_Scale Location.png", scale_loc)

# ------------------------------ Multivariate normality ---------------------------- #
qq = plot(lm1, 2)
ggsave("../Data/Output/Images/Linear Regression/Model 1_Q Q.png", qq)

res_hist = data.frame(Residuals = unname(lm1$residuals)) %>%
  ggplot(aes(x = Residuals)) +
  geom_histogram(bins = ut_sturges(data.frame(Residuals = unname(lm1$residuals)))) +
  labs(y = '', title = 'Model 1: Linear Regression', subtitle = 'Residual Histogram')
ggsave("../Data/Output/Images/Linear Regression/Model 1_Residual Histogram.png", res_hist)




# ---------------------------------------------------------------------------------- #
# ------------------------------------ Export Data --------------------------------- #
# ---------------------------------------------------------------------------------- #
write.csv(tidy(summary(lm1)), '../Data/Output/Model 1_Multivariate Regression_First Pass.csv')
write.csv(data.frame(Predictors = names(summary(lm1)$coefficients[,4]), 
                     P_Values = unname(summary(lm1)$coefficients[,4])), 
          '../Data/Output/Model 1_Multivariate Regression_Variable Importance.csv')
# ---------------------------------------------------------------------------------- #
# ------------------------------------- Clean up ----------------------------------- #
# ---------------------------------------------------------------------------------- #
to_remove = sapply(ls(), function(x) !grepl('^ut_', x))
rm(list = ls()[to_remove])











