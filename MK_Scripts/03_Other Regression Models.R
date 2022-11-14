# ---------------------------------------------------------------------------------- #
# ------------------------------------ Description --------------------------------- #
# ---------------------------------------------------------------------------------- #
# This scripts tests regression models on the accidents dataset
#




# ---------------------------------------------------------------------------------- #
# ------------------------ (Early stages) Create Sample ---------------------------- #
# ---------------------------------------------------------------------------------- #
# Since it's taking forever for me to run things I'm going to use a random sample 
# of 100K
set.seed(1469)
sample = accidents %>% sample_frac(0.25)


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


set.seed(9372)
split_index = createDataPartition(mod_dat$Weighted_Severity, p = .5, 
                                  list = FALSE, 
                                  times = 1)
train = mod_dat[ split_index,]
test = mod_dat[-split_index,]


# ---------------------------------------------------------------------------------- #
# -------------------------------------- LASSO ------------------------------------- #
# ---------------------------------------------------------------------------------- #
lasso = glmnet(train[, -1], 
               train[, 1], 
                   alpha = 1) 

summary(lasso)

set.seed(8493)
cv.out = cv.glmnet(as.matrix(train[, -1]), train[, 1], alpha = 1) 
plot(cv.out)


bestlam_lasso = cv.out$lambda.min 
lasso_pred = predict(lasso, s = bestlam_lasso, newx = as.matrix(test[,-1])) 
mean((lasso_pred - test[,1])^2)

lasso_coef = predict(lasso, type = "coefficients", s = bestlam_lasso)
var_coeffs_lasso = as.data.frame(as.matrix(lasso_coef)) %>%
  rownames_to_column("Variable") %>%
  rename('Coefficient' = 2) %>%
  mutate(Abs_Coefficent = abs(Coefficient)) %>%
  arrange(-Abs_Coefficent)




# ---------------------------------------------------------------------------------- #
# -------------------------------------- Ridge ------------------------------------- #
# ---------------------------------------------------------------------------------- #
# using same grid as before ---
ridge_mod = glmnet(as.matrix(train[, -1]), 
                   train[, 1], alpha=0)

# using lambda = 4 based on example --
ridge_pred = predict(ridge_mod, s = 4, newx = as.matrix(test[, -1]))
mean((ridge_pred - test[,1])^2)


cv.out = cv.glmnet(as.matrix(train[, -1]), 
                   train[, 1], alpha = 0) 
bestlam_ridge = cv.out$lambda.min  
bestlam_ridge
mse_lam_ridge = plot(cv.out)
ggsave(file = "../Data/Output/Images/Other Regression Models/Model 1b_Ridge_MSE vs Lambda.png", mse_lam_ridge)

ridge_coef = predict(ridge_mod, type = "coefficients", s = bestlam_ridge)
var_coeffs_ridge = as.data.frame(as.matrix(ridge_coef)) %>%
  rownames_to_column("Variable") %>%
  rename('Coefficient' = 2) %>%
  mutate(Abs_Coefficent = abs(Coefficient)) %>%
  arrange(-Abs_Coefficent)



# ---------------------------------------------------------------------------------- #
# ------------------------------------ Export Data --------------------------------- #
# ---------------------------------------------------------------------------------- #
write.csv(summary(lasso), '../Data/Output/Model 1a_LASSO_First Pass.csv')
write.csv(var_coeffs_lasso, '../Data/Output/Model 1a_LASSO_Variable Importance.csv')

write.csv(summary(ridge_mod), '../Data/Output/Model 1b_Ridge_First Pass.csv')
write.csv(var_coeffs_ridge, '../Data/Output/Model 1b_Ridge_Variable Importance.csv')


