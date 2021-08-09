# Code 2: Models ----------------------------------------------------------
rm(list = ls())

# 1. Load packages --------------------------------------------------------

install.packages("estimatr") 
pacman::p_load(tidyverse, performance, parameters, estimatr)

# 2. Load data ------------------------------------------------------------
issp <- readRDS(file = "input/data/proc/issp-paper.rds")

# 3. Models ---------------------------------------------------------------
# Define
model <- lm(scale_2 ~ .,
            data = issp, weights = issp$WEIGHT,
            na.action=na.omit)
summary(model)
parameters::model_parameters(model)


#Models 1 without clustered SE
#Model 1 (class_2)
model1 <- lm(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_2 + c_alphan,
            data = issp, weights = issp$WEIGHT,
            na.action=na.omit)
summary(model1)
parameters::model_parameters(model1)

#Model 1A (class_3)
model1A <- lm(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_3 + c_alphan,
             data = issp, weights = issp$WEIGHT,
             na.action=na.omit)
summary(model1A)
parameters::model_parameters(model1A)


#Models 1 WITH clustered SE
#Model 1 (class_2)
model1_cse <-estimatr::lm_robust(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_2 + c_alphan,
                                 data = issp, weights = issp$WEIGHT,
                                 clusters = c_alphan)
summary(model1_cse)
parameters::model_parameters(model1_cse)

#Model 1A (class_3)
model1A_cse <-estimatr::lm_robust(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_3 + c_alphan,
                                 data = issp, weights = issp$WEIGHT,
                                 clusters = c_alphan)
summary(model1A_cse)
parameters::model_parameters(model1A_cse)



# Explore combinations of models (stepwise)
#step(model, keep = nobs)
## best model: scale_2 ~ AGE + TYPORG2 + UNION + class + c_alphan
