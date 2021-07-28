# Code 2: Models ----------------------------------------------------------
rm(list = ls())

# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, performance, parameters)

# 2. Load data ------------------------------------------------------------
issp <- readRDS(file = "input/data/proc/issp-paper.rds")

# 3. Models ---------------------------------------------------------------
# Define
model <- lm(scale_2 ~ .,
            data = issp, weights = issp$WEIGHT,
            na.action=na.omit)

model2 <- lm(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class + c_alphan,
            data = issp, weights = issp$WEIGHT,
            na.action=na.omit)

parameters::model_parameters(model)

parameters::model_parameters(model2)

# Explore combinations of models (stepwise)
#step(model, keep = nobs)
## best model: scale_2 ~ AGE + TYPORG2 + UNION + class + c_alphan
