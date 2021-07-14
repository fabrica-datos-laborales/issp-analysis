# Code 2: Models ----------------------------------------------------------
rm(list = ls())

# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data ------------------------------------------------------------
issp <- readRDS(file = "input/data/proc/issp-paper.rds")

# 3. Models ---------------------------------------------------------------
# Define
model <- lm(scale_2 ~ ., data = issp, weights = issp$"WEIGHT")

summary(model)

# Explore combinations of models (stepwise)
step(model)


