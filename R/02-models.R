# Code 2: Models ----------------------------------------------------------
rm(list = ls())

# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, sjPlot,  performance, parameters, estimatr)
ggplot2::theme_set(theme_sjplot2())

# ¿que es estimatr?

# 2. Load data ------------------------------------------------------------
issp <- readRDS(file = "input/data/proc/issp-paper.rds")

# 3. Models ---------------------------------------------------------------
# Define
model <- lm(scale_2 ~ .,
            data = issp, weights = issp$WEIGHT,
            na.action=na.omit)

#Models 1 without clustered SE
#Model 1 (class_2)
model1 <- lm(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_2 + c_alphan,
             data = issp, weights = issp$WEIGHT, na.action=na.omit)
model2 <- lm(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_2 + c_alphan,
             data = issp, weights = issp$WEIGHT, na.action=na.omit)

parameters::model_parameters(model)

parameters::model_parameters(model2)

# Explore combinations of models (stepwise)
#step(model, keep = nobs)
## best model: scale_2 ~ AGE + TYPORG2 + UNION + class + c_alphan


# #Model 1A (class_3) -----------------------------------------------------
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


# 4. Graphs ---------------------------------------------------------------
ggeffects::ggpredict(model2, terms = c("c_alphan", "class_2")) %>% 
  plot(.,facet = T, color = "greyscale",
       ci.style = "errorbar") +
  coord_flip() +
  labs(title = "", x = "Country code (prefix ISO 3166)", y = "Conflict perception scale")


ggeffects::ggpredict(model2, terms = c("c_alphan[CL, VE, US, MX]", "class_2")) %>% 
  plot(.,facet = T, color = "bw",
       ci.style = "errorbar") +
  coord_flip() +
  labs(title = "", x = "Country code (prefix ISO 3166)", y = "Conflict perception scale")


# Manual graph ------------------------------------------------------------
## Colors
issp <- issp %>% 
  mutate(color_group = ifelse(c_alphan == 'CL', 'red', 'black'))

color_group = setNames(as.character(issp$c_alphan), issp$color_group)

## Ggplot
as.data.frame(ggeffects::ggpredict(model2, terms = c("c_alphan", "class_2"))) %>%
  select(class_2 = group, c_alphan = x, pred = predicted, everything()) %>%
  group_by(class_2) %>% 
  mutate(pred_group = mean(pred)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = c_alphan ,  y = pred, color = class_2)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), stat = "identity") +
  geom_hline(aes(yintercept = mean(pred_group)),
             linetype = "dashed") +
  facet_wrap(~ class_2, nrow = 2, dir = "h") +
  guides(color = F) +
  coord_flip()  +
  scale_color_grey(start = 0.2, end = 0.5) +
  labs(title = "", x = "Country code (prefix ISO 3166)", y = "Conflict perception scale")


# Class without faced -----------------------------------------------------

as.data.frame(ggeffects::ggpredict(model2, terms = c("c_alphan", "class_2"))) %>%
  select(class_2 = group, c_alphan = x, pred = predicted, everything()) %>%
  group_by(class_2) %>% 
  mutate(pred_group = mean(pred)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = c_alphan ,  y = pred, color = class_2)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), stat = "identity") +
  geom_hline(aes(yintercept = mean(pred_group)),
             linetype = "dashed") +
  guides(color = F) +
  coord_flip()  +
  scale_color_grey(start = 0.2, end = 0.5) +
  labs(title = "", x = "Country code (prefix ISO 3166)", y = "Conflict perception scale")


# Country-class -----------------------------------------------------------

as.data.frame(ggeffects::ggpredict(model2, terms = c("class_2", "c_alphan"))) %>%
  select(c_alphan = group, class_2 = x, pred = predicted, everything()) %>%
  group_by(c_alphan) %>% 
  mutate(pred_group = mean(pred)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = class_2 ,  y = pred, color = c_alphan)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), stat = "identity") +
  geom_hline(aes(yintercept = mean(pred_group)),
             linetype = "dashed") +
  facet_wrap(~ c_alphan, nrow = 3, dir = "v") +
  guides(color = F) +
  coord_flip()  +
  scale_color_grey(start = 0.2, end = 0.5) +
  labs(title = "", x = "Class", y = "Conflict perception scale")


