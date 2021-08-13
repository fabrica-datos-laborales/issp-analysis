# Code 2: Models ----------------------------------------------------------
rm(list = ls())

# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, sjPlot,  performance, parameters, estimatr, apaTables, stargazer)
ggplot2::theme_set(theme_sjplot2())

# 2. Load data ------------------------------------------------------------
issp <- readRDS(file = "input/data/proc/issp-paper.rds")

# 3. Models ---------------------------------------------------------------
#Model 1 without clustered SE----------
#Model 1 (class_2)
model1 <- lm(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_2 + c_alphan,
             data = issp, weights = issp$WEIGHT, na.action=na.omit)

parameters::model_parameters(model1)
summary(model1)


#Models 1 WITH CLUSTERED SE----------
#Model 1 (class_2)
model1_cse <-estimatr::lm_robust(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_2 + c_alphan,
                                 data = issp, weights = issp$WEIGHT,
                                 clusters = c_alphan)
summary(model1_cse)
parameters::model_parameters(model1_cse)

#Model 1a = INTERACTION class*country (Warning: model matrix rank deficient)
model1a_cse <-estimatr::lm_robust(scale_2 ~ AGE + SEX+ TYPORG2 + UNION + class_2 + 
                                   c_alphan + class_2*c_alphan,
                                 data = issp, weights = issp$WEIGHT,
                                 clusters = c_alphan)
summary(model1a_cse)
parameters::model_parameters(model1a_cse)

###  TABLES----------

stargazer(model1_cse,
          type="html",
          out="star_linear.doc") #No funciona con ESTIMATR

# se hace igual con MODEL 1 para probar
stargazer(model1_cse,
          type="html",
          out="Model1.doc",
          intercept.bottom = T,
          intercept.top = F,
          ci = F, digits=3,
          notes = "XXXXXX",
          model.names = T,
          single.row = T)


# OTRO ejempolo más completo: 
stargazer(model1,
          type="html",
          out="Model1.doc",
          intercept.bottom = T,
          intercept.top = F,
          ci = F, digits=2,
          notes = "XXXXXX",
          model.names = T,
          single.row = T,
          covariate.labels = c("Age", "Sex", "Private sector", "Previously unionized",
                               "Never unionized", "Small employers", "PB", "etc", "etc"))

#Otras formas para lm 
apa.reg.table(model1_cse)
apa.reg.table(model1_cse,filename="Table_model1_cse.doc") #Doesn't work


# 4. Figures  ---------------------------------------------------------------
#1- Predicted values for each CLASS, based on coef of model1_cse
ggeffects::ggpredict(model1_cse, terms = c("class_2")) %>% 
  plot(., color = "greyscale",
       ci.style = "errorbar")+
  labs(title = "", x = "", y = "Perception of conflict") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10))+
   scale_y_continuous(breaks=c(5,10,15,20,25,30), limits = c(5,30))


#2- Predicted values for each COUNTRY, based on coef of model1_cse
# Weighted mean (descriptive)
ggeffects::ggpredict(model1_cse, terms = c("c_alphan")) %>% 
  plot(., color = "greyscale",
       ci.style = "errorbar") +
  coord_flip()  +
  geom_hline(aes(yintercept = 13.4),  #13.4 = regression intercept
             linetype = "dashed") +
  labs(title = "", x = "", y = "Perception of conflict") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10))+
  scale_y_continuous(breaks=c(5,10,15,20,25,30), limits = c(5,30))


#AYUDAAA 3. CLASS and COUNTRY: all COUNTRIES... AYUDA CON ESTO#####
#Quiero: 
#1) cambiar "shape" para cada categoria de clase (triangulo, cuadrado, etc.) (lo intenté con labs(shape="class_2") pero no funcionó
#2) Ordenar los países de menor a mayor (o mayor a menor), segun nuntajepuntaje vble dependiente
#1- Predicted values for each classes, based on coef of model1_cse
ggeffects::ggpredict(model1_cse, terms = c("c_alphan","class_2")) %>% 
  plot(., color = "greyscale",
       ci.style = "errorbar") +
  labs(title = "", x = "", y = "Perception of conflict") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10),
        legend.position = "bottom") +
  scale_y_continuous(breaks=c(5,10,15,20,25,30,35), limits = c(5,35))




#4. CLASS and COUNTRY: SELECTED COUNTRIES
ggeffects::ggpredict(model1_cse, terms = c("c_alphan[CL, AU, US, MX, SE]","class_2")) %>% 
  plot(., color = "greyscale",
       ci.style = "errorbar") +
  labs(title = "", x = "", y = "Perception of conflict") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10))+
  scale_y_continuous(breaks=c(5,10,15,20,25,30,35), limits = c(5,35))



as.data.frame(ggeffects::ggpredict(model1_cse, terms = c("c_alphan"))) %>%
  select(pred = predicted, everything()) %>%
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), stat = "identity") +
  geom_hline(aes(yintercept = mean(pred_group)),
             linetype = "dashed") +
  facet_wrap(~ class_2, nrow = 2, dir = "h") +
  guides(color = F) +
  coord_flip()  +
  scale_color_grey(start = 0.2, end = 0.5) +
  labs(title = "", x = "Country code", y = "Perception of conflict")



### otros graficos para país y año

ggeffects::ggpredict(model1_cse, terms = c("c_alphan", "class_2")) %>% 
  plot(.,facet = T, color = "greyscale",
       ci.style = "errorbar") +
  coord_flip() +
  labs(title = "", x = "Country code (prefix ISO 3166)", y = "Conflict perception scale")


ggeffects::ggpredict(model1_cse, terms = c("c_alphan[CL, VE, US, MX]", "class_2")) %>% 
  plot(.,facet = T, color = "bw",
       ci.style = "errorbar") +
  coord_flip() +
  labs(title = "", x = "Country code (prefix ISO 3166)", y = "Conflict perception scale")







# Manual graph ------------------------------------------------------------
## Colors
issp <- issp %>% 
  mutate(color_group = ifelse(c_alphan == 'CL', 'red', 'black'))

color_group = setNames(as.character(issp$c_alphan), issp$color_group)

# Class & Country-------------------------------------------------------------------
## Definitiva
as.data.frame(ggeffects::ggpredict(model1_cse, terms = c("c_alphan", "class_2"))) %>%
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
  labs(title = "", x = "Country code", y = "Perception of conflict")


##OTRA Opcion: selected countries


