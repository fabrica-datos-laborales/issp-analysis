# Code 1 Proc Data ----------------------------------------------------------
# Author: Valentina Andrade
# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, sjPlot)

# 2. Load data ------------------------------------------------------------
issp <- haven::read_dta("input/data/original/ZA6770_v2-1-0.dta")

# 3. Explorar variables de interes ----------------------------------------
# class: NEMPLOY, EMPREL, WRKSUP, ISOC08, WORK
# sociodemographics: SEX, AGE, DEGREE, TYPORG2, country, WEIGHT
# predictors: UNION, v42, v43 (control)
frq(issp$v42) # 8 cant choose y 9 NA
frq(issp$UNION) #7 y 9 NA
frq(issp$SEX) # 9 NA
frq(issp$DEGREE) # 9 NA
frq(issp$TYPORG2) # 8 y 9 NA

# 4. Recode ---------------------------------------------------------------
issp <- issp %>%
  mutate_at(vars(UNION, SEX, DEGREE, TYPORG2, AGE), funs(car::recode(., c("c(7,8,9,999)=NA")))) %>% 
  mutate_at(vars(v42,v43), funs(car::recode(., c("9=NA")))) %>% 
  mutate(conflict = case_when(v42 %in% c(5,4) ~ 1,
                                           TRUE ~ 0),
         year = 2015) %>% 
# 5. Select  --------------------------------------------------------------
select(country, year, SEX, AGE, DEGREE, TYPORG2, UNION, NEMPLOY, EMPREL, WRKSUP, ISCO08, WORK,
       v42, v43, conflict,
       WEIGHT) %>% filter(AGE != 0)
# 6. Save  ----------------------------------------------------------------------
saveRDS(issp, file = "input/data/proc/issp-proc.rds")
