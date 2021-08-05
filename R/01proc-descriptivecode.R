# Code Paper ----------------------------------------------------------
# Author: Valentina Andrade
# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, sjPlot, magrittr, labelled, lavaan, psych)
'%!in%' <- function(x,y)!('%in%'(x,y)) #own function

# 2. Load data ------------------------------------------------------------
issp <- haven::read_dta("input/data/original/ZA6770_v2-1-0.dta")

view_df(issp) # explorar


# A. Recode variables -----------------------------------------------------
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
  mutate_at(vars(v45:v47), funs(car::recode(., c("1=0;2=1;3=2;4=3;5=4;c(0,8,9)=NA")))) %>% 
  mutate_at(vars(v45:v47), funs(labelled(.,c("Strongly agree"=0,"Agree"=1,"Neither agree nor disagree"=2,"Disagree"=3,"Strongly disagree"=4)))) %>% 
  mutate(v42 = car::recode(.$v42, c("1=0;2=1;3=2;4=3;5=4;c(0,8,9)=NA")),
         v44 = car::recode(.$v44, c("1=0;2=1;3=2;4=3;5=4;6=5;7=6;c(0,8,9)=NA"))) %>% 
  mutate(v42 = labelled(.$v42,c("Very good"=0,"Quite good"=1,"Neither good nor bad"=2,"Quite bad"=3,"Very bad"=4)),
         v44 = labelled(.$v44,c("Completely satisfied"=0,"Very satisfied"=1,"Fairly satisfied"=2,"Neither satisfied nor dissatisfied"=3,"Fairly dissatisfied"=4, "Very dissatisfied"= 5, "Completely dissatisfied" = 6)), 
         UNION = car::recode(.$UNION, c("c(7,9)=NA")),
         SEX = car::recode(.$SEX, c("9=NA")),
         DEGREE = car::recode(.$DEGREE, c("9=NA")),
         TYPORG2 = car::recode(.$TYPORG2, c("c(8,9)=NA")),
         AGE = car::recode(.$AGE, c("999=NA")),
         conflict = case_when(v42 %in% c(4,3) ~ 1,
                              v42 %in% c(2,1,0) ~ 0), 
         year = 2015) %>% 
  # 5. Select  --------------------------------------------------------------
dplyr::select(country, year, SEX, AGE, DEGREE, TYPORG2, UNION, NEMPLOY, EMPREL, WRKSUP, ISCO08, WORK,
              v42,v44,v45,v46,v47, conflict, c_alphan,
              WEIGHT) %>% filter(AGE != 0)

# B. Construct class variable --------------------------------------------------
# 3. Recode variables ----------------------------------------------------------
# 3.1 Employment relation ------------------------------------------------------

# 3.1 Employment relation ------------------------------------------------------

# A.1 Filtrar inactivos
#Variable WRKTYPE: R: Working for private or public sector or self employed 
issp <- filter(issp, WORK != 3)
# --> 3 Never had paid work

# A.2 Crear variable asalariados
issp$EMPREL <- as.numeric(issp$EMPREL) 
issp$prop_salariedA <- car::recode(issp$EMPREL,recodes = "c(1,4)='Salaried';2='3.Petite bourgeoisie';3='Employer';c(8,9)=NA")

# Verificar
issp %>% count(prop_salariedA) %>% mutate(prop = prop.table(n))
issp %>% count(EMPREL) %>% mutate(prop = prop.table(n))


# 4.2 Propietarios -------------------------------------------------------------
# Proxy: NEMPLOY (How many employees do/ did you have, not counting yourself?)
# A.1 Crear variable "owners":
#1.Pequeña burguesia: 0 a 1 empleados
#2. Pequeños empleadores: de 2 a 9 empleados
#3. Capitalist: de 10 a más empleados
issp$owners <- as.numeric(issp$NEMPLOY)
issp <- issp %>% mutate(owners = case_when(owners %in% c(10:9995)& prop_salariedA=="Employer"~ "1.Capitalist",
                                           owners %in% c(2:9, 9999,9998)& prop_salariedA=="Employer" ~ "2.Small employers",                                           owners %in% c(2:9)& prop_salariedA=="Employer" ~ "2.Small employers",
                                           owners %in% c(1)| prop_salariedA=="3.Petite bourgeoisie"~ "3.Petite bourgeoisie",
                                           TRUE ~ NA_character_))

# Importante --> hay 1 que indican no tener ni 1 a 9995, indican NA asi que son incorporados en petite bourgoise
# A.2 Verificar
issp %>% count(owners) %>% mutate(prop = prop.table(n))
issp %>% count(NEMPLOY) %>% mutate(prop = prop.table(n))

# A.3 Var final salaried -------------------------------------------------------
issp <- issp %>% mutate(prop_salaried = if_else(is.na(owners), prop_salariedA, owners))

# A.2 Verificar
issp %>% count(prop_salaried) %>% mutate(prop = prop.table(n))
issp %>% count(prop_salariedA) %>% mutate(prop = prop.table(n))


# 3.2 Salaried workers ---------------------------------------------------------
# A.1 Supervisan ---------------------------------------------------------------
# WRKSUP #Do/ did you supervise other employees? Yes/No
issp$control <- as.numeric(issp$WRKSUP)
issp$control <- car::recode(issp$control,recodes = "1='Control';2='No control';c(8,9)=NA",as.factor =T, 
                            levels = c("Control", "No control"))

# A.2 Skills--------------------------------------------------------------------
# ISCO08: In your main job, what kind of activities do/ did you do most of the time?

## (A) Education (to control)---------------------------------------------------
issp$educ <- as.numeric(issp$DEGREE)
issp$educ <- car::recode(issp$educ, recodes = c("0:5='No';6:9='Yes';c(88,99)=NA"))

## (B) ISCO 08 ---------------------------------------------------------------------
# A.2.1 Numeric
issp$isco <- as.numeric(issp$ISCO08)

#A.2.2 Delete FFAA
issp <- issp %>% filter(isco %!in% c(1100, 1000, 110, 210, 310)) 

#A.2.3 ISCO 2 digits
issp$isco_2 <- substr(issp$isco, start = 1, stop = 2) 

#A.2.4 Create "skills" ---------------------------------------------------------
#Grup 1: Experts (del 10 al 26)
#Grup 2: Skilled.  30 to 35, and add  60 61, 72. 
#Grup 3: Unskilled. Son 40, 41, 42, 43, 44,50, 51, 52, 53, 54, 62, 63, 71. Luego del 73 al 96.
#Grup 4: NA (99)
issp$skillsA <- car::recode(issp$isco_2, 
                            recodes = "10:26='Experts';c(30,31,32,33,34,35,36,60,61,72)='Skilled'
                               ;c(40,41,42,43, 44,50,51,52,53,54,62,63,70,71,73,74,75,76,76,77,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96)='Unskilled'; 99=NA")

## Control by educ -------------------------------------------------------------
issp <- issp %>% mutate(skills = if_else(skillsA =="Experts" & educ=="Yes", "Experts",
                                         if_else(skillsA == "Experts" & educ=="No", "Skilled", skillsA)),
                        qual = case_when(isco_2 %in% c(11:44) ~ 1,
                                         isco_2 %in% c(51:96) ~ 2, TRUE ~ NA_real_))
# Correct experts and skilled workers (+skilled)

# 4. Final class variable ------------------------------------------------------
## 4.1 Create variable ---------------------------------------------------------
issp$class<- NA # empty variable
issp$class <- with(issp, ifelse(prop_salaried=="1.Capitalist", 1, class))
issp$class <- with(issp, ifelse(prop_salaried=="2.Small employers", 2, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie" & qual == 1, 3, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Experts", 4, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Experts", 5, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Skilled", 6, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Unskilled", 7, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Skilled", 8, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Unskilled", 9, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie" & qual==2, 10, class))

## 4.2 Label variable ----------------------------------------------------------
issp$class <- factor(issp$class,levels = c(1:10),
                     labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                "4.Expert managers","5.Nonmanagerial experts",
                                "6.Skilled supervisors","7.Unskilled supervisors",
                                "8.Skilled workers","9.Unskilled workers", "10. Informal self-employed"))
## 4.3 Result class ------------------------------------------------------------
issp %>% 
  filter(!is.na(class)) %>% 
  count(class) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporción


## 4.1 Create variable class_2 ---------------------------------------------------------
issp$class_2<- NA # empty variable
issp$class_2 <- with(issp, ifelse(prop_salaried=="1.Capitalist", 1, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="2.Small employers", 2, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie", 3, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Experts", 4, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Experts", 5, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Skilled", 6, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Unskilled", 7, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Skilled", 8, class_2))
issp$class_2 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Unskilled", 9, class_2))

## 4.2 Label variable ----------------------------------------------------------
issp$class_2 <- factor(issp$class_2,levels = c(1:9),
                     labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                "4.Expert managers","5.Nonmanagerial experts",
                                "6.Skilled supervisors","7.Unskilled supervisors",
                                "8.Skilled workers","9.Unskilled workers"))
## 4.3 Result class_2 ------------------------------------------------------------
issp %>% 
  filter(!is.na(class_2)) %>% 
  count(class_2) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporción

## 4.1 Create variable class_3 ---------------------------------------------------------
issp$class_3<- NA # empty variable
issp$class_3 <- with(issp, ifelse(prop_salaried %in% c("1.Capitalist", "2.Small employers"), 1, class_3))
issp$class_3 <- with(issp, ifelse(prop_salaried == "3.Petite bourgeoisie", 2, class_3))
issp$class_3 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Experts", 3, class_3))
issp$class_3 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Experts", 4, class_3))
issp$class_3 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Skilled", 5, class_3))
issp$class_3 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Unskilled", 6, class_3))
issp$class_3 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Skilled", 7, class_3))
issp$class_3 <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Unskilled", 8, class_3))

## 4.2 Label variable ----------------------------------------------------------
issp$class_3 <- factor(issp$class_3,levels = c(1:8),
                     labels = c("1.Employers", "2.Petite bourgeoisie",
                                "3.Expert managers","4.Nonmanagerial experts",
                                "5.Skilled supervisors","6.Unskilled supervisors",
                                "7.Skilled workers","8.Unskilled workers"))
## 4.3 Result class_3 ------------------------------------------------------------
issp %>% 
  filter(!is.na(class_3)) %>% 
  count(class_3) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporción

# C. Descriptive analysis ---------------------------------------------------------

# C.1 Univariate --------------------------------------------------------------
## ----Descriptive sociodemographics --------------------------------------
issp %>% select(SEX, UNION, DEGREE, TYPORG2) %$%
  sjmisc::frq(.)

## ---- Descriptive age------------------------------------------------
plot_frq(issp$AGE, type = "h", show.mean = TRUE, show.mean.val = TRUE,normal.curve = TRUE, show.sd = TRUE, normal.curve.color = "blue")


## ----Descriptive class--------------------------------------------------------
sjPlot::plot_frq(issp$class, weight.by = issp$WEIGHT) + labs(x = "Class position", caption = "Source: E.O.Wright (1997) based on ISSP (2015)") + theme(axis.text.x = element_text(angle = -90))


## ----Descriptive variables to construct class------------------------------------------
issp %>% select(prop_salaried, control, educ, qual, skillsA, skills) %$%
  sjmisc::frq(.)

## ----Descriptive dependent variable conflict-------------------------------------------
sjPlot::plot_frq(issp$conflict, weight.by = issp$WEIGHT, show.ci = T, errorbar.color = "black") + labs(x = "Relations: between management and employees", caption = "Source: E.O.Wright (1997) based on ISSP (2015)")

## ---- Descriptive v42(non-recode dependent variable ------------------------------------------------
issp %>% select(v42) %$%
  sjmisc::frq(.)


# C.2 Bivariate -----------------------------------------------------------

# Sex-conflict ------------------------------------------------------------
issp %$% 
  tab_xtab(.$SEX, .$conflict,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

# Union-conflict ------------------------------------------------------------
issp %$% 
  tab_xtab(.$UNION, .$conflict,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

# Sector-conflict ------------------------------------------------------------
issp %$% 
  tab_xtab(.$TYPORG2, .$conflict,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

# Class-all variables ------------------------------------------------------------
## Class-conflict ------------------------------------------------------------
issp %$% 
  tab_xtab(.$class, .$conflict,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

## Class- conflict items ------------------------------------------------------------
issp %$% 
  tab_xtab(.$class, .$v42,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

issp %$% 
  tab_xtab(.$class, .$v44,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

issp %$% 
  tab_xtab(.$class, .$v45,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

issp %$% 
  tab_xtab(.$class, .$v46,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

issp %$% 
  tab_xtab(.$class, .$v47,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

## Class- sex ------------------------------------------------------------
issp %$% 
  tab_xtab(.$class, .$SEX,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

## Class- UNION ------------------------------------------------------------
issp %$% 
  tab_xtab(.$class, .$UNION,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

## class-conflict ------------------------------------------------------------
issp %$% 
  tab_xtab(.$class, .$TYPORG2,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)


# country-all variables ---------------------------------------------------
## c_alphan-conflict ------------------------------------------------------------
issp %$% 
  tab_xtab(.$c_alphan, .$conflict,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

## c_alphan- sex ------------------------------------------------------------
issp %$% 
  tab_xtab(.$c_alphan, .$SEX,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

## c_alphan- UNION ------------------------------------------------------------
issp %$% 
  tab_xtab(.$c_alphan, .$UNION,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

## c_alphan-conflict ------------------------------------------------------------
issp %$% 
  tab_xtab(.$c_alphan, .$TYPORG2,
           statistics = "cramer", weight.by = .$WEIGHT, show.row.prc = T)

# Mean by items "v4" class and country ------------------------------------

## By class
issp %>%
  group_by(class) %>%
  summarise_at(vars(starts_with("v4")), funs (weighted.mean(.,WEIGHT,na.rm = T)))

## By country
issp %>%
  group_by(c_alphan) %>%
  summarise_at(vars(starts_with("v4")), funs (weighted.mean(.,WEIGHT,na.rm = T))) %>%
  print(n = 46)

# Correlation matrix ------------------------------------------------------
## v42,v44, v45, v46 y v47
issp %>% select(starts_with("v4")) %>% 
  sjp.corr(.,show.legend = TRUE, title = "Correlation plot")


# Test para FA -----------------------------------------------------------------
# KMO --------------------------------------------------------------------------
issp %>% select(starts_with("v4")) %>% 
  psych::KMO(.)

# Test de Bartlett -------------------------------------------------------------
issp %>% select(starts_with("v4")) %>% 
  psych::cortest.bartlett(.)

# Scree plot -------------------------------------------------------------------
issp %>% select(starts_with("v4")) %>% 
  scree(.)

# PCA and Cronbach ---------------------------------------------------------------------
issp %>%
  select(starts_with("v4")) %>% 
  tab_pca(., rotation =c("varimax"), 
          title = "Principal Component Analysis with `v4` items", 
          string.pov = "Proportion of Variance",
          string.cpov = "Cumulative Proportion", nmbr.fctr = 2,
          show.msa = T)
# By Kaiser criteria is only one principal component
# We force two componentes with nmbr.fctr = 2
# cronbach is shown bottom


## Factor Analysis (ML) and Alpha Cronbach----------------------------------------
## Correlation
issp %>%
  select(starts_with("v4")) %>% 
  corr.test(., alpha = 0.05,
            method='pearson')

issp %>%
  select(starts_with("v4")) %$% 
  fa(., nfactors = 5, fm = "ml", rotate = "varimax")$Vaccounted 


# Factor loading matrix ML ---------------------------------------------------
f <- issp %>%
  select(starts_with("v4")) %$% 
  fa(., nfactors = 5, fm = "ml", rotate = "varimax")

data.frame(unclass(f$loadings), h2=f$communalities, u2= f$uniqueness,com=f$complexity)


# Factor analysis (principal axis) ----------------------------------------
issp %>%
  select(starts_with("v4")) %$% 
  fa(.,  fm = "pa", rotate = "varimax")

## Factr loadings
f <- issp %>%
  select(starts_with("v4")) %$% 
  fa(.,  fm = "pa", rotate = "varimax")
data.frame(unclass(f$loadings), h2=f$communalities, u2= f$uniqueness,com=f$complexity)


## otra forma (cada vbe individual; para ir probando)
issp %>%
  select(v42,v45,v46,v47) %$% 
  fa(., fm = "pa", rotate = "varimax") 


##Alpha cronbach----------------

#Correlation
issp %>%
  select(starts_with("v4")) %>% 
  corr.test(., alpha = 0.05,
            method='pearson')

#Alpha cronbach 
issp %>%
  select(starts_with("v4")) %>% 
  psych::alpha(.)


#With different combinations of items (to check)
issp %>%
  select(v42,v45,v46,v47) %>% 
  psych::alpha(.)


# PCA with policoric variables --------------------------------------------------------------
# scale <- issp %>% select(v42,v45,v46,v47)
#   homals(scale, ndim = 1, level = "ordinal", active = TRUE)

# Mean scale --------------------------------------------------------------
# scale_1 con todas las v4* e scale_2 solo con indicadas

# 1. Mean scale_1 and scale_2 by class ----------------------------------------------------------------

issp %>% 
  select(starts_with("v4"), class, c_alphan, WEIGHT) %>%
  filter_at(vars(v42:v47), all_vars(!is.na(.))) %>%
  mutate_at(vars(starts_with("v4")), ~.*100/6) %>%
  rowwise() %>% 
  mutate(scale_1 = mean(v42:v47, na.rm =  T),
         scale_2 = mean(c(v42,v45,v46,v47),  na.rm =  T)) %>%
  ungroup() %>%
  group_by(class) %>%
  summarise_at(vars(starts_with("scale")), funs(weighted.mean(.,WEIGHT,na.rm = T)))

# 2. Mean scale_1 and scale_2 by country -------------------------------------
issp %>% 
  select(starts_with("v4"), class, c_alphan, WEIGHT) %>%
  filter_at(vars(v42:v47), all_vars(!is.na(.))) %>% 
  mutate_at(vars(starts_with("v4")), ~.*100/6) %>%
  rowwise() %>% 
  mutate(scale_1 = mean(v42:v47, na.rm =  T),
         scale_2 = mean(c(v42,v45,v46,v47),  na.rm =  T)) %>%
  ungroup() %>%
  group_by(c_alphan) %>%
  summarise_at(vars(starts_with("scale")), funs(weighted.mean(.,WEIGHT,na.rm = T))) %>%
  print(n = 46)

# Anova -------------------------------------------------------------------
issp %>% 
  select(starts_with("v4"), class, c_alphan, WEIGHT) %>%
  filter_at(vars(v42:v47), all_vars(!is.na(.))) %>% 
  mutate_at(vars(starts_with("v4")), ~.*100/6) %>%
  rowwise() %>% 
  mutate(scale_1 = mean(v42:v47, na.rm =  T),
         scale_2 = mean(c(v42,v45,v46,v47),  na.rm =  T)) %>%
  ungroup() %>%
  pivot_longer(., cols = c(scale_1, scale_2), names_to = "type_scale") %$% 
  sjPlot::sjp.aov1(.$value, .$type_scale, title = "Anova by type scale")


# Anova scale by class ----------------------------------------------------
issp %>% 
  select(starts_with("v4"), class, c_alphan, WEIGHT) %>%
  filter_at(vars(v42:v47), all_vars(!is.na(.))) %>% 
  mutate_at(vars(starts_with("v4")), ~.*100/6) %>%
  rowwise() %>% 
  mutate(scale_1 = mean(v42:v47, na.rm =  T),
         scale_2 = mean(c(v42,v45,v46,v47),  na.rm =  T)) %>%
  ungroup()  %$% 
  sjPlot::sjp.aov1(.$scale_2, .$class, title = "Anova scale N°2 by class")

# Anova scale by country ----------------------------------------------------
issp %>% 
  select(starts_with("v4"), class, c_alphan, WEIGHT) %>%
  filter_at(vars(v42:v47), all_vars(!is.na(.))) %>% 
  mutate_at(vars(starts_with("v4")), ~.*100/6) %>%
  rowwise() %>% 
  mutate(scale_1 = mean(v42:v47, na.rm =  T),
         scale_2 = mean(c(v42,v45,v46,v47),  na.rm =  T)) %>%
  ungroup()  %$% 
  sjPlot::sjp.aov1(.$scale_2, .$c_alphan, title = "Anova scale N°2 by country")

# Create scales -----------------------------------------------------------
issp <- issp %>% 
  select(2:7,starts_with("v4"), starts_with("class"), c_alphan, WEIGHT) %>%
  filter_at(vars(v42:v47), all_vars(!is.na(.))) %>%
  mutate_at(vars(starts_with("v4")), ~.*100/6) %>%
  rowwise() %>% 
  mutate(scale_1 = mean(v42:v47, na.rm =  T),
         scale_2 = mean(c(v42,v45,v46,v47),  na.rm =  T)) %>%
  ungroup() %>% 
  mutate_at(vars(3:7, -AGE), ~as_factor(.)) %>% 
  select(-starts_with("v4"), -scale_1)

# 6. Save  ----------------------------------------------------------------------
saveRDS(issp, file = "input/data/proc/issp-paper.rds")
