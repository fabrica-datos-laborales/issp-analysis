# Code 2: Estructura de Clases de E. O Wright ----------------------------------
# Auhor: Valentina Andrade
rm(list = ls()) # empty env
# 1. Load packages -------------------------------------------------------------
pacman::p_load(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y)) #own function

# 2. Load data -----------------------------------------------------------------
issp <- readRDS("input/data/proc/issp-proc.rds")

# 3. Recode variables ----------------------------------------------------------
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


# 4.3 Asalariados --------------------------------------------------------------
# A.1 Supervisan ---------------------------------------------------------------
# Proxy: WRKSUP #Do/ did you supervise other employees? Yes/No
issp$WRKSUP <- as.numeric(issp$WRKSUP)
table(issp$WRKSUP)
issp$control <- car::recode(issp$WRKSUP,recodes = "1='Control';2='No control';99=NA",as.factor =T, 
                            levels = c("Control", "No control"))

# Verificar
issp %>% count(control) %>% mutate(prop = prop.table(n)) 

# A.2 Skills--------------------------------------------------------------------
# ISCO08: In your main job, what kind of activities do/ did you do most of the time?

## (A) Education (to control)---------------------------------------------------
frq(issp$DEGREE)
issp$educ <- as.numeric(issp$DEGREE)
issp$educ <- car::recode(issp$educ, recodes = c("0:5='No';6:9='Yes';c(88,99)=NA"))
frq(issp$educ)

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
#Note: variable 'qual' used later to distinguis petite bourgeoise from informal self-employed
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
frq(issp$class)

## 4.2 Label variable ----------------------------------------------------------
issp$class <- factor(issp$class,levels = c(1:10),
                     labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                "4.Expert managers","5.Nonmanagerial experts",
                                "6.Skilled supervisors","7.Unskilled supervisors",
                                "8.Skilled workers","9.Unskilled workers", "10. Informal self-employed"))
## 4.3 Result class ------------------------------------------------------------
frq(issp$class)
issp %>% 
  filter(!is.na(class)) %>% 
  count(class) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporción

# 5. Save ----------------------------------------------------------------------
saveRDS(issp, file = "output/data/issp.rds") 
