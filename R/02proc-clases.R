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

# A.1 Filter inactive people
# (1) WORK: Are you currently working for pay, did you work for pay in the past, or have you never been in paid work?
issp <- filter(issp, WORK != 3) # 8 y 9 NS/NR

# A.2 Create salaried workers
# (2) EMPREL: Are/ were you an employee, self-employed, or working for your own family's business?
issp$prop_salaried <- as.numeric(issp$EMPREL) 
issp$prop_salaried <- car::recode(issp$prop_salaried,recodes = "1='Salaried';c(5,2)='3.Petite bourgeoisie';3='2.Small employers';4='1.Capitalist';c(8,9)=NA", as.factor = T,
                                  levels = c("Salaried", "3.Petite bourgeoisie", "2.Small employers","1.Capitalist"))

# 3.2 Salaried workers ---------------------------------------------------------
# A.1 Supervisan ---------------------------------------------------------------
# WRKSUP #Do/ did you supervise other employees? Yes/No
issp$control <- as.numeric(issp$WRKSUP)
issp$control <- car::recode(issp$control,recodes = "1='Control';2='No control';99=NA",as.factor =T, 
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

# 5. Save ----------------------------------------------------------------------
saveRDS(issp, file = "output/data/issp.rds") 
