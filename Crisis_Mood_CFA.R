## Author: Valerie Hofmann
## Date: 27/02/2022
## CFAs 

library(lavaan)
library(readr)
library(tidyverse)
library(cSEM)
library(dplyr)
library(corrplot)
library(foreign)
library(psych)
#remotes::install_github("JonasMoss/reliable", force = TRUE)
library(reliable)
library(semTools)

#load data 
crisis_75 <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/
                              crisis_75_sep.csv", col_names = TRUE,
                              col_types = cols(ID = "c"))
colnames(crisis_75)
head(crisis_75)
#load 3 M prior 
crisis_75 <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/
                              crisis_75_sep.csv", col_names = TRUE,
                              col_types = cols(ID = "c"))


col_t1 = c('M1_T1', 'M10_T1', 'M11_T1', 'M2_T1', 'M3_T1', 'M4_T1',
           'M5_T1', 'M6_T1', 'M7_T1', 'M8_T1', 'M9_T1')
t1 <- as_data_frame(crisis_75[, col_t1])
t1[] <- lapply(t1, function(x) as.numeric(as.character(x)))


col_t2 = c('M1_T2', 'M10_T2', 'M11_T2', 'M2_T2', 'M3_T2', 'M4_T2',
           'M5_T2', 'M6_T2', 'M7_T2', 'M8_T2', 'M9_T2')
t2 <- as_data_frame(crisis_75[, col_t2])
t2[] <- lapply(t2, function(x) as.numeric(as.character(x)))

col_t3 = c('M1_T3', 'M10_T3', 'M11_T3', 'M2_T3', 'M3_T3', 'M4_T3',
           'M5_T3', 'M6_T3', 'M7_T3', 'M8_T3', 'M9_T3')
t3 <- as_data_frame(crisis_75[, col_t3])
t3[] <- lapply(t3, function(x) as.numeric(as.character(x)))

#### MOOD FScores 
m3_raw <- readr::read_csv2("/Users/hofma/Desktop/LGC/02_data/
                           Crisis_BL_3M.csv", col_names = TRUE,
                           col_types = cols(ID = "c"))
m3c <- merge(m3_raw, crisis_75, by="ID")
colnames(m3_raw)
col_m3 = c("M1_M3", "M10_M3", "M11_M3", "M2_M3", "M3_M3", "M4_M3",
           "M5_M3", "M6_M3", "M7_M3", "M8_M3", "M9_M3")
m3 <- (m3c[, col_m3])
m3[] <- lapply(m3, function(x) as.numeric(as.character(x)))
#is.data.frame(m3)
summary(m3)

######Single Factor Score 
M_M3 <-"
! regressions 
   Mood_M3=~lambda1*M1_M3
   Mood_M3=~lambda2*M10_M3
   Mood_M3=~lambda3*M11_M3
   Mood_M3=~lambda4*M2_M3
   Mood_M3=~lambda5*M3_M3
   Mood_M3=~lambda6*M4_M3
   Mood_M3=~lambda7*M5_M3
   Mood_M3=~lambda8*M6_M3
   Mood_M3=~lambda9*M7_M3
   Mood_M3=~lambda10*M8_M3
   Mood_M3=~lambda11*M9_M3
! residuals, variances and covariances
   M1_M3 ~~ e1*M1_M3
   M10_M3 ~~ e2*M10_M3
   M11_M3 ~~ e3*M11_M3
   M2_M3 ~~ e4*M2_M3
   M3_M3 ~~ e5*M3_M3
   M4_M3 ~~ e6*M4_M3
   M5_M3 ~~ e7*M5_M3
   M6_M3 ~~ e8*M6_M3
   M7_M3 ~~ e9*M7_M3
   M8_M3 ~~ e10*M8_M3
   M9_M3 ~~ e11*M9_M3
   Mood_M3 ~~ 1.0*Mood_M3
! means
   Mood_M3~mu*1
   M1_M3~0*1;
   M10_M3~0*1;
   M11_M3~0*1;
   M2_M3~0*1;
   M3_M3~0*1;
   M4_M3~0*1;
   M5_M3~0*1;
   M6_M3~0*1;
   M7_M3~0*1;
   M8_M3~0*1;
   M9_M3~0*1;
";
result_mood_M3 <-lavaan(M_M3, data=m3, fixed.x=FALSE, missing="FIML");
summary(result_mood_M3, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(result_mood_M3)

ID <- crisis_75[, 1]
M3_fscore <- lavPredict(result_mood_M3, method ="regression")
#will be used as predictor 
M3_fscore <- as.data.frame(M3_fscore)
m3ex <- cbind(ID, M3_fscore)
head(m3ex)


#export
write.csv(m3ex, file="C:/Users/hofma/Desktop/LGC/02_data/
          Crisis_Mood_M3_fscores.txt", row.names = FALSE)
#
round(coefficient_H(result_mood_M3),3)

##################
#T1
##################

##########single factor cfa
M_T1<-"
! regressions 
   Mood_T1=~lambda1*M1_T1
   Mood_T1=~lambda2*M10_T1
   Mood_T1=~lambda3*M11_T1
   Mood_T1=~lambda4*M2_T1
   Mood_T1=~lambda5*M3_T1
   Mood_T1=~lambda6*M4_T1
   Mood_T1=~lambda7*M5_T1
   Mood_T1=~lambda8*M6_T1
   Mood_T1=~lambda9*M7_T1
   Mood_T1=~lambda10*M8_T1
   Mood_T1=~lambda11*M9_T1
! residuals, variances and covariances
   M1_T1 ~~ e1*M1_T1
   M10_T1 ~~ e2*M10_T1
   M11_T1 ~~ e3*M11_T1
   M2_T1 ~~ e4*M2_T1
   M3_T1 ~~ e5*M3_T1
   M4_T1 ~~ e6*M4_T1
   M5_T1 ~~ e7*M5_T1
   M6_T1 ~~ e8*M6_T1
   M7_T1 ~~ e9*M7_T1
   M8_T1 ~~ e10*M8_T1
   M9_T1 ~~ e11*M9_T1
   Mood_T1 ~~ 1.0*Mood_T1
! means
   Mood_T1~mu*1
   M1_T1~0*1;
   M10_T1~0*1;
   M11_T1~0*1;
   M2_T1~0*1;
   M3_T1~0*1;
   M4_T1~0*1;
   M5_T1~0*1;
   M6_T1~0*1;
   M7_T1~0*1;
   M8_T1~0*1;
   M9_T1~0*1;
";
result_mood_T1 <-lavaan(M_T1, data=t1, fixed.x=FALSE, missing="FIML");
summary(result_mood_T1, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(result_mood_T1)
mi <- modindices(result_mood_T1, sort = TRUE, maximum.number = 5)
T1_fscore <- lavPredict(result_mood_T1, method ="Bartlett")
head(T1_fscore)
round(coefficient_H(result_mood_T1),2)

#check 
max(T1_fscore)
which.max(T1_fscore) 
#sum the row
sum(t1[81,]) #40 #maximum possible = 44
round((max(T1_fscore)/40)*44,3)#6.79
round((max(T1_fscore)/40)*22,3)#3.395

#get Omega 
semTools::reliability(result_mood_T1)

####################
# T2
####################
#########
#Single Factor 

M_T2 <-"
! regressions 
   Mood_T2=~lambda1*M1_T2
   Mood_T2=~lambda2*M10_T2
   Mood_T2=~lambda3*M11_T2
   Mood_T2=~lambda4*M2_T2
   Mood_T2=~lambda5*M3_T2
   Mood_T2=~lambda6*M4_T2
   Mood_T2=~lambda7*M5_T2
   Mood_T2=~lambda8*M6_T2
   Mood_T2=~lambda9*M7_T2
   Mood_T2=~lambda10*M8_T2
   Mood_T2=~lambda11*M9_T2
! residuals, variances and covariances
   M1_T2 ~~ e1*M1_T2
   M10_T2 ~~ e2*M10_T2
   M11_T2 ~~ e3*M11_T2
   M2_T2 ~~ e4*M2_T2
   M3_T2 ~~ e5*M3_T2
   M4_T2 ~~ e6*M4_T2
   M5_T2 ~~ e7*M5_T2
   M6_T2 ~~ e8*M6_T2
   M7_T2 ~~ e9*M7_T2
   M8_T2 ~~ e10*M8_T2
   M9_T2 ~~ e11*M9_T2
   Mood_T2 ~~ 1.0*Mood_T2
! means
   Mood_T2~mu*1
   M1_T2~0*1;
   M10_T2~0*1;
   M11_T2~0*1;
   M2_T2~0*1;
   M3_T2~0*1;
   M4_T2~0*1;
   M5_T2~0*1;
   M6_T2~0*1;
   M7_T2~0*1;
   M8_T2~0*1;
   M9_T2~0*1;
";
result_mood_T2 <-lavaan(M_T2, data=t2, fixed.x=FALSE, missing="FIML");
summary(result_mood_T2, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(result_mood_T2)
T2_fscore <- lavPredict(result_mood_T2, method ="Bartlett")
head(T2_fscore)
round(coefficient_H(result_mood_T2),2)
#
semTools::reliability(result_mood_T2)

####################
# T3
####################

######Single factor 

M_T3 <-"
! regressions 
   Mood_T3=~lambda1*M1_T3
   Mood_T3=~lambda2*M10_T3
   Mood_T3=~lambda3*M11_T3
   Mood_T3=~lambda4*M2_T3
   Mood_T3=~lambda5*M3_T3
   Mood_T3=~lambda6*M4_T3
   Mood_T3=~lambda7*M5_T3
   Mood_T3=~lambda8*M6_T3
   Mood_T3=~lambda9*M7_T3
   Mood_T3=~lambda10*M8_T3
   Mood_T3=~lambda11*M9_T3
! residuals, variances and covariances
   M1_T3 ~~ e1*M1_T3
   M10_T3 ~~ e2*M10_T3
   M11_T3 ~~ e3*M11_T3
   M2_T3 ~~ e4*M2_T3
   M3_T3 ~~ e5*M3_T3
   M4_T3 ~~ e6*M4_T3
   M5_T3 ~~ e7*M5_T3
   M6_T3 ~~ e8*M6_T3
   M7_T3 ~~ e9*M7_T3
   M8_T3 ~~ e10*M8_T3
   M9_T3 ~~ e11*M9_T3
   Mood_T3 ~~ 1.0*Mood_T3
! means
   Mood_T3~mu*1
   M1_T3~0*1;
   M10_T3~0*1;
   M11_T3~0*1;
   M2_T3~0*1;
   M3_T3~0*1;
   M4_T3~0*1;
   M5_T3~0*1;
   M6_T3~0*1;
   M7_T3~0*1;
   M8_T3~0*1;
   M9_T3~0*1;
";
result_mood_T3 <-lavaan(M_T3, data=t3, fixed.x=FALSE, missing="FIML");
summary(result_mood_T3, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(result_mood_T3)
T3_fscore <- lavPredict(result_mood_T3, method ="Bartlett")
head(T3_fscore)
#get H
round(coefficient_H(result_mood_T3),2)
#get Omega 
semTools::reliability(result_mood_T3)

#check correlations
T1 <- crisis_75[,c('T_T1')]
T2 <- crisis_75[,c('T_T2')]
T3 <- crisis_75[,c('T_T3')]

#CFA
f <- cbind(T1_fscore, T2_fscore ,T3_fscore)
corf <- cor(f)
round(f, digits = 2)
head(f)
f <- cbind(ID, T1, T2, T3, T1_fscore, T2_fscore, T3_fscore)
#probably due to time variations 

ICC(f[5:6])
ICC(f[6:7])
###########
#Write DataFrame containing all FScores of Mood and the timestamps 
###########
write.csv(f, file="C:/Users/hofma/Desktop/LGC/02_data/
          Crisis_Mood_fscores.txt", row.names = FALSE)


