library(readr)
library(tidyr)
library(dplyr)
library(lavaan)
library(lmtest)
library(pander)
library(semoutput)
options(max.print = 10000000)
getOption("max.print")

# In this Version two subfactors fear and distress + costant 
dawba <- readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU3_CFA.csv", col_names=TRUE, col_types = cols(.default = "n", ID = "c"));

# NA 
sum(is.na(dawba))#check for NAs 
#Problem for PTSD there are more Missings than non Missings - however those who provided info 
#are important to consider, imptation of missings above 10% of data is not
#recommended. The status is 
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- dawba[,PTSD_names]
#if first questions is answered with no all others will be NA
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] 
2223-2182 #41 Participants 

#Check if they also filled in the stress score questionnaires 
stress <- readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/stress_score.csv", col_names = TRUE, col_types = cols(stress_mean = "n", ID = "c"))
ds <- merge(dawba, stress)
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- ds[,PTSD_names]
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] 
671-664 #7 

#Check if these continued in IMACOV
crisis <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt")
dsc <- merge(ds, crisis)
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- dsc[,PTSD_names]
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] #none of them continued 
#Exclude PTSD

#Check if these continued in IMACOV
crisis <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt")
dsc <- merge(ds, crisis)
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- dsc[,PTSD_names]
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] #none of them continued 
#Exclude PTSD

#Specific Fear and Phobia 
SPE_names <- c('sb1a' , 'sb1a' , 'sb1b' , 'sb1c' , 'sb1d' , 'sb1e' , 'sb1f' ,
'sb1g' , 'sb1h' , 'sb1i' , 'sb1j' , 'sb1k' , 'sb1l' , 'sb1m' , 'sb2' , 'sb3' ,
' sb4' , 'sb5' , 'sb6' , 'sb7' , 'sb8' , 'sb9' , 'sb10')
SPE <- dsc[,SPE_names]
SPE_NA <- SPE[rowSums(is.na(SPE)) > 0, ]

dd <- dawba[,!names(dawba) %in% PTSD_names]
d <- dd[,!names(dd) %in% SPE_names]

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(d,2,pMiss)
apply(d,1,pMiss)
#install.packages("mice")
library(mice)
md.pattern(d)
#complete observations

#marginplot(d[c(1,2)])

#0 indicated no burden of the symptom 
dawba[is.na(dawba)] <- 0 #replace NA with0s 
sum(is.na(dawba))#check if it worked

Int_model_2_constant <- model<-"
! regressions 
   SPE=~1.0*sb1a
   SPE=~SPE__sb1f*sb1f
   SPE=~SPE__sb1l*sb1l
   SPE=~SPE__sb6*sb6
   SPE=~SPE__sb1e*sb1e
   SPE=~SPE__sb1k*sb1k
   SPE=~SPE__sb5*sb5
   SPE=~SPE__sb1d*sb1d
   SPE=~SPE__sb1j*sb1j
   SPE=~SPE__sb4*sb4
   SPE=~SPE__sb10*sb10
   SPE=~SPE__sb9*sb9
   SPE=~SPE__sb3*sb3
   SPE=~SPE__sb1i*sb1i
   SPE=~SPE__sb1c*sb1c
   SPE=~SPE__sb8*sb8
   SPE=~SPE__sb2*sb2
   SPE=~SPE__sb1h*sb1h
   SPE=~SPE__sb1b*sb1b
   SPE=~SPE__sb7*sb7
   SPE=~SPE__sb1m*sb1m
   SPE=~SPE__sb1g*sb1g
   PAN=~PAN__sd3f*sd3f
   PAN=~1.0*sd1
   PAN=~PAN__sd7b*sd7b
   PAN=~PAN__sd3i*sd3i
   PAN=~PAN__sd3j*sd3j
   PAN=~PAN__sd7a*sd7a
   PAN=~PAN__sd3e*sd3e
   PAN=~PAN__sd3k*sd3k
   PAN=~PAN__sd6*sd6
   PAN=~PAN__sd3d*sd3d
   PAN=~PAN__sd3l*sd3l
   PAN=~PAN__sd5*sd5
   PAN=~PAN__sd3c*sd3c
   PAN=~PAN__sd7c*sd7c
   PAN=~PAN__sd4d*sd4d
   PAN=~PAN__sd3b*sd3b
   PAN=~PAN__sd7d*sd7d
   PAN=~PAN__sd4c*sd4c
   PAN=~PAN__sd3a*sd3a
   PAN=~PAN__sd3g*sd3g
   PAN=~PAN__sd4b*sd4b
   PAN=~PAN__sd2c*sd2c
   PAN=~PAN__sd3h*sd3h
   PAN=~PAN__sd4a*sd4a
   PAN=~PAN__sd2b*sd2b
   PAN=~PAN__sd3m*sd3m
   PAN=~PAN__sd3n*sd3n
   PAN=~PAN__sd2a*sd2a
   DEP=~1.0*sh1
   DEP=~DEP__sh20d*sh20d
   DEP=~DEP__sh17*sh17
   DEP=~DEP__sh20c*sh20c
   DEP=~DEP__sh18a*sh18a
   DEP=~DEP__sh20a*sh20a
   DEP=~DEP__sh15*sh15
   DEP=~DEP__sh16*sh16
   DEP=~DEP__sh19*sh19
   DEP=~DEP__sh14*sh14
   DEP=~DEP__sh20b*sh20b
   DEP=~DEP__sh18l*sh18l
   DEP=~DEP__sh13*sh13
   DEP=~DEP__sh18f*sh18f
   DEP=~DEP__sh18k*sh18k
   DEP=~DEP__sh5*sh5
   DEP=~DEP__sh18e*sh18e
   DEP=~DEP__sh18j*sh18j
   DEP=~DEP__sh4*sh4
   DEP=~DEP__sh18d*sh18d
   DEP=~DEP__sh18i*sh18i
   DEP=~DEP__sh3*sh3
   DEP=~DEP__sh18c*sh18c
   DEP=~DEP__sh18h*sh18h
   DEP=~DEP__sh2*sh2
   DEP=~DEP__sh18b*sh18b
   DEP=~DEP__sh18g*sh18g
   GAD=~1.0*sg4b
   GAD=~GAD__sg4e*sg4e
   GAD=~GAD__sg4j*sg4j
   GAD=~GAD__sg8b*sg8b
   GAD=~GAD__sg9*sg9
   GAD=~GAD__sg10d*sg10d
   GAD=~GAD__sg8f*sg8f
   GAD=~GAD__sg8a*sg8a
   GAD=~GAD__sg4i*sg4i
   GAD=~GAD__sg4d*sg4d
   GAD=~GAD__sg10c*sg10c
   GAD=~GAD__sg8e*sg8e
   GAD=~GAD__sg7*sg7
   GAD=~GAD__sg4h*sg4h
   GAD=~GAD__sg4c*sg4c
   GAD=~GAD__sg10b*sg10b
   GAD=~GAD__sg8d*sg8d
   GAD=~GAD__sg6*sg6
   GAD=~GAD__sg4g*sg4g
   GAD=~GAD__sg10a*sg10a
   GAD=~GAD__sg8c*sg8c
   GAD=~GAD__sg4k*sg4k
   GAD=~GAD__sg4f*sg4f
   GAD=~GAD__sg4a*sg4a
   Fear=~Distress__GAD*GAD
   Fear=~Distress__SPE*SPE
   Fear=~Distress__PAN*PAN
   Distress=~Distress__GAD*GAD
   Distress=~Distress__SPE*SPE
   Distress=~Distress__DEP*DEP
! residuals, variances and covariances
   SPE ~~ VAR_SPE*SPE
   sb1a ~~ VAR_sb1a*sb1a
   sb1b ~~ VAR_sb1b*sb1b
   sb1c ~~ VAR_sb1c*sb1c
   sb1d ~~ VAR_sb1d*sb1d
   sb1e ~~ VAR_sb1e*sb1e
   sb1f ~~ VAR_sb1f*sb1f
   sb1g ~~ VAR_sb1g*sb1g
   sb1h ~~ VAR_sb1h*sb1h
   sb1i ~~ VAR_sb1i*sb1i
   sb1j ~~ VAR_sb1j*sb1j
   sb1k ~~ VAR_sb1k*sb1k
   sb1l ~~ VAR_sb1l*sb1l
   sb1m ~~ VAR_sb1m*sb1m
   sb2 ~~ VAR_sb2*sb2
   sb3 ~~ VAR_sb3*sb3
   sb4 ~~ VAR_sb4*sb4
   sb5 ~~ VAR_sb5*sb5
   sb6 ~~ VAR_sb6*sb6
   sb7 ~~ VAR_sb7*sb7
   sb8 ~~ VAR_sb8*sb8
   sb9 ~~ VAR_sb9*sb9
   sb10 ~~ VAR_sb10*sb10
   sc2a ~~ VAR_sc2a*sc2a
   sc2b ~~ VAR_sc2b*sc2b
   sc2c ~~ VAR_sc2c*sc2c
   sc8 ~~ VAR_sc8*sc8
   sc10a ~~ VAR_sc10a*sc10a
   sc10c ~~ VAR_sc10c*sc10c
   sc11 ~~ VAR_sc11*sc11
   sc12 ~~ VAR_sc12*sc12
   sc13 ~~ VAR_sc13*sc13
   PAN ~~ VAR_PAN*PAN
   sd1 ~~ VAR_sd1*sd1
   sd2a ~~ VAR_sd2a*sd2a
   sd2b ~~ VAR_sd2b*sd2b
   sd2c ~~ VAR_sd2c*sd2c
   sd3a ~~ VAR_sd3a*sd3a
   sd3b ~~ VAR_sd3b*sd3b
   sd3c ~~ VAR_sd3c*sd3c
   sd3d ~~ VAR_sd3d*sd3d
   sd3e ~~ VAR_sd3e*sd3e
   sd3f ~~ VAR_sd3f*sd3f
   sd3g ~~ VAR_sd3g*sd3g
   sd3h ~~ VAR_sd3h*sd3h
   sd3i ~~ VAR_sd3i*sd3i
   sd3j ~~ VAR_sd3j*sd3j
   sd3k ~~ VAR_sd3k*sd3k
   sd3l ~~ VAR_sd3l*sd3l
   sd3m ~~ VAR_sd3m*sd3m
   sd3n ~~ VAR_sd3n*sd3n
   sd4a ~~ VAR_sd4a*sd4a
   sd4b ~~ VAR_sd4b*sd4b
   sd4c ~~ VAR_sd4c*sd4c
   sd4d ~~ VAR_sd4d*sd4d
   sd5 ~~ VAR_sd5*sd5
   sd6 ~~ VAR_sd6*sd6
   sd7a ~~ VAR_sd7a*sd7a
   sd7b ~~ VAR_sd7b*sd7b
   sd7c ~~ VAR_sd7c*sd7c
   sd7d ~~ VAR_sd7d*sd7d
   sh1 ~~ VAR_sh1*sh1
   sh2 ~~ VAR_sh2*sh2
   sh3 ~~ VAR_sh3*sh3
   sh4 ~~ VAR_sh4*sh4
   sh5 ~~ VAR_sh5*sh5
   sh13 ~~ VAR_sh13*sh13
   sh14 ~~ VAR_sh14*sh14
   sh15 ~~ VAR_sh15*sh15
   sh16 ~~ VAR_sh16*sh16
   sh17 ~~ VAR_sh17*sh17
   sh18a ~~ VAR_sh18a*sh18a
   sh18b ~~ VAR_sh18b*sh18b
   sh18c ~~ VAR_sh18c*sh18c
   sh18d ~~ VAR_sh18d*sh18d
   sh18e ~~ VAR_sh18e*sh18e
   sh18f ~~ VAR_sh18f*sh18f
   sh18g ~~ VAR_sh18g*sh18g
   sh18h ~~ VAR_sh18h*sh18h
   sh18i ~~ VAR_sh18i*sh18i
   sh18j ~~ VAR_sh18j*sh18j
   sh18k ~~ VAR_sh18k*sh18k
   sh18l ~~ VAR_sh18l*sh18l
   sh19 ~~ VAR_sh19*sh19
   sh20a ~~ VAR_sh20a*sh20a
   sh20b ~~ VAR_sh20b*sh20b
   sh20c ~~ VAR_sh20c*sh20c
   sh20d ~~ VAR_sh20d*sh20d
   sg4a ~~ VAR_sg4a*sg4a
   sg4b ~~ VAR_sg4b*sg4b
   sg4c ~~ VAR_sg4c*sg4c
   sg4d ~~ VAR_sg4d*sg4d
   sg4e ~~ VAR_sg4e*sg4e
   sg4f ~~ VAR_sg4f*sg4f
   sg4g ~~ VAR_sg4g*sg4g
   sg4h ~~ VAR_sg4h*sg4h
   sg4i ~~ VAR_sg4i*sg4i
   sg4j ~~ VAR_sg4j*sg4j
   sg4k ~~ VAR_sg4k*sg4k
   sg6 ~~ VAR_sg6*sg6
   sg7 ~~ VAR_sg7*sg7
   sg8a ~~ VAR_sg8a*sg8a
   sg8b ~~ VAR_sg8b*sg8b
   sg8c ~~ VAR_sg8c*sg8c
   sg8d ~~ VAR_sg8d*sg8d
   sg8e ~~ VAR_sg8e*sg8e
   sg8f ~~ VAR_sg8f*sg8f
   sg9 ~~ VAR_sg9*sg9
   sg10a ~~ VAR_sg10a*sg10a
   sg10b ~~ VAR_sg10b*sg10b
   sg10c ~~ VAR_sg10c*sg10c
   sg10d ~~ VAR_sg10d*sg10d
   GAD ~~ VAR_GAD*GAD
   se8d ~~ VAR_se8d*se8d
   Fear ~~ 1.0*Fear
   Distress ~~ 1.0*Distress
   Fear ~~ COV_Fear_Distress*Distress
   DEP~~0*DEP;
   SPE ~~ 0.0*PAN
   SPE ~~ 0.0*DEP
   SPE ~~ 0.0*GAD
   PAN ~~ 0.0*DEP
   PAN ~~ 0.0*GAD
   PAN ~~ 0.0*Distress
   DEP ~~ 0.0*GAD
   DEP ~~ 0.0*Fear
! means
   Distress~const__Distress*1
   Fear~const__Fear*1
   SPE~0*1;
   sb1a~0*1;
   sb1b~0*1;
   sb1c~0*1;
   sb1d~0*1;
   sb1e~0*1;
   sb1f~0*1;
   sb1g~0*1;
   sb1h~0*1;
   sb1i~0*1;
   sb1j~0*1;
   sb1k~0*1;
   sb1l~0*1;
   sb1m~0*1;
   sb2~0*1;
   sb3~0*1;
   sb4~0*1;
   sb5~0*1;
   sb6~0*1;
   sb7~0*1;
   sb8~0*1;
   sb9~0*1;
   sb10~0*1;
   sc2a~0*1;
   sc2b~0*1;
   sc2c~0*1;
   sc8~0*1;
   sc10a~0*1;
   sc10c~0*1;
   sc11~0*1;
   sc12~0*1;
   sc13~0*1;
   PAN~0*1;
   sd1~0*1;
   sd2a~0*1;
   sd2b~0*1;
   sd2c~0*1;
   sd3a~0*1;
   sd3b~0*1;
   sd3c~0*1;
   sd3d~0*1;
   sd3e~0*1;
   sd3f~0*1;
   sd3g~0*1;
   sd3h~0*1;
   sd3i~0*1;
   sd3j~0*1;
   sd3k~0*1;
   sd3l~0*1;
   sd3m~0*1;
   sd3n~0*1;
   sd4a~0*1;
   sd4b~0*1;
   sd4c~0*1;
   sd4d~0*1;
   sd5~0*1;
   sd6~0*1;
   sd7a~0*1;
   sd7b~0*1;
   sd7c~0*1;
   sd7d~0*1;
   DEP~0*1;
   sh1~0*1;
   sh2~0*1;
   sh3~0*1;
   sh4~0*1;
   sh5~0*1;
   sh13~0*1;
   sh14~0*1;
   sh15~0*1;
   sh16~0*1;
   sh17~0*1;
   sh18a~0*1;
   sh18b~0*1;
   sh18c~0*1;
   sh18d~0*1;
   sh18e~0*1;
   sh18f~0*1;
   sh18g~0*1;
   sh18h~0*1;
   sh18i~0*1;
   sh18j~0*1;
   sh18k~0*1;
   sh18l~0*1;
   sh19~0*1;
   sh20a~0*1;
   sh20b~0*1;
   sh20c~0*1;
   sh20d~0*1;
   sg4a~0*1;
   sg4b~0*1;
   sg4c~0*1;
   sg4d~0*1;
   sg4e~0*1;
   sg4f~0*1;
   sg4g~0*1;
   sg4h~0*1;
   sg4i~0*1;
   sg4j~0*1;
   sg4k~0*1;
   sg6~0*1;
   sg7~0*1;
   sg8a~0*1;
   sg8b~0*1;
   sg8c~0*1;
   sg8d~0*1;
   sg8e~0*1;
   sg8f~0*1;
   sg9~0*1;
   sg10a~0*1;
   sg10b~0*1;
   sg10c~0*1;
   sg10d~0*1;
   GAD~0*1;
";

Int_result_2_constant <-lavaan(Int_model_2_constant, dawba[, 2:160], fixed.x=FALSE, missing="FIML");
summary(Int_result_2_constant, fit.measures=TRUE)
standardizedSolution(Int_result_2_constant)
sem_tables(Int_result_2_constant)

#check modification inices 
#modificationindices(Int_result_2_constant) %>%
#  as_tibble() %>%
# arrange(-mi) %>%
#filter(mi > 11) %>%
#select(DEP, GAD, PAN, SOC, SPE, Fear, Distress) %>%
#pander(caption="Largest MI values for hz.fit")
 

#Get RMSEA 

#RMSEA is one of the only fit indices for which the asymptotic
#sampling distribution is known, so we can make confidence
#intervals and conduct hypothesis tests about its population value.

#Test of Exact Fit
#. H0: RMSEA = 0 in the population
#. Equivalent to the significance test on the chi-square statistic

#. Test of Close Fit (MacCallum et al., 1996)
#. H0: Null hypothesis: RMSEA < RMSEAgood in the population.
#. RMSEAgood is some acceptable value of RMSEA (in lavaan: 0.05)
Tm <- fitMeasures(Int_result_2_constant)[['chisq']]
DFm <- fitMeasures(Int_result_2_constant)['df']

sqrt((Tm - DFm)/(2223* DFm))

fitMeasures(Int_result_2_constant)[['rmsea']]
## [1] 0.06325373

lambda_c <- 0.05^2 * 2223 * DFm
pchisq(Tm, DFm, lambda_c, lower.tail = FALSE)
0

fitMeasures(Int_result_2_constant)['rmsea.pvalue']


#. Test of Not-Close Fit (MacCallum et al., 1996)
#. H0: Null hypothesis: RMSEA > RMSEAbad in the population.
#. RMSEAbad is some unacceptable value of RMSEA (e.g., 0.08)
lambda_c <- 0.08^2 * 2223 * DFm
pchisq(Tm, DFm, lambda_c, lower.tail = TRUE)
0


#compute fscores 
fscores <- lavPredict(Int_result_2_constant, method ="regression")
head(fscore)


#write frame 
ID <- dawba$ID
fscores_d <-  as.data.frame(cbind(ID, fscores))
colnames(fscores_d) <- c("ID", "DEP", "GAD", "PAN", "SOC", "SPE", "Fear", "Distress")
head(fscores_d)
write.csv(fscores_d, "C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FSCORES.txt", row.names = FALSE)

#Export with Stress
fscores_stress_d <- merge(fscores_d, leq, by="ID")
leq_fu3 <- readr::read_csv("LEQ_FU3_rawdata_stripped.csv", col_names=TRUE, col_types = cols(.default = "n", ID = "c"));
dawba_leq <- merge(dawba, leq_fu3, by="ID")
head(fscores_stress_d)
head(dawba_leq)
write.csv(fscores_stress_d, "C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_LEQ_FSCORES.txt", row.names = FALSE)
write.csv(dawba_leq, "C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_LEQ_allitems.txt", row.names = FALSE)

colnames(d) 
write.csv(d, "C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_PCA.txt", row.names = FALSE)


