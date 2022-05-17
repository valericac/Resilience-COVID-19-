library(readr)
library(tidyr)
library(dplyr)
library(lavaan)
library(semPlot)
library(sjPlot)
library(semoutput)
options(max.print = 10000000)
getOption("max.print")

dawba <- readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_CFA.csv", col_names=TRUE, col_types = cols(.default = "n", ID = "c"));

# NA 
sum(is.na(dawba))#check for NAs 
dawba[is.na(dawba)] <- 0 #replace NA with0s 
sum(is.na(dawba))#check if it worked

stress <- readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/stress_score.csv", col_names = TRUE, col_types = cols(stress_mean = "n", ID = "c"))
d_dawba <- merge(stress, dawba, by="ID")
crisis <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/crisis_75.csv", col_names = TRUE, col_types = cols(stress_mean = "n", ID = "c"))
d_all <- merge(d_dawba, crisis, by="ID")

#CFA
 Int_model<- '! regressions 
   SPE=~1.0*sb1a
   SPE=~SPE__sb1g*sb1g
   SPE=~SPE__sb1m*sb1m
   SPE=~SPE__sb7*sb7
   SPE=~SPE__sb1f*sb1f
   SPE=~SPE__sb1l*sb1l
   SPE=~SPE__sb6*sb6
   SPE=~SPE__sb5*sb5
   SPE=~SPE__sb1k*sb1k
   SPE=~SPE__sb1e*sb1e
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
   SOC=~1.0*sc1
   SOC=~SOC__sc2a*sc2a
   SOC=~SOC__sc2b*sc2b
   SOC=~SOC__sc2c*sc2c
   SOC=~SOC__sc2d*sc2d
   SOC=~SOC__sc2e*sc2e
   SOC=~SOC__sc2f*sc2f
   SOC=~SOC__sc6*sc6
   SOC=~SOC__sc13*sc13
   SOC=~SOC__sc12*sc12
   SOC=~SOC__sc11*sc11
   SOC=~SOC__sc10c*sc10c
   SOC=~SOC__sc10b*sc10b
   SOC=~SOC__sc10a*sc10a
   SOC=~SOC__sc8*sc8
   SOC=~SOC__sc7*sc7
   SOC=~SOC__sc16*sc16
   SOC=~SOC__sc15*sc15
   SOC=~SOC__sc14*sc14
   PAN=~1.0*sd1
   PAN=~PAN__sd3d*sd3d
   PAN=~PAN__sd3c*sd3c
   PAN=~PAN__sd3b*sd3b
   PAN=~PAN__sd3a*sd3a
   PAN=~PAN__sd2c*sd2c
   PAN=~PAN__sd2b*sd2b
   PAN=~PAN__sd2a*sd2a
   PAN=~PAN__sd3l*sd3l
   PAN=~PAN__sd3k*sd3k
   PAN=~PAN__sd3j*sd3j
   PAN=~PAN__sd3i*sd3i
   PAN=~PAN__sd3h*sd3h
   PAN=~PAN__sd3g*sd3g
   PAN=~PAN__sd3f*sd3f
   PAN=~PAN__sd3e*sd3e
   PAN=~PAN__sd6*sd6
   PAN=~PAN__sd5*sd5
   PAN=~PAN__sd4d*sd4d
   PAN=~PAN__sd4c*sd4c
   PAN=~PAN__sd4b*sd4b
   PAN=~PAN__sd4a*sd4a
   PAN=~PAN__sd3n*sd3n
   PAN=~PAN__sd3m*sd3m
   PAN=~PAN__sd7d*sd7d
   PAN=~PAN__sd7c*sd7c
   PAN=~PAN__sd7b*sd7b
   PAN=~PAN__sd7a*sd7a
   DEP=~1.0*sh1
   DEP=~GAD__sh18i*sh18i
   DEP=~GAD__sh17*sh17
   DEP=~GAD__sh2*sh2
   DEP=~GAD__sh18a*sh18a
   DEP=~GAD__sh18j*sh18j
   DEP=~GAD__sh3*sh3
   DEP=~GAD__sh18b*sh18b
   DEP=~GAD__sh18k*sh18k
   DEP=~GAD__sh4*sh4
   DEP=~GAD__sh18c*sh18c
   DEP=~GAD__sh18l*sh18l
   DEP=~GAD__sh5*sh5
   DEP=~GAD__sh18d*sh18d
   DEP=~GAD__sh19*sh19
   DEP=~GAD__sh13*sh13
   DEP=~GAD__sh18e*sh18e
   DEP=~GAD__sh20a*sh20a
   DEP=~GAD__sh20b*sh20b
   DEP=~GAD__sh18f*sh18f
   DEP=~GAD__sh14*sh14
   DEP=~GAD__sh20c*sh20c
   DEP=~GAD__sh18g*sh18g
   DEP=~GAD__sh15*sh15
   DEP=~GAD__sh20d*sh20d
   DEP=~GAD__sh18h*sh18h
   DEP=~GAD__sh16*sh16
   GAD=~PTSD__sg4a*sg4a
   GAD=~PTSD__sg4b*sg4b
   GAD=~PTSD__sg4c*sg4c
   GAD=~PTSD__sg8a*sg8a
   GAD=~PTSD__sg8b*sg8b
   GAD=~PTSD__sg8c*sg8c
   GAD=~PTSD__sg4d*sg4d
   GAD=~PTSD__sg8d*sg8d
   GAD=~PTSD__sg4e*sg4e
   GAD=~PTSD__sg8e*sg8e
   GAD=~PTSD__sg4f*sg4f
   GAD=~PTSD__sg8f*sg8f
   GAD=~PTSD__sg4g*sg4g
   GAD=~PTSD__sg9*sg9
   GAD=~PTSD__sg4h*sg4h
   GAD=~PTSD__sg10a*sg10a
   GAD=~PTSD__sg4i*sg4i
   GAD=~PTSD__sg10b*sg10b
   GAD=~PTSD__sg10c*sg10c
   GAD=~PTSD__sg4j*sg4j
   GAD=~PTSD__sg10d*sg10d
   GAD=~PTSD__sg4k*sg4k
   GAD=~PTSD__sg6*sg6
   GAD=~PTSD__sg7*sg7
   GAD=~1.0*sg2a
   GAD=~GAD__sg2*sg2
   GAD=~GAD__sg3*sg3
   PTSD=~1.0*se7
   PTSD=~PTSD__se1*se1
   PTSD=~PTSD__se4a*se4a
   PTSD=~PTSD__se4o*se4o
   PTSD=~PTSD__se2a*se2a
   PTSD=~PTSD__se4b*se4b
   PTSD=~PTSD__se5*se5
   PTSD=~PTSD__se2b*se2b
   PTSD=~PTSD__se4c*se4c
   PTSD=~PTSD__se6*se6
   PTSD=~PTSD__se2c*se2c
   PTSD=~PTSD__se4d*se4d
   PTSD=~PTSD__se2d*se2d
   PTSD=~PTSD__se4e*se4e
   PTSD=~PTSD__se8a*se8a
   PTSD=~PTSD__se2e*se2e
   PTSD=~PTSD__se4f*se4f
   PTSD=~PTSD__se8b*se8b
   PTSD=~PTSD__se2f*se2f
   PTSD=~PTSD__se4g*se4g
   PTSD=~PTSD__se8c*se8c
   PTSD=~PTSD__se2g*se2g
   PTSD=~PTSD__se4h*se4h
   PTSD=~PTSD__se8d*se8d
   PTSD=~PTSD__se2h*se2h
   PTSD=~PTSD__se4i*se4i
   PTSD=~PTSD__se2i*se2i
   PTSD=~PTSD__se4j*se4j
   PTSD=~PTSD__se2j*se2j
   PTSD=~PTSD__se4k*se4k
   PTSD=~PTSD__se4l*se4l
   PTSD=~PTSD__se2k*se2k
   PTSD=~PTSD__se4m*se4m
   PTSD=~PTSD__se3*se3
   PTSD=~PTSD__se3a*se3a
   PTSD=~PTSD__se4n*se4n
   Internalizing=~Fear__DEP*PTSD
   Internalizing=~Fear__PTSD*GAD
   Internalizing=~Fear__GAD*DEP
   Internalizing=~Fear__SPE*SPE
   Internalizing=~Fear__SOC*SOC
   Internalizing=~Fear__PAN*PAN
! residuals, variances and covariances
   PAN ~~ VAR_PAN*PAN
   SOC ~~ VAR_SOC*SOC
   SPE ~~ VAR_SPE*SPE
   DEP ~~ VAR_GAD*DEP
   GAD ~~ VAR_PTSD*GAD
   PTSD ~~ VAR_DEP*PTSD
   Internalizing ~~ VAR_Fear*Internalizing
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
   sc1 ~~ VAR_sc1*sc1
   sc2a ~~ VAR_sc2a*sc2a
   sc2b ~~ VAR_sc2b*sc2b
   sc2c ~~ VAR_sc2c*sc2c
   sc2d ~~ VAR_sc2d*sc2d
   sc2e ~~ VAR_sc2e*sc2e
   sc2f ~~ VAR_sc2f*sc2f
   sc6 ~~ VAR_sc6*sc6
   sc7 ~~ VAR_sc7*sc7
   sc8 ~~ VAR_sc8*sc8
   sc10a ~~ VAR_sc10a*sc10a
   sc10b ~~ VAR_sc10b*sc10b
   sc10c ~~ VAR_sc10c*sc10c
   sc11 ~~ VAR_sc11*sc11
   sc12 ~~ VAR_sc12*sc12
   sc13 ~~ VAR_sc13*sc13
   sc14 ~~ VAR_sc14*sc14
   sc15 ~~ VAR_sc15*sc15
   sc16 ~~ VAR_sc16*sc16
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
   sg2 ~~ VAR_sg2*sg2
   sg2a ~~ VAR_sg2a*sg2a
   sg3 ~~ VAR_sg3*sg3
   se1 ~~ VAR_se1*se1
   se2a ~~ VAR_se2a*se2a
   se2b ~~ VAR_se2b*se2b
   se2c ~~ VAR_se2c*se2c
   se2d ~~ VAR_se2d*se2d
   se2e ~~ VAR_se2e*se2e
   se2f ~~ VAR_se2f*se2f
   se2g ~~ VAR_se2g*se2g
   se2h ~~ VAR_se2h*se2h
   se2i ~~ VAR_se2i*se2i
   se2j ~~ VAR_se2j*se2j
   se2k ~~ VAR_se2k*se2k
   se3 ~~ VAR_se3*se3
   se3a ~~ VAR_se3a*se3a
   se4a ~~ VAR_se4a*se4a
   se4b ~~ VAR_se4b*se4b
   se4c ~~ VAR_se4c*se4c
   se4d ~~ VAR_se4d*se4d
   se4e ~~ VAR_se4e*se4e
   se4f ~~ VAR_se4f*se4f
   se4g ~~ VAR_se4g*se4g
   se4h ~~ VAR_se4h*se4h
   se4i ~~ VAR_se4i*se4i
   se4j ~~ VAR_se4j*se4j
   se4k ~~ VAR_se4k*se4k
   se4l ~~ VAR_se4l*se4l
   se4m ~~ VAR_se4m*se4m
   se4n ~~ VAR_se4n*se4n
   se4o ~~ VAR_se4o*se4o
   se5 ~~ VAR_se5*se5
   se6 ~~ VAR_se6*se6
   se7 ~~ VAR_se7*se7
   se8a ~~ VAR_se8a*se8a
   se8b ~~ VAR_se8b*se8b
   se8c ~~ VAR_se8c*se8c
   se8d ~~ VAR_se8d*se8d
   PAN ~~ 0.0*SOC
   PAN ~~ 0.0*SPE
   PAN ~~ 0.0*DEP
   PAN ~~ 0.0*GAD
   PAN ~~ 0.0*PTSD
   SOC ~~ 0.0*SPE
   SOC ~~ 0.0*DEP
   SOC ~~ 0.0*GAD
   SOC ~~ 0.0*PTSD
   SPE ~~ 0.0*DEP
   SPE ~~ 0.0*GAD
   SPE ~~ 0.0*PTSD
   DEP ~~ 0.0*GAD
   DEP ~~ 0.0*PTSD
   GAD ~~ 0.0*PTSD
! means
   Internalizing~1.0*1;
   PAN~0*1;
   SOC~0*1;
   SPE~0*1;
   DEP~0*1;
   GAD~0*1;
   PTSD~0*1;
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
   sc1~0*1;
   sc2a~0*1;
   sc2b~0*1;
   sc2c~0*1;
   sc2d~0*1;
   sc2e~0*1;
   sc2f~0*1;
   sc6~0*1;
   sc7~0*1;
   sc8~0*1;
   sc10a~0*1;
   sc10b~0*1;
   sc10c~0*1;
   sc11~0*1;
   sc12~0*1;
   sc13~0*1;
   sc14~0*1;
   sc15~0*1;
   sc16~0*1;
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
   sg2~0*1;
   sg2a~0*1;
   sg3~0*1;
   se1~0*1;
   se2a~0*1;
   se2b~0*1;
   se2c~0*1;
   se2d~0*1;
   se2e~0*1;
   se2f~0*1;
   se2g~0*1;
   se2h~0*1;
   se2i~0*1;
   se2j~0*1;
   se2k~0*1;
   se3~0*1;
   se3a~0*1;
   se4a~0*1;
   se4b~0*1;
   se4c~0*1;
   se4d~0*1;
   se4e~0*1;
   se4f~0*1;
   se4g~0*1;
   se4h~0*1;
   se4i~0*1;
   se4j~0*1;
   se4k~0*1;
   se4l~0*1;
   se4m~0*1;
   se4n~0*1;
   se4o~0*1;
   se5~0*1;
   se6~0*1;
   se7~0*1;
   se8a~0*1;
   se8b~0*1;
   se8c~0*1;
   se8d~0*1;
';

Int_result <-lavaan(Int_model, data=dawba[, 2:160], fixed.x=FALSE, missing="FIML");
summary(Int_result, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(Int_result)
one_fscore <- lavPredict(Int_result, method ="Bartlett")
sem_tables(Int_result)

