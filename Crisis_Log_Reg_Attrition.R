#Ordinal Logistic Regression
#03.04.2022
#Valerie Hofmann
#IMACOV 

library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
require(reshape2)
library(readr)

#prep
demo <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/Crisis_BL_sep.csv", col_names = TRUE, col_types = cols(ID = "c"))
colnames(demo)

# L_T1 = Language 
describe(demo$Language)
demo$Language[demo$Language == "en"] <- 1
demo$Language[demo$Language == "fr"] <- 2
demo$Language[demo$Language == "de"] <- 3
describe(demo$Language)
# age = Age in days at begin of study 
# BG1 = Profession_Status (Sind Sie derzeit berufstätig?) 
#(1)	Working for pay
#(2)	On leave
#(3)	On payed leave 
#(4)	Unemployed and looking for a job,
#(8)	Enrolled in school/college/university
describe(demo$BG1) #Looks like the answers are multiplied by 10 
demo$BG1 <- (demo$BG1/10)
describe(demo$BG1)
demo$BG1[demo$BG1 == 2] <- 1
demo$BG1[demo$BG1 == 4] <- 2
demo$BG1[demo$BG1 == 3] <- 1
demo$BG1[demo$BG1 == 5] <- 1
#demo$BG1 <- recode(demo$BG1, "1" = "1", "2" = "1", "3" = "1", "4" = "2", "5" = "1")
describe(demo$BG1) 
#being unemployed = 2
#being working or on leave, payed or unpayed or a student

# BG2 = Urban_Rual (Was beschreibt am besten die Gegend, in der Sie leben?)
#(1)	Large city 
#(2)	Suburbs of a large city
#(3)	Small city
#(4)	Town or village
#(5)	Rural area
describe(demo$BG2) #Looks like the answers are multiplied by 10 
demo$BG2 <- (demo$BG2/10)
describe(demo$BG2)
demo$BG2[demo$BG2 == 2] <- 1
demo$BG2[demo$BG2 == 3] <- 2
demo$BG2[demo$BG2 == 4] <- 2
demo$BG2[demo$BG2 == 5] <- 2
describe(demo$BG2) 
#living in a small town or rual area
#living in a bigger city or in the suburbs of a bigger city = 1

# BG3 = Size_of_household (Wie viele Personen leben derzeit in Ihrem Haushalt (Sie eingeschlossen)?
#- open 
describe(demo$BG3)
colnames(demo)
demo$BG3 <- sapply(demo$BG3, as.numeric)
demo$BG3[demo$BG3 == 1] <- 2
demo$BG3[demo$BG3 == 0] <- 1
demo$BG3[demo$BG3 > 2] <- 1
#0 value does not fit the question of (how many people including yourself are living )
describe(demo$BG3) 
#living in a household of more than two people increased --> 1=2 people, 2 = more

# BG6 = Phys_Health_percieved - Wie würden Sie Ihre allgemeine körperliche Gesundheit einschätzen?
#(1)	Excellent 
#(2)	Very Good 
#(3)	Good 
#(4)  Fair
#(5)	Poor 
describe(demo$BG6) #Looks like the answers are multiplied by 10 
demo$BG6 <- (demo$BG6/10)
demo$BG6 <- sapply(demo$BG6, as.numeric)
demo$BG6[demo$BG6 == 2] <- 1
demo$BG6[demo$BG6 == 3] <- 1
demo$BG6[demo$BG6 == 4] <- 2
demo$BG6[demo$BG6 == 5] <- 2
describe(demo$BG6) 
#fair and poor health = 2, excelent to good = 1
#having fair to poor physical health increased the by 

# BG11 = Psych_Health_percieved - Wie würden Sie Ihre allgemeine psychische/emotionale Gesundheit vor der Coronavirus/COVID-19-Krise in Ihrer Region einschätzen? 5 = Schelcht
#(1)	Excellent
#(2)	Very Good
#(3)	Good
#(4)	Fair
#(5)	Poor
describe(demo$BG11) #Looks like the answers are multiplied by 10 
demo$BG11 <- (demo$BG11/10)
describe(demo$BG11)
demo$BG11 <- sapply(demo$BG11, as.numeric)
demo$BG11[demo$BG11 == 2] <- 1
demo$BG11[demo$BG11 == 3] <- 1
demo$BG11[demo$BG11 == 4] <- 2
demo$BG11[demo$BG11 == 5] <- 2
describe(demo$BG11) 
#fair and poor = 2, excelent to good = 1
#having fair to poor psychological health increased the by 

colnames(demo)
demodat <- demo[, c(1, 2:3, 8:12)]
colnames(demodat) <- c("ID","Origin", "Age", "Employment","Urbanicity", "Household", "Phy_Health", "Psy_Health")
head(demodat)

#Transform age in days to age 
demodat[,3] <- (demodat[,3])/365
demodat[,3] <- round(demodat[,3]) #round 
demodat[,2:8] <- lapply(demodat[,2:8], function(x) as.numeric(as.character(x)))
describe(demodat)

#get gender from BL 
gender <- readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_BL_Gender.txt")
#merge
table(gender[2])
dat <- merge(demodat, gender, by="ID")
head(dat)

#export file
write.csv(dat, "C:/Users/hofma/Desktop/LGC/02_data/Crisis_bl_demo.txt", row.names = FALSE)


################################
#Mulitvariate Regression of those who did not continue
################################

#load, data was preped in Python 
h <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/FU3COVBL.csv",
                      col_names = TRUE, col_types = cols(ID = "c"))
i <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/COVBLFU.csv", 
                      col_names = TRUE, col_types = cols(ID = "c"))
j <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/COVFUFU2.csv",
                      col_names = TRUE, col_types = cols(ID = "c"))

table(h[3])
table(i[3])
table(j[3])

#Attrition 
a <- merge(h, gender, by ="ID")
head(a)
table(a$gender)
b <- merge(i, dat, by="ID")
c <- merge(j, dat, by="ID")

a[,3:11] <- lapply(a[,3:11], function(x) as.numeric(as.character(x)))
b[,3:11] <- lapply(b[,3:11], function(x) as.numeric(as.character(x)))
c[,3:11] <- lapply(c[,3:11], function(x) as.numeric(as.character(x)))


#check attrition, we only have information on gender on T1 and T2 
t1t2 <- glm(Dummy_A ~ gender, data = a, family = 'binomial') #dummy 0= continued
## view a summary of the model
summary(t1t2)
round(coef(summary(t1t2))[,4],3)
lreg1.or <-exp(cbind(OR = coef(t1t2), confint(t1t2)))
round(lreg1.or, digits=3)

#now t2 to t3
t2t3 <- glm(Dummy_A ~ Age + gender + Phy_Health + Psy_Health + Employment
            + Urbanicity + Household , data = b, family = 'binomial')
## view a summary of the model
summary(t2t3)
round(coef(summary(t2t3))[,4],3)
lreg2.or <-exp(cbind(OR = coef(t2t3), confint(t2t3)))
round(lreg2.or, digits=4)

#check attrition from t3 to t4
t3t4 <- glm(Dummy_A ~ Age + gender + Phy_Health +
              Psy_Health + Employment + Urbanicity + Household ,
            data = c, family = 'binomial')
## view a summary of the model
summary(t3t4)
round(coef(summary(t3t4))[,4],3)
lreg3.or <-exp(cbind(OR = coef(t3t4), confint(t3t4)))
round(lreg3.or, digits=4)

###Attrition Rates 
#BL to T1
a <- (2087+1298)/2
b <- 2087-1298
round(b/a,4)

#T1 to T2
a <- (1298+411)/2
b <- 1298-411
round(b/a,4)
1.038*100
 
#T2 und T3
a <- (411+342)/2
b <- 411-342
round(b/a,4)

#T2 und T3
a <- (342+300)/2
b <- 342-300
round(b/a,4)
