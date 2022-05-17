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
demo$Language <- recode(demo$Language, "en" = 1, "fr" = 2, "de" = 3)
describe(demo$Language)
#en = 1
#fr = 3
#de = 3

# age = Age in days at begin of study 

# BG1 = Profession_Status (Sind Sie derzeit berufstätig?) 
#(1)	Working for pay
#(2)	On leave
#(3)	Laid off or lost job
#(4)	Unemployed and looking for a job,
#(5)	Retired
#(6)	Staying at home / homemaker,
#(7)	Disabled
#(8)	Enrolled in school/college/university
describe(demo$BG1) #Looks like the answers are multiplied by 10 
demo$BG1 <- (demo$BG1/10)
describe(demo$BG1)

# BG2 = Urban_Rual (Was beschreibt am besten die Gegend, in der Sie leben?)
#(1)	Large city 
#(2)	Suburbs of a large city
#(3)	Small city
#(4)	Town or village
#(5)	Rural area
describe(demo$BG2) #Looks like the answers are multiplied by 10 
demo$BG2 <- (demo$BG2/10)
describe(demo$BG2)

# BG3 = Size_of_household (Wie viele Personen leben derzeit in Ihrem Haushalt (Sie eingeschlossen)?
#- open 
describe(demo$BG3)

# BG6 = Phys_Health_percieved - Wie würden Sie Ihre allgemeine körperliche Gesundheit einschätzen?
#(1)	Excellent 
#(2)	Very Good 
#(3)	Good 
#(4)  Fair
#(5)	Poor 
describe(demo$BG6) #Looks like the answers are multiplied by 10 
demo$BG6 <- (demo$BG6/10)
describe(demo$BG6)

# BG11 = Psych_Health_percieved - Wie würden Sie Ihre allgemeine psychische/emotionale Gesundheit vor der Coronavirus/COVID-19-Krise in Ihrer Region einschätzen? 5 = Schelcht
#(1)	Excellent
#(2)	Very Good
#(3)	Good
#(4)	Fair
#(5)	Poor
describe(demo$BG11) #Looks like the answers are multiplied by 10 
demo$BG11 <- (demo$BG11/10)
describe(demo$BG11)

colnames(demo)
demodat <- demo[, c(1, 2:3, 8:12)]
colnames(demodat) <- c("ID","Origin", "Age", "Employment","Urbanicity", "Household", "Phy_Health", "Psy_Health")
head(demodat)

#Trasnform age in days to age 
demodat[,3] <- (demodat[,3])/365
demodat[,3] <- round(demodat[,3]) #round 
demodat[,2:8] <- lapply(demodat[,2:8], function(x) as.numeric(as.character(x)))
summary(demodat)

write.csv(demo, "C:/Users/hofma/Desktop/LGC/02_data/Crisis_bl_demo.txt", row.names = FALSE)








ab <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/FU3COVBL.csv", col_names = TRUE, col_types = cols(ID = "c"))
colnames(ab)
bc <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/COVBLFU.csv", col_names = TRUE, col_types = cols(ID = "c"))


m <- polr(Dummy_A ~ pared + public + gpa, data = , Hess=TRUE)

## view a summary of the model
summary(m)

# Fit the model
model <- glm(  ~., data = train.data, family = binomial)
# Summarize the model
summary(model)
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == test.data$diabetes)
