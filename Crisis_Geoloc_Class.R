
#load data 
#with class
geo <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Geo.csv", col_names = TRUE)
crisis_class <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_3Classes_ident.txt")

g <- merge(geo, crisis_class, by="ID" )
g <- na.omit(g)
g <- as.data.frame(g)
g$geoLoc_latt <- as.numeric(g$geoLoc_latt)
g$geoLoc_long <- as.numeric(g$geoLoc_long)

latlong <- sapply(g[,2:3],as.numeric)
latlong <- na.omit(latlong)

options(viewer=NULL)
install.packages("leaftlet")
library(leaflet)

#Use the GeoData on the Sample
#https://www.geo.fu-berlin.de/en/v/soga/Basics-of-statistics/
#Logistic-Regression/Logistic-Regression-in-R---An-Example/index.html

m <- leaflet()
m <- addTiles(m)

cols <- c("red", "navy", "darkgreen")
m <- addCircleMarkers(m, 
                      lng = g$geoLoc_long, 
                      lat = g$geoLoc_latt,
                      radius = 2.5,
                      color = cols[g$class])
m

#searched locations
s <- g[4]

print(s)

#other than UK, Germany and France
#cities 
#Aalborg - Denmark
#Zürich - Schweiz
#Barcelona - Spanien 
#Maastrich - NL
#Bergen - Norway

