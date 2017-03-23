
library(foreign)
library(dplyr)
library(tidyr)
library(data.table)
library(maptools)
library(DataCombine)
library(lubridate)
library(ggplot2)
library(ggmap)
library(zipcode)
ls()
getwd()
setwd('C:/Users/Daekyoo Kim/Documents/1) BU/2017 Spring/MA415/MA615-midterm-spring-17-master/Files')

OSHA_dbf <- read.dbf("osha.DBF")
VIOL_dbf <- read.dbf("viol.DBF")
HAZASUB_dbf <- read.dbf("hazsub.DBF")
ACCIDENT_dbf <- read.dbf("accid.DBF")
HISTORY_dbf <- read.dbf("history.DBF")
ADMPAY_dbf <- read.dbf("admpay.DBF")

ACC <- read.dbf("acc.dbf")
OCC <- read.dbf("occ.DBF")
SCC <- read.dbf("scc.dbf")

#---------------------------------------------------------------------------------
# Select the columns that related to the geographic information 
OSHA <- select(OSHA_dbf, ACTIVITYNO, SITESTATE, ESTABNAME, SITEADD, SITEZIP,
               SITECITY, SITECNTY)

# Find and remove the null (i.e., NA)
OSHA_NA_ROW <- OSHA[!complete.cases(OSHA$SITEADD),]
newdata_OSHA <- na.omit(OSHA)

# Select the columns that related to the Amount of penalty 
ADMPAY <- select(ADMPAY_dbf, ACTIVITYNO, PENAMT)

# Find and remove the null
ADMPAY_NA_ROW <- ADMPAY[!complete.cases(ADMPAY$PENAMT),]
newdata_ADMPAY <- na.omit(ADMPAY)

# Aggregate all rows from OSHA data, and all columns from OSHA and ADMPAY.
OSHA_ADMPAY <- left_join(newdata_OSHA,newdata_ADMPAY, by = "ACTIVITYNO")

# Find and remove the null
OSHA_ADMPAY_NA_ROW <- OSHA_ADMPAY[!complete.cases(OSHA_ADMPAY$PENAMT),]
newdata_OSHA_ADMPAY <- na.omit(OSHA_ADMPAY)

# Select the columns with regard to the violations
VIOL <- select(VIOL_dbf, ACTIVITYNO, ISSUEDATE,
               VIOLTYPE,GRAVITY,NUMEXPOSED,PENINITIAL)

# Aggregate the data set for OSHA, ADMPAY, and VIOL
COMB_OSHA_ADMPAY_VIOL <- left_join(newdata_OSHA_ADMPAY,VIOL, by = "ACTIVITYNO")

# Find and remove the null
OSHA_O_AP_V_NA_ROW <- COMB_OSHA_ADMPAY_VIOL[!complete.cases(COMB_OSHA_ADMPAY_VIOL$PENINITIAL),]
newdata_COMB_OSHA_ADMPAY_VIOL <- na.omit(COMB_OSHA_ADMPAY_VIOL)

# Select the columns related with accident data
ACCI = select(ACCIDENT_dbf,ACTIVITYNO,DEGREE)

# Filter the fatality
Fatality <- filter(ACCI, DEGREE == 1)

# Aggregate the data set for OSHA, ADMPAY, VIOL, and Fatality
newdata_COMB_OSHA_ADMPAY_VIOL_Ftl <- left_join(newdata_COMB_OSHA_ADMPAY_VIOL,Fatality, by = "ACTIVITYNO")
newdata_COMB_OSHA_ADMPAY_VIOL_Ftl$DEGREE[is.na(newdata_COMB_OSHA_ADMPAY_VIOL_Ftl$DEGREE)] <- 0

# Remove the zeros
FINAL_COMB_OSHA_ADMPAY_VIOL <- newdata_COMB_OSHA_ADMPAY_VIOL_Ftl[!apply(newdata_COMB_OSHA_ADMPAY_VIOL_Ftl[, -c(1:11,13)], 
                                                                        1, function(row) any(row == 0)), ]

# Group and summarize and mutate data for all geographic information about the places 
# that fatalities occured, which is a proof of danger area to work.
G <- FINAL_COMB_OSHA_ADMPAY_VIOL %>% 
  group_by(SITECITY,SITESTATE,SITEZIP,ACTIVITYNO,ESTABNAME,SITEADD,DEGREE) %>%
  summarise(mean(PENAMT),mean(PENINITIAL),mean(NUMEXPOSED)) %>%
  mutate(number = n())

# Rename the column names
colnames(G) <- c("SITECITY","SITESTATE","SITEZIP","ACTIVITYNO","ESTABNAME","SITEADD","DEGREE","PENAMT", "PENINITIAL","NUMEXPOSED","number")

# Call the data for including the zipcode
data(zipcode)
ZipCode <- select(zipcode,c(1:5))

# Merge two different data using zipcode
Location <- merge(ZipCode, G, by.x = "zip", by.y = "SITEZIP")

#plot the Google Maps basemap
map <- qmap('Massachusetts', zoom = 8)

#plot the all places that fatalities occured 
map + geom_point(data=Location, aes(x = longitude, y = latitude,group=NUMEXPOSED), 
                 color="red", size=3, alpha=0.5) +
  ggtitle("Places that fatalities occured ") + 
  theme(plot.title = element_text(size=15)) +
  theme(plot.title=element_text(hjust=0.5))

#-----------------------------------------------------------------------------------------
# Group and summarize and mutate data for all information about the number of violations,
# total amount of penalty, and the number of employees exposed to hazard violated, 
# which may be highly related to the danger area to work.
G <- FINAL_COMB_OSHA_ADMPAY_VIOL %>% 
  group_by(SITECITY,SITESTATE,ACTIVITYNO) %>%
  summarise(mean(PENAMT),mean(PENINITIAL),mean(NUMEXPOSED)) %>%
  mutate(number = n())

# Rename the column names
colnames(G) <- c("SITECITY","SITESTATE","ACTIVITYNO","PENAMT", "PENINITIAL","NUMEXPOSED","number")

# Group and summarize the data based on the city
GG <- G %>%
  group_by(SITECITY,SITESTATE,number) %>%
  summarise(mean(PENAMT),mean(PENINITIAL),mean(NUMEXPOSED))

# Round 3-digit numbers
GG[,-c(1,2)] <- round(GG[,-c(1,2)],2)

# Rename the column names
colnames(GG) <- c("SITECITY","SITESTATE","Num_of_Viol","PENAMT", "PENINITIAL","NUMEXPOSED")

# Merge two different data set based on the name of city
GG_city <- merge(GG, SCC, by.x = "SITECITY", by.y = "CITY")

# Filter the data based on the name of state (i.e., MA)
GG_filtered <- filter(GG_city, STATE == "MA")

# Remove the column number 7, 8, and 9
GGG <- subset(GG_filtered, select = -c(7,8,9))

# Rename the column names
colnames(GGG) <- c("SITECITY","SITESTATE","Num_of_Viol","PENAMT", "PENINITIAL","NUMEXPOSED","CITY")

# Rearrange the order of the columns
GGGG <- GGG[,c(1,7,2:6)]

# Among the places that fatalities occured in the past, the most dangerous 8 cities 
# to work in Massachusetts were chosen on the basis of 1) the number of violation, 
# 2) the amount of payment received for assessed penalties, 
# and 3) the number of employees exposed to hazard violated.
Dangerous_Cities <- filter(GGGG, Num_of_Viol > 1 & PENAMT > 4000 & NUMEXPOSED > 5)

# Histgram for the number of violation in the most dangerous 8 cities
ggplot(Dangerous_Cities, aes(x=CITY, y=Num_of_Viol)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(face="bold", color="#333333", 
                                   size=7),
        axis.text.y = element_text(face="bold", color="#333333", 
                                   size=7)) +
  ggtitle("Number of Violation") + 
  theme(plot.title = element_text(size=15)) +
  theme(plot.title=element_text(hjust=0.5))

# Histgram for the amount of Payment received for Assessed Penalty
ggplot(Dangerous_Cities, aes(x=CITY, y=PENAMT)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(face="bold", color="#333333", 
                                   size=7),
        axis.text.y = element_text(face="bold", color="#333333", 
                                   size=7)) +
  ggtitle("Amount of Payment received for Assessed Penalties") + 
  theme(plot.title = element_text(size=15)) +
  theme(plot.title=element_text(hjust=0.5))

# Histogram for the number of Employees exposed to Hazard Violated
ggplot(Dangerous_Cities, aes(x=CITY, y=NUMEXPOSED)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(face="bold", color="#333333", 
                                   size=7),
        axis.text.y = element_text(face="bold", color="#333333", 
                                   size=7)) +
  ggtitle("Number of Employees exposed to Hazard Violated") + 
  theme(plot.title = element_text(size=15)) +
  theme(plot.title=element_text(hjust=0.5))

