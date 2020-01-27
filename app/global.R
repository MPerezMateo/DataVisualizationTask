#install.packages("shiny")
#devtools::install_github("56north/leafletDK")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("usethis")
#install.packages("devtools")
#install.packages("leaflet")
#install.packages("demogR")
#install.packages("treemap")
#install.packages('rsconnect')

library(shiny)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(viridis)
library(data.table)
library(usethis)
library(devtools)
library(leaflet)
library(demogR)
library(dplyr)
# https://github.com/mikkelkrogsholm/leafletDK
devtools::install_github("56north/leafletDK")
library(leafletDK)
library(treemap)
library(d3heatmap)
library(rsconnect)
library(sp)

data<-read.csv("./data/Denmark_Crime_Regionwise.csv",stringsAsFactors = F)
colnames(data)
population<-read.csv("./data/Denmark_Population.csv", stringsAsFactors = F)
trimesters<-colnames(data)[3:(ncol(data))]

match<- function(dataset){
  res<-as.integer(regmatches(dataset, gregexpr("[[:digit:]]+", dataset))[[1]])
  out<-res[1]+(res[2]-1)*0.25
}
matches <-unlist(lapply(trimesters,match))
matchesString<-as.character(matches)
setnames(data, old = trimesters, new = matchesString)
popTrimesters<-colnames(population)[2:ncol(population)]
popMatches <-unlist(lapply(popTrimesters,match))
setnames(population, old = popTrimesters, new = as.character(popMatches))
total_crimes<- data %>%
  dplyr::filter(TYPE.OF.OFFENCE =="Penal Code, total" )
total_crimes[,3:6]<-NULL
total_crimes<-total_crimes[c(2:31,33:49,51:72,74:92,94:104),]
population<-population[c(1,3:32,34:50,52:73,75:93,95:105),-(ncol(population))]

sumation<-colSums(total_crimes[,3:ncol(total_crimes)])
total_crimes[nrow(total_crimes)+1,1:2]<-c("All Denmark","Penal Code, total")
total_crimes[nrow(total_crimes),3:ncol(total_crimes)]<-sumation
total_crimes<-arrange(total_crimes,REGION)
population<-arrange(population,REGION)
crime_rates <- cbind(total_crimes[1],1000*total_crimes[,-(1:2)]/population[,-1])

#######################
#######################
#### Second Part

#the last rows are about unknown municipality so i shouldn't consider them
tail(data$REGION)
nrow(data[data$REGION == "Unknown municipality",])

#adding column sum
crime_types <- mutate(
  data,
  total_number_of_crimes = data %>%
    select(3:ncol(data)) %>%
    rowSums/12
)

#select only the 3 columns I need
crime_types <- crime_types %>%
  select(REGION, TYPE.OF.OFFENCE, total_number_of_crimes)

#the columns are the regions
crime_types <- spread(
  crime_types,
  key = TYPE.OF.OFFENCE,
  value = total_number_of_crimes
) 

#taking only the general types of crimes
crime_types <- crime_types[, c(1,47, 72,21,54,59,75)]


#taking only the rows of the macroregions
crime_types_macroregions <- crime_types[73:77,]

#vector of hinabitants
inhab <- c(1848989,837087,1223894,1326913,590580)

#dividing for number of hinabitants and multipying per 10000
for (i in (1:nrow(crime_types_macroregions)))
{
  for (j in (2:ncol(crime_types_macroregions))) {
    crime_types_macroregions[i,j] <- crime_types_macroregions[i,j]/inhab[i]*10000
  }
}


names(crime_types_macroregions)[2] <- "All crimes"
names(crime_types_macroregions)[3] <- "Sexual offences"
names(crime_types_macroregions)[4] <- "Crimes of violence"
names(crime_types_macroregions)[5] <- "Offences against property"
names(crime_types_macroregions)[6] <- "Other offences"
names(crime_types_macroregions)[6] <- "Special acts"

crime_types_macroregions$REGION <- c("Hovedstaden", "Midtjylland","Nordjylland", "Sjaelland","Syddanmark")

#################################
#################################
#########Third and Fourth part
denmarkCrimesByTrimester <- read.csv("data/Denmark_Crime_Regionwise.csv", header = TRUE, fileEncoding='ISO-8859-1')
denmarkCrimesByTrimester$LAT <- jitter(denmarkCrimesByTrimester$LAT)
denmarkCrimesByTrimester$LONG <- jitter(denmarkCrimesByTrimester$LONG)

# Create DataFrame With Values Per Year

denmarkCrimesByYear <- denmarkCrimesByTrimester

for(i in 0:12) {
  year <- as.character(2007 + i)
  
  idxYearQ1 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q1", sep = ""))
  idxYearQ2 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q2", sep = ""))
  idxYearQ3 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q3", sep = ""))
  idxYearQ4 <- which(names(denmarkCrimesByYear)==paste("X", year, "Q4", sep = ""))
  
  if(year != '2019'){
    denmarkCrimesByYear[year] <- denmarkCrimesByYear[idxYearQ1] + denmarkCrimesByYear[idxYearQ2] + denmarkCrimesByYear[idxYearQ3] + denmarkCrimesByYear[idxYearQ4]
  } else {
    denmarkCrimesByYear[year] <- denmarkCrimesByYear[idxYearQ1] + denmarkCrimesByYear[idxYearQ2] + denmarkCrimesByYear[idxYearQ3]
    
  }
  
  yearColumns <- names(select(denmarkCrimesByYear, contains(paste("X", year, "Q", sep = ""))))
  denmarkCrimesByYear[yearColumns] <- NULL
}

denmarkCrimesTotal <- denmarkCrimesByYear
denmarkCrimesTotal['TOTAL'] <- 0

for(i in 0:12) {
  year <- as.character(2007 + i)
  
  denmarkCrimesTotal['TOTAL'] <- denmarkCrimesTotal['TOTAL'] + denmarkCrimesTotal[year]
  denmarkCrimesTotal[year] <- NULL
}

denmarkCrimesTotal$X <- NULL
denmarkCrimesTotal$LAT <- NULL
denmarkCrimesTotal$LONG <- NULL

denmarkCrimesTotal <- denmarkCrimesTotal[denmarkCrimesTotal$REGION != "Region Hovedstaden", ]

df <- denmarkCrimesTotal

for (crime in unique(df$TYPE.OF.OFFENCE)){
  v1 <- df[df$TYPE.OF.OFFENCE == crime,]$TOTAL
  df[df$TYPE.OF.OFFENCE == crime,]$TOTAL = scales::rescale(v1, to=c(0,1))
}

nameRows <- sort(unique(df$TYPE.OF.OFFENCE))
nameCols <- sort(unique(df$REGION))

denmarkCrimesMatrix <- matrix(0, length(nameRows), length(nameCols), dimnames = list(nameRows, nameCols))

# fill in the matrix with matrix indexing on row and column names 
denmarkCrimesMatrix[as.matrix(df[c("TYPE.OF.OFFENCE", "REGION")])] <- df[["TOTAL"]]


