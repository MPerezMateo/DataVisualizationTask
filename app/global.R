library(dplyr)
# library(usethis)
library(devtools)
library(tidyverse)
library(data.table)

devtools::install_github("56north/leafletDK")
# https://github.com/mikkelkrogsholm/leafletDK
library(leafletDK)

varsBefore2012 <- c(
  "Nature of the offence, total" = "Nature of the offence, total",
  "Penal Code, total" = "Penal Code, total",
  "Penal code, unspecified" = "Penal code, unspecified",
  "Sexual offenses, total" = "Sexual offenses, total",
  "Incest, etc." = "Incest, etc.",
  "Rape, etc." = "Rape, etc.",
  "Heterosexual offence against a child under 12" = "Heterosexual offence against a child under 12",
  "Any other kind of heterosexual offence" = "Any other kind of heterosexual offence",
  "Homosexual offence against a child under 12" = "Homosexual offence against a child under 12",
  "Any other kind of homosexual offence" = "Any other kind of homosexual offence",
  "Offence against public decency by groping" = "Offence against public decency by groping",
  "Offence against public decency by indecent exposure" = "Offence against public decency by indecent exposure",
  "Any other kind of offence against public decency" = "Any other kind of offence against public decency",
  "Prostitution, etc." = "Prostitution, etc.",
  "Crimes of violence, total" = "Crimes of violence, total",
  "Violence against public authority" = "Violence against public authority",
  "Unlawful assembly/disturbance of public order" = "Unlawful assembly/disturbance of public order",
  "Homicide" = "Homicide",
  "Attempted homicide" = "Attempted homicide",
  "Coercive control" = "Coercive control",
  "Common assault" = "Common assault",
  "Assault causing actual bodily harm" = "Assault causing actual bodily harm",
  "Particularly aggravated assault" = "Particularly aggravated assault",
  "Unprovoked assault" = "Unprovoked assault",
  "Any other kind of intentional trespass to the person" = "Any other kind of intentional trespass to the person",
  "Intentional bodily harm" = "Intentional bodily harm",
  "Causing death or bodily harm by negligence" = "Causing death or bodily harm by negligence",
  "Offences against life and limb" = "Offences against life and limb",
  "Offences against personal liberty" = "Offences against personal liberty",
  "Threats" = "Threats",
  "Offences against property, total" = "Offences against property, total",
  "Forgery" = "Forgery",
  "Cheque forgery" = "Cheque forgery",
  "Arson" = "Arson",
  "Burglary - business and community" = "Burglary - business and community",
  "Residential burglaries" = "Residential burglaries",
  "Burglary (uninhabited buildings)" = "Burglary (uninhabited buildings)",
  "Theft from conveyances" = "Theft from conveyances",
  "Shoplifting, etc." = "Shoplifting, etc.",
  "Other kinds of theft" = "Other kinds of theft",
  "Theft of/taking vehicle without the owners consent (TWOC)" = "Theft of/taking vehicle without the owners consent (TWOC)",
  "Theft of/taking moped without the owners consent (TWOC)" = "Theft of/taking moped without the owners consent (TWOC)",
  "Theft of/taking bicycle without the owners consent (TWOC)" = "Theft of/taking bicycle without the owners consent (TWOC)",
  "Theft of/taking other objects without the owners consent (TWOC)" = "Theft of/taking other objects without the owners consent (TWOC)",
  "Theft by finding" = "Theft by finding",
  "Embezzlement" = "Embezzlement",
  "Fraud" = "Fraud",
  "Fraud by cheque" = "Fraud by cheque",
  "Fraud by abuse of position" = "Fraud by abuse of position",
  "Blackmail and usury" = "Blackmail and usury",
  "Fraud against creditors" = "Fraud against creditors",
  "Handling stolen goods" = "Handling stolen goods",
  "Robbery" = "Robbery",
  "Aggravated tax evasion etc." = "Aggravated tax evasion etc.",
  "Malicious damage to property" = "Malicious damage to property",
  "Receiving stolen goods by negligence" = "Receiving stolen goods by negligence",
  "Offence against and infringement of property" = "Offence against and infringement of property",
  "Other offences, total" = "Other offences, total",
  "Offences against public authority, etc." = "Offences against public authority, etc.",
  "Offences by public servants" = "Offences by public servants",
  "Perjury" = "Perjury",
  "Any other kind of false statement" = "Any other kind of false statement",
  "Offences concerning money and evidence" = "Offences concerning money and evidence",
  "Trafficking of drugs, etc." = "Trafficking of drugs, etc.",
  "Smuggling etc. of drugs" = "Smuggling etc. of drugs",
  "General public offences etc." = "General public offences etc.",
  "Illegal trade, etc." = "Illegal trade, etc.",
  "Family relation offences" = "Family relation offences",
  "Involuntary manslaughter etc. in connection with traffic accident" = "Involuntary manslaughter etc. in connection with traffic accident",
  "Non-molestation order" = "Non-molestation order",
  "Invasion of privacy and defamation" = "Invasion of privacy and defamation",
  "Special acts, total" = "Special acts, total",
  "Euphoriants Act" = "Euphoriants Act",
  "The Offensive Weapons Act" = "The Offensive Weapons Act",
  "Tax legislation and fiscal acts, etc." = "Tax legislation and fiscal acts, etc.",
  "Other special acts in criminal law" = "Other special acts in criminal law",
  "Health and social security legislation" = "Health and social security legislation",
  "Building and housing legislation" = "Building and housing legislation",
  "The environmental protection act" = "The environmental protection act",
  "Legislation on animals, hunting, etc." = "Legislation on animals, hunting, etc.",
  "Legislation on employment, transport, etc." = "Legislation on employment, transport, etc.",
  "The Companies Act" = "The Companies Act",
  "Legislation on the national defence" = "Legislation on the national defence",
  "Legislation applying to public utilities" = "Legislation applying to public utilities",
  "Legislation on gambling, licencing, trade" = "Legislation on gambling, licencing, trade",
  "Any other special legislation" = "Any other special legislation",
  "Special legislation, unspecified'" = "Special legislation, unspecified"
)

varsAfter2012Before2013 <- c(
  "Nature of the offence, total" = "Nature of the offence, total",
  "Penal Code, total" = "Penal Code, total",
  "Penal code, unspecified" = "Penal code, unspecified",
  "Sexual offenses, total" = "Sexual offenses, total",
  "Incest, etc." = "Incest, etc.",
  "Rape, etc." = "Rape, etc.",
  "Heterosexual offence against a child under 12" = "Heterosexual offence against a child under 12",
  "Any other kind of heterosexual offence" = "Any other kind of heterosexual offence",
  "Homosexual offence against a child under 12" = "Homosexual offence against a child under 12",
  "Any other kind of homosexual offence" = "Any other kind of homosexual offence",
  "Offence against public decency by groping" = "Offence against public decency by groping",
  "Offence against public decency by indecent exposure" = "Offence against public decency by indecent exposure",
  "Any other kind of offence against public decency" = "Any other kind of offence against public decency",
  "Prostitution, etc." = "Prostitution, etc.",
  "Crimes of violence, total" = "Crimes of violence, total",
  "Violence against public authority" = "Violence against public authority",
  "Unlawful assembly/disturbance of public order" = "Unlawful assembly/disturbance of public order",
  "Homicide" = "Homicide",
  "Attempted homicide" = "Attempted homicide",
  "Coercive control" = "Coercive control",
  "Common assault" = "Common assault",
  "Assault causing actual bodily harm" = "Assault causing actual bodily harm",
  "Particularly aggravated assault" = "Particularly aggravated assault",
  "Unprovoked assault" = "Unprovoked assault",
  "Any other kind of intentional trespass to the person" = "Any other kind of intentional trespass to the person",
  "Intentional bodily harm" = "Intentional bodily harm",
  "Causing death or bodily harm by negligence" = "Causing death or bodily harm by negligence",
  "Offences against life and limb" = "Offences against life and limb",
  "Offences against personal liberty" = "Offences against personal liberty",
  "Threats" = "Threats",
  "Offences against property, total" = "Offences against property, total",
  "Forgery" = "Forgery",
  "Cheque forgery" = "Cheque forgery",
  "Arson" = "Arson",
  "Burglary - business and community" = "Burglary - business and community",
  "Residential burglaries" = "Residential burglaries",
  "Burglary (uninhabited buildings)" = "Burglary (uninhabited buildings)",
  "Theft from conveyances" = "Theft from conveyances",
  "Shoplifting, etc." = "Shoplifting, etc.",
  "Other kinds of theft" = "Other kinds of theft",
  "Theft of/taking vehicle without the owners consent (TWOC)" = "Theft of/taking vehicle without the owners consent (TWOC)",
  "Theft of/taking moped without the owners consent (TWOC)" = "Theft of/taking moped without the owners consent (TWOC)",
  "Theft of/taking bicycle without the owners consent (TWOC)" = "Theft of/taking bicycle without the owners consent (TWOC)",
  "Theft of/taking other objects without the owners consent (TWOC)" = "Theft of/taking other objects without the owners consent (TWOC)",
  "Theft by finding" = "Theft by finding",
  "Embezzlement" = "Embezzlement",
  "Fraud" = "Fraud",
  "Fraud by cheque" = "Fraud by cheque",
  "Fraud by abuse of position" = "Fraud by abuse of position",
  "Blackmail and usury" = "Blackmail and usury",
  "Fraud against creditors" = "Fraud against creditors",
  "Handling stolen goods" = "Handling stolen goods",
  "Robbery" = "Robbery",
  "Aggravated tax evasion etc." = "Aggravated tax evasion etc.",
  "Malicious damage to property" = "Malicious damage to property",
  "Receiving stolen goods by negligence" = "Receiving stolen goods by negligence",
  "Offence against and infringement of property" = "Offence against and infringement of property",
  "Other offences, total" = "Other offences, total",
  "Offences against public authority, etc." = "Offences against public authority, etc.",
  "Offences by public servants" = "Offences by public servants",
  "Perjury" = "Perjury",
  "Any other kind of false statement" = "Any other kind of false statement",
  "Offences concerning money and evidence" = "Offences concerning money and evidence",
  "Trafficking of drugs, etc." = "Trafficking of drugs, etc.",
  "Smuggling etc. of drugs" = "Smuggling etc. of drugs",
  "General public offences etc." = "General public offences etc.",
  "Illegal trade, etc." = "Illegal trade, etc.",
  "Family relation offences" = "Family relation offences",
  "Involuntary manslaughter etc. in connection with traffic accident" = "Involuntary manslaughter etc. in connection with traffic accident",
  "Invasion of privacy and defamation" = "Invasion of privacy and defamation",
  "Special acts, total" = "Special acts, total",
  "Euphoriants Act" = "Euphoriants Act",
  "The Offensive Weapons Act" = "The Offensive Weapons Act",
  "Tax legislation and fiscal acts, etc." = "Tax legislation and fiscal acts, etc.",
  "Other special acts in criminal law" = "Other special acts in criminal law",
  "Health and social security legislation" = "Health and social security legislation",
  "Building and housing legislation" = "Building and housing legislation",
  "The environmental protection act" = "The environmental protection act",
  "Legislation on animals, hunting, etc." = "Legislation on animals, hunting, etc.",
  "Legislation on employment, transport, etc." = "Legislation on employment, transport, etc.",
  "The Companies Act" = "The Companies Act",
  "Legislation on the national defence" = "Legislation on the national defence",
  "Legislation applying to public utilities" = "Legislation applying to public utilities",
  "Legislation on gambling, licencing, trade" = "Legislation on gambling, licencing, trade",
  "Any other special legislation" = "Any other special legislation",
  "Special legislation, unspecified'" = "Special legislation, unspecified"
)

varsAfter2013 <- c(
  "Nature of the offence, total" = "Nature of the offence, total",
  "Penal Code, total" = "Penal Code, total",
  "Penal code, unspecified" = "Penal code, unspecified",
  "Sexual offenses, total" = "Sexual offenses, total",
  "Incest, etc." = "Incest, etc.",
  "Rape, etc." = "Rape, etc.",
  "Sexual offence against a child under 12" = "Sexual offence against a child under 12",
  "Sexual offence against a child under 15" = "Sexual offence against a child under 15",
  "Any other kind of sexual offence" = "Any other kind of sexual offence",
  "Offence against public decency by groping" = "Offence against public decency by groping",
  "Offence against public decency by indecent exposure" = "Offence against public decency by indecent exposure",
  "Any other kind of offence against public decency" = "Any other kind of offence against public decency",
  "Prostitution, etc." = "Prostitution, etc.",
  "Crimes of violence, total" = "Crimes of violence, total",
  "Violence against public authority" = "Violence against public authority",
  "Unlawful assembly/disturbance of public order" = "Unlawful assembly/disturbance of public order",
  "Homicide" = "Homicide",
  "Attempted homicide" = "Attempted homicide",
  "Coercive control" = "Coercive control",
  "Common assault" = "Common assault",
  "Assault causing actual bodily harm" = "Assault causing actual bodily harm",
  "Particularly aggravated assault" = "Particularly aggravated assault",
  "Unprovoked assault" = "Unprovoked assault",
  "Any other kind of intentional trespass to the person" = "Any other kind of intentional trespass to the person",
  "Intentional bodily harm" = "Intentional bodily harm",
  "Causing death or bodily harm by negligence" = "Causing death or bodily harm by negligence",
  "Offences against life and limb" = "Offences against life and limb",
  "Offences against personal liberty" = "Offences against personal liberty",
  "Threats" = "Threats",
  "Offences against property, total" = "Offences against property, total",
  "Forgery" = "Forgery",
  "Cheque forgery" = "Cheque forgery",
  "Arson" = "Arson",
  "Burglary - business and community" = "Burglary - business and community",
  "Residential burglaries" = "Residential burglaries",
  "Burglary (uninhabited buildings)" = "Burglary (uninhabited buildings)",
  "Theft from conveyances" = "Theft from conveyances",
  "Shoplifting, etc." = "Shoplifting, etc.",
  "Other kinds of theft" = "Other kinds of theft",
  "Theft of/taking vehicle without the owners consent (TWOC)" = "Theft of/taking vehicle without the owners consent (TWOC)",
  "Theft of/taking moped without the owners consent (TWOC)" = "Theft of/taking moped without the owners consent (TWOC)",
  "Theft of/taking bicycle without the owners consent (TWOC)" = "Theft of/taking bicycle without the owners consent (TWOC)",
  "Theft of/taking other objects without the owners consent (TWOC)" = "Theft of/taking other objects without the owners consent (TWOC)",
  "Theft by finding" = "Theft by finding",
  "Embezzlement" = "Embezzlement",
  "Fraud" = "Fraud",
  "Fraud by cheque" = "Fraud by cheque",
  "Fraud by abuse of position" = "Fraud by abuse of position",
  "Blackmail and usury" = "Blackmail and usury",
  "Fraud against creditors" = "Fraud against creditors",
  "Handling stolen goods" = "Handling stolen goods",
  "Robbery" = "Robbery",
  "Aggravated tax evasion etc." = "Aggravated tax evasion etc.",
  "Malicious damage to property" = "Malicious damage to property",
  "Receiving stolen goods by negligence" = "Receiving stolen goods by negligence",
  "Offence against and infringement of property" = "Offence against and infringement of property",
  "Other offences, total" = "Other offences, total",
  "Offences against public authority, etc." = "Offences against public authority, etc.",
  "Offences by public servants" = "Offences by public servants",
  "Perjury" = "Perjury",
  "Any other kind of false statement" = "Any other kind of false statement",
  "Offences concerning money and evidence" = "Offences concerning money and evidence",
  "Trafficking of drugs, etc." = "Trafficking of drugs, etc.",
  "Smuggling etc. of drugs" = "Smuggling etc. of drugs",
  "General public offences etc." = "General public offences etc.",
  "Illegal trade, etc." = "Illegal trade, etc.",
  "Family relation offences" = "Family relation offences",
  "Involuntary manslaughter etc. in connection with traffic accident" = "Involuntary manslaughter etc. in connection with traffic accident",
  "Invasion of privacy and defamation" = "Invasion of privacy and defamation",
  "Special acts, total" = "Special acts, total",
  "Euphoriants Act" = "Euphoriants Act",
  "The Offensive Weapons Act" = "The Offensive Weapons Act",
  "Tax legislation and fiscal acts, etc." = "Tax legislation and fiscal acts, etc.",
  "Other special acts in criminal law" = "Other special acts in criminal law",
  "Health and social security legislation" = "Health and social security legislation",
  "Building and housing legislation" = "Building and housing legislation",
  "The environmental protection act" = "The environmental protection act",
  "Legislation on animals, hunting, etc." = "Legislation on animals, hunting, etc.",
  "Legislation on employment, transport, etc." = "Legislation on employment, transport, etc.",
  "The Companies Act" = "The Companies Act",
  "Legislation on the national defence" = "Legislation on the national defence",
  "Legislation applying to public utilities" = "Legislation applying to public utilities",
  "Legislation on gambling, licencing, trade" = "Legislation on gambling, licencing, trade",
  "Any other special legislation" = "Any other special legislation",
  "Special legislation, unspecified'" = "Special legislation, unspecified"
)

folk1 <- readr::read_csv2("http://api.statbank.dk/v1/data/folk1a/CSV?OMR%C3%85DE=*")
municipalityDK("INDHOLD", "OMR?DE", data = folk1)

data<-read.csv("./data/Denmark_Crime_Regionwise.csv", stringsAsFactors = F)
regions<-unique(data$REGION)
trimesters<-colnames(data)[3:ncol(data)]

match<- function(dataset){
  res<-as.integer(regmatches(dataset, gregexpr("[[:digit:]]+", dataset))[[1]])
  out<-res[1]+(res[2]-1)*0.25
}
matches <-unlist(lapply(trimesters,match))
matchesString<-as.character(matches)
setnames(data, old = trimesters, new = matchesString)

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
