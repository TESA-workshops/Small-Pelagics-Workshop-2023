######################################################################################################################
#  Description: Summary Script for Biomass Surveys performed
#  Created by: Allan Debertin
#  Created on: 2023
#  R version: 
#  R Studio  Version:
#
#  Required input files and data for script
#  1. 
#  2. 
#
#  Outputs:
#  1. Target strength for German bank for a given year
#
#  Time 
#
######################################################################################################################

rm(list = ls())
library(lubridate)

right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}


### German Bank ###

source(paste0(getwd(),"/Acoustic_biomass functions.R"))

bioFiles = dir( paste0(getwd(), "/Purse Seine/Biomass/German Bank/"), full.names = T, pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) { #This takes the summary information from each survey report into a separate list
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}
Surveys <-
  do.call("rbind", lapply(fileData,head,n=1)) #This aggregates the lists into a single data.frame.

Surveys$Date <-
  as.Date(paste0(
    left(Surveys$Date_S, 4),
    "-",
    left(right(Surveys$Date_S, 4), 2),
    "-",
    right(Surveys$Date_S, 2)
  ))
Surveys$Date <- as.Date(Surveys$Date)

### TURNOVER EQUATION FOR GERMAN BANK####
### SEE Melvin, MARTIN & Power 2014

y_intercept <- 0.199392662629964
x_Var_1 <-0.528381832773883
daysturnover <- 31
Date <- Surveys$Date
Survey <- 1:length(Surveys$Date)
Biomass <- Surveys$total_biomass
sum(Surveys$total_biomass)


turnoverBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)
bio <- turnoverBio_adjustedBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)
sum(bio)


GB_bio <- turnoverBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)
Surveys$cov_perc <-sqrt(Surveys$Variance)/Surveys$total_biomass*100


GB_var <- sum(Surveys$Variance)   ### VARIANCE APPLIED and converted... need to explain this too.
GB_st_err <-(sqrt(GB_var)/GB_bio) *GB_bio
GB_ci <- GB_st_err*1.96
GB_ci


Sa <- 10^(Surveys$TS/10)*Surveys$total_biomass*1000 * (bio/Surveys$total_biomass) # converted to Sa

Surveys$Adjusted_bio <- bio
Surveys$Total_Variance<-rep(GB_var, length(Surveys$Date))
Surveys$Total_StErr<-rep(GB_st_err, length(Surveys$Date))
Surveys$ci<-rep(GB_ci, length(Surveys$Date))
Surveys$SA <- Sa

write.csv(Surveys,    paste0(getwd(),"/Purse Seine/Data Tables Figures/GB_Surveys.csv"), row.names = F)


#Seal Island

#setwd("Y:/2020 Herring/Herring Biomass Estimates/")
bioFiles = dir(paste0(getwd(), "/Purse Seine/Biomass/Seal Island"), full.names = T, pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}

Surveys <-
  do.call("rbind", lapply(fileData,head,n=1)) #This aggregates the lists into a single data.frame.

Surveys$Date <-
  as.Date(paste0(
    left(Surveys$Date_S, 4),
    "-",
    left(right(Surveys$Date_S, 4), 2),
    "-",
    right(Surveys$Date_S, 2)
  ))
Surveys$Date <- as.Date(Surveys$Date)

writeClipboard(as.character(Surveys$standard_error_perc/100))


Seal_bio <- sum(Surveys$total_biomass)   ### NO TURNOVER EQUATION APPLIED TO Seal Island, but surveys occur typically every 14 days... (see Melvin et al. 2014)
Seal_var <-  sum(Surveys$Variance)
Seal_cov_perc <- sqrt(Seal_var) / Seal_bio *100
Seal_sum_St_err <- Seal_bio*(Seal_cov_perc/100)

Seal_si <- Seal_bio*(Seal_cov_perc/100)*1.96
Seal_si

Sa <- 10^(Surveys$TS/10)*Surveys$total_biomass*1000 * (Seal_bio/Surveys$total_biomass)

Surveys$Total_Variance<-rep(Seal_var, length(Surveys$Date))
Surveys$Total_StErr<-rep(Seal_sum_St_err, length(Surveys$Date))
Surveys$ci<-rep(Seal_si, length(Surveys$Date))
Surveys$SA <- Sa

write.csv(Surveys,  paste0(getwd(), "/Purse seine/Data Tables Figures/seal_Surveys.csv"), row.names = F)


