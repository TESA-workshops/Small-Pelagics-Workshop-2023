######################################################################################################################
#  Description: Sumamry Script for surveys performed
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

rm(list=ls())
library(ggplot2)
library(shapefiles)
library(grid)
library(geosphere)
library(grDevices)
library(gridExtra)
library(MASS)
library(maptools)
library(rgdal)
library(sp)
#library(plyr)
library(raster)
library(RODBC) #SQL
library(sqldf) #SQL
library(lubridate)
library(dplyr)
library(scales)
library(tidyr)
library(stringr)

yr <-"2020"
dt<-"230302"




write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}

# Table 3. Summary of 2019 fish sampled by survey date
# and location with TS estimate from samples and TS estimate for a 28 cm herring using the length/weight equation.


GB_TS<-read.csv(paste0(getwd(),"/Purse Seine/Data Tables Figures/TS_est_",yr,"_GB.csv"), header=T) #German bank TS
SI_TS<-read.csv(paste0(getwd(),"/Purse Seine/Data Tables Figures/TS_est_",yr,"_SI.csv"), header=T) #Seal Island TS

#building German Bank Table

GB_TS<- GB_TS %>% arrange(DATE)

Survey_Name <- NA
for(i in 1:length(GB_TS$DATE)){
  Survey_Name[i]<-paste0("German Bank #",i)
}
GB_TS$Survey_Name <- Survey_Name

Interval_days<- interval(GB_TS$DATE[1:(length(GB_TS$DATE)-1)], GB_TS$DATE[2:(length(GB_TS$DATE))]) %/% days(1)
GB_TS$Interval_days <-c(0, Interval_days)
GB_TS$Mean_weight<-round(GB_TS$Mean_weight*1000,0)
GB_TS$TS <-round(GB_TS$TS,4)

#building Seal Island Table

SI_TS<- SI_TS %>% arrange(DATE)

Survey_Name <- NA
for(i in 1:length(SI_TS$DATE)){
  Survey_Name[i]<-paste0("Seal Island #",i)
}
SI_TS$Survey_Name <- Survey_Name

Interval_days<- interval(SI_TS$DATE[1:(length(SI_TS$DATE)-1)], SI_TS$DATE[2:(length(SI_TS$DATE))]) %/% days(1)
SI_TS$Interval_days <-c(0, Interval_days)
SI_TS$Mean_weight<-round(SI_TS$Mean_weight*1000,0)
SI_TS$TS <-round(SI_TS$TS,4)

#building Seal Island Table

SI_TS<- SI_TS %>% arrange(DATE)

Survey_Name <- NA
for(i in 1:length(SI_TS$DATE)){
  Survey_Name[i]<-paste0("Seal Island #",i)
}
SI_TS$Survey_Name <- Survey_Name

Interval_days<- interval(SI_TS$DATE[1:(length(SI_TS$DATE)-1)], SI_TS$DATE[2:(length(SI_TS$DATE))]) %/% days(1)
SI_TS$Interval_days <-c(0, Interval_days)
SI_TS$Mean_weight<-round(SI_TS$Mean_weight*1000,0)
SI_TS$TS <-round(SI_TS$TS,4)




### Building Table 1

#Table 1. Summary of the number of surveys undertaken in 2019, and the number of surveys 
#examined in the estimation of spawning stock biomass (SSB) for the Atlantic herring 4VWX 
#stock and coastal component complexes. Numbers in brackets indicate the number of surveys 
#excluded from total biomass estimates.

#notes: Offshore is never surveyed, but I like the inclusion
# "+1" or altnernate numbers represents ad-hoc or alternate surveys.

spawning_grounds <-
  c(
    "German Bank",
    "Seal Island"
     )


tryCatch({
  GB_TS_l <- length(GB_TS$DATE)
},
error = function(err) {
  print("Error in evaluating a. Initializing it to NA")
  GB_TS_l <<- NA
})

tryCatch({
  SI_TS_l <- length(SI_TS$DATE)
},
error = function(err) {
  print("Error in evaluating a. Initializing it to NA")
  SI_TS_l <<- NA
})



Number_of_Surveys <-c(GB_TS_l,SI_TS_l)
Number_of_Surveys[is.na(Number_of_Surveys)] <- 0

Tab1<- data.frame(spawning_grounds=spawning_grounds, Number_of_Surveys=Number_of_Surveys)
Tab1<-rbind(Tab1, c("Total", sum(Number_of_Surveys)))

Tab1$Notes <- c("","","")
Tab1


colnames(Tab1) <- c(
  "Spawning Grounds",
  "Number of Surveys",
  "Notes"
)
saveRDS(Tab1,paste0(getwd(),"/Purse Seine/Data Tables Figures/Tab1_table.rda"))


#Time Series plot SB & GB####


GB_SB_TIME <-read.csv(paste0(getwd(),"/Purse Seine/Bubble Biomass Plot Data/GB_SB_TIME.csv"), header=T)
GB_SB_TIME[,2:11] <- GB_SB_TIME[,2:11]/1000

str(GB_SB_TIME)


p1<- ggplot(GB_SB_TIME) +
  scale_color_manual(
    name = 'Spawning Ground',
    breaks = c('German Bank', 'Scots Bay'),
    values = c('German Bank' = 'black', 'Scots Bay' = 'red')  ) +
  geom_line(aes(x = Year, y = GB_Bio, color = "German Bank"), linewidth = 1) +
  geom_line(aes(x = Year + 0.1, y = SB_Bio, color = 'Scots Bay'), linewidth = 1) +
  
  geom_errorbar(aes(
    x = Year,
    ymin = GB_Bio - GB_CI,
    ymax = GB_Bio + GB_CI
  ),
  width = .3) +
  geom_errorbar(aes(
    x = Year + 0.1,
    ymin = SB_Bio - SB_CI,
    ymax = SB_Bio + SB_CI,
    color = 'Scots Bay'
  ),
  width = .3) +
  scale_y_continuous(breaks = seq(0, 800, 100)) +
  scale_x_continuous(breaks = (seq(
    min(GB_SB_TIME$Year), max(GB_SB_TIME$Year), 3
  ))) +
  ylab("Spawning Biomass ('000t)") + 
  theme_bw()+
  theme(legend.position = c(0.85,0.80))

p1


saveRDS(p1,paste0(getwd(),"/Purse Seine/Data Tables Figures/GB_SB_TIME.rda"))

GB_SB_TIME$LRP <-  rep(317.846, length(GB_SB_TIME$Year))
GB_SB_TIME$SB_GB_BIO <- GB_SB_TIME$SB_Bio + GB_SB_TIME$GB_Bio 

mean(GB_SB_TIME$SB_GB_BIO)

p2<- ggplot(GB_SB_TIME) +
  scale_color_manual(
    name = NULL,
   breaks = c('Scots Bay and German Bank', '3-year Moving Average', 'Long-term Average', 'Limit Reference Point'),
   values = c('Scots Bay and German Bank' = 'black', 
              '3-year Moving Average' = 'red',
              'Long-term Average' = 'purple',
              'Limit Reference Point' = 'blue')  ) +
  geom_line(aes(x = Year, y = SB_GB_BIO, color = 'Scots Bay and German Bank'), linewidth = 1) +
  geom_line(aes(x = Year + 0.1, y = X3_yr_MA, color = '3-year Moving Average'), linewidth = 1) +
  
  geom_errorbar(aes(
    x = Year,
    ymin = SB_GB_BIO - CI_overall ,
    ymax = SB_GB_BIO + CI_overall 
  ),
  width = .3) +
  geom_errorbar(aes(
    x = Year + 0.1,
    ymin = X3_yr_MA - X3_yr_MA_CI,
    ymax = X3_yr_MA + X3_yr_MA_CI,
    color =  '3-year Moving Average'
  ),
  width = .3) +
  geom_hline(aes(yintercept=mean(SB_GB_BIO), color = 'Long-term Average'), linewidth = .7, linetype = 2) +
  geom_hline(aes(yintercept=317.846, color = 'Limit Reference Point'), linewidth = .7, linetype =2 ) +
  scale_y_continuous(breaks = seq(0, 800, 100)) +
  scale_x_continuous(breaks = (seq(
    min(GB_SB_TIME$Year), max(GB_SB_TIME$Year), 1
  ))) +
  ylab("Spawning Biomass ('000t)") + 
  theme_bw()+
  theme(legend.position = c(0.80,0.84))

p2

seq(
  min(GB_SB_TIME$Year-1), max(GB_SB_TIME$Year )
)

saveRDS(p2,paste0(getwd(),"/Purse Seine/Data Tables Figures/GB_SB_TIME_index.rda"))



#Table 2 - summary information####




#German####


report.dir<-paste0(getwd(),"/Purse Seine/Biomass/German Bank/")

bioFiles = dir(report.dir, full.names = T, pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  fileData[[i]] <-   fileData[[i]] %>% dplyr::select(SurveyArea, Date_S, Region_name)
  fileData[[i]]$Date_S<-paste0(left(fileData[[i]]$Date_S,4),
                               "/",
                               right(left(fileData[[i]]$Date_S,6),2),
                               "/",
                               right(fileData[[i]]$Date_S,2))
  fileData[[i]]$AcousticBoats<- rep(length(unique(left(fileData[[i]]$Region_name,2))), length(fileData[[i]]$Date_S))
}  


GermanBank<-do.call("rbind",lapply(fileData,'[',1,))

GermanBank$Date_S <- as.Date(GermanBank$Date_S , "%Y/%m/%d")

for(i in 1:length(GermanBank$SurveyArea)){
  GermanBank$SurveyArea[i]<-paste0(
    "German Bank"
    ," #",i)
}
GermanBank
GermanBank<-GermanBank %>% dplyr::arrange(Date_S)

for(i in 1:length(GermanBank$Date_S)){
  GermanBank$Day[i]<-  difftime(GermanBank$Date_S[i+1],GermanBank$Date_S[i], units="days")
}
GermanBank$Day <- GermanBank$Day[c(length(GermanBank$Day),1:(length(GermanBank$Day)-1))]
GermanBank


#Seal####



report.dir<-paste0(getwd(),"/Purse Seine/Biomass/Seal Island/")

bioFiles = dir(report.dir, full.names = T, pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  fileData[[i]] <-   fileData[[i]] %>% dplyr::select(SurveyArea, Date_S, Region_name)
  fileData[[i]]$Date_S<-paste0(left(fileData[[i]]$Date_S,4),
                               "/",
                               right(left(fileData[[i]]$Date_S,6),2),
                               "/",
                               right(fileData[[i]]$Date_S,2))
  fileData[[i]]$AcousticBoats<- rep(length(unique(left(fileData[[i]]$Region_name,2))), length(fileData[[i]]$Date_S))
}  

SealIsland<-do.call("rbind",lapply(fileData,'[',1,))

SealIsland$Date_S <- as.Date(SealIsland$Date_S , "%Y/%m/%d")

for(i in 1:length(SealIsland$SurveyArea)){
  SealIsland$SurveyArea[i]<-paste0(
    "Seal Island"
    ," #",i)
}
SealIsland<-SealIsland %>% dplyr::arrange(Date_S)

for(i in 1:length(SealIsland$Date_S)){
  SealIsland$Day[i]<-  difftime(SealIsland$Date_S[i+1],SealIsland$Date_S[i], units="days")
}
SealIsland$Day <- SealIsland$Day[c(length(SealIsland$Day),1:(length(SealIsland$Day)-1))]



SurveyDateSummary <-do.call("rbind", list(GermanBank,SealIsland))


SurveyDateSummary<-SurveyDateSummary %>% dplyr::select(SurveyArea,Date_S,Day,AcousticBoats)

names(SurveyDateSummary) <- c("Location of Survey",
                              "Survey Date",
                              "Interval (days)",
                              "Acoustic Boats")


saveRDS(SurveyDateSummary,paste0(getwd(),"/Purse Seine/Data Tables Figures/Tab2_table.rda"))


rm(list=ls())

right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}
findlast <- function(string,char){max(gregexpr(pattern = char,string)[[1]])}

library(ggplot2)
library(dplyr)

yr <- 2020
BubblePlot <- read.csv(paste0(getwd(), "/Purse Seine/Bubble Biomass Plot Data/GermanBankBubble",".csv"))

BubblePlot$Adjust_biomass <- BubblePlot$Adjust_biomass /1000

GermanBubbleBio_Adj <-ggplot(BubblePlot, aes(x=Week, y=Year, size = Adjust_biomass, label=round(Adjust_biomass,0))) +
  geom_point(alpha=0.6, colour="blue") +
  geom_text(size=5)+
  scale_y_continuous(breaks=(rev(unique(BubblePlot$Year)))) +
  scale_x_continuous(breaks=(  (min(BubblePlot$Week)-1):( max(BubblePlot$Week)+1)))+
  scale_size(range= c(.1,20), name="Biomass (kt)")+
  theme_bw()

saveRDS(GermanBubbleBio_Adj,paste0(getwd(), "/Purse Seine/Data Tables Figures/GermanBubbleBio_Adj.rda"))



