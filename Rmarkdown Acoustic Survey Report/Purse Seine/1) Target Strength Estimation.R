######################################################################################################################
#  Description: Target strength estimates
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
library(grid)
library(grDevices)
library(gridExtra)
library(taRifx)
library(MASS)
library(sp)
library(RODBC) #SQL
library(sqldf) #SQL
#library(ROracle) #SQL
library(ggpubr) #ggarrange
library(xlsx) #write to Excel
library(dplyr)
library(data.table)
library(ggthemes)
library(plotly)
library(scales)
library(lubridate)

#### Setup and Coding####

yr <- 2020 # Year of surveys

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


t1 <- Sys.time()
right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}



######################################################################################################################
# Bring in Data and Survey dates
######################################################################################################################


DFD <-  read.csv(paste0(getwd(), "/Purse Seine/Data Tables Figures/DFD.csv")) #Detailed Samples from MAR Small Pelagics Database
DFL <-  read.csv(paste0(getwd(), "/Purse Seine/Data Tables Figures/DFL.csv")) #Length-Frequency Samples from MAR Small Pelagics Database
inf <-  read.csv(paste0(getwd(), "/Purse Seine/Data Tables Figures/inf.csv")) #Meta-data ("information") for the samples from MAR Small Pelagics Database



#Filter data for appropriate dates
inf$SDATE <- as.Date(inf$SDATE)
inf<- inf %>% tidyr::drop_na(LAT, LON) # check for missing good for QC


DATE <- as.Date(c("2020-08-16",	"2020-08-31", "2020-09-13", "2020-09-27", "2020-10-11", "2020-10-25"), "%Y-%m-%d") #Dates in which PURSE SEINES persued surveys
SURV <- data.frame(YEAR = rep(2020, length(DATE)), S_GROUND = rep("GB", length(DATE)), DATE=DATE, BIOMASS = rep(1, length(DATE))) #data setup

for(i in which(!is.na(inf$LAT) & !is.na(inf$LON))) #this loop converts eg( "4500.00" ) coordinates to decimal degrees 45.00
{
  
  ny <- nchar(inf$LAT[i])
  nx <- nchar(inf$LON[i])
  if(ny==4)
  {
    inf$Y[i] <- as.numeric(left(inf$LAT[i],2))+as.numeric(right(inf$LAT[i],2))/60
  } 
  if(ny==6)
  {
    inf$Y[i] <- as.numeric(left(inf$LAT[i],2))+as.numeric(right(inf$LAT[i],4))/60
  }
  if(ny==7)
  {
    inf$Y[i] <- as.numeric(left(inf$LAT[i],2))+as.numeric(right(inf$LAT[i],5))/60
  } 
  if(nx==4)
  {
    inf$X[i] <- -(as.numeric(left(inf$LON[i],2))+as.numeric(right(inf$LON[i],2))/60)
  } 
  if(nx==6)
  {
    inf$X[i] <- -(as.numeric(left(inf$LON[i],2))+as.numeric(right(inf$LON[i],4))/60)
  }
  if(nx==7)
  {
    inf$X[i] <- -(as.numeric(left(inf$LON[i],2))+as.numeric(right(inf$LON[i],5))/60)
  } 
}



#SURVEY box for surveys ##### This creates polygon areas to search within
survey_box <- rbind(c(43.215,-66.47), c(43.57,-66.47), c(43.57,-66.269), c(43.215,-66.269), c(43.215,-66.47))
survey_box <- as.data.frame(cbind(rep(1,nrow(survey_box)),survey_box))
names(survey_box) <- c("ID","Y","X")

catch_area <- rbind(c(43,-66.833), c(43.75,-66.833), c(43.75,-66.0833), c(43,-66.0833), c(43,-66.833))
catch_area <- as.data.frame(cbind(rep(1,nrow(catch_area)),catch_area))
names(catch_area) <- c("ID","Y","X")


SB_area <- rbind(c(44.6666667,-66), c(46,-66), c(46,-63), c(44.6666667,-63), c(44.6666667,-66))
SB_area<- as.data.frame(cbind(rep(1,nrow(SB_area)),SB_area))
names(SB_area) <- c("ID","Y","X")

#### subfiltering for data inside survey box####

inf$S_GROUND <- NA    
for(i in 1:nrow(inf))
  if(point.in.polygon(inf$X[i],inf$Y[i],survey_box$X,survey_box$Y)>0) #Can be updated German_bank Catch area
  {
    inf$S_GROUND[i] <- "GB"
  }  else if(point.in.polygon(inf$X[i],inf$Y[i],SB_area$X,SB_area$Y)>0) {
    inf$S_GROUND[i] <- "SB"
  }
inf <- inf[!is.na(inf$S_GROUND),]  ##inf is the data frame of LFs on the spawning grounds


DFL <-select(DFL, c("SAMPLID", "LEN", "CLEN"))


SIDS <- right_join(inf, DFL, by = "SAMPLID")  #Merge to make data easier to filter
SIDS <- SIDS %>% filter(S_GROUND == "GB")
SIDS$DATE <- as.Date(SIDS$SDATE,format='%Y-%m-%d')


SIDet <- right_join(inf, DFD, by = "SAMPLID") #Merge to make data easier to filter
SIDet <- SIDet %>% filter(S_GROUND == "GB")
SIDet$DATE <- as.Date(SIDet$SDATE,format='%Y-%m-%d')


#Create lists of SAMPLID

SAMPLIST_L <- list()
SAMPLIST_D <- list()
for(i in 1:nrow(SURV))
  if(isTRUE (any(SIDS$DATE == SURV$DATE[i]))){
    l_id <- which((SIDS$DATE == (SURV$DATE[i]))) 
    if(length(l_id)==0)
    {
      l_id = NA
    }
    SAMPLIST_L[[i]] <- SIDS[l_id,]$SAMPLID  
  } else if(isTRUE (any(SIDS$DATE != SURV$DATE[i]))){
    l_id <- which((SIDS$DATE >= (SURV$DATE[i]-2) & SIDS$DATE <= (SURV$DATE[i]+2))) 
    if(length(l_id)==0)
    {
      l_id = NA
    }
    
    SAMPLIST_L[[i]] <- SIDS[l_id,]$SAMPLID  
  }
SAMPLIST_L


for(i in 1:nrow(SURV))  # get samplids within 5 days of survey DATE
{
  a_id <- which((SIDet$DATE >= (SURV$DATE[i]-4) & SIDet$DATE <= (SURV$DATE[i]+4)))
  TEMP <- SIDet[SIDet$S_GROUND==SURV$S_GROUND[i],]
  TEMP$DATE_Dif <- abs(TEMP$DATE - SURV$DATE[i])
  if(length(a_id)==0)
  {
    a_id  = NA
  }
  
  SAMPLIST_D[[i]] <- SIDet[a_id,]$SAMPLID
}

#Review below of data filtered

detailed_samples<-unique(unlist(SAMPLIST_D))
length_samples<-unique(unlist(SAMPLIST_L))

GB_DFD<-DFD %>% filter(SAMPLID %in% detailed_samples)
GB_DFL<-DFL %>% filter(SAMPLID %in% length_samples)
GB_inf<- inf %>% filter(SAMPLID %in% length_samples)

### QC QA CHECK

GB_DFD
GB_DFL
GB_inf

###


#### setup for target strength estimation ####

SURV$TS <- NULL
SURV$TS_at28cm <- NULL
SURV$Mean_length_mm <- NULL
SURV$Mean_weight <- NULL
for(i in 1:nrow(SURV))
  SURV$TS[i]<- NA 
SURV$TS_at28cm[i]<- NA 
SURV$Mean_length_mm[i] <- NA
SURV$Mean_weight[i] <- NA
SURV$Mean_weight_at28cm[i] <- NA
SURV$n[i] <- NA
SURV$N_LF[i] <- NA
SURV$N_DET[i] <- NA
SURV$No_Measured[i]<-NA
SURV$lessthan215[i]<-NA
SURV$lessthan23[i]<-NA

SURV$stage1prop[i]<-NA
SURV$stage2prop[i]<-NA
SURV$stage3prop[i]<-NA
SURV$stage4prop[i]<-NA
SURV$stage5prop[i]<-NA
SURV$stage6prop[i]<-NA
SURV$stage7prop[i]<-NA
SURV$stage8prop[i]<-NA
SURV$stage9prop[i]<-NA
for(i in 1:nrow(SURV))
  if (any(c(is.na(SAMPLIST_L[[i]][1]),  is.na(SAMPLIST_D[[i]][1] ))) == TRUE){ 
    SURV$TS[i] <- -35.5
    
  }



for(i in 1:nrow(SURV)) ## sometimes run twice or three times 
  if (any(c(is.na(SAMPLIST_L[[i]][1]),  is.na(SAMPLIST_D[[i]][1] ))) == TRUE){ 
    SAMPLIST_L[[i]] <-  NULL
    SAMPLIST_D[[i]] <-  NULL
  }

#Check to see if NA still exist, if so run lines 238-242 again.
unique(is.na(SAMPLIST_L)) #if return is false good.
unique(is.na(SAMPLIST_D)) #if return is false good.


#separate between missing surveys
SURV2 <-SURV[(is.na(SURV$TS)) == 0, ]   # Standard target strength for surveys without samples
SURV <-SURV[(is.na(SURV$TS)) != 0, ]   #Keeping surveys with samples for next steps

###setup below 

plotlist <- list()
plotlist_LF <- list()
plotlist_mat <- list()
DF_mat <- list()

TS <- NULL
TS_at28cm <- NULL
Mean_weight <- NULL
Mean_weight_at28cm <- NULL
Mean_length_mm <- NULL
Mean_length_cm <- NULL
N_LF <- NULL
n <- NULL
No_Measured <- NULL
lessthan23 <- NULL
lessthan215 <- NULL
stage1prop <- NULL
stage2prop <- NULL
stage3prop <- NULL
stage4prop <- NULL
stage5prop <- NULL
stage6prop <- NULL
stage7prop <- NULL
stage8prop <- NULL
stage9prop <- NULL
N_DET <- NULL
Date_Label <- NULL

for(i in 1:nrow(SURV))  #Counts by Length
{
  LF <- SIDS[SIDS$SAMPLID %in% unique(SAMPLIST_L[[i]]),]
  LF_summary <-LF%>%
    group_by(LEN) %>%
    dplyr::summarise(No_Measured = sum(CLEN), wTED_NO_MEAS = ceiling(sum(CLEN*MARKET_WEIGHT_KG/1000)))
  LF_summary <- as.data.frame(LF_summary)  
  LF_summary$measured <- LF_summary$LEN*LF_summary$No_Measured
  
  LF_summary$wTED_NO_MEAS  <-   LF_summary$wTED_NO_MEAS[  is.na(LF_summary$wTED_NO_MEAS) ] <- 1 
  
  No_Measured[i] <- sum(LF_summary$No_Measured) #sum(LF_summary$wTED_NO_MEAS)# Number measured (which makes no sense... but # in catch I guess.)
  Mean_length_mm[i] <- round(sum(LF_summary$LEN*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0) 
  Mean_length_cm[i] <- round(sum((LF_summary$LEN/10)*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0)
  N_LF[i] <- length(unique(SAMPLIST_L[[i]]))
  L23 <-filter(LF_summary, LEN <= 230)
  (sum(L23$No_measured)/sum(LF_summary$No_Measured))
  lessthan23[i]<-label_percent(accuracy = 0.01)(sum(L23$No_Measured)/sum(LF_summary$No_Measured))
  
  DET <- DFD[DFD$SAMPLID %in% unique(SAMPLIST_D[[i]]),]
  DET$logwt <- log10(DET$WT)
  DET$loglen <- log10(DET$LEN*1.02)
  stage1prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==1),]$MAT) / length(DET$MAT))
  stage2prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==2),]$MAT) / length(DET$MAT))
  stage3prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==3),]$MAT) / length(DET$MAT))
  stage4prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==4),]$MAT) / length(DET$MAT))
  stage5prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==5),]$MAT) / length(DET$MAT))
  stage6prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==6),]$MAT) / length(DET$MAT))
  stage7prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==7),]$MAT) / length(DET$MAT))
  stage8prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==8),]$MAT) / length(DET$MAT))
  stage9prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==9),]$MAT) / length(DET$MAT))
  
  N_DET[i] <- length(unique(SAMPLIST_D[[i]]))
  
  #Linear Regression model used to determine mean weight
  r_id <- which(!is.na(DET$WT) & !is.na(DET$LEN))
  cond_model <- lm(DET$logwt[r_id]~DET$loglen[r_id]) #plot(log10(DET$LEN[r_id]),log10(DET$WT[r_id]))
  a <-  cond_model$coefficients[1]
  b  <- cond_model$coefficients[2]   
  n[i]  <- cond_model$df.residual+2 #Sample size
  
  Mean_weight[i] <- (10^(b*log10(Mean_length_mm[i])+a))/1000
  Mean_weight_at28cm[i] <-(10^(b*log10(280)+a))/1000
  
  LF_all <- data.frame(LEN = LF_summary$LEN, No_Measured = LF_summary$No_Measured)
  LF_all$Size <- rep(">23 & <30cm", length(LF_all$LEN))
  LF_LOW <- filter(LF_all, LEN <= 230)
  LF_LOW$Size <- rep(paste0(intToUtf8(8804), "23cm"), length(LF_LOW$LEN))
  LF_HI <- filter(LF_all, LEN >= 300)
  LF_HI$Size <- rep(paste0(intToUtf8(8805),"30cm"), length(LF_HI$LEN))
  LF_all <- filter(LF_all, LEN >230 & LEN <300)
  LF_ploty <- do.call("rbind", list(LF_all, LF_LOW, LF_HI))
  
  ### Plots
  theme_set(theme_bw())
  plotlist[[i]]<- ggplot(data=data.frame(X=(DET$loglen[r_id]),Y=(DET$logwt[r_id])),aes(x=X,y=Y)) + geom_point() +   geom_smooth(method=lm, se=F) +
    labs(title = paste0("Length Weight Relationship for ",SURV$S_GROUND[i]," ",SURV$DATE[i]))+
    xlab(expression("Log"[10]*" Length (mm)")) + 
    ylab(expression("Log"[10]*" Weight (g)"))+
    theme(plot.title = element_text(size=9))
  
  
  ggsave(paste0(getwd(),"/Purse Seine/Data Tables Figures/","SB_LEN_WT",SURV$DATE[i],".png"), plot = plotlist[[i]])
  
  ### Length Frequency Plot  
  theme_set(theme_bw())
  plotlist_LF[[i]]<- ggplot(data = LF_ploty, aes(x = (LEN / 10), y = No_Measured, fill = Size)) +
    geom_bar(color="black", stat ="identity" , position = position_dodge()) +
    scale_fill_manual(values=c("white","dark grey","black")) +
    labs(title = paste0("Fishery Samples used for survey on ", SURV$DATE[i])) +
    xlab("Length (cm)") + ylab("No. Measured") +
    xlim(12.0, 37.0) +
    theme(plot.title = element_text(size=8), legend.position = c(0.20,0.75))
  
  ggsave(paste0(getwd(),"/Purse Seine/Data Tables Figures/","GB_LF",SURV$DATE[i],".png"), plot = plotlist_LF[[i]])
  
  
  ### Maturity Plot
  
  Date_Label <- grobTree(textGrob(lubridate::stamp("March 1, 1999")(SURV$DATE[i]), 
                                  x=0.1,  y=0.4, hjust=0,
                                  gp=gpar(col="black")))
  Day_Label <- grobTree(textGrob(paste0("Day - ",yday(SURV$DATE[i])), 
                                 x=0.1,  y=0.6, hjust=0,
                                 gp=gpar(col="black")))
  No_DET <- grobTree(textGrob(paste0("n = ",(n[i])), 
                              x=0.75,  y=0.5, hjust=0,
                              gp=gpar(col="black")))
  
  X <- c(1:9)   
  Y <- c(as.numeric(left(stage1prop[i],4)),
         as.numeric(left(stage2prop[i],4)),
         as.numeric(left(stage3prop[i],4)),
         as.numeric(left(stage4prop[i],4)),
         as.numeric(left(stage5prop[i],4)),
         as.numeric(left(stage6prop[i],4)),
         as.numeric(left(stage7prop[i],4)),
         as.numeric(left(stage8prop[i],4)),
         as.numeric(left(stage9prop[i],4)))
  
  DF_mat[[i]] <- list("MatStage"=X,"Perc_Mat"=Y)
  
  data.frame(MatStage=unlist(DF_mat[[i]]$MatStage), Perc_Mat=unlist(DF_mat[[i]]$Perc_Mat) )
  
  theme_set(theme_bw())
  plotlist_mat[[i]]<- ggplot(data = data.frame(MatStage=unlist(DF_mat[[i]]$MatStage), 
                                               Perc_Mat=unlist(DF_mat[[i]]$Perc_Mat)), 
                             aes(x = MatStage, 
                                 y = Perc_Mat)) +
    geom_bar(color="black", stat ="identity" , position = position_dodge()) +
    geom_text(aes(label=Perc_Mat), vjust=-0.2, color="black",
              position = position_dodge(0.8), size=3.5)+
    xlab("Maturity Stage") + ylab("Percentage") +
    ylim(0,100)+
    scale_x_continuous(breaks=seq(1,9,by=1))+
    annotation_custom(Date_Label)+
    annotation_custom(Day_Label)+
    annotation_custom(No_DET)
  
  ggsave(paste0(getwd(),"/Purse Seine/Data Tables Figures/","GB_MAT",SURV$DATE[i],".png"), plot = plotlist_mat[[i]])
  
  TS[i] <-(20*log10(Mean_length_mm[i]/10)-71.9)-(10*log10(Mean_weight[i]))
  TS_at28cm[i] <-(20*log10(280/10)-71.9)-(10*log10(Mean_weight_at28cm[i]))
  
}


saveRDS(plotlist_LF, paste0(getwd(),"/Purse Seine/Data Tables Figures/","/GB_LF.rda")) # Save RDS for RMarkdown
saveRDS(plotlist_mat, paste0(getwd(),"/Purse Seine/Data Tables Figures/","/GB_mat.rda")) #Save RDS for Rmarkdown

#REFERENCE VALUES FOR OTHER FREQUENCIES

#TS50 <- -0.10727
#TS75 <- -0.26575
#TS120 <- -0.44946

SURV$TS <- TS
SURV$TS_at28cm <- TS_at28cm
SURV$N_LF <- N_LF
SURV$No_Measured <- No_Measured
SURV$Mean_length_mm <- Mean_length_mm
SURV$N_DET <- N_DET
SURV$n <- n
SURV$Mean_weight <- Mean_weight
SURV$Mean_weight_at28cm <- Mean_weight_at28cm
SURV$lessthan23 <- lessthan23
SURV$stage1prop <- stage1prop
SURV$stage2prop <- stage2prop
SURV$stage3prop <- stage3prop
SURV$stage4prop <- stage4prop
SURV$stage5prop <- stage5prop
SURV$stage6prop <- stage6prop
SURV$stage7prop <- stage7prop
SURV$stage8prop <- stage8prop
SURV$stage9prop <- stage9prop

SURV <-rbind(SURV, SURV2)


plots<-grid.arrange(
  grobs = plotlist_LF
)
ggsave(paste0(getwd(),"/Purse Seine/Data Tables Figures/","GB_LF","plots",".png"), plot = plots, width=14 , height=15, units="in") #Save matrix of plots


plots<-grid.arrange(
  grobs = plotlist_mat,
  nrow = length(plotlist_mat)
)
ggsave(paste0(getwd(),"/Purse Seine/Data Tables Figures/","GB_mat","plots",".png"), plot = plots, width=8.5 , height=12, units="in") #Save matrix of plots


write.csv(SURV, paste0(getwd(),"/Purse Seine/Data Tables Figures/","/TS_est_",yr,"_GB.csv")) #Create a table of Target Strength.
