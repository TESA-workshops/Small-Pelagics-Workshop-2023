#### biomass calculation from transects exported from Echoview into excel
#Integrations must be placed in same folder and seperated (e.g. GB and SI)

rm(list = ls())

library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)
library(maptools)
library(lubridate)

surveydate <- "2020-08-16"

yr<-year(as.Date(surveydate))

#TARGET STRENGTH CALCULATION
SURV <-read.csv(paste0(getwd(), "/Purse Seine/Data Tables Figures/" ,"TS_est_",yr,"_GB.csv")) # This is the Target Strength calculation we created earlier
#SURV$DATE <- as.Date(SURV$DATE, "%m/%d/%Y")
TS38 <- SURV$TS[SURV$DATE==surveydate] 
TS50 <- SURV$TS[SURV$DATE==surveydate]-0.10727
TS75 <- SURV$TS[SURV$DATE==surveydate] -0.26575
TS120 <- SURV$TS[SURV$DATE==surveydate] -0.44946

#TARGET STRENGTH CALCULATION
SURV_SI <-read.csv(paste0(getwd(), "/Purse Seine/Data Tables Figures/" ,"TS_est_",yr,"_SI.csv")) # This one is imported into seal island.
#SURV$DATE <- as.Date(SURV$DATE, "%m/%d/%Y")
TS38_SI <- SURV_SI$TS[SURV$DATE==surveydate] 
TS50_SI <- SURV_SI$TS[SURV$DATE==surveydate]-0.10727
TS75_SI <- SURV_SI$TS[SURV$DATE==surveydate] -0.26575
TS120_SI <- SURV_SI$TS[SURV$DATE==surveydate] -0.44946

source(paste0(getwd(),"/Acoustic_biomass functions.R"))

file_list <- list.files(
  paste0(getwd(),"/Purse Seine/Integration/",surveydate,"/"))
transects <- data.frame()

#identify columns to keep from csv files
keep <-
  c(
    "Region_name",
    "Lat_S",
    "Lon_S",
    "Lat_E",
    "Lon_E",
    "Dist_S",
    "Dist_E",
    "Area_Backscatter_Strength",
    "Frequency",
    "Time_S",
    "Time_E",
    "Date_S"
  )

#for each file in file_list, read in csv, keep specified columns, rbind to dataset to produce one dataset
for (i in 1:length(file_list)) {
  temp_data <- read.csv(paste0(getwd(),"/Purse Seine/Integration/",surveydate,"/",file_list[i]))
  temp_data2 <- temp_data[keep]
  transects <- rbind(transects, temp_data2)
}

rm(temp_data, temp_data2)


#add survey boxes
German_survey_box <- rbind(c(43.233,-66.473), c(43.567,-66.473), c(43.567,-66.26), c(43.233,-66.26), c(43.233,-66.473))
German_survey_box <- as.data.frame(cbind(rep(1,nrow(German_survey_box)),German_survey_box))
names(German_survey_box) <- c("ID","Y","X")

#create column with boat name for plotting
transects$Boat <- as.character(transects$Region_name)
transects$Boat <- substr(transects$Boat,1,2)
transects$Boat



transect_a<-array(dim=c(2,2,length(transects$Region_name)))
ln_a <- list()
ln_b <- list()
for(i in 1:length(transects$Region_name)){
  transect_a[,,i]<-matrix(c(transects$Lon_S[i], transects$Lon_E[i], transects$Lat_S[i], transects$Lat_E[i]), ncol=2) 
  ln_a[[i]] <- Line(transect_a[,,i])
  ln_b[[i]] <- Lines(ln_a[[i]], ID = as.character(transects$Region_name[i]))
}

# Create SpatialPoints
SP <- SpatialPoints(coords = cbind(transects$Lon_S, transects$Lat_S))
# Add label variable
SP$ID <- transects$Region_name
projection(SP)<- CRS("+proj=utm +zone=19 +datum=WGS84")

sp_lns <- SpatialLines(ln_b)
projection(sp_lns) <- CRS("+proj=utm +zone=19 +datum=WGS84")


gerP<-Polygon(German_survey_box[,3:2])
gerPs = Polygons(list(gerP),1)
gerspS = SpatialPolygons(list(gerPs))
projection(gerspS) <-  CRS("+proj=utm +zone=19 +datum=WGS84")
plot(sp_lns,col="red",  axes=FALSE)
plot(gerspS, add=TRUE)
pointLabel(coordinates(SpatialPoints(coords = cbind(transects$Lon_S, transects$Lat_S))),
           labels=as.character(transects$Region_name))
plot(SpatialPoints(coords = cbind(transects$Lon_S, transects$Lat_S)), add=TRUE)
axis(1, at = c(-66.5 + 0:16 *0.025), cex.axis=0.7)
axis(2, at = c(43.2 + 0:10 *0.05), cex.axis=0.7)



transects_German <-
  filter(transects,
         Lat_S < 43.67 &
           Lat_E < 43.67 &
           Lon_S < -66.225 &
           Lon_E < -66.225) #need to check whether this is accurate
transects_SealIsland <-
  filter(transects,
         !Region_name %in% as.character(transects_German$Region_name))

transects_German
transects_SealIsland




#### German Bank ####

area <- area_calc(transects_German)
map_area_ob_GB <- map_area_buffered(transects_German, transectEastWest = F)
map_area_ob_GB[[1]]
area_GB<-map_area_ob_GB[[2]]


ggsave(paste0(getwd(),
              "/Purse Seine/Data Tables Figures/",
              "Area_GermanBank_",
              surveydate,
              ".png"
),map_area_ob_GB[[1]],width=7,height=10,units="in")  



#Seal Island
area <- area_calc(transects_SealIsland)
area
map_area_ob_SI<- map_area_buffered(transects_SealIsland, transectEastWest = F)
map_area_ob_SI[[1]]
area_SI <- map_area_ob_SI[[2]]

ggsave(paste0(getwd(),
              "/Purse Seine/Data Tables Figures/",
              "Area_SealIsland_",
              surveydate,
              ".png"
),map_area_ob_SI[[1]],width=7,height=10,units="in") 

#Determine overlap area

polygon_bbox <-st_bbox(map_area_ob_GB[[4]])
polygon_bbox
overlapPlot<- map_area_ob_GB[[1]]+
  geom_sf(data = map_area_ob_SI[[4]], color = "black", alpha = 0.3, fill = "tomato") +
  geom_sf(data = map_area_ob_SI[[5]], color = "red") +
  geom_sf(data= land.all, color = "black") +
  labs(title = paste0(surveydate)) +
  coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.25), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 

overlapPlot

ggsave(paste0(getwd(),
              "/Purse Seine/Data Tables Figures/",
              "Area_GermanBank_overlap",
              surveydate,
              ".png"
),overlapPlot,width=7,height=10,units="in")  

saveRDS(overlapPlot, paste0(getwd(),
                            "/Purse Seine/Data Tables Figures/",
                            "Area_GermanBank_overlap",
                            surveydate,
                            ".rda"))


intersecting_points<-st_intersection(map_area_ob_GB[[4]],map_area_ob_SI[[4]])
ggplot(data = intersecting_points) +
  geom_sf( color = "black", alpha = 0.3, fill = "tomato") 

sf::sf_use_s2(FALSE)
overlap_area<-st_area(intersecting_points)
overlap_area<-set_units(overlap_area, km^2)
overlap_area<-as.numeric(overlap_area)

area_GB <-   area_GB -  ((if(length(overlap_area) ==0) {0} else {overlap_area} )/2)
area_SI <-   area_SI -  ((if(length(overlap_area) ==0) {0} else {overlap_area} )/2)


GermanBank <- biomassCalc(transects_German, area_GB, TS38, TS50, TS75, TS120)
GermanBank$SurveyArea <- rep("German",length(GermanBank[,1]))

GermanBank$Reader <- rep("Allan",length(GermanBank[,1]))
#GermanBank$Reader <- rep("Claire",length(GermanBank[,1]))
#GermanBank$Reader <- rep("Jenna",length(GermanBank[,1]))
GermanBank




write.csv(GermanBank, paste0(getwd(),  "/Purse Seine/Biomass/German Bank/",surveydate,".csv"))
#write.csv(GermanBank, paste0("German_Claire_",surveydate,".csv"))
#write.csv(GermanBank, paste0("German_Jenna_",surveydate,".csv"))

#transects_SealIsland_area <- transects_SealIsland %>% filter(!Region_name %in% c("MS_T03")) 



transects_SealIsland

transects_SealIsland

SealIsland <-biomassCalc(transects_SealIsland, area_SI, TS38=-35.5, TS50=-35.5-0.10727, TS75=-35.5-0.26575, TS120=-35.5-0.44946)
SealIsland$SurveyArea <- rep("SealIsland",length(SealIsland[,1]))

SealIsland$Reader <- rep("Allan",length(SealIsland[,1]))
#SealIsland$Reader <- rep("Claire",length(SealIsland[,1]))
#SealIsland$Reader <- rep("Jenna",length(SealIsland[,1]))
SealIsland

write.csv(SealIsland, paste0("Y:/Acoustic Index Review Files/Biomass Estimates Versions/Estimated using current Rscripts/2020/Seal Island/",surveydate,".csv"))
#write.csv(SealIsland, paste0("SealIsland_Claire_",surveydate,".csv"))
#write.csv(SealIsland, paste0("SealIsland",surveydate,".csv"))


#### STOP THE PRESSES####

#### PENALTY APPLIED due to high number of juveniles #####





library(ggplot2)
library(grid)
library(grDevices)
library(gridExtra)
library(taRifx)
library(MASS)
library(sp)
library(ggpubr) #ggarrange
library(xlsx) #write to Excel
library(dplyr)
library(data.table)
library(ggthemes)
library(plotly)
library(scales)


DFL<-read.csv(paste0(getwd(),"/Purse Seine/Juvenile in survey data/DFL_2020_08_16.csv")) #exctracted from MAR Small Pelagics Database
DFD<-read.csv(paste0(getwd(),"/Purse Seine/Juvenile in survey data/DFD_2020_08_16.csv")) #extracted from MAR small Pelagics Database
inf<- read.csv(paste0(getwd(),"/Purse Seine/Juvenile in survey data/inf_2020_08_16.csv")) #extracted from MAR small pelagics database

# (20200561, 20200562, 20200563, 20200569, 20200662, 20200680) # these are LFs for German_bank 08-16
# c(20200662, 20200671) #These are the DETs for German 08-16


SIDS <- right_join(inf,DFL, by = "SAMPLID")
SIDS$DATE <- as.Date(SIDS$SDATE,format='%Y-%m-%d')

SIDet <- right_join(inf, DFD, by = "SAMPLID")
SIDet$DATE <- as.Date(SIDet$SDATE,format='%Y-%m-%d')

DET <- DFD[DFD$SAMPLID %in% unique(DFD$SAMPLID),]
DET$logwt <- log10(DET$WT)
DET$loglen <- log10(DET$LEN*1.02) # 2% Frozen

#Linear Regression model used to determine mean weight
r_id <- which(!is.na(DET$WT) & !is.na(DET$LEN))
cond_model <- lm(DET$logwt[r_id]~DET$loglen[r_id]) #plot(log10(DET$LEN[r_id]),log10(DET$WT[r_id]))
a <-  cond_model$coefficients[1]
b  <- cond_model$coefficients[2]   
n  <- cond_model$df.residual+2 


LF <- SIDS[SIDS$SAMPLID %in% unique(DFL$SAMPLID),]
LF_summary <-LF%>%
  group_by(LEN) %>%
  dplyr::summarise(CLEN = CLEN,
            No_Measured = sum(CLEN), 
            wTED_NO_MEAS = ceiling(sum(CLEN*MARKET_WEIGHT_KG/1000)),
            Len_cm = LEN/10,
            len_mm_x_freq = LEN*CLEN,
            lencm_x_freq <- (LEN/10)*CLEN,
            prop_length = CLEN/(sum(CLEN)),
            mid_point_length = LEN+2.5)
LF_summary


Mean_length_mm <- round(sum(LF_summary$LEN*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0) 
Mean_length_cm <- round(sum((LF_summary$LEN/10)*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0)

Mean_weight <- (10^(b*log10(Mean_length_mm)+a))/1000  #lenght to weight relationship where A and B calculated above
MidpointCalcWt <- (10^(b*log10(LF_summary$mid_point_length)+a))/1000
TS_number_midpoint_nowt <- (20*log10(LF_summary$mid_point_length/10)-71.9)    # Foote equation modified for each length class
TS_number_midpoint <- (20*log10(LF_summary$mid_point_length/10)-71.9) - 10*log10(MidpointCalcWt) # Foote equation modified for each length class and wt
TS_number_midpoint_to_arithmetic <- 10^(TS_number_midpoint_nowt/10) #TS to arithmetic or SA
TS_no_measured <- TS_number_midpoint_to_arithmetic*LF_summary$CLEN #Number measured
perc_TS_no_meas <- TS_no_measured/(sum(TS_no_measured)) #create a % matrix
sum(perc_TS_no_meas) #should equal 1


SURV

Arith_mSa <-10^(GermanBank$meanSa[1]/10)
10*log10(sum(Arith_mSa)) #check with weighted mean Sa
GermanBank$meanSa [1] #yep


Prop_Sa_length <- Arith_mSa*perc_TS_no_meas #convert TS weighted perc to prop of SA by length
Sa_prop_by_TS_no_meas <-10*log10(Prop_Sa_length) #convert TS weighted perc to prop of SA by length
Cal_dens_in_nos <-10^((Sa_prop_by_TS_no_meas-TS_number_midpoint_nowt)/10) #Density

Nos_by_area <-Cal_dens_in_nos*GermanBank$areaKm[1]*1000 #abundance
Wt_by_area <-Nos_by_area*MidpointCalcWt #biomass

Avg_dens_by_TS_perc_no_meas <-GermanBank$mean_biomass_density[1]*perc_TS_no_meas
sum(Avg_dens_by_TS_perc_no_meas) #should match biomass_density

GermanBank$mean_biomass_density[1]

Log_density_by_perc <- 10*log10(Avg_dens_by_TS_perc_no_meas)
Log_density_by_perc #density by percent
biomass_by_length_by_area <-Avg_dens_by_TS_perc_no_meas*GermanBank$areaKm[1]*1000
sum(biomass_by_length_by_area)
LF_summary

LF_summary <-as.data.frame(LF_summary)

LF_summary$biomass_by_length_by_area <- biomass_by_length_by_area

L23 <-filter(LF_summary, LEN < 230) #The LENGTH filter could be updated to L50 estimate for each survey/season
sum(L23$biomass_by_length_by_area)

adjustedBio <-GermanBank$total_biomass[1] - sum(L23$biomass_by_length_by_area) # Penalty to apply to biomass
GermanBank$total_biomass[1] #unadjusted

GermanBank$total_biomass <- rep(adjustedBio, length(GermanBank$total_biomass)) # Adjusted biomass
GermanBank

GermanBank$standard_error_tonnes  <- (GermanBank$standard_error_perc/100)*GermanBank$total_biomass #Adjusted SE 
GermanBank$Variance <- GermanBank$standard_error_tonnes^2


GermanBank$Reader <- rep("Allan_penalty_Bio", length(GermanBank$Reader))
GermanBank

write.csv(GermanBank, paste0(getwd(),  "/Purse Seine/Biomass/German Bank/",surveydate,".csv"))#
