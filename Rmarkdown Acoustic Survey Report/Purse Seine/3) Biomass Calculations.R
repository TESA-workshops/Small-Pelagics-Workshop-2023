######################################################################################################################
#  Description: Biomass Estimates
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
#  1. Biomass estimate from XX survey 
#
#  Time 
#
######################################################################################################################

rm(list = ls())

library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)
library(maptools)

surveydate <- "2020-09-27"

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

transects

#add survey boxes
survey_box <- rbind(c(43.233,-66.473), c(43.567,-66.473), c(43.567,-66.26), c(43.233,-66.26), c(43.233,-66.473))
survey_box <- as.data.frame(cbind(rep(1,nrow(survey_box)),survey_box))
names(survey_box) <- c("ID","Y","X")

#create column with boat name for plotting
transects$Boat <- as.character(transects$Region_name)
transects$Boat <- substr(transects$Boat,1,2)
length(unique(transects$Boat))



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


gerP<-Polygon(survey_box[,3:2])
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
           Lon_E < -66.225) 

transects_SealIsland <-
  filter(transects,
         !Region_name %in% as.character(transects_German$Region_name))

transects_German
transects_SealIsland


#### German Bank ####

map_area_ob_GB <- map_area_buffered(transects_German, transectEastWest = F)
map_area_ob_GB[[1]]
area_GB<-map_area_ob_GB[[2]]
area_GB


ggsave(paste0(getwd(),
              "/Purse Seine/Data Tables Figures/",
              "Area_GermanBank_",
              surveydate,
              ".png"
),map_area_ob_GB[[1]],width=7,height=10,units="in")  



#Seal Island
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
  coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.2), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 

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


SealIsland <-biomassCalc(transects_SealIsland, area_SI, TS38=TS38_SI, TS50=TS50_SI, TS75=TS75_SI, TS120=TS120_SI)
SealIsland$SurveyArea <- rep("SealIsland",length(SealIsland[,1]))

SealIsland$Reader <- rep("Allan",length(SealIsland[,1]))
#SealIsland$Reader <- rep("Claire",length(SealIsland[,1]))
#SealIsland$Reader <- rep("Jenna",length(SealIsland[,1]))
SealIsland


write.csv(SealIsland, paste0(getwd(),  "/Purse Seine/Biomass/Seal Island/",surveydate,".csv"))
#write.csv(SealIsland, paste0("SealIsland_Claire_",surveydate,".csv"))
#write.csv(SealIsland, paste0("SealIsland",surveydate,".csv"))