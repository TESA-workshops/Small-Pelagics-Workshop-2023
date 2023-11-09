######################################################################################################################
#  Description: Integreation Script to be used with EchoviewR
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


rm(list = ls(all = TRUE))

###pre-requisites

library(EchoviewR)
library(RDCOMClient)
library(stringr)

source(paste0(getwd(),"/Echoview_Integration_Functions.R"))


### setup file paths 
home.path = paste0(getwd(),"/Purse Seine/EV Files/") #EV file path
evfile.path = home.path

data.path = paste0(getwd(),"/Purse Seine/Hydro Data/") # RAW data path.

export.path = paste0(getwd(),"/Purse Seine/Integration/2020-09-27/") # export full transects
exportbycell.path = paste0(getwd(),"/Purse Seine/Integrationbycell/2020-09-27/") # export by EDSU (see PRC_NASC map script)

calfile.path = paste0(getwd(),"/Purse Seine/EV Files/Calibration Files/")
transect.path =  paste0(getwd(), "/Purse Seine/EV Files/Transect Regions//2020-09-27/")
region.path  =  paste0(getwd(), "/Purse Seine/EV Files/Local Regions//2020-09-27/")


#check to see if they exist
dir.create(export.path)
dir.create(region.path)
dir.create(exportbycell.path)

# Get a list of .EV files in your data directory
evFiles = dir(evfile.path,full.names=T,pattern='.EV$') # this part matters if multiple vessels, if not modify script to accommodate for single vessel.

#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}
#substrRight <- function(x, n){  substr(x, nchar(x)-n+1, nchar(x))}


#Setting up EV file paths and vessel names
vessel_search <- NULL
for(i in 1:length(evFiles)){
  vessel_search [i] <- right(left(evFiles[i],167),2) #### MODIFY 167 to find naming convention with 2 characters####
}
vessel_search


#Setting up calibrations file paths
calFiles = dir(calfile.path,full.names=T,pattern='.ecs$')
ordered_calFiles <- NULL
for(i in 1:length(vessel_search)){
  ordered_calFiles[i] <- grep(vessel_search[i], calFiles, value = TRUE, fixed = TRUE)
}
ordered_calFiles<-ordered_calFiles[!is.na(ordered_calFiles)]
ordered_calFiles


# in 2020, we had data recorded both as .RAW and .HAC files, ignore/delete .HAC Rscript stuff if your data only has .RAW files.
evFiles 
rawFiles = dir(data.path, full.names=TRUE, pattern='.raw$')
hacFiles = dir(data.path, full.names=TRUE, pattern='.HAC$')


rawFiles 
hacFiles


rawFileslist<-list()
for(i in 1:length(ordered_calFiles)){
  rawFileslist[[i]] <- grep(vessel_search[i], rawFiles, value = TRUE, fixed = TRUE)
}

rawFileslist


### Pre_HAC_sort is required if multiple vessels recorded in HAC. The original Vessel convention from HDPS was T420
pre_HAC_sort <- list()
for(i in  (length(ordered_calFiles)+1):length(vessel_search)){
  HAC_file_lookup <- data.frame(Vessel_initials = c("TM"), HSDPS_acy = c("T420"))
  pre_HAC_sort[[i]] <-HAC_file_lookup[HAC_file_lookup$Vessel_initials == vessel_search[i]]$HSDPS_acy
}


### sort HAC files with EV files.
hacFileslist<-list()
for(i in  (length(ordered_calFiles)+1):length(vessel_search)){
  hacFileslist[[i]] <- grep(pre_HAC_sort[[i]], hacFiles, value = TRUE, fixed = TRUE)
}
hacFileslist


#Combined list for some functions, just use "RawFileslist" above if only using .RAW data from multiple vessels.
RawHacFilesList <-c(rawFileslist,hacFileslist[(length(ordered_calFiles)+1):length(vessel_search)])




#Export Region Logs


#Export Region Logs
regionFiles<-Local_Regions_export(RawHacFilesList, evFiles,region.path,vessel_search)
regionFiles


#Export Region Logs
transectFiles<-Transect_Regions_export(RawHacFilesList, evFiles,transect.path,vessel_search)
transectFiles


### you can also create Line exports that can be used with any version of Echoview, see my integration functions for example code.


EvAppObj = COMCreate('EchoviewCom.EvApplication')

#open template for export
EVFile <- EvAppObj$OpenFile(evFiles[1])

# Load existing EV File with regions.
EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')

EVRegionClass <- EVFile[["RegionClasses"]]
EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
EVtransect <-EVRegionClass$FindByName("Transect")
EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
EVDeleteRegionClass(EVFile=EVFile,EVtransect)

# Add raw data to the new file
EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[1]))
# Add calibration files
EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[1])

#Import Regions
#open Files for import
EVFile$Import(transectFiles[1])
EVFile$Import(regionFiles[1])

varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
Ex_P_ABC <- varProp$Item("ABC")
Ex_P_ABC[["Enabled"]] <- TRUE
ExPropABS<- varProp$Item("Area_Backscatter_Strength")
ExPropABS[["Enabled"]] <- TRUE
Ex_P_Lat_S <- varProp$Item("Lat_S")
Ex_P_Lat_S[["Enabled"]] <- TRUE
Ex_P_Lat_E <- varProp$Item("Lat_E")
Ex_P_Lat_E[["Enabled"]] <- TRUE
Ex_P_Lon_S <- varProp$Item("Lon_S")
Ex_P_Lon_S[["Enabled"]] <- TRUE
Ex_P_Lon_E <- varProp$Item("Lon_E")
Ex_P_Lon_E[["Enabled"]] <- TRUE
Ex_P_Dist_S <- varProp$Item("Dist_S")
Ex_P_Dist_S[["Enabled"]] <- TRUE
Ex_P_Dist_E <- varProp$Item("Dist_E")
Ex_P_Dist_E[["Enabled"]] <- TRUE
Ex_P_Frequency <- varProp$Item("Frequency")
Ex_P_Frequency[["Enabled"]] <- TRUE
Ex_P_Time_S <- varProp$Item("Time_S")
Ex_P_Time_S[["Enabled"]] <- TRUE
Ex_P_Time_E <- varProp$Item("Time_E")
Ex_P_Time_E[["Enabled"]] <- TRUE
Ex_P_Date_S <- varProp$Item("Date_S")
Ex_P_Date_S[["Enabled"]] <- TRUE
Ex_P_Date_E <- varProp$Item("Date_E")
Ex_P_Date_E[["Enabled"]] <- TRUE

#perform export integretation on region classes called "Transect" only.
EVVar <- EVFile[["Variables"]]$FindByName("Processed data 1")
if(is.null(EVVar) == TRUE){
  EVVar <- EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
}
if(is.null(EVVar) == TRUE){
  EVVar <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
}
if(is.null(EVVar) == TRUE){
  EVVar <- EVFile[["Variables"]]$FindByName("Sv pings")
}


EVVar_above <- EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
if(is.null(EVVar) == TRUE){
  EVVar_above <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
}
if(is.null(EVVar) == TRUE){
  EVVar_above <- EVFile[["Variables"]]$FindByName("Sv pings")
}
EVminThresholdSet(EVVar, -70)


varObj = EVVar
varDat <- varObj[["Properties"]][["Data"]]
preThresApplyFlag <- varDat$ApplyMaximumThreshold()
varDat[["ApplyMaximumThreshold"]] <- TRUE
postThresApplyFlag <- varDat$ApplyMaximumThreshold()
varDat[["LockSvMaximum"]] <- FALSE
varDat[["MaximumThreshold"]] <- -10

varAnaly <- EVVar[["Properties"]][["Analysis"]]
varAnaly[["ExcludeAbove"]] <- "Surface Line" 
varAnaly[["ExcludeBelow"]] <- "Bottom offset - 0.5 meters" 
varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
varAnaly[["ExcludeAbove"]] <- "Surface Line" 
varAnaly[["ExcludeBelow"]] <- "Bottom offset - 0.5 meters" 

EVRegionClass <- EVFile[["RegionClasses"]]
EVTransect<-EVRegionClass$FindByName("Transect")
EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[1],'.csv'),EVTransect)
EvAppObj$Quit()

Integration_original_2022_07_06(rawFileslist,  #### this function exports full transects for EV files that use .RAW
                                evFiles,           #### ASK WORKSHOP if they want to see the details?
                                ordered_calFiles,
                                transectFiles,
                                regionFiles,
                                export.path,
                                vessel_search)



Integration_original_2022_07_06_Hac(hacFileslist, #### this function exports full transects for EV files that use .HAC
                                    evFiles,
                                    ordered_calFiles,
                                    transectFiles,
                                    regionFiles,
                                    export.path,
                                    vessel_search)


Integrationbycell_original_2022_07_06(rawFileslist, #### this function exports EDSUs from the larger transect, for mapping purposes, but could be used
                                      evFiles,      #     as an alternative (better?) way to estimate biomass too.
                                      ordered_calFiles,
                                      transectFiles,
                                      regionFiles,
                                      exportbycell.path,
                                      vessel_search)


Integrationbycell_original_2022_07_06_Hac(hacFileslist,
                                          evFiles,
                                          transectFiles,
                                          regionFiles,
                                          exportbycell.path,
                                          vessel_search)





