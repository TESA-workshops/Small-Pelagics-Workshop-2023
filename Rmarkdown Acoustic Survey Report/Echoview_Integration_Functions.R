library(EchoviewR)
library(RDCOMClient)

Local_Regions_export<-function(rawFileslist, evFiles,region.path,vessel_search){

#Export Region Logs
for(i in 1:length(rawFileslist)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Unclassified regions")
  EVTransect$ExportDefinitions(paste0(region.path,vessel_search[i],'.EVR'))
  EvAppObj$Quit()
}

regionFiles = dir(region.path,full.names=T,pattern='.EVR$')
return(regionFiles)
}

Transect_Regions_export<-function(rawFileslist, evFiles,transect.path,vessel_search){
  
#Export Region Logs
for(i in 1:length(rawFileslist)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVTransect$ExportDefinitions(paste0(transect.path,vessel_search[i],'.EVR'))
  EvAppObj$Quit()
}
  
  transectFiles = dir(transect.path,full.names=T,pattern='.EVR$')
  transectFiles  
  
}


Integration_original_2022_07_06 <-
  function(rawFileslist,
           evFiles,
           ordered_calFiles,
           transectFiles,
           regionFiles,
           export.path,
           vessel_search) {
    
  #For EV files with RAW files. 
  for(i in 1:length(rawFileslist)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    # Load existing EV File with regions.
    EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
    EVtransect <-EVRegionClass$FindByName("Transect")
    EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
    EVDeleteRegionClass(EVFile=EVFile,EVtransect)
    
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
    # Add calibration files
    EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
    
    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    
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
    EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    EvAppObj$Quit()
  }
  }


Integration_original_2022_11_02 <-
  function(rawFileslist,
           evFiles,
           ordered_calFiles,
           transectFiles,
           regionFiles,
           export.path,
           bottomLine,
           surfaceLine,
           vessel_search) {
    
    #For EV files with RAW files. 
    for(i in 1:length(rawFileslist)){  
      
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
      EVtransect <-EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
      EVDeleteRegionClass(EVFile=EVFile,EVtransect)
      
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
      # Add calibration files
      EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
      
      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
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
      varAnaly[["ExcludeAbove"]] <- surfaceLine
      varAnaly[["ExcludeBelow"]] <- bottomLine 
      varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
      varAnaly[["ExcludeAbove"]] <- "Surface Line" 
      varAnaly[["ExcludeBelow"]] <- "Bottom offset - 0.5 meters" 
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect<-EVRegionClass$FindByName("Transect")
      EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
      EvAppObj$Quit()
    }
  }

Integration_original_2022_07_06_Hac_gill<-
  function(hacFileslist,
           evFiles,
           transectFiles,
           regionFiles,
           export.path,
           vessel_search) {
    
    for(i in 1:length(hacFileslist)){
      
      Integration_original_2022_07_06_Hac
      
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
      EVtransect <-EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
      EVDeleteRegionClass(EVFile=EVFile,EVtransect)
      
      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
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
      
      EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
      
      EvAppObj$Quit()
    }
    
    EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    
    #Export Region Logs
    for(i in 1:length(lengthHacFiles)){  
      
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      #Export Regions
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect<-EVRegionClass$FindByName("Transect")
      EVTransect$ExportDefinitions(paste0(transect.path,vessel_search[i],'.EVR'))
      EvAppObj$Quit()
    }
  }



Integration_original_2022_07_06_Hac_gill_fromTemplate<-
  function(hacFileslist,
           evFiles,
           transectFiles,
           regionFiles,
           export.path,
           vessel_search,
           SurfaceLineName) {
    
    for(i in 1:length(hacFileslist)){
      
      Integration_original_2022_07_06_Hac
      
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
      EVtransect <-EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
      EVDeleteRegionClass(EVFile=EVFile,EVtransect)
      
      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
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
      varAnaly[["ExcludeAbove"]] <- SurfaceLineName
      varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 meters" 
      varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
      varAnaly[["ExcludeAbove"]] <- SurfaceLineName 
      varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 meters" 
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect<-EVRegionClass$FindByName("Transect")
      
      EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
      
      EvAppObj$Quit()
    }
    
    EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    
    #Export Region Logs
    for(i in 1:length(lengthHacFiles)){  
      
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      #Export Regions
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect<-EVRegionClass$FindByName("Transect")
      EVTransect$ExportDefinitions(paste0(transect.path,vessel_search[i],'.EVR'))
      EvAppObj$Quit()
    }
  }




Integration_original_2022_07_06_Hac <-
  function(hacFileslist,
           evFiles,
           ordered_calFiles,
           transectFiles,
           regionFiles,
           export.path,
           vessel_search) {
    
    
#For EV files with HAC files. 
for(i in  (length(ordered_calFiles)+1):length(vessel_search)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  # Load existing EV File with regions.
  EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
  # Add raw data to the new file
  EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
  
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
  EVtransect <-EVRegionClass$FindByName("Transect")
  EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
  EVDeleteRegionClass(EVFile=EVFile,EVtransect)
  
  #Import Regions
  #open Files for import
  EVFile$Import(transectFiles[i])
  EVFile$Import(regionFiles[i])
  
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
  EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
  
  EvAppObj$Quit()
}}


Integration_original_2022_11_02_Hac <-
  function(hacFileslist,
           evFiles,
           ordered_calFiles,
           transectFiles,
           regionFiles,
           export.path,
           bottomLine,
           surfaceLine,
           vessel_search) {
    
    
#For EV files with HAC files. 
for(i in  (length(ordered_calFiles)+1):length(vessel_search)){  
      
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
      EVtransect <-EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
      EVDeleteRegionClass(EVFile=EVFile,EVtransect)
      
      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
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
      EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
      
      EvAppObj$Quit()
    }
    
  }





Integration_original_2022_07_06_Raw_fromTemplate<- function(rawFileslist,
                                                            evFiles,
                                                            ordered_calFiles,
                                                            transectFiles,
                                                            regionFiles,
                                                            export.path,
                                                            vessel_search,
                                                            SurfaceLineName) {
  
  for(i in 1:length(rawFileslist)){
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    # Load existing EV File with regions.
    EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
    # Add calibration files
    EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
    EVtransect <-EVRegionClass$FindByName("Transect")
    EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
    EVDeleteRegionClass(EVFile=EVFile,EVtransect)
    
    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    
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
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i]
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i] 
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVTransect<-EVRegionClass$FindByName("Transect")
    
    EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    
    EvAppObj$Quit()
  }
  
}


Integration_original_2022_07_06_Raw_fromTemplate_bycell<- function(rawFileslist,
                                                            evFiles,
                                                            ordered_calFiles,
                                                            transectFiles,
                                                            regionFiles,
                                                            exportbycell.path,
                                                            vessel_search,
                                                            SurfaceLineName) {
  
  for(i in 1:length(rawFileslist)){
    
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    # Load existing EV File with regions.
    EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
    # Add calibration files
    EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
    EVtransect <-EVRegionClass$FindByName("Transect")
    EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
    EVDeleteRegionClass(EVFile=EVFile,EVtransect)
    
    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    
    varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
    Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
    Ex_P_PRC_NASC[["Enabled"]] <- TRUE
    ExPropLat_M <- varProp$Item("Lat_M")
    ExPropLat_M[["Enabled"]] <- TRUE
    ExPropLon_M <- varProp$Item("Lon_M")
    ExPropLon_M[["Enabled"]] <- TRUE
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
    Ex_P_Time_M <- varProp$Item("Time_M")
    Ex_P_Time_M[["Enabled"]] <- TRUE
    Ex_P_Date_S <- varProp$Item("Date_S")
    Ex_P_Date_S[["Enabled"]] <- TRUE
    Ex_P_Date_E <- varProp$Item("Date_E")
    Ex_P_Date_E[["Enabled"]] <- TRUE
    Ex_P_Date_M <- varProp$Item("Date_M")
    Ex_P_Date_M[["Enabled"]] <- TRUE

    
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
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i]
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i] 
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"

    EVRegionClass <- EVFile[["RegionClasses"]]
    EVTransect<-EVRegionClass$FindByName("Transect")
    EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    
    varGrid <- varObj[["Properties"]][["Grid"]]
    varGrid$SetTimeDistanceGrid(5, 926)
    varGrid$SetDepthRangeGrid(1, 400)
    
    EVVar$ExportIntegrationByRegionsByCells(paste0(exportbycell.path, vessel_search[i], '.csv'),
                                                      EVTransect)
    EVFile$SaveAs(paste0(EV.Export.path,vessel_search[i],'.EV'))
    EvAppObj$Quit() 
  }
  
  EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
  

}



LineExporter <- function(rawFileslist, evFiles,lines.path,vessel_search,SurfaceLine,BottomLine,Integration){
  
  #Export Region Logs
  for(i in 1:length(rawFileslist)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    #Export Regions
    EVLine<- EVFile[["Lines"]]
    EVSurface<-EVLine$FindByName(SurfaceLine)
    EVSurface[["Name"]] <- "Surface Line"
    EVBottom<-EVLine$FindByName(BottomLine)
    EVBottom <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, 0.5, 0)
    EVBottom[["Name"]] <- "Bottom Line"
    
    EVIntLine<-EVLine$FindByName(Integration)
    EVIntLine <- EVFile[["Lines"]]$CreateOffsetLinear(EVIntLine, 1, 0.5, 0)   
    EVIntLine[["Name"]] <- "Herring Integration editing line"
    EVSurface$Export(paste0(lines.path,vessel_search[i],"-","Surface",'.EVL'))
    EVBottom$Export(paste0(lines.path,vessel_search[i],"-","Bottom",'.EVL'))
    EVIntLine$Export(paste0(lines.path,vessel_search[i],"-","IntLine",'.EVL'))
    EvAppObj$Quit()
  }
  
  lineFiles = dir(lines.path,full.names=T,pattern='.EVL$')
  return(lineFiles)
}





Integration_original_2022_07_06_hac_fromTemplate<- function(hacFileslist,
                                                            evFiles,
                                                            transectFiles,
                                                            regionFiles,
                                                            export.path,
                                                            vessel_search,
                                                            SurfaceLineName) {
  
  #For EV files with HAC files. 
  for(i in  (length(ordered_calFiles)+1):length(vessel_search)){  
    
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    # Load existing EV File with regions.
    EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))

    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
    EVtransect <-EVRegionClass$FindByName("Transect")
    EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
    EVDeleteRegionClass(EVFile=EVFile,EVtransect)
    
    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    
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
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i]
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i] 
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVTransect<-EVRegionClass$FindByName("Transect")
    
    EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    
    EvAppObj$Quit()
  }
}


Integration_original_2022_07_06_hac_fromTemplate_bycell<- function(hacFileslist,
                                                            evFiles,
                                                            transectFiles,
                                                            regionFiles,
                                                            export.path,
                                                            vessel_search,
                                                            SurfaceLineName) {
  
  #For EV files with HAC files. 
  for(i in  (length(ordered_calFiles)+1):length(vessel_search)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    # Load existing EV File with regions.
    EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
    
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
    EVtransect <-EVRegionClass$FindByName("Transect")
    EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
    EVDeleteRegionClass(EVFile=EVFile,EVtransect)
    
    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    
    varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
    Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
    Ex_P_PRC_NASC[["Enabled"]] <- TRUE
    ExPropLat_M <- varProp$Item("Lat_M")
    ExPropLat_M[["Enabled"]] <- TRUE
    ExPropLon_M <- varProp$Item("Lon_M")
    ExPropLon_M[["Enabled"]] <- TRUE
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
    Ex_P_Date_M <- varProp$Item("Date_M")
    Ex_P_Date_M[["Enabled"]] <- TRUE
    Ex_P_Time_M <- varProp$Item("Time_M")
    Ex_P_Time_M[["Enabled"]] <- TRUE
    
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
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i]
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters" 
    varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
    varAnaly[["ExcludeAbove"]] <- SurfaceLineName[i] 
    varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVTransect<-EVRegionClass$FindByName("Transect")
    
    varGrid <- varObj[["Properties"]][["Grid"]]
    varGrid$SetTimeDistanceGrid(5, 926)
    varGrid$SetDepthRangeGrid(1, 400)
    
    EVVar$ExportIntegrationByRegionsByCells(paste0(exportbycell.path, vessel_search[i], '.csv'),
                                            EVTransect)
    EVFile$SaveAs(paste0(EV.Export.path,vessel_search[i],'.EV'))
    EvAppObj$Quit() 
  }
}


LineExporter <- function(rawFileslist, evFiles,lines.path,vessel_search,SurfaceLine,BottomLine,Integration){
  
  #Export Region Logs
  for(i in 1:length(rawFileslist)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    #Export Regions
    EVLine<- EVFile[["Lines"]]
    EVSurface<-EVLine$FindByName(SurfaceLine)
    EVSurface[["Name"]] <- "Surface Line"
    EVBottom<-EVLine$FindByName(BottomLine)
    EVBottom <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, 0.5, 0)
    EVBottom[["Name"]] <- "Bottom Line"
    
    EVIntLine<-EVLine$FindByName(Integration)
    EVIntLine <- EVFile[["Lines"]]$CreateOffsetLinear(EVIntLine, 1, 0.5, 0)   
    EVIntLine[["Name"]] <- "Herring Integration editing line"
    EVSurface$Export(paste0(lines.path,vessel_search[i],"-","Surface",'.EVL'))
    EVBottom$Export(paste0(lines.path,vessel_search[i],"-","Bottom",'.EVL'))
    EVIntLine$Export(paste0(lines.path,vessel_search[i],"-","IntLine",'.EVL'))
    EvAppObj$Quit()
  }
  
  lineFiles = dir(lines.path,full.names=T,pattern='.EVL$')
  return(lineFiles)
}



LineExporter_noINT <- function(rawFileslist, evFiles,lines.path,vessel_search,SurfaceLine,BottomLine){
  
  #Export Region Logs
  for(i in 1:length(rawFileslist)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(evFiles[i])
    
    #Export Regions
    EVLine<- EVFile[["Lines"]]
    EVSurface<-EVLine$FindByName(SurfaceLine)
    EVSurface[["Name"]] <- "Surface Line"
    EVIntLine <- EVFile[["Lines"]]$CreateOffsetLinear(EVSurface, 1, 0, 0)
    EVIntLine[["Name"]] <- "Herring Integration editing line"
    EVBottom<-EVLine$FindByName(BottomLine)
    EVBottom <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, 03, 0)
    EVBottom[["Name"]] <- "Bottom Line"
    
    EVSurface$Export(paste0(lines.path,vessel_search[i],"-","Surface",'.EVL'))
    EVBottom$Export(paste0(lines.path,vessel_search[i],"-","Bottom",'.EVL'))
    EVIntLine$Export(paste0(lines.path,vessel_search[i],"-","IntLine",'.EVL'))
    EvAppObj$Quit()
  }
  
  lineFiles = dir(lines.path,full.names=T,pattern='.EVL$')
  return(lineFiles)
}




Integration_to_template_2022_11_04 <- function(rawFileslist,
         evFiles,
         ordered_calFiles,
         transectFiles,
         regionFiles,
         export.path,
         TemplateEV,
         vessel_search) {
  
  #For EV files with RAW files. 
  for(i in 1:length(rawFileslist)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(TemplateEV)
    
    
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
    # Add calibration files
    EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
    
    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    EVFile$Import(lineFileslist[[i]][1])
    EVFile$Import(lineFileslist[[i]][2])
    EVFile$Import(lineFileslist[[i]][3])
    
    EVBottom <- EVFile[["Lines"]]$FindByName(paste0(vessel_search[i],"-","Bottom"))
    EVBottomOffset <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, -0.5, 0)
    EVBottomOffset[["Name"]] <- "Bottom Line - 0.5 Meters"
    EVAttenuateRemove <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, 5, 0)
    EVAttenuateRemove[["Name"]] <- "Attenuated Signal Removal Line"
    
    EVVar_raw  <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
    EVVar_raw_analys <- EVVar_raw[["Properties"]][["Analysis"]]
    EVVar_raw_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_raw_analys[["ExcludeBelow"]] <- paste0(vessel_search[i],"-","Bottom")
    
    EVVar_attenuate <- EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
    EVVar_attenuate_analys <- EVVar_attenuate[["Properties"]][["Analysis"]]
    EVVar_attenuate_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_attenuate_analys[["ExcludeBelow"]] <- paste0(vessel_search[i],"-","Bottom")
    
    EVVar_attenuate_prop <-EVVar_attenuate[["Properties"]][["AttenuatedSignal"]]
    EVVar_attenuate_prop[["ExcludeAboveLine"]] <- paste0(vessel_search[i],"-","Bottom")
    EVVar_attenuate_prop[["ExcludeBelowLine"]]  <- "Attenuated Signal Removal Line"
    
    EVVar_processed <- EVFile[["Variables"]]$FindByName("Processed data 1")
    EVVar_processed_analys <- EVVar_processed[["Properties"]][["Analysis"]]
    EVVar_processed_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_processed_analys[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    
    
    
    
    
    EVminThresholdSet(EVVar_processed, -70)
    
    varObj = EVVar_processed
    varDat <- varObj[["Properties"]][["Data"]]
    preThresApplyFlag <- varDat$ApplyMaximumThreshold()
    varDat[["ApplyMaximumThreshold"]] <- TRUE
    postThresApplyFlag <- varDat$ApplyMaximumThreshold()
    varDat[["LockSvMaximum"]] <- FALSE
    varDat[["MaximumThreshold"]] <- -10
    
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVTransect<-EVRegionClass$FindByName("Transect")
    EVVar_processed$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    EVFile$SaveAs(paste0(EV.Export.path,vessel_search[i],'.EV'))
    EvAppObj$Quit() 
  }
}

Integration_to_template_2022_11_04_Hac <- function(hacFileslist,
                                                   evFiles,
                                                   ordered_calFiles,
                                                   transectFiles,
                                                   regionFiles,
                                                   export.path,
                                                   TemplateEV_HAC,
                                                   vessel_search) {
  
  #For EV files with RAW files. 
  for(i in  (length(ordered_calFiles)+1):length(vessel_search)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(TemplateEV_HAC)
    
    
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
    
    
    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    EVFile$Import(lineFileslist[[i]][1])
    EVFile$Import(lineFileslist[[i]][2])
    EVFile$Import(lineFileslist[[i]][3])
    
    EVBottom <- EVFile[["Lines"]]$FindByName(paste0(vessel_search[i],"-","Bottom"))
    EVBottomOffset <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, -0.5, 0)
    EVBottomOffset[["Name"]] <- "Bottom Line - 0.5 Meters"
    EVAttenuateRemove <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, 5, 0)
    EVAttenuateRemove[["Name"]] <- "Attenuated Signal Removal Line"
    
    EVVar_raw  <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
    EVVar_raw_analys <- EVVar_raw[["Properties"]][["Analysis"]]
    EVVar_raw_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_raw_analys[["ExcludeBelow"]] <- paste0(vessel_search[i],"-","Bottom")
    
    EVVar_attenuate <- EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
    EVVar_attenuate_analys <- EVVar_attenuate[["Properties"]][["Analysis"]]
    EVVar_attenuate_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_attenuate_analys[["ExcludeBelow"]] <- paste0(vessel_search[i],"-","Bottom")
    
    EVVar_attenuate_prop <-EVVar_attenuate[["Properties"]][["AttenuatedSignal"]]
    EVVar_attenuate_prop[["ExcludeAboveLine"]] <- paste0(vessel_search[i],"-","Bottom")
    EVVar_attenuate_prop[["ExcludeBelowLine"]]  <- "Attenuated Signal Removal Line"
    
    EVVar_processed <- EVFile[["Variables"]]$FindByName("Processed data 1")
    EVVar_processed_analys <- EVVar_processed[["Properties"]][["Analysis"]]
    EVVar_processed_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_processed_analys[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    
    
    varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
    Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
    Ex_P_PRC_NASC[["Enabled"]] <- TRUE
    ExPropLat_M <- varProp$Item("Lat_M")
    ExPropLat_M[["Enabled"]] <- TRUE
    ExPropLon_M <- varProp$Item("Lon_M")
    ExPropLon_M[["Enabled"]] <- TRUE
    Ex_P_ABC <- varProp$Item("ABC")
    Ex_P_ABC[["Enabled"]] <- TRUE
    ExPropABS <- varProp$Item("Area_Backscatter_Strength")
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
    
    
    
    
    EVminThresholdSet(EVVar_processed, -70)
    
    varObj = EVVar_processed
    varDat <- varObj[["Properties"]][["Data"]]
    preThresApplyFlag <- varDat$ApplyMaximumThreshold()
    varDat[["ApplyMaximumThreshold"]] <- TRUE
    postThresApplyFlag <- varDat$ApplyMaximumThreshold()
    varDat[["LockSvMaximum"]] <- FALSE
    varDat[["MaximumThreshold"]] <- -10
    
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVTransect<-EVRegionClass$FindByName("Transect")
    EVVar_processed$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    
    EVFile$SaveAs(paste0(EV.Export.path,vessel_search[i],'.EV'))
    EvAppObj$Quit() 
  }
}


Integration_to_template_2023_08_16_Hac <- function(hacFileslist,
                                               evFiles,
                                               ordered_calFiles,
                                               transectFiles,
                                               regionFiles,
                                               export.path,
                                               TemplateEV_HAC,
                                               vessel_search) {
  
  #For EV files with RAW files. 
  for(i in  (length(ordered_calFiles)+1):length(vessel_search)){  
    
    # Open Echoview COM connection
    EvAppObj = COMCreate('EchoviewCom.EvApplication')
    
    #open template for export
    EVFile <- EvAppObj$OpenFile(TemplateEV_HAC)
    
    
    # Add raw data to the new file
    EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
    

    #Import Regions
    #open Files for import
    EVFile$Import(transectFiles[i])
    EVFile$Import(regionFiles[i])
    EVFile$Import(lineFileslist[[i]][1])
    EVFile$Import(lineFileslist[[i]][2])
    EVFile$Import(lineFileslist[[i]][3])
    
    EVBottom <- EVFile[["Lines"]]$FindByName(paste0(vessel_search[i],"-","Bottom"))
    EVBottomOffset <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, -0.5, 0)
    EVBottomOffset[["Name"]] <- "Bottom Line - 0.5 Meters"
    EVAttenuateRemove <- EVFile[["Lines"]]$CreateOffsetLinear(EVBottom, 1, 5, 0)
    EVAttenuateRemove[["Name"]] <- "Attenuated Signal Removal Line"
    
    EVVar_raw  <- EVFile[["Variables"]]$FindByName("Sv pings")
    EVVar_raw_analys <- EVVar_raw[["Properties"]][["Analysis"]]
    EVVar_raw_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_raw_analys[["ExcludeBelow"]] <- paste0(vessel_search[i],"-","Bottom")
    
    EVVar_attenuate <- EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
    EVVar_attenuate_analys <- EVVar_attenuate[["Properties"]][["Analysis"]]
    EVVar_attenuate_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_attenuate_analys[["ExcludeBelow"]] <- paste0(vessel_search[i],"-","Bottom")
    
    EVVar_attenuate_prop <-EVVar_attenuate[["Properties"]][["AttenuatedSignal"]]
    EVVar_attenuate_prop[["ExcludeAboveLine"]] <- paste0(vessel_search[i],"-","Bottom")
    EVVar_attenuate_prop[["ExcludeBelowLine"]]  <- "Attenuated Signal Removal Line"
    
    EVVar_processed <- EVFile[["Variables"]]$FindByName("Processed data 1")
    EVVar_processed_analys <- EVVar_processed[["Properties"]][["Analysis"]]
    EVVar_processed_analys[["ExcludeAbove"]] <- paste0(vessel_search[i],"-","Surface")
    EVVar_processed_analys[["ExcludeBelow"]] <- "Bottom Line - 0.5 Meters"
    
    
    varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
    Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
    Ex_P_PRC_NASC[["Enabled"]] <- TRUE
    ExPropLat_M <- varProp$Item("Lat_M")
    ExPropLat_M[["Enabled"]] <- TRUE
    ExPropLon_M <- varProp$Item("Lon_M")
    ExPropLon_M[["Enabled"]] <- TRUE
    Ex_P_ABC <- varProp$Item("ABC")
    Ex_P_ABC[["Enabled"]] <- TRUE
    ExPropABS <- varProp$Item("Area_Backscatter_Strength")
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
    EVVar_processed <- EVFile[["Variables"]]$FindByName("Processed data 1")
    if (is.null(EVVar_processed) == TRUE) {
      EVVar_processed <-
        EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
    }
    if (is.null(EVVar_processed) == TRUE) {
      EVVar_processed <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
    }
    if (is.null(EVVar_processed) == TRUE) {
      EVVar_processed <- EVFile[["Variables"]]$FindByName("Sv pings")
    }
    EVVar_above <-
      EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
    if (is.null(EVVar_processed) == TRUE) {
      EVVar_above <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
    }
    if (is.null(EVVar_processed) == TRUE) {
      EVVar_above <- EVFile[["Variables"]]$FindByName("Sv pings")
    }
    
    
    EVminThresholdSet(EVVar_processed, -70)
    
    varObj = EVVar_processed
    varDat <- varObj[["Properties"]][["Data"]]
    preThresApplyFlag <- varDat$ApplyMaximumThreshold()
    varDat[["ApplyMaximumThreshold"]] <- TRUE
    postThresApplyFlag <- varDat$ApplyMaximumThreshold()
    varDat[["LockSvMaximum"]] <- FALSE
    varDat[["MaximumThreshold"]] <- -10
    
    
    EVRegionClass <- EVFile[["RegionClasses"]]
    EVTransect<-EVRegionClass$FindByName("Transect")
    EVVar_processed$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
    
    varGrid <- varObj[["Properties"]][["Grid"]]
    varGrid$SetTimeDistanceGrid(5, 926)
    varGrid$SetDepthRangeGrid(1, 400)
    
    EVVar_processed$ExportIntegrationByRegionsByCells(paste0(exportbycell.path, vessel_search[i], '.csv'),
                                            EVTransect)
    EVFile$SaveAs(paste0(EV.Export.path,vessel_search[i],'.EV'))
    EvAppObj$Quit() 
  }
}

Integrationbycell_original_2022_11_02 <-
  function(rawFileslist,
           evFiles,
           ordered_calFiles,
           transectFiles,
           regionFiles,
           exportbycell.path,
           bottomLine,
           surfaceLine,
           vessel_search) {
    #For EV files with RAW files.
    for (i in 1:length(rawFileslist)) {
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile = EVFile, filesetName = 'Fileset 1')
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <- EVRegionClass$FindByName("Unclassified regions")
      EVtransect <- EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile = EVFile, EVunclassified)
      EVDeleteRegionClass(EVFile = EVFile, EVtransect)
      
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
      # Add calibration files
      EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
      
      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
      
      varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
      Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
      Ex_P_PRC_NASC[["Enabled"]] <- TRUE
      ExPropLat_M <- varProp$Item("Lat_M")
      ExPropLat_M[["Enabled"]] <- TRUE
      ExPropLon_M <- varProp$Item("Lon_M")
      ExPropLon_M[["Enabled"]] <- TRUE
      Ex_P_ABC <- varProp$Item("ABC")
      Ex_P_ABC[["Enabled"]] <- TRUE
      ExPropABS <- varProp$Item("Area_Backscatter_Strength")
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
      if (is.null(EVVar) == TRUE) {
        EVVar <-
          EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
      }
      if (is.null(EVVar) == TRUE) {
        EVVar <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
      }
      if (is.null(EVVar) == TRUE) {
        EVVar <- EVFile[["Variables"]]$FindByName("Sv pings")
      }
      EVVar_above <-
        EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
      if (is.null(EVVar) == TRUE) {
        EVVar_above <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
      }
      if (is.null(EVVar) == TRUE) {
        EVVar_above <- EVFile[["Variables"]]$FindByName("Sv pings")
      }
      EVminThresholdSet(EVVar,-70)
      
      varObj = EVVar
      varDat <- varObj[["Properties"]][["Data"]]
      preThresApplyFlag <- varDat$ApplyMaximumThreshold()
      varDat[["ApplyMaximumThreshold"]] <- TRUE
      postThresApplyFlag <- varDat$ApplyMaximumThreshold()
      varDat[["LockSvMaximum"]] <- FALSE
      varDat[["MaximumThreshold"]] <- -10
      varAnaly <- EVVar[["Properties"]][["Analysis"]]
      varAnaly[["ExcludeAbove"]] <- "Surface Line"
      varAnaly[["ExcludeBelow"]] <- "Bottom Line"
      varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
      varAnaly[["ExcludeAbove"]] <- "Surface Line"
      varAnaly[["ExcludeBelow"]] <- "Bottom offset - 0.5 meters"
      
      varGrid <- varObj[["Properties"]][["Grid"]]
      varGrid$SetTimeDistanceGrid(5, 926)
      varGrid$SetDepthRangeGrid(1, 400)
      
      
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect <- EVRegionClass$FindByName("Transect")
      EVVar$ExportIntegrationByRegionsByCells(paste0(exportbycell.path, vessel_search[i], '.csv'),
                                              EVTransect)
      EvAppObj$Quit()
    }
  }


Integrationbycell_original_2022_07_06 <-
  function(rawFileslist,
           evFiles,
           ordered_calFiles,
           transectFiles,
           regionFiles,
           exportbycell.path,
           vessel_search) {
    #For EV files with RAW files.
    for (i in 1:length(rawFileslist)) {
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile = EVFile, filesetName = 'Fileset 1')
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <- EVRegionClass$FindByName("Unclassified regions")
      EVtransect <- EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile = EVFile, EVunclassified)
      EVDeleteRegionClass(EVFile = EVFile, EVtransect)
      
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
      # Add calibration files
      EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
      
      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
      
      varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
      Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
      Ex_P_PRC_NASC[["Enabled"]] <- TRUE
      ExPropLat_M <- varProp$Item("Lat_M")
      ExPropLat_M[["Enabled"]] <- TRUE
      ExPropLon_M <- varProp$Item("Lon_M")
      ExPropLon_M[["Enabled"]] <- TRUE
      Ex_P_ABC <- varProp$Item("ABC")
      Ex_P_ABC[["Enabled"]] <- TRUE
      ExPropABS <- varProp$Item("Area_Backscatter_Strength")
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
      
      
      varGrid <- varObj[["Properties"]][["Grid"]]
      varGrid$SetTimeDistanceGrid(5, 926)
      varGrid$SetDepthRangeGrid(1, 400)
      
      
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect <- EVRegionClass$FindByName("Transect")
      EVVar$ExportIntegrationByRegionsByCells(paste0(exportbycell.path, vessel_search[i], '.csv'),
                                              EVTransect)
      EvAppObj$Quit()
    }
  }



Integrationbycell_original_2022_07_06_Hac <-
  function(hacFileslist,
           evFiles,
           transectFiles,
           regionFiles,
           exportbycell.path,
           vessel_search) {
    #For EV files with RAW files.
    for (i in (length(ordered_calFiles)+1):length(vessel_search)) {
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile = EVFile, filesetName = 'Fileset 1')
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <- EVRegionClass$FindByName("Unclassified regions")
      EVtransect <- EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile = EVFile, EVunclassified)
      EVDeleteRegionClass(EVFile = EVFile, EVtransect)
      
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
      # Add calibration files

      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
      
      varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
      Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
      Ex_P_PRC_NASC[["Enabled"]] <- TRUE
      ExPropLat_M <- varProp$Item("Lat_M")
      ExPropLat_M[["Enabled"]] <- TRUE
      ExPropLon_M <- varProp$Item("Lon_M")
      ExPropLon_M[["Enabled"]] <- TRUE
      Ex_P_ABC <- varProp$Item("ABC")
      Ex_P_ABC[["Enabled"]] <- TRUE
      ExPropABS <- varProp$Item("Area_Backscatter_Strength")
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
      
      
      varGrid <- varObj[["Properties"]][["Grid"]]
      varGrid$SetTimeDistanceGrid(5, 926)
      varGrid$SetDepthRangeGrid(1, 400)
      
      
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect <- EVRegionClass$FindByName("Transect")
      EVVar$ExportIntegrationByRegionsByCells(paste0(exportbycell.path, vessel_search[i], '.csv'),
                                              EVTransect)
      EvAppObj$Quit()
    }
  }



Integrationbycell_original_2022_07_06_Hac_fromTemplate <-
  function(hacFileslist,
           evFiles,
           transectFiles,
           regionFiles,
           exportbycell.path,
           vessel_search,
           SurfaceLineName ) {
    #For EV files with RAW files.
    for (i in 1:length(hacFileslist)) {
      # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
      
      # Load existing EV File with regions.
      EVClearRawData(EVFile = EVFile, filesetName = 'Fileset 1')
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVunclassified <- EVRegionClass$FindByName("Unclassified regions")
      EVtransect <- EVRegionClass$FindByName("Transect")
      EVDeleteRegionClass(EVFile = EVFile, EVunclassified)
      EVDeleteRegionClass(EVFile = EVFile, EVtransect)
      
      # Add raw data to the new file
      EVAddRawData(EVFile, 'Fileset1', unlist(hacFileslist[i]))
      # Add calibration files
      
      #Import Regions
      #open Files for import
      EVFile$Import(transectFiles[i])
      EVFile$Import(regionFiles[i])
      
      
      varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
      Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
      Ex_P_PRC_NASC[["Enabled"]] <- TRUE
      ExPropLat_M <- varProp$Item("Lat_M")
      ExPropLat_M[["Enabled"]] <- TRUE
      ExPropLon_M <- varProp$Item("Lon_M")
      ExPropLon_M[["Enabled"]] <- TRUE
      Ex_P_ABC <- varProp$Item("ABC")
      Ex_P_ABC[["Enabled"]] <- TRUE
      ExPropABS <- varProp$Item("Area_Backscatter_Strength")
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
      varAnaly[["ExcludeAbove"]] <- SurfaceLineName 
      varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 meters" 
      varAnaly <- EVVar_above[["Properties"]][["Analysis"]]
      varAnaly[["ExcludeAbove"]] <- SurfaceLineName  
      varAnaly[["ExcludeBelow"]] <- "Bottom Line - 0.5 meters" 
      
      
      varGrid <- varObj[["Properties"]][["Grid"]]
      varGrid$SetTimeDistanceGrid(5, 926)
      varGrid$SetDepthRangeGrid(1, 400)
      
      
      
      EVRegionClass <- EVFile[["RegionClasses"]]
      EVTransect <- EVRegionClass$FindByName("Transect")
      EVVar$ExportIntegrationByRegionsByCells(paste0(exportbycell.path, vessel_search[i], '.csv'),
                                              EVTransect)
      EvAppObj$Quit()
    }
  }



Dataframe_to_RectRegion<- function(startDepth, stopDepth, startTime, endTime, startDate, endDate){
  depths <-list()
  times <- list()
  dates <- list()
  for(i in 1:length(startDepth)){
    depths[[i]] <-append(startDepth[i], stopDepth[i],1)
    times[[i]] <- append(paste0(right(left(startTime[i],3),
                                      2), right(left(startTime[i],6),2),
                                right(left(startTime[i],9),2)),
                         paste0(right(left(endTime[i],3),
                                      2), right(left(endTime[i],6),2),
                                right(left(endTime[i],9),2)))
    dates[[i]] <-append(startDate[i], endDate[i],1)
  }
  return( list(depths,times,dates))
}

MakeEVL<- function (depths, times, dates, linestatus, dir = NULL, fn = NULL){
  if (!all(c(length(depths) == length(times))))  #checks to make sure data is orgnized
    stop("ARGS depths and times must have the same length.")
  opFile = paste(fn, ".evl", sep = "") #create data type
  if (!is.null(dir))  
    opFile = paste(dir, opFile, sep = "") # creates file in working dir
  nreg <- length(dates) #length of date list 
  cat(paste("EVBD 3 13.0.395.45232"), file = opFile, append = FALSE, 
      sep = "\n")
  cat(paste(nreg), file = opFile, append = TRUE, 
      sep = "\n")
  
  for (i in 1:nreg) {
    cat(paste(dates[i], times[i],depths[i], linestatus[i]), 
        file = opFile, append = TRUE, sep = "\n")
  }   
}




MakeEVR<- function (depths, times, dates, rName = "Region", rClass = "Selection", 
                    rType = 1, dir = NULL, fn = NULL, rNotes = list("")){
  if (!all(c(length(depths) == length(times))))  #checks to make sure data is orgnized
    stop("ARGS depths and times must have the same length.")
  opFile = paste(fn, ".evr", sep = "") #create data type
  if (!is.null(dir))  
    opFile = paste(dir, opFile, sep = "") # creates file in working dir
  nreg <- length(dates) #length of date list 
  cat(paste("EVRG 7 8.0.91.31697"), file = opFile, append = FALSE, 
      sep = "\n")
  cat(paste(nreg, "\n"), file = opFile, append = TRUE, 
      sep = "\n")
  for (i in 1:nreg) {
    message(paste(Sys.time(), "Processing region", 
                  i))
    if (length(rNotes) >= i) { # if there is a note, add it if not, make it a zero
      if (nchar(rNotes[[i]]) > 0) {
        linesNotes = 1
      }
      else {
        linesNotes = 0
      }
    } # until this line.
    linesDetec = 0
    min_ind <- which(as.numeric(times[[i]]) == min(as.numeric(times[[i]])))
    max_ind <- which(as.numeric(times[[i]]) == max(as.numeric(times[[i]])))
    cat(paste(13, length(dates[[i]]), i, 0, 2, -1, 1, dates[[i]][min_ind], 
              substr(paste0(as.character(times[[i]][min_ind]), 
                            "0000"), 1, 10), formatC(depths[[i]][min_ind], 
                                                     digits = 6, format = "f"), dates[[i]][max_ind], 
              substr(paste0(as.character(times[[i]][max_ind]), 
                            "0000"), 1, 10), depths[[i]][max_ind]), 
        file = opFile, append = TRUE, sep = "\n")
    cat(paste(linesNotes), file = opFile, append = TRUE, 
        sep = "\n")
    cat(paste(linesDetec), file = opFile, append = TRUE, 
        sep = "\n")
    cat(paste(rClass), file = opFile, append = TRUE, sep = "\n")
    pts = ""
    for (d in 1:length(dates[[i]])) {
      pts <- append(pts, paste(dates[[i]][d], substr(paste0(as.character(times[[i]][d]), 
                                                            "0000"), 1, 10), formatC(depths[[i]][d], 
                                                                                     digits = 6, format = "f")))
    }
    cat(c(pts[2:length(pts)], rType, "\n"), file = opFile, 
        append = TRUE)
    cat(paste0(rName, i, "\n"), file = opFile, append = TRUE, 
        sep = "\n")
  }
  message(paste(Sys.time(), "Echoview region (.evr) file written to: ", 
                file = opFile))
}




