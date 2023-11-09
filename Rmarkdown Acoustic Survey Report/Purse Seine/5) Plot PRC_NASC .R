rm(list = ls())

library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)
library(maptools)
library(maps)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(dplyr)
library(ggrepel)
library(data.table)
library(cowplot)
library(gridExtra)

right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}

#Year
yr <- 2020

#legend_plot
#grid.arrange( p, legend, 2/1)
library(cowplot)
get_only_legend <- function(plot) {
  
  # get tabular interpretation of plot
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  #  Mark only legend in plot
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend
  legend <- plot_table$grobs[[legend_plot]]
  
  # return legend
  return(legend) 
}


plot_survey_german <- list()
plot_survey_seal <- list()
plot_survey_german_collect <- list()
plot_survey_seal_collect <- list()

surveydate <- list.dirs(path = paste0( getwd(), "/Purse Seine/integration/"), full.names = F, recursive = F)

for(j in 1:length(surveydate)){
  file_list1 = dir(paste0( getwd(), "/Purse Seine/integration/",surveydate[j], "/"), full.names = T, pattern = '.csv$')
  
  fileData <- list()
  for (i in 1:length(file_list1)) {
    fileData[[i]] <- read.csv(file_list1[i])
  }
  
  transects<-rbindlist(fileData)  

  #for each file in file_list, read in csv, keep specified columns, rbind to dataset to produce one dataset
 
  transects_German <-
    filter(transects,
           Lat_S < 43.67 &
             Lat_E < 43.67 &
             Lon_S < -66.235 &
             Lon_E < -66.235) #need to check whether this is accurate
  transects_SealIsland <-
    filter(transects,!Region_name %in% as.character(transects_German$Region_name))

  
  
  file_list2 = dir(paste0( getwd(), "/Purse Seine/integrationbycell/",surveydate[j], "/"), full.names = T, pattern = '.csv$')
  
  fileData2 <- list()
  for (i in 1:length(file_list2)) {
    fileData2[[i]] <- read.csv(file_list2[i])
  }
  transect_label<-rbindlist(fileData2)
  
  #create column with boat name for plotting
  transect_label$Boat <- as.character(transect_label$Region_name)
  transect_label$Boat <- substr(transect_label$Boat, 1, 2)
  transect_label$Boat
  

  transects_Label_German <-
    filter(transect_label,
           Lat_S < 43.67 &
             Lat_E < 43.67 &
             Lon_S < -66.235 &
             Lon_E < -66.235) #need to check whether this is accurate
  transects__label_SealIsland <-
    filter(transect_label,!Region_name %in% as.character(transects_German$Region_name))

 
  
  land.all <-
    ne_countries(scale = "large",
                 returnclass = "sf",
                 continent = "North America")
  
  #add survey boxes
  German_survey_box <-
    st_linestring(rbind(
      c(-66.473, 43.233),
      c(-66.473, 43.567),
      c(-66.26, 43.567),
      c(-66.26, 43.233),
      c(-66.473, 43.233)
    ))
  crs_use <- st_crs(land.all)
  German_survey_box <- st_sfc(German_survey_box, crs = crs_use)
  Ger_attrib <- data.frame(name = "German Survey Box")
  Ger_sf <- st_sf(Ger_attrib, geometry = German_survey_box)
  
  #### Base Map
  #German Bank
  
  theme_set(theme_bw())
  plot_survey_german[[j]] <- ggplot(data = land.all) +
    geom_sf(color = "black", fill = "grey") +
    geom_sf(data = Ger_sf, color = "black") +
    geom_point(data = transects_Label_German,
               aes(
                 x = Lon_M,
                 y = Lat_M,
                 size = PRC_NASC,
                 alpha = 0.5,
               ),
               color = "tomato") +
    guides(alpha = "none") +
    scale_size(
      breaks = c(0, 1000, 2500, 5000, 10000, 20000, 50000),
      limits = c(0, 100000),
      range = c(1, 20)
    ) +
    geom_text_repel(data = transects_Label_German, aes(x = Lon_S, y = Lat_S, label = Boat)) +
    xlab("Longitude") + ylab("Latitude") +
    labs(title = paste0("Acoustic survey on ", surveydate[j])) +
    coord_sf(xlim = c(-66.6, -66), ylim = c(43.2, 43.7)) +
    theme(plot.title = element_text(hjust = 0.5))


  ggsave(paste0("Acoustic_survey_German_",surveydate[j],".png"),
         plot = plot_survey_german[[j]],
         width=6,
         height=6,
         path =  paste0(getwd(), "/Purse Seine/Data Tables Figures/"))
  
  saveRDS(plot_survey_german, 
          paste0(getwd(), "/Purse Seine/Data Tables Figures/",
                 "plot_survey_german_PRC.rda"
          ))
  
  
  plot_survey_german_collect[[j]] <- ggplot(data = land.all) +
    geom_sf(color = "black", fill = "grey") +
    geom_sf(data = Ger_sf, color = "black") +
    geom_point(data = transects_Label_German,
               aes(
                 x = Lon_M,
                 y = Lat_M,
                 size = PRC_NASC,
                 alpha = 0.5,
               ),
               color = "tomato") +
    guides(alpha = "none") +
    scale_size(
      breaks = c(0, 1000, 2500, 5000, 10000, 20000, 50000),
      limits = c(0, 100000),
      range = c(1, 20)
    ) +
    #geom_text_repel(data = transects_Label_German, aes(x = Lon_S, y = Lat_S, label = Boat)) +
    xlab("Longitude") + ylab("Latitude") +
    labs(title = paste0("Acoustic survey on ", surveydate[j])) +
    coord_sf(xlim = c(-66.6, -66), ylim = c(43.2, 43.7)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")
  
  #Seal Island
  theme_set(theme_bw())
  plot_survey_seal[[j]] <- ggplot(data = land.all) +
    geom_sf(color = "black", fill = "grey") +
    geom_sf(data = Ger_sf, color = "black") +
    geom_point(data = transects__label_SealIsland,
               aes(
                 x = Lon_M,
                 y = Lat_M,
                 size = PRC_NASC,
                 alpha = 0.5,
               ),
               color = "tomato") +
    guides(alpha = "none") +
    scale_size(
      breaks = c(0, 1000, 2500, 5000, 10000, 20000, 50000),
      limits = c(0, 100000),
      range = c(1, 20)
    ) +
    geom_text_repel(data = transects__label_SealIsland, aes(x = Lon_S, y = Lat_S, label = Boat)) +
    xlab("Longitude") + ylab("Latitude") +
    labs(title = paste0("Acoustic survey on ", surveydate[j])) +
    coord_sf(xlim = c(-66.6, -66), ylim = c(43.2, 43.7)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(
    paste0("Acoustic_survey_Seal_", surveydate[j], ".png"),
    plot = plot_survey_seal[[j]],
    width=5,
    height=5,
    path =  paste0(getwd(), "/Purse Seine/Data Tables Figures/")
  )
  
  saveRDS(plot_survey_seal, 
          paste0(getwd(), "/Purse Seine/Data Tables Figures/",
                 "plot_survey_seal_PRC.rda"
          ))
  
  #Seal Island
  theme_set(theme_bw())
  plot_survey_seal_collect[[j]] <- ggplot(data = land.all) +
    geom_sf(color = "black", fill = "grey") +
    geom_sf(data = Ger_sf, color = "black") +
    geom_point(data = transects__label_SealIsland,
               aes(
                 x = Lon_M,
                 y = Lat_M,
                 size = PRC_NASC,
                 alpha = 0.5,
               ),
               color = "tomato") +
    guides(alpha = "none") +
    scale_size(
      breaks = c(0, 1000, 2500, 5000, 10000, 20000, 50000),
      limits = c(0, 100000),
      range = c(1, 20)
    ) +
    #geom_text_repel(data = transects__label_SealIsland, aes(x = Lon_S, y = Lat_S, label = Boat)) +
    xlab("Longitude") + ylab("Latitude") +
    labs(title = paste0("Acoustic survey on ", surveydate[j])) +
    coord_sf(xlim = c(-66.6, -66), ylim = c(43.2, 43.7)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position="none")


}

get_only_legend(plot_survey_seal[[1]])
plots <-append(plot_survey_seal_collect, list(get_only_legend(plot_survey_seal[[1]])))
plots_seal<-grid.arrange(
  grobs = plots
)
ggsave(paste0(getwd(), "/Purse Seine/Data Tables Figures/","PRC_SEAL_","plots",".png"), 
       plot = plots_seal,width=12, height=12,units="in")


get_only_legend(plot_survey_german[[1]])
plots <-append(plot_survey_german_collect, list(get_only_legend(plot_survey_german[[1]])))
plots_german<-grid.arrange(
  grobs = plots
)
ggsave(paste0(getwd(), "/Purse Seine/Data Tables Figures/","PRC_GERMAN_","plots",".png"), 
       plot = plots_german, width=12 , height=12, units="in")


