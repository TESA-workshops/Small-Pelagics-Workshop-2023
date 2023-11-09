
library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)
library(maptools)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires) #devtools::install_github("ropensci/rnaturalearthhires") needed to run this to install it with my version of R
library(sf)
library(ggplot2)
library(units)
library(magrittr)
library(sjmisc)

options(warn = -1)

#sort points
#' Function to sort a set of points/coordinates in a clockwise or anti-clockwise
#' direction.
#' 
#' \code{sort_points} can be useful for creating spatial lines or polygons from 
#' points. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing y and x points. 
#' @param y Name of y variable in \code{df}. 
#' @param x Name of x variable in \code{df}. 
#' @param clockwise Should the points be arranged in a clockwise direction? If 
#' \code{FALSE}, the order will be an anti-clockwise direction. 
#'
#' @export
sort_points <- function(df, y = "latitude", x = "longitude", clockwise = TRUE) {
  
  # NA check, if NAs drop them
  if (any(is.na(c(df[, y], df[, x])))) {
    
    # Remove NAs
    df <- df[!(is.na(df[, y]) & is.na(df[, x])), ]
    
    # Raise warning
    warning("Missing coordinates were detected and have been removed.", 
            call. = FALSE)
    
    # Check 
    if (nrow(df) == 0) stop("There are no valid coordinates.", call. = FALSE)
    
  }
  
  # Get centre (-oid) point of points
  x_centre <- mean(df[, x])
  y_centre <- mean(df[, y])
  
  # Calculate deltas
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # Resolve angle, in radians
  df$angle <- atan2(df$y_delta, df$x_delta)
  # d$angle_degrees <- d$angle * 180 / pi
  
  # Arrange by angle
  if (clockwise) {
    
    df <- df[order(df$angle, decreasing = TRUE), ]
    
  } else {
    
    df <- df[order(df$angle, decreasing = FALSE), ]
    
  }
  
  # Drop intermediate variables
  df[, c("x_delta", "y_delta", "angle")] <- NULL
  
  # Return
  df
  
}


land.all <- st_read("Y:/Acoustic Index Review Files/NE_10M_Coastline/ne_10m_coastline.shp")
land.all<-st_cast(land.all$geometry, "POLYGON")
land.all

# 
# topline_broken = function(x, broken = TRUE){
# if (broken){
#   topline_dist <-      st_distance(st_point(c(topline$Lon_S, topline$Lat_S)), st_point(c(topline$Lon_E, topline$Lat_E)), crs = crs_use)
#   x[x$Region_name == topline$Region_name,]$Dist_S <- 0
#   x[x$Region_name == topline$Region_name,]$Dist_E <- topline_dist*1000
# }
# }



map_area_buffered = function(x, transectEastWest = TRUE){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    East_point_distance <- East_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    East_point_distance<-East_point_distance$dist[!is.na( East_point_distance$dist)]
    West_point_distance <- West_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    West_point_distance<-West_point_distance$dist[!is.na( West_point_distance$dist)]
    distance_between_transects<-c(East_point_distance, West_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  sf::sf_use_s2(FALSE)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  
  options(warn = defaultW)
  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf))
  
}


map_area_buffered_gill = function(x, transectEastWest = TRUE){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    East_point_distance <- East_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    East_point_distance<-East_point_distance$dist[!is.na( East_point_distance$dist)]
    West_point_distance <- West_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    West_point_distance<-West_point_distance$dist[!is.na( West_point_distance$dist)]
    distance_between_transects<-c(East_point_distance, West_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    coord_sf(xlim = c((polygon_bbox[1]-0.02), polygon_bbox[3]+0.02), ylim = c((polygon_bbox[2]-0.02),(polygon_bbox[4]+0.02))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  sf::sf_use_s2(FALSE)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  
  options(warn = defaultW)
  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf))
  
}

map_area_buffered_standard = function(x, transectEastWest = TRUE, d = 1250){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
   
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  sf::sf_use_s2(FALSE)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  
  options(warn = defaultW)
  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf))
  
}




fix_distance_topline<-function(x, topline_broken = TRUE){
  if (topline_broken){
    land.all <-
      ne_countries(scale = "large",
                   returnclass = "sf",
                   continent = "North America")
    crs_use <- st_crs(land.all)
    topline <-x %>% filter( Lat_S == max(Lat_S) | Lat_E == max(Lat_S)) #determine which line is topline
    PT1 <- st_sfc(st_point(c(topline$Lon_S, topline$Lat_S)), crs = crs_use)
    PT2 <- st_sfc(st_point(c(topline$Lon_E, topline$Lat_E)), crs = crs_use)
    x[x$Region_name == topline$Region_name,]$Dist_S <- 0
    x[x$Region_name == topline$Region_name,]$Dist_E <- as.numeric(st_distance(PT1,PT2))
  }
  return(x)
}


fix_distance_customline<-function(x, topline_broken = TRUE, transectName){ #Where transectName needs to be as.character
  if (topline_broken){
    land.all <-
      ne_countries(scale = "large",
                   returnclass = "sf",
                   continent = "North America")
    crs_use <- st_crs(land.all)
    custom_line <-x %>% filter( Region_name == transectName) #determine which line is custom_line
    PT1 <- st_sfc(st_point(c(custom_line$Lon_S, custom_line$Lat_S)), crs = crs_use)
    PT2 <- st_sfc(st_point(c(custom_line$Lon_E, custom_line$Lat_E)), crs = crs_use)
    x[x$Region_name == custom_line$Region_name,]$Dist_S <- 0
    x[x$Region_name == custom_line$Region_name,]$Dist_E <- as.numeric(st_distance(PT1,PT2))
  }
  return(x)
}



map_area = function(x, transectEastWest = TRUE){ 
  land.all <-
    ne_countries(scale = "large",
                 returnclass = "sf",
                 continent = "North America")
  crs_use <- st_crs(land.all)
  
  Latitude  <- c(x$Lat_S, x$Lat_E)
  Longitude  <- c(x$Lon_S, x$Lon_E)
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2)
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,])
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2)))
  transect_p_poly <- st_sfc(transect_p, crs = crs_use)
  transect_p_attrib <- data.frame(name = paste0(surveydate))
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly)
  
  centy <- st_transform(transect_p_sf, crs_use) %>% 
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy)
  
  # Arrange by angle
  if (transectEastWest) {
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),]
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    df2<-rbind(West,East)
    df2 <- rbind(df2,df2[1,])
    
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2)))
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red")
  p
  sf::sf_use_s2(FALSE)
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  
  
  return(list(p,a))
}


Org_point = function(x){
  Latitude  <- c(x$Lat_S, x$Lat_E)
  Longitude  <- c(x$Lon_S, x$Lon_E)
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2)
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  return(LatLondf)
}


#calculate area of survey
area_calc = function(x) {
  Longitude  <- c(x$Lon_S, x$Lon_E)
  Latitude  <- c(x$Lat_S, x$Lat_E)
  LatLonMat <- matrix(c(Longitude, Latitude), ncol = 2)
  survey_coord <- SpatialPoints(LatLonMat)
  crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")
  
  proj4string(survey_coord) <- crs.geo
  is.projected(survey_coord)
  summary(survey_coord)
  
  survey_area <- mcp(survey_coord, percent = 100)
  area <-
    survey_area$area * 100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.
  return(area)
}


biomassCalc = function(x, area, TS38, TS50, TS75, TS120) {
  right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
  left = function (string,char){substr(string,1,char)}
  #x = transects_German
  x$areaKm = as.numeric(area)
  
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = 10 ^ (x$Area_Backscatter_Strength / 10) * x$Actual_Weighting
  x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)
  x$TS <- as.numeric(ifelse(grepl(38, x$Frequency), TS38, ifelse(
    grepl(50, x$Frequency), TS50, ifelse(grepl(75, x$Frequency), TS75, TS120)
  )))
  x$biomass_density = 10 ^ ((x$Area_Backscatter_Strength - (x$TS)) / 10) # same as Jenna's Transects.R script calculation
  x$density = (x$biomass_density) * (x$distance)
  x$weighted_mean_biomass_calc = (x$biomass_density) * (x$Actual_Weighting)
  x$trans_biomass = (x$weighted_mean_biomass_calc) * x$areaKm * 1000
  x$total_biomass = sum(x$trans_biomass)
  
  se <- function(x)
    sqrt(var(x) / length(x))
  x$se = se(x$biomass_density)
  x$standard_error_tonnes = x$se * x$areaKm * 1000
  x$standard_error_perc = x$standard_error_tonnes / x$total_biomass * 100
  x$mean_biomass_density = mean(x$biomass_density)
  x$meanSa = 10 * log10(sum(x$calc_actual_mean_sa))
  x$Date_S = min(x$Date_S)
  x$Vessel = left(x$Region_name, 2)
  x$Transect_No =  right(left(x$Region_name, 6),3)
  x$TransCount = length(x$biomass_density)
  x$SD = sd(x$biomass_density) * x$areaKm * 1000
  x$Variance = x$standard_error_tonnes^2
  
  return(x)
}



#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char) {
  substr(string, nchar(string) - (char - 1), nchar(string))
}
left = function (string, char) {
  substr(string, 1, char)
}
datediff = function(date2, date1) {
  if(
    as.numeric(gsub("S", "", gsub("d", "", left(
      as.character(days(date2) - days(date1)), 2
    )))) < 0
  ){
    0
  } else {
    as.numeric(gsub("S", "", gsub("d", "", left(
      as.character(days(date2) - days(date1)), 2
    ))))
  }
}

turnoverBio = function(y_intercept,
                       x_Var_1,
                       daysturnover,
                       Date,
                       Survey,
                       Biomass) {
  turnMat <-
    matrix(nrow = (length(Date) - 1), ncol = (length(Survey) -
                                                1))
  
  for (j in 1:(length(Survey) - 1)) {
    for (i in 1:(length(Date) - 1)) {
      turnMat[i, j] <-
        if (datediff(Date[i + 1], Date[j]) >= daysturnover) {
          turnMat[i, j] <- 0
        } else  if ((1 - (x_Var_1 * log10(
          datediff(Date[i + 1], Date[j])
        ) + y_intercept)) * Biomass[Survey[j]] < 0) {
          turnMat[i, j] <- 0
        } else {
          turnMat[i, j] <- (1 - (x_Var_1 * log10(
            datediff(Date[i + 1], Date[j])
          ) + y_intercept)) * Biomass[Survey[j]]
        }
    }
  }
  turnMat[turnMat == Inf] = 0
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
    as.integer(sum(c(Biomass[1], finalbiomass)))
  return(totalbiomass)
}



turnoverBio_adjustedBio = function(y_intercept,
                                   x_Var_1,
                                   daysturnover,
                                   Date,
                                   Survey,
                                   Biomass) {
  turnMat <-
    matrix(nrow = (length(Date) - 1), ncol = (length(Survey) -
                                                1))
  
  for (j in 1:(length(Survey) - 1)) {
    for (i in 1:(length(Date) - 1)) {
      turnMat[i, j] <-
        if (datediff(Date[i + 1], Date[j]) >= daysturnover) {
          turnMat[i, j] <- 0
        } else  if ((1 - (x_Var_1 * log10(
          datediff(Date[i + 1], Date[j])
        ) + y_intercept)) * Biomass[Survey[j]] < 0) {
          turnMat[i, j] <- 0
        } else {
          turnMat[i, j] <- (1 - (x_Var_1 * log10(
            datediff(Date[i + 1], Date[j])
          ) + y_intercept)) * Biomass[Survey[j]]
        }
    }
  }
  turnMat[turnMat == Inf] = 0
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
    (c(Biomass[1], finalbiomass))
  return(totalbiomass)
}

