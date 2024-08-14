# Calculate NLCD fractions for the states in the d03 domain
## Finalized: 2023-02-03

NLCD_open_and_low_int <- function(){
  library(raster)
  library(sf)
  ################################################################################
  #Manually defined variables
  nlcd_file <- file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles",
                         "nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")
  domain <- raster(domain)
  State_Tigerlines <- as(State_Tigerlines,"Spatial")
  
  ################################################################################
  #subset NLCD to the appropriate states
  
  nlcd <- raster(nlcd_file)
  
  # Make a polygon for the domain (with a small buffer just to be safe)
  area_x_coords <- c(xmin(domain),xmax(domain),xmax(domain),xmin(domain))+c(-0.5,0.5,0.5,-0.5)
  area_y_coords <- c(ymin(domain),ymin(domain),ymax(domain),ymax(domain))+c(-0.5,-0.5,0.5,0.5)
  xym <- cbind(area_x_coords,area_y_coords)
  p <- Polygon(xym)
  ps <- Polygons(list(p),1)
  crop_area <- SpatialPolygons(list(ps))
  proj4string(crop_area) <- CRS(SRS_string="EPSG:4326")  # WGS84
  crop_area_trans <- spTransform(crop_area, crs(nlcd))
  
  
  # reproject states to NLCD crs
  states_trans <- spTransform(State_Tigerlines,crs(nlcd))
  
  #crop/mask the nlcd to the states in the domain so that it's a bit more
  #manageable in size from the get-go.  Again, add a slight buffer.
  nlcd <- crop(nlcd,1.01*extent(states_trans))
  
  ################################################################################
  # Make new rasters for Developed, Open Space, and Developed, Low Intensity
  # Do everything one at a time to avoid memory issues
  nlcd_open <- nlcd
  values(nlcd_open) <- 0
  nlcd_open[nlcd==21] <- 1
  nlcd_low_int <- nlcd
  values(nlcd_low_int) <- 0
  nlcd_low_int[nlcd==22] <- 1
  remove(nlcd)
  ################################################################################
  # calculate the sum total for each state
  
  #initialize output data frame for state totals
  area_df <- data.frame("developed_open"=vector(),
                        "developed_low_intensity"=vector())
  
  ################################################################################
  #project/crop to exactly the domain, then save
  
  #first aggregate to a similar resolution
  reprojected_open <- aggregate(nlcd_open,na.rm=T,
                                fact=floor(res(projectRaster(from=domain,crs=crs(nlcd_open)))/res(nlcd_open)/10)*10,
                                mean)
  reprojected_low_int <- aggregate(nlcd_low_int,na.rm=T,
                                   fact=floor(res(projectRaster(from=domain,crs=crs(nlcd_low_int)))/res(nlcd_low_int)/10)*10,
                                   mean)
  #then reproject to the exact desired domain using nearest neighbor to have less
  #of an impact on the state/domain totals
  reprojected_open <- projectRaster(reprojected_open,to=domain,method="ngb")
  reprojected_low_int <- projectRaster(reprojected_low_int,to=domain,method="ngb")
  
  writeRaster(reprojected_open,file.path(output_directory,
                                 paste0("raster_NLCD_open_regridded.nc")),
           overwrite=T)
  writeRaster(reprojected_low_int,file.path(output_directory,
                                    paste0("raster_NLCD_low_int_regridded.nc")),
           overwrite=T)

  # plot(septic_emiss2,ylim=c(39.5,39.8),xlim=c(-75.85,-75.7))
  # abline(v=-75.77);abline(v=-75.78);abline(v=-75.79)
  ## DE and MD overlap here, so they can be compared more clearly.
}
