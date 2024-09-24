#code to disaggregate Wetcharts by a factor of ~5 using NALCMS or NLCD data.
#Assumes that NALCMS value 14 = wetlands and NLCD values > 89 = wetlands.

################################################################################
#User input

# d01_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97)) 
# resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.  Will be used as the grid
#for projectraster, would need to be defined as the grid to project to for
#XESMF.

#If blank, instead crop to the landcover extent, clipping part of AK with NALCMS
#to avoid crossing the antimeridian (longitude -180 to +180).  Will take a
#considerable amount of time to run.


Wetcharts_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/WetCHARTs_v1_3_1_2019.nc"
# https://daac.ornl.gov/CMS/guides/MonthlyWetland_CH4_WetCHARTs.html
NLCD_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"
# https://www.mrlc.gov/
NALCMS_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/NALCMS_2020_land_cover/NA_NALCMS_landcover_2020_30m/data/NA_NALCMS_landcover_2020_30m.tif"
# http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2015-landsat-and-rapideye/

#can use NALCMS or NLCD.

XESMF_check <- F
#True/False whether you plan to use the XESMF regridder in python or use
#projectraster in R.

setwd("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/Wetcharts")
#where to save output, including progressive output throughout processing

# aggregated_raster_name <- paste0(Sys.Date(),"_laea_0.1_deg_wetland_fraction.grd")
aggregated_raster_name <- "2023-06-02_laea_0.1_deg_wetland_fraction.grd"
#Name for partial output.  This is landcover after converting landcover to 0 for
#every type except wetlands, and wetlands to 1. The data is then aggregated from
#30 m native resolution to ~0.1 deg resolution (factor of 370) using sum and
#projected from the native landcover projection to lat/long.  Code will check if
#this file already exists and load it if so, otherwise it will calculate and
#save it. Saves processing time if rerunning only latter parts of the code.


state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
state_name_list <- sort(c("NJ","NY","PA","MD","DE"))

#the old one to compare against
GHGRP_file <- file.path("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/",
                        "US_GHGRP_WWTP_only_all_years.xls")

input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite"
output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/"

inventory_year=2019

domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","sp","maps","fBasics","pracma","sf","terra", "readxl","jsonlite","dplyr")

while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

################################################################################
#create the domain and set it to all NaN
if(length(domain_res)==1){
  domain_res <- rep(domain_res,2)
}

if(class(domain)=="SpatRaster"){
  values(domain) <- NaN
}else if(class(domain)=="data.frame"){
  domain <- rast(nrows=diff(range(domain[,2]))/domain_res[2], 
                 ncols=diff(range(domain[,1]))/domain_res[1],
                 xmin=min(domain[,1]), xmax=max(domain[,1]),
                 ymin=min(domain[,2]), ymax=max(domain[,2]), 
                 crs=domain_crs)
  rm(domain_res,domain_crs)
}
domain=raster(domain)
State_Tigerlines <- vect(state_shapefile)

################################################################################
#load in  wetcharts

Wetcharts_old <- list(brick(Wetcharts_file,level=1),
                  brick(Wetcharts_file,level=2),
                  brick(Wetcharts_file,level=3),
                  brick(Wetcharts_file,level=4),
                  brick(Wetcharts_file,level=5),
                  brick(Wetcharts_file,level=6),
                  brick(Wetcharts_file,level=7),
                  brick(Wetcharts_file,level=8),
                  brick(Wetcharts_file,level=9),
                  brick(Wetcharts_file,level=10),
                  brick(Wetcharts_file,level=11),
                  brick(Wetcharts_file,level=12))
#load each month separately, since each brick also has 18 layers, each a
#different modeled result

names(Wetcharts_old) <- sprintf("%02d",01:12)
#name the list levels appropriately.

Wetcharts <- rast(Wetcharts_file)
Wetcharts <- crop(Wetcharts,ext(project(rast(domain),crs(Wetcharts)))+0.5)


rm(Wetcharts_file)
################################################################################
#load in the landcover - national land cover database or North
#American Land Change Monitoring System (NLCD and NALCMS)


#old
{
  #adding in the cropping as processing without first cropping is bonkers resource
  #use for no reason.
  NLCD_old <- brick(NLCD_file)
  NALCMS_old <- brick(NALCMS_file)
  template <- raster(ext=extent(raster(Wetcharts)),res=res(domain),crs=crs(Wetcharts[[1]][[1]]))
  NLCD_old <- crop(NLCD_old,
                   projectRaster(from=template,crs=crs(NLCD_old),res=res(NLCD_old)))
  NALCMS_old <- crop(NALCMS_old,
                     projectRaster(from=template,crs=crs(NALCMS_old),res=res(NALCMS_old)))
  
  ################################################################################
  #set wetlands to a value of 1 and all other land cover to 0, and aggregate to
  #0.1 deg
  
  template <- disagg(Wetcharts,fact=5)
  NALCMS_old_0.1_deg <- reclassify(NALCMS_old,matrix(c(0,13,0,
                                                       13.5,14.5,1,
                                                       14.6,5000,0),
                                                     ncol=3,byrow=T),
                                   include.lowest=T)
  #force all values between 0 and 13 or 14.6 to 5000 to 0.  values between 13.5
  #and 14.5 are 1.  14 = wetland land cover for NALCMS.
  NALCMS_old_0.1_deg <- aggregate(x = NALCMS_old_0.1_deg,na.rm=T,
                                  fact = (res(project(template,crs(rast(NALCMS_old))))/res(NALCMS_old))[1],
                                  fun = sum,expand=T)
  
  NLCD_old_0.1_deg <- reclassify(NLCD_old,matrix(c(0,89,0,
                                                   89,200,1),
                                                 ncol=3,byrow=T))
  #force all values between 0 and 89 to 0.  values between 89 and 200 are forced
  #to 1.  90 and 95 = wetland land cover for NLCD.
  
  NLCD_old_0.1_deg <- aggregate(x = NLCD_old_0.1_deg,na.rm=T,
                                fact = (res(project(template,crs(rast(NLCD_old))))/res(NLCD_old))[1],
                                fun = sum,expand=T)
  ################################################################################
  #Either save the data to project it using XESMF or regrid with projectraster
  #and save it to easily pick up mid-processing later
  template <- raster(ext=extent(raster(Wetcharts[[1]][[1]])),res=0.1,crs=crs(Wetcharts[[1]][[1]]))

  NLCD_old_0.1_deg_proj <- projectRaster(NLCD_old_0.1_deg,to=template)
  NALCMS_old_0.1_deg_proj <- projectRaster(NALCMS_old_0.1_deg,to=template)
  
  #for some reason this fails.  It just produces an empty raster.  I have no
  #clue why as they certainly cover the same region..... This would allow better
  #comparison because it would be direct equivalent reprojection math.
  # NLCD_old_0.1_deg_proj <- project(rast(NLCD_old_0.1_deg),template[[1]])
  # NALCMS_old_0.1_deg_proj <- project(rast(NALCMS_old_0.1_deg),template[[1]])

}

#new
{
  NLCD <- rast(NLCD_file)
  NLCD <- crop(NLCD,
               project(x=ext(Wetcharts),from=crs(Wetcharts),to=crs(NLCD)))
  NALCMS <- rast(NALCMS_file)
  NALCMS <- crop(NALCMS,
                 project(x=ext(Wetcharts),from=crs(Wetcharts),to=crs(NALCMS)))
  ################################################################################
  #set wetlands to a value of 1 and all other land cover to 0, then project to
  #domain CRS at 0.1 deg.
  
  #solely for the exact grid to project to
  template <- disagg(Wetcharts,fact=5)
  
  #force all values between 0 and 89 to 0.  values between 89 and 200 are forced
  #to 1.  90 and 95 = wetland land cover for NLCD.
  NLCD <- classify(NLCD,matrix(c(0,89,0,
                                 89,200,1),
                               ncol=3,byrow=T))
  #aggregate to near 0.1 deg resolution, then reproject to the domain CRS
  NLCD <- aggregate(NLCD,
                    fact=res(project(template,crs(NLCD)))/res(NLCD),
                    fun=sum)
  NLCD <- project(NLCD,template)
  
  #force all values between 0 and 13 or 14.6 to 5000 to 0.  values between 13.5
  #and 14.5 are 1.  14 = wetland land cover for NALCMS.
  NALCMS <- classify(NALCMS,matrix(c(0,13,0,
                                     13.5,14.5,1,
                                     14.6,5000,0),
                                   ncol=3,byrow=T))
  #aggregate to a similar resolution, then reproject to the domain CRS
  NALCMS <- aggregate(NALCMS,
                      fact=res(project(template,crs(NALCMS)))/res(NALCMS),
                      fun=sum)
  NALCMS <- project(NALCMS,template)
}

divergent <- colorRampPalette(c("red","white","blue"))

plot(NLCD)
plot(rast(NLCD_old_0.1_deg_proj))

delta <- rast(NLCD_old_0.1_deg_proj) - NLCD
plot(delta,main="NLCD old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(rast(NLCD_old_0.1_deg_proj),sum);global(NLCD,sum)
global(NLCD,sum)/global(rast(NLCD_old_0.1_deg_proj),sum)


plot(NALCMS)
plot(rast(NALCMS_old_0.1_deg_proj))

delta <- rast(NALCMS_old_0.1_deg_proj) - NALCMS
plot(delta,main="NALCMS old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(rast(NALCMS_old_0.1_deg_proj),sum);global(NALCMS,sum)
global(NALCMS,sum)/global(rast(NALCMS_old_0.1_deg_proj),sum)

################################################################################
#define the domain of interest + a little buffer, crop wetcharts

Wetcharts <- crop(Wetcharts,ext(project(rast(domain),crs(Wetcharts)))+0.5)

for(A in 1:length(Wetcharts_old)){
  Wetcharts_old[[A]] <- crop(Wetcharts_old[[A]],extent(raster(NALCMS)))
}
#crop Wetcharts to the same region

#since old averaged across all models and cold season, new averages some models
#and not across seasons, just averaging to annual, all model for both to compare
#processing.

Wetcharts <- mean(Wetcharts)
Wetcharts_old <- mean(brick(Wetcharts_old))

#even these 2 differ by rounding error somehow, just averaging a file...
delta <- Wetcharts - project(rast(Wetcharts_old),crs(Wetcharts))
plot(delta,main="old - new",range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(rast(Wetcharts_old),sum,na.rm=T);global(Wetcharts,sum,na.rm=T)
global(Wetcharts,sum,na.rm=T)/global(rast(Wetcharts_old),sum,na.rm=T)

################################################################################
#now calculate the wetland fraction

#old
{
  NLCD_0.5_deg_proj <- aggregate(x = raster(NLCD),na.rm=T,
                                      fact = 5,
                                      fun = sum,expand=T)
  NALCMS_0.5_deg_proj <- aggregate(x = raster(NALCMS),na.rm=T,
                                      fact = 5,
                                      fun = sum,expand=T)
  #aggregate from 0.1 to 0.5 degrees.  Each 0.5 deg pixel = sum of 30 m wetland
  #pixels that are wetlands (i.e., the fraction of the land in the pixel that is
  #wetlands).
  
  #this process was taken in part from
  #https://gis.stackexchange.com/questions/262015/calculation-of-fractional-cover-for-each-vegetation-class-at-30-m-resolution-mat/262958#262958
  
  NLCD_0.5_deg_proj <- disaggregate(NLCD_0.5_deg_proj,fact=5)
  NALCMS_0.5_deg_proj <- disaggregate(NALCMS_0.5_deg_proj,fact=5)
  #convert the 0.5 deg version to a 0.1 deg version.  This is just so pixels
  #align perfectly with the 0.1 version and does NOT change the values in each
  #pixel.
  
  NLCD_wetland_fraction_old <- raster(NLCD)/NLCD_0.5_deg_proj
  NALCMS_wetland_fraction_old <- raster(NALCMS)/NALCMS_0.5_deg_proj
  #now get the ratio of wetlands in each 0.1 deg pixel relative to the 0.5 deg
  #pixels.  Note doing this and then projecting, rather than projecting first,
  #will NOT conserve mass as the ratios within the 0.5 deg pixel will no longer
  #sum exactly to 1.
  
  NLCD_wetland_fraction_old[is.na(NLCD_wetland_fraction_old)] <- 1/25
  NALCMS_wetland_fraction_old[is.na(NALCMS_wetland_fraction_old)] <- 1/25
  #for any without a value, just distribute equally to the 25 pixels in each 0.5
  #deg pixel (0/# or no data from landcover)
  
  ################################################################################
  #disaggregate Wetcharts using wetland fraction from NLCD
  
  NLCD_Downscaled_Wetcharts_old <- disaggregate(Wetcharts_old*raster(cellSize(rast(Wetcharts_old))),fact=5)
  NLCD_Downscaled_Wetcharts_old <- NLCD_Downscaled_Wetcharts_old*NLCD_wetland_fraction_old/raster(cellSize(rast(NLCD_Downscaled_Wetcharts_old)))
  
  NALCMS_Downscaled_Wetcharts_old <- disaggregate(Wetcharts_old*raster(cellSize(rast(Wetcharts_old))),fact=5)
  NALCMS_Downscaled_Wetcharts_old <- NALCMS_Downscaled_Wetcharts_old*NALCMS_wetland_fraction_old/raster(cellSize(rast(NALCMS_Downscaled_Wetcharts_old)))
  #important - wetcharts is in flux units (which is per area).  Multiply by the
  #area before downscaling, then divide by the new smaller area after downscaling.
  #This conserves the total EMISSIONS, not the total FLUX.
}



#new
{
  ################################################################################
  #now calculate the wetland fraction
  
  #this process was taken in part from
  #https://gis.stackexchange.com/questions/262015/calculation-of-fractional-cover-for-each-vegetation-class-at-30-m-resolution-mat/262958#262958
  
  #aggregate to 0.5 degrees.  Each 0.5 deg pixel = sum of 30 m wetland
  #pixels that are wetlands (i.e., the fraction of the land in the pixel that is
  #wetlands).
  NLCD_0.5_deg <- aggregate(NLCD,
                            na.rm=T,
                            fact=5,
                            fun=sum)
  #convert the 0.5 deg version to the same resolution as domain.  This is just
  #so pixels align and does NOT change the values in each pixel.
  NLCD_0.5_deg <- disagg(NLCD_0.5_deg,fact=5)
  #now get the ratio of wetlands in each 0.1 deg pixel relative to the 0.5 deg
  #pixels.  Note doing this and then projecting, rather than projecting first,
  #will NOT conserve mass as the ratios within the 0.5 deg pixel will no longer
  #sum exactly to 1.
  NLCD_wetland_fraction <- NLCD/NLCD_0.5_deg
  #for any without a value, just distribute equally to the 25 pixels in each 0.5
  #deg pixel (0/# or no data from landcover)
  NLCD_wetland_fraction[is.na(NLCD_wetland_fraction)] <- 1/25
  
  
  #aggregate to 0.5 degrees.  Each 0.5 deg pixel = sum of 30 m wetland
  #pixels that are wetlands (i.e., the fraction of the land in the pixel that is
  #wetlands).
  NALCMS_0.5_deg <- aggregate(NALCMS,
                              na.rm=T,
                              fact=5,
                              fun=sum)
  #convert the 0.5 deg version to the same resolution as domain.  This is just
  #so pixels align and does NOT change the values in each pixel.
  NALCMS_0.5_deg <- disagg(NALCMS_0.5_deg,fact=5)
  #now get the ratio of wetlands in each 0.1 deg pixel relative to the 0.5 deg
  #pixels.  Note doing this and then projecting, rather than projecting first,
  #will NOT conserve mass as the ratios within the 0.5 deg pixel will no longer
  #sum exactly to 1.
  NALCMS_wetland_fraction <- NALCMS/NALCMS_0.5_deg
  #for any without a value, just distribute equally to the 25 pixels in each 0.5
  #deg pixel (0/# or no data from landcover)
  NALCMS_wetland_fraction[is.na(NALCMS_wetland_fraction)] <- 1/25
  
  ################################################################################
  #disaggregate Wetcharts using wetland fractions from the landcover
  
  #important - wetcharts is in flux units (which is per area).  Multiply by the
  #area before downscaling, then divide by the new smaller area after downscaling.
  #This conserves the total EMISSIONS, not the total FLUX.
  
  #Disaggregate to 0.1 deg and 
  Downscaled_Averaged_wetcharts <- disagg(Wetcharts*cellSize(Wetcharts),fact=5)
  
  #redistribute using wetland fraction and crop to domain
  NLCD_Downscaled_Averaged_wetcharts <- crop(Downscaled_Averaged_wetcharts*NLCD_wetland_fraction/cellSize(Downscaled_Averaged_wetcharts),
                                             project(rast(domain),crs(NLCD_wetland_fraction)),snap="out")
  
  NALCMS_Downscaled_Averaged_wetcharts <- crop(Downscaled_Averaged_wetcharts*NALCMS_wetland_fraction/cellSize(Downscaled_Averaged_wetcharts),
                                               project(rast(domain),crs(NALCMS_wetland_fraction)),snap="out")
}



delta <- NLCD_wetland_fraction - rast(NLCD_wetland_fraction_old)
plot(delta,main="old - new",range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(rast(NLCD_wetland_fraction_old),sum,na.rm=T);global(NLCD_wetland_fraction,sum,na.rm=T)
global(NLCD_wetland_fraction,sum,na.rm=T)/global(rast(NLCD_wetland_fraction_old),sum,na.rm=T)

delta <- NALCMS_wetland_fraction - rast(NALCMS_wetland_fraction_old)
plot(delta,main="old - new",range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(rast(NALCMS_wetland_fraction_old),sum,na.rm=T);global(NALCMS_wetland_fraction,sum,na.rm=T)
global(NALCMS_wetland_fraction,sum,na.rm=T)/global(rast(NALCMS_wetland_fraction_old),sum,na.rm=T)




NLCD_Downscaled_Wetcharts_old=NLCD_Downscaled_Wetcharts_old*1e9/(1000*16.043*24*3600)
NLCD_Downscaled_Averaged_wetcharts=NLCD_Downscaled_Averaged_wetcharts*1e9/(1000*16.043*24*3600)
NALCMS_Downscaled_Wetcharts_old=NALCMS_Downscaled_Wetcharts_old*1e9/(1000*16.043*24*3600)
NALCMS_Downscaled_Averaged_wetcharts=NALCMS_Downscaled_Averaged_wetcharts*1e9/(1000*16.043*24*3600)

old_comparison <- project(rast(NLCD_Downscaled_Wetcharts_old),crs(NLCD_Downscaled_Averaged_wetcharts))
old_comparison <- crop(old_comparison,NLCD_Downscaled_Averaged_wetcharts)

delta <- NLCD_Downscaled_Averaged_wetcharts - old_comparison
plot(delta,main="old - new",range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(old_comparison,sum,na.rm=T);global(NLCD_Downscaled_Averaged_wetcharts,sum,na.rm=T)
global(NLCD_Downscaled_Averaged_wetcharts,sum,na.rm=T)/global(old_comparison,sum,na.rm=T)


old_comparison <- project(rast(NALCMS_Downscaled_Wetcharts_old),crs(NALCMS_Downscaled_Averaged_wetcharts))
old_comparison <- crop(old_comparison,NALCMS_Downscaled_Averaged_wetcharts)

delta <- NALCMS_Downscaled_Averaged_wetcharts - old_comparison
plot(delta,main="old - new",range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(old_comparison,sum,na.rm=T);global(NALCMS_Downscaled_Averaged_wetcharts,sum,na.rm=T)
global(NALCMS_Downscaled_Averaged_wetcharts,sum,na.rm=T)/global(old_comparison,sum,na.rm=T)





