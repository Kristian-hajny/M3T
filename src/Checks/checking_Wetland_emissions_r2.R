## Wetland_emissions_r2.R
## In use: 2021-11-02 20:00
#
# Load in the various state wetland fraction rasters
# These overlap somewhat, so crop each to the squares within each state
# Then add together and assign fluxes to each class
################################################################################
#Manually defined variables

input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/"
output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/"

NWI_files <- list.files(paste0(output_directory,"/NWI/"),".tiff",full.names = T)

state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
state_name_list <- sort(c("NJ","NY","PA","MD","DE"))
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
packagecheck <- c("raster","ncdf4","readxl","terra","jsonlite","dplyr","sp","sf")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster + ncdf4 = raster and .nc filetype functionalities
#readxl = ability to load more excel filetypes flexibly
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
#load in and process the Wetland_fraction_r1 output to convert from wetland
#coverage to wetland emissions - except the emissions parts is just an EF - so
#that's irrelevant, just compare the summed wetland maps

#old
{
  states_wgs84 <- project(State_Tigerlines,CRS(SRS_string="EPSG:4326"))  # Transform to WGS84
  
  # Initialise rasters that will hold the total fluxes (all states)
  target_raster <- raster(domain)  # WGS84
  target_raster[] <- 0
  E2_frac <- target_raster
  M2_frac <- target_raster
  R1_frac <- target_raster
  R2_frac <- target_raster
  R3_frac <- target_raster
  R4_frac <- target_raster
  L1_frac <- target_raster
  L2_frac <- target_raster
  PFO_frac <- target_raster
  PNF_frac <- target_raster
  
  # Load in state by state, and in each case retain only the fluxes for cells within that state
  # Combine all the fluxes in the _frac rasters as we go
  for(i in 1:length(state_name_list)){
    state_border <- subset(st_as_sf(states_wgs84), STUSPS==state_name_list[i])
    
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_E2.tiff"))
    E2_frac <- E2_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_M2.tiff"))
    M2_frac <- M2_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_R1.tiff"))
    R1_frac <- R1_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_R2.tiff"))
    R2_frac <- R2_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_R3.tiff"))
    R3_frac <- R3_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_R4.tiff"))
    R4_frac <- R4_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_L1.tiff"))
    L1_frac <- L1_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_L2.tiff"))
    L2_frac <- L2_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_PFO.tiff"))
    PFO_frac <- PFO_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
    filepick <- grep(basename(NWI_files),pattern=paste0(state_name_list[i],"_PNF.tiff"))
    PNF_frac <- PNF_frac + mask(raster(NWI_files[filepick]),state_border,updatevalue=0)
  }
}

#new

{
  Wetland_types <- vector()
  Wetland_types <- c(Wetland_types,"M2","E2","PFO","PNF")
  Wetland_types <- c(Wetland_types,"R1","R2","R3","R4","L1","L2")

  #process separately for each type (different EFs)
  for(i in 1:length(Wetland_types)){
    subset_files <- NWI_files[grep(Wetland_types[i],NWI_files)]
    subset_data <- rast(subset_files)
    #given NWI extends somewhat beyond state bounds, there is overlap.  So max
    #should combine them akin to sum, but without double counting.
    subset_data <- max(subset_data)
    # subset_data <- sum(subset_data)
    # names(subset_data) <- Wetland_types[i]
    assign(paste0(Wetland_types[i],"_new"),subset_data)
  }
}


divergent <- colorRampPalette(c("red","white","blue"))

checker <- function(input){
  new <- get(paste0(input,"_new"))
  old <- get(paste0(input,"_frac"))
  compare_new <- crop(new,rast(old))

  plot(rast(old),main="old")
  plot(compare_new,main="new")
  
  delta <- rast(old) - compare_new
  plot(delta,main=paste0(input," old - new"),range=unlist(global(abs(delta),max))*c(-1,1),
       col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
  lines(State_Tigerlines)
  cat("max delta = ",unlist(global(abs(delta),max)))
  cat("\nmax new = ",unlist(global(compare_new,max)))
  cat("\nnew=",as.numeric(global(compare_new,sum)))
  cat("\nold=",as.numeric(global(rast(old),sum)))
  cat("\nold/new=",as.numeric(global(rast(old),sum)/global(compare_new,sum)))
}

checker("E2")
checker("M2")
checker("R1")
checker("R2")
checker("R3")
checker("R4")
checker("L1")
checker("L2")
checker("PFO")
checker("PNF")





