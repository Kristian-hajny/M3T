## NG_transmission_emissions_r1.R
## In use: 2021-11-02 20:00
## Finalized: 2023-02-03
#
# Calculate NG transmission emissions for d03 domain

################################################################################
#Manually defined variables

input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/"

plot_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_rewrite/stat_comb_intercomparison"

inventory_year=2019
domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.

GHGI_file=file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx")
GHGI_Emissions_sheet="3.6-1"
GHGI_Activity_sheet="3.6-7"

EIA_pipeline_file <- 'G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/NaturalGas_InterIntrastate_Pipelines_US_EIA/NaturalGas_Pipelines_US_202001.shp'
# Pipeline file comes from the EIA (https://www.eia.gov/maps/layer_info-m.php)
EIA_compressor_file <- 'G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/Natural_Gas_Compressor_Stations.csv'
# This compressor file comes from the Homeland Infrastructure Foundation-Level Database (https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::natural-gas-compressor-stations/about)
GHGRP_compressor_file <- 'G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/US_GHGRP_NG_transmission_and_Compression_only_all_years.xls'
# GHGRP compressor data comes in a spreadsheet from flight

state_name_list <- sort(c("NJ","NY","PA","MD","DE"))

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","terra","jsonlite","dplyr","sp","sf","pracma")
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
################################################################################
#process the transmission pipeline data

first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
EPA_p1 <- read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,skip=first_col,col_names = T)

first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
EPA_p2 <- read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,skip=first_col,col_names = T)
#p2 = emissions, p1 = activity data.  Columns = year, rows = various types of
#sources.  First col is just to identify the first column of useable data

Data_list <- c("Pipeline Leaks","M&R (Trans. Co. Interconnect)","M&R (Farm Taps + Direct Sales)",
               "Pipeline venting")
#all the sources we're looking for, written exactly as in the EPA file

EPA_Pipeline <- data.frame("Type"=Data_list,
                           "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[which(EPA_p2[,1]==x)[1],as.character(inventory_year)]})))*
                             1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                           "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p1[which(EPA_p1[,1]==x)[1],as.character(inventory_year)]})))*
                             1609.344,#convert from miles to meters
                           row.names = NULL)
#use sapply to find the row using data list, specify the column as the inventory_year and
#grab the relevant EF and activity data into a dataframe.

pipeline_EF <- sum(EPA_Pipeline[,2])/EPA_Pipeline[1,3] #mol/m/s
#sum of emissions / miles of pipelines (activity data from leaks entry)


Data_list <- c("Station Total Emissions","Dehydrator vents (Transmission)",
               "Flaring (Transmission)","Engines (Transmission)",
               "Turbines (Transmission)","Engines (Storage)",
               "Turbines (Storage)","Generators (Engines)",
               "Generators (Turbines)","Pneumatic Devices Transmission",
               "Station Venting Transmission")
#transmission station total + emissions during operations (vents, flaring,
#leaks, exhaust, etc.)

EPA_transmission_compressors <- data.frame("Type"=Data_list,
                                           "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[which(EPA_p2[,1]==x)[1],as.character(inventory_year)]})))*
                                             1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                           "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p1[which(EPA_p1[,1]==x)[1],as.character(inventory_year)]}))),
                                           row.names = NULL)
#use sapply to find the row using data list, specify the column as the inventory_year and
#grab the relevant EF and activity data into a dataframe.

Engine_transmission_fraction <- EPA_transmission_compressors[4,2]/sum(EPA_transmission_compressors[c(4,6),2])
Turbine_transmission_fraction <- EPA_transmission_compressors[5,2]/sum(EPA_transmission_compressors[c(5,7),2])
#calculate the ratio between transmission and storage emissions from engines and
#turbines

EPA_transmission_compressors[8,2] <- Engine_transmission_fraction*EPA_transmission_compressors[8,2]
EPA_transmission_compressors[9,2] <- Turbine_transmission_fraction*EPA_transmission_compressors[9,2]
#apply those ratios to the Generators for engines or turbines since they're not
#separated into transmission and storage

EPA_transmission_compressors <- EPA_transmission_compressors[c(1:5,8:11),]
#remove the storage data
compressor_avg_emissions <- sum(EPA_transmission_compressors[,2])/EPA_transmission_compressors[3,3] #mol/station/s
#sum of emissions / N stations (activity data from flaring entry)

rm(EPA_transmission_compressors,EPA_Pipeline,EPA_p1,EPA_p2,first_col,Data_list,
   Engine_transmission_fraction,Turbine_transmission_fraction)
################################################################################
#just compare downloaded and API accessed input data

#old
{
  # Load pipeline data into a SpatialLinesDataFrame
  pipes <- terra::vect(EIA_pipeline_file)
  compressors_EIA <- read.csv(EIA_compressor_file)
  coordinates(compressors_EIA) <- ~LONGITUDE + LATITUDE
  proj4string(compressors_EIA) <- CRS(SRS_string="EPSG:4326")  # WGS84
  
  compressors_ghgrp <- read_xls(GHGRP_compressor_file,sheet=as.character(inventory_year),col_names = T,skip = 5)
  #read the appropriate inventory_year.  
  coordinates(compressors_ghgrp) <- ~LONGITUDE + LATITUDE
  proj4string(compressors_ghgrp) <- crs(domain) # WGS84
  compressors_ghgrp <- crop(compressors_ghgrp,domain)
}

#new
{
  pipes_EIA=vect("https://services7.arcgis.com/FGr1D95XCGALKXqM/arcgis/rest/services/NaturalGas_InterIntrastate_Pipelines_US_EIA/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  # compressors_HIFLD=vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Compressor_Stations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  ################################################################################
  #Download the relevant GHGRP emissions data using the API
  #(https://www.GHGI.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant naturalgas-sector data
  #(https://www.GHGI.gov/enviro/greenhouse-gas-model).  Must download the relevant
  #data for each possible sector sGHGIrately as emissions are split by sector. The
  #total is combustion + NG systems, dominated by NG systems.  
  ghgrp_transmission_compressor_emissions <- fromJSON("https://data.epa.gov/efservice/ef_w_emissions_source_ghg/json")
  ghgrp_combustion_emissions <- fromJSON("https://data.epa.gov/efservice/C_SUBPART_LEVEL_INFORMATION/json")
  
  #because we're getting sub-facility level information for transmission
  #compressor, first need to aggregate.  Subsetting to only the year of interest
  #now instead of later.
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$reporting_year==inventory_year,]
  processing_CH4 <- aggregate(ghgrp_transmission_compressor_emissions$total_reported_ch4_emissions,
                              by=list(ghgrp_transmission_compressor_emissions$facility_id,
                                      ghgrp_transmission_compressor_emissions$reporting_year,
                                      ghgrp_transmission_compressor_emissions$facility_name,
                                      ghgrp_transmission_compressor_emissions$industry_segment),
                              sum,na.rm=T)
  processing_CH4 <- processing_CH4[,c(1:3,5,4)]
  
  #then split into transmission/compression and gas processing (some are both)
  ghgrp_transmission_compressor_emissions <- processing_CH4[processing_CH4[,5]=="Onshore natural gas transmission compression [98.230(a)(4)]",]
  processing_CH4 <- processing_CH4[processing_CH4[,5]=="Onshore natural gas processing [98.230(a)(3)]",]
  
  #reorganize slightly to match combustion.  Below function won't work right as
  #it's a competely different table
  colnames(ghgrp_transmission_compressor_emissions) <- colnames(ghgrp_combustion_emissions)
  ghgrp_transmission_compressor_emissions$ghg_gas_name <- "methane"
  
  #simple function to make sure gas names are limited to methane, and column names
  #are consistent
  make_consistent <- function(input){
    colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
    colnames(input) <- gsub("reporting_year","year",colnames(input))
    input$ghg_name <- tolower(input$ghg_name)
    input$facility_name <- tolower(input$facility_name)
    input <- input[input$ghg_name=="methane",]
    return(input)
  }
  
  ghgrp_transmission_compressor_emissions <- make_consistent(ghgrp_transmission_compressor_emissions)
  ghgrp_combustion_emissions <- make_consistent(ghgrp_combustion_emissions)
  
  #rename so the columns are different
  colnames(ghgrp_transmission_compressor_emissions) <- gsub("ghg_quantity","W_emissions",colnames(ghgrp_transmission_compressor_emissions))
  colnames(ghgrp_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(ghgrp_combustion_emissions))
  
  #combine both into 1 dataframe - using NG_system emissions as the base to get
  #ID/year matches from
  ghgrp_transmission_compressor_emissions=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id","year","facility_name","ghg_name"), all.x = TRUE)},
                                                 list(ghgrp_transmission_compressor_emissions,
                                                      ghgrp_combustion_emissions))
  
  #convert the relevant columns to numeric class
  ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")] <- apply(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],
                                                                                    2,FUN = function(x){as.numeric(x)})
  ghgrp_transmission_compressor_emissions$ghg_quantity <- rowSums(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],na.rm=T)
  
  #for those facilities that are involved in processing, the combustion emissions
  #are not considered part of the transmission/compression total, so remove it
  #here (very small number of facilities)
  processing_facilities <- ghgrp_transmission_compressor_emissions$facility_id %in% processing_CH4[,1]
  ghgrp_transmission_compressor_emissions$ghg_quantity[processing_facilities] <- ghgrp_transmission_compressor_emissions$W_emissions[processing_facilities]
  
  #now filter out those without any emissions
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$ghg_quantity>0,]
  
  rm(processing_facilities,processing_CH4,ghgrp_combustion_emissions,make_consistent)
  ################################################################################
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.GHGI.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_name_list,"/JSON")
  
  #initialize output
  ghgrp_facility_info <- data.frame()
  for(A in 1:length(state_name_list)){
    # download data and read/combine in an R dataframe
    ghgrp_facility_info <- rbind(ghgrp_facility_info,fromJSON(data_URLs[A]))
  }
  
  #combine the datasets by ID, and year
  ghgrp <- merge(ghgrp_facility_info,ghgrp_transmission_compressor_emissions,
                 by=c("facility_id","year"), all=F)
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  
  #delete all tempfiles and clean up working environment
  rm(A,ghgrp_facility_info,ghgrp_transmission_compressor_emissions)
  
  ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(ghgrp) <- "epsg:4326"
  ghgrp <- crop(ghgrp,rast(domain))
  
}

#perfect overlap except for a small patch in the midwest, visibly different in
#the input data, so that's just an update.
plot(pipes - pipes_EIA,main="old - new")
plot(pipes)
lines(pipes_EIA,col="red")
plot(pipes_EIA)
lines(pipes,col="red")

# compressors_EIA

#identical spatially
# plot(vect(compressors_ghgrp) - ghgrp,main="old - new")
plot(vect(compressors_ghgrp))
points(ghgrp,col="red")
plot(ghgrp)
points(vect(compressors_ghgrp),col="red")

plot(ghgrp$ghg_quantity[order(ghgrp$facility_id)] - (compressors_ghgrp$`GHG QUANTITY (METRIC TONS CO2e)`/25)[order(compressors_ghgrp$`GHGRP ID`)])

################################################################################
#process the transmission pipeline data

#old
{
  # Load pipeline data into a SpatialLinesDataFrame
  pipes <- terra::vect(EIA_pipeline_file)
  # Crop to just larger than d03 - don't know if it's necessary to have this buffer but it can't hurt
  e <- extent(domain)+c(-0.5,0.5,-0.5,0.5)
  pipes_crop <- crop(pipes,e)
  
  #had to update this bit as rgdal no longer exists
  pipes_by_cell <- rasterizeGeom(pipes_crop,rast(domain),"length")

  pipes_rast <- raster(domain)  # Create new raster to contain pipe emissions
  pipes_rast <- pipes_by_cell*pipeline_EF   # Set values to the pipe length (in metres) in each cell, multiplied by the effective emission factor in mol/m/s
  pipes_flux <- pipes_rast*1e9/(cellSize(pipes_rast,unit="m"))  # Calculate flux, mol/s to nmol/m2/s
  pipes_flux[is.na(pipes_flux)]<-0
  
  ################################################################################
  # Now onto the transmission compressor stations
  compressors_EIA <- read.csv(EIA_compressor_file)
  coordinates(compressors_EIA) <- ~LONGITUDE + LATITUDE
  proj4string(compressors_EIA) <- CRS(SRS_string="EPSG:4326")  # WGS84
  compressors_crop_EIA <- crop(compressors_EIA, domain)
  
  compressors_final <- compressors_crop_EIA

  compressors_final$emiss <- compressor_avg_emissions
  #default for all are the national avg
  
  ################################################################################
  #Download the relevant GHGRP emissions data using the API
  #(https://www.GHGI.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant naturalgas-sector data
  #(https://www.GHGI.gov/enviro/greenhouse-gas-model).  Must download the relevant
  #data for each possible sector sGHGIrately as emissions are split by sector. The
  #total is combustion + NG systems, dominated by NG systems.  
  ghgrp_transmission_compressor_emissions <- fromJSON("https://data.epa.gov/efservice/ef_w_emissions_source_ghg/json")
  ghgrp_combustion_emissions <- fromJSON("https://data.epa.gov/efservice/C_SUBPART_LEVEL_INFORMATION/json")
  
  #because we're getting sub-facility level information for transmission
  #compressor, first need to aggregate.  Subsetting to only the year of interest
  #now instead of later.
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$reporting_year==inventory_year,]
  processing_CH4 <- aggregate(ghgrp_transmission_compressor_emissions$total_reported_ch4_emissions,
                              by=list(ghgrp_transmission_compressor_emissions$facility_id,
                                      ghgrp_transmission_compressor_emissions$reporting_year,
                                      ghgrp_transmission_compressor_emissions$facility_name,
                                      ghgrp_transmission_compressor_emissions$industry_segment),
                              sum,na.rm=T)
  processing_CH4 <- processing_CH4[,c(1:3,5,4)]
  
  #then split into transmission/compression and gas processing (some are both)
  ghgrp_transmission_compressor_emissions <- processing_CH4[processing_CH4[,5]=="Onshore natural gas transmission compression [98.230(a)(4)]",]
  processing_CH4 <- processing_CH4[processing_CH4[,5]=="Onshore natural gas processing [98.230(a)(3)]",]
  
  #reorganize slightly to match combustion.  Below function won't work right as
  #it's a competely different table
  colnames(ghgrp_transmission_compressor_emissions) <- colnames(ghgrp_combustion_emissions)
  ghgrp_transmission_compressor_emissions$ghg_gas_name <- "methane"
  
  #simple function to make sure gas names are limited to methane, and column names
  #are consistent
  make_consistent <- function(input){
    colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
    colnames(input) <- gsub("reporting_year","year",colnames(input))
    input$ghg_name <- tolower(input$ghg_name)
    input$facility_name <- tolower(input$facility_name)
    input <- input[input$ghg_name=="methane",]
    return(input)
  }
  
  ghgrp_transmission_compressor_emissions <- make_consistent(ghgrp_transmission_compressor_emissions)
  ghgrp_combustion_emissions <- make_consistent(ghgrp_combustion_emissions)
  
  #rename so the columns are different
  colnames(ghgrp_transmission_compressor_emissions) <- gsub("ghg_quantity","W_emissions",colnames(ghgrp_transmission_compressor_emissions))
  colnames(ghgrp_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(ghgrp_combustion_emissions))
  
  #combine both into 1 dataframe - using NG_system emissions as the base to get
  #ID/year matches from
  ghgrp_transmission_compressor_emissions=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id","year","facility_name","ghg_name"), all.x = TRUE)},
                                                 list(ghgrp_transmission_compressor_emissions,
                                                      ghgrp_combustion_emissions))
  
  #convert the relevant columns to numeric class
  ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")] <- apply(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],
                                                                                    2,FUN = function(x){as.numeric(x)})
  ghgrp_transmission_compressor_emissions$ghg_quantity <- rowSums(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],na.rm=T)
  
  #for those facilities that are involved in processing, the combustion emissions
  #are not considered part of the transmission/compression total, so remove it
  #here (very small number of facilities)
  processing_facilities <- ghgrp_transmission_compressor_emissions$facility_id %in% processing_CH4[,1]
  ghgrp_transmission_compressor_emissions$ghg_quantity[processing_facilities] <- ghgrp_transmission_compressor_emissions$W_emissions[processing_facilities]
  
  #now filter out those without any emissions
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$ghg_quantity>0,]
  
  rm(processing_facilities,processing_CH4,ghgrp_combustion_emissions,make_consistent)
  ################################################################################
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.GHGI.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_name_list,"/JSON")
  
  #initialize output
  ghgrp_facility_info <- data.frame()
  for(A in 1:length(state_name_list)){
    # download data and read/combine in an R dataframe
    ghgrp_facility_info <- rbind(ghgrp_facility_info,fromJSON(data_URLs[A]))
  }
  
  #combine the datasets by ID, and year
  ghgrp <- merge(ghgrp_facility_info,ghgrp_transmission_compressor_emissions,
                 by=c("facility_id","year"), all=F)
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  
  #delete all tempfiles and clean up working environment
  rm(A,ghgrp_facility_info,ghgrp_transmission_compressor_emissions)
  
  
  compressors_ghgrp <- ghgrp
  coordinates(compressors_ghgrp) <- ~longitude + latitude
  proj4string(compressors_ghgrp) <- crs(domain) # WGS84
  compressors_ghgrp_crop <- crop(compressors_ghgrp, domain)
  
  ################################################################################
  #Now add the emissions data from the ones in GHGRP.  First scale them so that
  #the domain-averaged GHGRP emissions match the GHGI average emissions.
  
  match_numbers <- apply(compressors_ghgrp_crop@coords,MARGIN=1,FUN=function(x){
    which.min(apply(compressors_final@coords,MARGIN=1,FUN=function(y){
      haversine(x,y)}))})
  Distances <- vector(length=length(match_numbers))
  for(A in 1:length(match_numbers)){
    Distances[A] <- haversine(compressors_ghgrp_crop@coords[A,],compressors_final@coords[match_numbers,][A,])
  }
  #compressor names differ between these 2 datasets, so just calculate which EIA
  #and GHGRP facilities are closest
  # if(length(match_numbers)>0){
  #   check <- cbind(compressors_ghgrp_crop@data[,c("FACILITY NAME","CITY NAME","STATE")],
  #                  compressors_final@data[match_numbers,c("NAME","CITY","STATE")],
  #                  Distances)
  #   colnames(check) <- c("GHGRP_name","GHGRP_city","GHGRP_state","EIA_name","EIA_city","EIA_state","Distance_km")
  #   #subset to variables that should make it clear if the facilities are or are
  #   #not the same, rename for clarity
  #   View(check)
  #   stop("Line 156 - Check the subset facilities (GHGRP vs EIA).  These are matched based on the nearest facility for each GHGRP facility, but names, distances, and details should be checked for reasonable agreement.  If they seem to be the same facilities, continue.  If not, make sure that they're not in the EIA files at all, then add the GHGRP facilities to EIA_compressor_update at the start to add them as a new facility in the EIA dataset, rerun, and continue.")
  #   View(cbind(compressors_final@coords,compressors_final@data[,3:38]))
  # }
  #Now check if the names seem similar (for Philly, all seem to have a perfect
  #match)
  
  GHGRP_scaling <- compressor_avg_emissions/mean(compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60))
  
  compressors_final$emiss[match_numbers] <- compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)*GHGRP_scaling
  #if you have exact matches based on location for each and are confident based on
  #names, etc that they are the same facilities, simply replace their emissions
  #accordingly
  
  compressor_rast <- rasterize(compressors_final, domain, "emiss", fun=sum) # in mol/s
  compressor_flux <- compressor_rast*1e9/raster(cellSize(rast(compressor_rast),unit="m"))  # Calculate flux in nmol/m2/s
  compressor_flux[is.na(compressor_flux)]<-0
}

#new
{
  ################################################################################
  #checked and all input data matches the old equivalent

  pipes_EIA=vect("https://services7.arcgis.com/FGr1D95XCGALKXqM/arcgis/rest/services/NaturalGas_InterIntrastate_Pipelines_US_EIA/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  compressors_EIA <- read.csv(EIA_compressor_file)
  coordinates(compressors_EIA) <- ~LONGITUDE + LATITUDE
  proj4string(compressors_EIA) <- CRS(SRS_string="EPSG:4326")  # WGS84
  compressors_crop_EIA <- crop(compressors_EIA, domain)
  compressors_HIFLD <- vect(compressors_crop_EIA)
  # compressors_HIFLD=vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Compressor_Stations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  ################################################################################
  #Download the relevant GHGRP emissions data using the API
  #(https://www.GHGI.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant naturalgas-sector data
  #(https://www.GHGI.gov/enviro/greenhouse-gas-model).  Must download the relevant
  #data for each possible sector sGHGIrately as emissions are split by sector. The
  #total is combustion + NG systems, dominated by NG systems.  
  ghgrp_transmission_compressor_emissions <- fromJSON("https://data.epa.gov/efservice/ef_w_emissions_source_ghg/json")
  ghgrp_combustion_emissions <- fromJSON("https://data.epa.gov/efservice/C_SUBPART_LEVEL_INFORMATION/json")
  
  #because we're getting sub-facility level information for transmission
  #compressor, first need to aggregate.  Subsetting to only the year of interest
  #now instead of later.
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$reporting_year==inventory_year,]
  processing_CH4 <- aggregate(ghgrp_transmission_compressor_emissions$total_reported_ch4_emissions,
                              by=list(ghgrp_transmission_compressor_emissions$facility_id,
                                      ghgrp_transmission_compressor_emissions$reporting_year,
                                      ghgrp_transmission_compressor_emissions$facility_name,
                                      ghgrp_transmission_compressor_emissions$industry_segment),
                              sum,na.rm=T)
  processing_CH4 <- processing_CH4[,c(1:3,5,4)]
  
  #then split into transmission/compression and gas processing (some are both)
  ghgrp_transmission_compressor_emissions <- processing_CH4[processing_CH4[,5]=="Onshore natural gas transmission compression [98.230(a)(4)]",]
  processing_CH4 <- processing_CH4[processing_CH4[,5]=="Onshore natural gas processing [98.230(a)(3)]",]
  
  #reorganize slightly to match combustion.  Below function won't work right as
  #it's a competely different table
  colnames(ghgrp_transmission_compressor_emissions) <- colnames(ghgrp_combustion_emissions)
  ghgrp_transmission_compressor_emissions$ghg_gas_name <- "methane"
  
  #simple function to make sure gas names are limited to methane, and column names
  #are consistent
  make_consistent <- function(input){
    colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
    colnames(input) <- gsub("reporting_year","year",colnames(input))
    input$ghg_name <- tolower(input$ghg_name)
    input$facility_name <- tolower(input$facility_name)
    input <- input[input$ghg_name=="methane",]
    return(input)
  }
  
  ghgrp_transmission_compressor_emissions <- make_consistent(ghgrp_transmission_compressor_emissions)
  ghgrp_combustion_emissions <- make_consistent(ghgrp_combustion_emissions)
  
  #rename so the columns are different
  colnames(ghgrp_transmission_compressor_emissions) <- gsub("ghg_quantity","W_emissions",colnames(ghgrp_transmission_compressor_emissions))
  colnames(ghgrp_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(ghgrp_combustion_emissions))
  
  #combine both into 1 dataframe - using NG_system emissions as the base to get
  #ID/year matches from
  ghgrp_transmission_compressor_emissions=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id","year","facility_name","ghg_name"), all.x = TRUE)},
                                                 list(ghgrp_transmission_compressor_emissions,
                                                      ghgrp_combustion_emissions))
  
  #convert the relevant columns to numeric class
  ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")] <- apply(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],
                                                                                    2,FUN = function(x){as.numeric(x)})
  ghgrp_transmission_compressor_emissions$ghg_quantity <- rowSums(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],na.rm=T)
  
  #for those facilities that are involved in processing, the combustion emissions
  #are not considered part of the transmission/compression total, so remove it
  #here (very small number of facilities)
  processing_facilities <- ghgrp_transmission_compressor_emissions$facility_id %in% processing_CH4[,1]
  ghgrp_transmission_compressor_emissions$ghg_quantity[processing_facilities] <- ghgrp_transmission_compressor_emissions$W_emissions[processing_facilities]
  
  #now filter out those without any emissions
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$ghg_quantity>0,]
  
  rm(processing_facilities,processing_CH4,ghgrp_combustion_emissions,make_consistent)
  ################################################################################
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.GHGI.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_name_list,"/JSON")
  
  #initialize output
  ghgrp_facility_info <- data.frame()
  for(A in 1:length(state_name_list)){
    # download data and read/combine in an R dataframe
    ghgrp_facility_info <- rbind(ghgrp_facility_info,fromJSON(data_URLs[A]))
  }
  
  #combine the datasets by ID, and year
  ghgrp <- merge(ghgrp_facility_info,ghgrp_transmission_compressor_emissions,
                 by=c("facility_id","year"), all=F)
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  
  #delete all tempfiles and clean up working environment
  rm(A,ghgrp_facility_info,ghgrp_transmission_compressor_emissions)
  ################################################################################
  #process the transmission pipeline data
  
  first_row <- which(read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p1 <- read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,skip=first_row,col_names = T)
  
  first_row <- which(read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p2 <- read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,skip=first_row,col_names = T)
  #p2 = emissions, p1 = activity data.  Columns = year, rows = various types of
  #sources.  First col is just to identify the first column of useable data
  
  Data_list <- c("Pipeline Leaks","M&R (Trans. Co. Interconnect)","M&R (Farm Taps + Direct Sales)",
                 "Pipeline venting")
  #all the sources we're looking for, written exactly as in the GHGI file
  
  GHGI_Pipeline <- data.frame("Type"=Data_list,
                              "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[which(GHGI_p2[,1]==x)[1],as.character(inventory_year)]})))*
                                1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                              "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p1[which(GHGI_p1[,1]==x)[1],as.character(inventory_year)]})))*
                                1609.344,#convert from miles to meters
                              row.names = NULL)
  #use sapply to find the row using data list, specify the column as the year and
  #grab the relevant EF and activity data into a dataframe.
  
  pipeline_EF <- sum(GHGI_Pipeline[,2])/GHGI_Pipeline[1,3] #mol/m/s
  #sum of emissions / miles of pipelines (activity data from leaks entry)
  
  
  Data_list <- c("Station Total Emissions","Dehydrator vents (Transmission)",
                 "Flaring (Transmission)","Engines (Transmission)",
                 "Turbines (Transmission)","Engines (Storage)",
                 "Turbines (Storage)","Generators (Engines)",
                 "Generators (Turbines)","Pneumatic Devices Transmission",
                 "Station Venting Transmission")
  #transmission station total + emissions during operations (vents, flaring,
  #leaks, exhaust, etc.)
  
  GHGI_transmission_compressors <- data.frame("Type"=Data_list,
                                              "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[which(GHGI_p2[,1]==x)[1],as.character(inventory_year)]})))*
                                                1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                              "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p1[which(GHGI_p1[,1]==x)[1],as.character(inventory_year)]}))),
                                              row.names = NULL)
  #use sapply to find the row using data list, specify the column as the year and
  #grab the relevant EF and activity data into a dataframe.
  
  Engine_transmission_fraction <- GHGI_transmission_compressors[4,2]/sum(GHGI_transmission_compressors[c(4,6),2])
  Turbine_transmission_fraction <- GHGI_transmission_compressors[5,2]/sum(GHGI_transmission_compressors[c(5,7),2])
  #calculate the ratio between transmission and storage emissions from engines and
  #turbines
  
  GHGI_transmission_compressors[8,2] <- Engine_transmission_fraction*GHGI_transmission_compressors[8,2]
  GHGI_transmission_compressors[9,2] <- Turbine_transmission_fraction*GHGI_transmission_compressors[9,2]
  #apply those ratios to the Generators for engines or turbines since they're not
  #separated into transmission and storage
  
  GHGI_transmission_compressors <- GHGI_transmission_compressors[c(1:5,8:11),]
  #remove the storage data
  compressor_avg_emissions <- sum(GHGI_transmission_compressors[,2])/GHGI_transmission_compressors[3,3] #mol/station/s
  #sum of emissions / N stations (activity data from flaring entry)
  
  rm(GHGI_transmission_compressors,GHGI_Pipeline,GHGI_p1,GHGI_p2,first_row,Data_list,
     Engine_transmission_fraction,Turbine_transmission_fraction)
  ################################################################################
  #process the transmission pipeline data
  
  # Crop to just larger than d03 - don't know if it's necessary to have this buffer but it can't hurt
  e <- ext(domain)*1.1
  pipes_crop_EIA <- crop(project(pipes_EIA,crs(domain)),e)
  
  pipes_by_cell_EIA=rasterizeGeom(pipes_crop_EIA,rast(domain),fun="length")
  pipes_rast_EIA <- pipes_by_cell_EIA*pipeline_EF   # Set values to the pipe length (in metres) in each cell, multiplied by the effective emission factor in mol/m/s
  pipes_flux_new <- pipes_rast_EIA*1e9/(cellSize(pipes_rast_EIA,unit="m"))  # Calculate flux, mol/s to nmol/m2/s
  pipes_flux_new[is.na(pipes_flux_new)]<-0
  
  ################################################################################
  # Now onto the transmission compressor stations
  compressors_crop_HIFLD <- crop(project(compressors_HIFLD,crs(domain)), domain)
  compressors_final <- compressors_crop_HIFLD
  #default for all are the national avg
  compressors_final$emiss <- compressor_avg_emissions
  
  compressors_ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(compressors_ghgrp) <- "epsg:4326"
  compressors_ghgrp_crop <- crop(project(compressors_ghgrp,crs(domain)),domain)
  ################################################################################
  #Now identify the best matching GHGRP facility to the HIFLD ones using location.
  #If > 1 km, flag an error.  Otherwise, overwrite avg national compressor
  #emissions with the GHGRP ones for the specific facility.
  
  location_matches=nearest(compressors_ghgrp_crop,compressors_crop_HIFLD)
  
  combined_data <- cbind(as.data.frame(compressors_ghgrp_crop),
                         as.data.frame(compressors_crop_HIFLD)[location_matches$to_id,],
                         round(location_matches$distance))
  
  combined_data <- combined_data[,c("facility_id","state","facility_name.x",
                                    "ghg_quantity","STATE","NAME",
                                    "round(location_matches$distance)")]
  colnames(combined_data) <- c("GHGRP_ID","GHGRP_state","GHGRP_name",
                               "GHGRP_emissions","HIFLD_state","HIFLD_name",
                               "distance_m")
  
  if(max(combined_data$distance)>1000){
    View(combined_data)
    plot(ext(domain))
    lines(State_Tigerlines)
    points(compressors_crop_HIFLD,cex=2)
    points(compressors_ghgrp_crop,col="red")
    add_legend("bottom",legend = c("HIFLD","GHGRP"),pt.cex = c(2,1),
               horiz=T,col=c("black","red"),pch=16,bty="n")
    stop("some GHGRP compressors didn't have a HIFLD compressor within 1 km")
  }
  
  #scale the GHGRP emissions so that the domain average is equal to the national
  #average
  GHGRP_scaling <- compressor_avg_emissions/mean(compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60))
  compressors_final$emiss[location_matches$to_id] <- compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)*GHGRP_scaling
  
  compressor_rast <- rasterize(compressors_final, rast(domain), "emiss", fun=sum) # in mol/s
  compressor_flux_new <- compressor_rast*1e9/(cellSize(compressor_rast,unit="m"))  # Calculate flux in nmol/m2/s
  compressor_flux_new[is.na(compressor_flux_new)]<-0
  
}



plot(pipes_flux - pipes_flux_new,main="old - new")
plot(pipes_flux_new)
global(pipes_flux,sum)
global(pipes_flux_new,sum)

plot(rast(compressor_flux) - compressor_flux_new,main="old - new")
plot(rast(compressor_flux))
plot(compressor_flux_new)




