################################################################################
#Manually defined variables

#the old one to compare against
GHGRP_file <- file.path("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/",
                        "US_GHGRP_WWTP_only_all_years.xls")

DMR_file=file.path(input_directory,'DMR_2022_from_8_10_2023.csv')
CWNS_file=file.path(input_directory,'CWNS_merged_data_2012_KH.xlsx')

GHGI_national_wastewater_septic <- 227 #kt CH4/yr
GHGI_national_wastewater_nonseptic <- 246 #kt CH4/yr
GHGI_septic_EF <- 10.7 #g/capita/day
Total_national_open_or_low_int_area <- 352032 #km2
Wastewater_State_info <- data.frame("State"=c("DE", "MD", "NJ", "NY", "PA"),
                                    "Population"=c(1018396,6164660,9261699,19677151,12972008),
                                    "Septic_Fraction"=c(0.257,0.181,0.116,0.159,0.245),
                                    "Method"=c("scaled","scaled","scaled","reported","scaled"))
Wastewater_State_info[,4] <- tolower(Wastewater_State_info[,4]) #just in case manually entered with caps
National_wastewater_info <- data.frame("Year"=c(1990,2021),
                                       "Septic_Fraction"=c(0.241,0.152))

state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
state_name_list <- sort(c("NJ","NY","PA","MD","DE"))
input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite"
output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/"

inventory_year=2019
domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long


starttime <- Sys.time()
cat("Starting wastewater sector: Wastewater\n")
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
#No need to compare approaches for WWTP.  Identical process, nearly line for
#line copy/paste, just switching sptransform with project.

cat("Finished calculating municipal treatment plant emissions at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
################################################################################
#Now septic systems.  First load in output from NLCD_fractions_by_state

#output from NLCD_fractions_by_state after reprojecting
Suburbia_rasterfile <- list.files(pattern=glob2rx("*NLCD_suburban.nc"),path=output_directory,full.names = T)

#output from NLCD_fractions_by_state.R
nlcd_state_total_areas <- read.table(file.path(output_directory,"nlcd_state_total_areas.csv"),header=T,sep=",")

#quickly ensure that the state data is all in the same order, alphabetical
Suburbia_rasterfile <- sort(Suburbia_rasterfile)
Wastewater_State_info <- Wastewater_State_info[order(Wastewater_State_info$State),]
nlcd_state_total_areas <- nlcd_state_total_areas[order(nlcd_state_total_areas$X),]

################################################################################
#Calculate septic emissions using emission factors and the land cover data
#from NLCD_fractions_by_state.R

septic_EPA_emiss <- GHGI_national_wastewater_septic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
tot_nlcd_area <- Total_national_open_or_low_int_area  # all states in km2

# blank output to combine the states
septic_flux <- rast(Suburbia_rasterfile[1])
septic_flux2 <- rast(Suburbia_rasterfile[1])
values(septic_flux2) <- 0
values(septic_flux) <- 0

domain_nlcd_frac <- raster(septic_flux)
septic_emiss2 <- raster(septic_flux2)
septic_emiss2_update <- raster(septic_flux2)
septic_emiss2_update <- raster(septic_flux2)

septic_flux_bystate <- vector()
septic_flux2_bystate <- vector()

for(A in 1:length(Suburbia_rasterfile)){
  #from NLCD_fraction_by_state.  The fractional coverage of NLCD open or low
  #intensity urban land cover per pixel.
  Suburbia <- rast(Suburbia_rasterfile[A])
  
  #method 1 old
  {
    domain_nlcd_frac <- sum(raster(Suburbia),domain_nlcd_frac,na.rm=T)
  }
  #method 1 new
  {
    #Calculate state-by-state totals by equally distributing GHGI totals
    #to developed, open and developed low intensity land cover nationally.
    
    state_flux <- septic_EPA_emiss*Suburbia/tot_nlcd_area  # in mol/s/km2
    
    #save within-domain state emissions for csv
    septic_flux_bystate <- c(septic_flux_bystate,
                             as.numeric(global(state_flux*cellSize(state_flux,unit="km"),sum,na.rm=T)))
    
    # Combine across states
    septic_flux <- mosaic(septic_flux,state_flux,fun=sum)
  }
  
  #Method 2 old
  {
    # Calculate state-by-state totals and disaggregate within each state
    Tot_area <- nlcd_state_total_areas[which(Wastewater_State_info[A,1]==nlcd_state_total_areas[,1]),2] # total area of both classes in km2 from nlcd_state_total_areas.csv
    pop <- Wastewater_State_info[A,2]
    if(Wastewater_State_info[A,4]=="scaled"){
      septic_frac <- Wastewater_State_info[A,3]*National_wastewater_info[2,2]/National_wastewater_info[1,2]
    }else if(Wastewater_State_info[A,4]=="reported"){
      septic_frac <- Wastewater_State_info[A,3]
    }else{
      stop("State info's method needs to be \"scaled\" or \"reported\" ")
    }
    state_tot_emiss <- pop*septic_frac*GHGI_septic_EF/(16.043*24*60*60)  #in mol/s (EF is in g/capita/day)
    state_emiss <- state_tot_emiss*raster(Suburbia)*raster(cellSize(Suburbia,unit="km"))/Tot_area #gridded and distributed equally in mol/s
    state_emiss_update <- state_tot_emiss*raster(Suburbia)/Tot_area #gridded and distributed equally in mol/s
    
    septic_emiss2 <- sum(septic_emiss2,state_emiss,na.rm=T)
    septic_emiss2_update <- sum(septic_emiss2_update,state_emiss_update,na.rm=T)
    #add this state's emissions in
    
    Wastewater_State_info$total_emissions_mol_per_s[A] <- state_tot_emiss
  }
  
  #Method 2 new
  {
    #identical code
    # # Calculate state-by-state totals using state-specific septic fraction data
    # Tot_area <- nlcd_state_total_areas[A,2] # total area of both classes in km2 from nlcd_state_total_areas.csv
    # pop <- Wastewater_State_info[A,2]
    # 
    # if(Wastewater_State_info[A,4]=="scaled"){
    #   septic_frac <- Wastewater_State_info[A,3]*National_wastewater_info[2,2]/National_wastewater_info[1,2]
    # }else if(Wastewater_State_info[A,4]=="reported"){
    #   septic_frac <- Wastewater_State_info[A,3]
    # }
    # state_tot_emiss <- pop*septic_frac*GHGI_septic_EF/(16.043*24*60*60)  #in mol/s (EF is in g/capita/day)
    state_flux <- (state_tot_emiss*Suburbia/Tot_area) #gridded and distributed equally in mol/s/km2
    
    #save within-domain state emissions for csv
    septic_flux2_bystate <- c(septic_flux2_bystate,
                              as.numeric(global(state_flux*cellSize(state_flux,unit="km"),sum,na.rm=T)))
    
    # Combine across states
    septic_flux2 <- mosaic(septic_flux2,state_flux,fun=sum)
  }
  cat("Finished processing septic for",Wastewater_State_info[A,1],"at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
}

#Method 1 old
{
  # Now multiply by total EPA emissions
  septic_emiss <- septic_EPA_emiss*domain_nlcd_frac*raster(cellSize(rast(domain_nlcd_frac),unit="km"))/tot_nlcd_area  # in mol/s
  septic_flux_old <- septic_emiss*1e9/raster(cellSize(rast(septic_emiss),unit="km")*1e6)  # Calculate flux in nmol/m2/s
  septic_flux_old[is.na(septic_flux_old)]<-0
  
  septic_emiss_update <- septic_EPA_emiss*domain_nlcd_frac/tot_nlcd_area
  septic_flux_old_update <- septic_emiss_update*1e9/1e6  # Calculate flux in nmol/m2/s
  septic_flux_old_update[is.na(septic_flux_old_update)]<-0
}

#Method 1 new
{
  # Now multiply by total EPA emissions and convert to flux (per area)
  septic_flux <- septic_flux*1e9*1E-6  # Calculate flux in nmol/m2/s
  septic_flux[is.na(septic_flux)]<-0
}





divergent <- colorRampPalette(c("red","white","blue"))

plot(septic_flux)
plot(rast(septic_flux_old))

delta <- rast(septic_flux_old) - septic_flux
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(septic_flux,sum);global(rast(septic_flux_old),sum)
global(rast(septic_flux_old),sum)/global(septic_flux,sum)

test=septic_flux*cellSize(septic_flux,unit="km")/cellSize(septic_flux,unit="km")
delta <- septic_flux - test
plot(delta,main="new - new * 1",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))

#these 2 differ, but only noise (1E-16).  The fact that test is slighlty
#different than new shows that even though multiplying by 1, rounding error is
#added in.  That seems to be all that's happening - I learned that in 2 separate
#steps we multiplied, then divided by area and now don't.  Updated the old
#approach to skip this process results in ~identical rasters (delta <2E-19 now)
delta <- rast(septic_emiss_update) - septic_flux
plot(delta,main="updated old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))


#Method 2 old
{
  # Now converting the totals to a per/area gridded product
  septic_flux2_old <- septic_emiss2*1e9/raster(cellSize(rast(septic_emiss2),unit="km")*1e6)  # Calculate flux in nmol/m2/s
  septic_flux2_old[is.na(septic_flux2_old)]<-0
  
  septic_flux2_old_update <- septic_emiss2_update*1e9/1e6  # Calculate flux in nmol/m2/s
  septic_flux2_old_update[is.na(septic_flux2_old_update)]<-0
  
}

#Method 2 new
{
  # Now converting the totals to flux (per area)
  septic_flux2 <- septic_flux2*1e9*1E-6  # Calculate flux in nmol/m2/s
  septic_flux2[is.na(septic_flux2)]<-0
}


plot(septic_flux2)
plot(rast(septic_flux2_old_update))

delta <- rast(septic_flux2_old) - septic_flux2
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(septic_flux2,sum);global(rast(septic_flux2_old),sum)
global(rast(septic_flux2_old),sum)/global(septic_flux2,sum)

#Broadly same story, though in this case the update has little effect...  It
#seems to have only removed pixels where new was higher... Either way negligible
#delta.
delta <- rast(septic_flux2_old_update) - septic_flux2
plot(delta,main="updated old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(septic_flux2,sum);global(rast(septic_flux2_old_update),sum)
global(rast(septic_flux2_old_update),sum)/global(septic_flux2,sum)

cat("Finished calculating septic emissions at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
################################################################################
#finally, industrial wastewater

#old
{
  ghgrp <- read_xls(GHGRP_file,sheet=as.character(inventory_year),col_names = T,skip = 5)
  
  #read the appropriate year.  If it's just the one year, the error will flag and
  #it will load that file without issue.
  
  coordinates(ghgrp) <- ~LONGITUDE + LATITUDE
  proj4string(ghgrp) <- CRS(SRS_string="EPSG:4326")  # WGS84
  ghgrp_crop_old <- crop(ghgrp, domain)
  ghgrp_crop_old$emiss <- ghgrp_crop_old$`GHG QUANTITY (METRIC TONS CO2e)`*1e6/(25*16.043*365*24*60*60)   # MT CO2e/yr to mol/s of CH4
  
  # Now rasterise
  ghgrp_rast <- rasterize(ghgrp_crop_old, domain, "emiss", fun=sum)
  ghgrp_flux_old <- ghgrp_rast*1e9/(raster(cellSize(rast(ghgrp_rast),unit="km"))*1e6)  # Calculate flux in nmol/m2/s
  ghgrp_flux_old[is.na(ghgrp_flux_old)]<-0
}

#new
{
  #Download the relevant emissions data using the API
  #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant industrial wastewater - sector data
  #(https://www.epa.gov/enviro/greenhouse-gas-model).  
  ghgrp_data <- fromJSON("https://data.epa.gov/efservice/II_SUBPART_LEVEL_INFORMATION/JSON")
  colnames(ghgrp_data) <- c("facility_id","year","facility_name","ghg_quantity","ghg_name")
  ################################################################################
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.epa.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_name_list,"/JSON")
  
  #initialize output
  ghgrp_facility_info <- data.frame()
  for(A in 1:length(state_name_list)){
    # download data and read/combine in an R dataframe
    ghgrp_facility_info <- rbind(ghgrp_facility_info,fromJSON(data_URLs[A]))
  }
  
  #combine the datasets by ID, and year
  ghgrp_all_data <- merge(ghgrp_facility_info,ghgrp_data,
                          by=c("facility_id","year"), all=F)
  
  #keep only data for the year of interest
  ghgrp <- ghgrp_all_data[ghgrp_all_data$year==inventory_year,]
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  ################################################################################
  # Now rasterize and save the data
  domain <- rast(domain)
  
  ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(ghgrp) <- "epsg:4326"
  ghgrp_crop <- project(ghgrp,domain)
  ghgrp_crop <- crop(ghgrp_crop,domain)
  ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60) #MT CH4/yr to mol/s
  
  # Now rasterise
  ghgrp_rast <- rasterize(ghgrp_crop, domain, "emiss", fun=sum)
  ghgrp_flux <- ghgrp_rast*1e9/(cellSize(ghgrp_rast,unit="m"))  # Calculate flux in nmol/m2/s
  ghgrp_flux[is.na(ghgrp_flux)]<-0
}



cat("old=",ghgrp_crop_old$emiss)
cat("new=",ghgrp_crop$emiss)

ghgrp_crop$emiss <- ghgrp_crop$emiss*(ghgrp_crop_old$emiss/ghgrp_crop$emiss)
ghgrp_rast <- rasterize(ghgrp_crop, domain, "emiss", fun=sum)
ghgrp_flux <- ghgrp_rast*1e9/(cellSize(ghgrp_rast,unit="m"))  # Calculate flux in nmol/m2/s
ghgrp_flux[is.na(ghgrp_flux)]<-0


delta <- rast(ghgrp_flux_old) - ghgrp_flux

#identical

