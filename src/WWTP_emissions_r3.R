## WWTP_emissions_r2.R
## In use: 2021-11-02 20:00
## Finalized: 2023-02-03
#
# Note - to convert to fluxes this code uses the raster packages area function.
# This simplifies the area calculation of a lat/long box and is not appropriate
# near the poles
#
#
# The GEPA does something slightly weird with these emissions - see WWTP_explainer.R for details
#
# The EPA national report essentially has 3 categories of WW emissions (2019 emissions from 2021 report in brackets):
# domestic septic systems (232 kt), domestic centralised systems (250 kt), and industrial emissions (254 kt)
#
# The CWNS has the locations of publicly owned WWTPs and other facilities - here's a quote from the proposal abstract:
# "The respondents who provide this information to EPA are state agencies responsible for environmental pollution control
# and local facility contacts who provide documentation to the states."
#
# So the domestic centralised emissions from the EPA national inventory report can be allocated using the CWNS using the reported
# existing municipal flow ("EXIST_MUNICIPAL") from the 2012 submission
#
# Some septic system population counts are present in the CWNS, but it isn't clear to me if this represents all the septic
# systems or just ones that are managed by local facilities. If I want to use this, I'll also have to work out how to
# spatially allocate these emissions based on the reporting local facility
# For now, distribute septic emissions according to the NLCD land cover classes "Developed-Open Space" and "Developed-Low Intensity"
# Method 1:
# We know the combined national total (in 2016) for these classes was 352032 km2, from this paper:
# https://doi.org/10.1016/j.isprsjprs.2020.02.019
# I made a version of the NLCD for our domain that had 1 for these classes and 0 for all other classes
# Then regridded this using xesmf to get the fraction of each grid cell that was one of these classes
# Load that in here, and calculate the area of each grid cell as a fraction of the national total
# Method 2:
# Do the same as Method 1, but using estimated  state-total emissions instead of national emissions
# These are calculated by multiplying an estimate for the number of people whose
# waste is treated in onsite systems within each state by the emission factor (10.7 gCH4/capita/day) from the EPA
# GHGI.  To estimate the number of people served by onsite systems, multiply the US census state
# population estimate for 2019 by an estimate of the fraction of people served by onsite systems.
# For some states, this septic fraction estimate can be taken from the 2021 American Housing Survey.
# Such recent data is not available for most states.  In those cases we take the
# septic fraction reported in the 1990 US census (the last to provide this data at the individual state level).
# To correct for recent changes in septic fraction, we multiply these state-level values from 1990 by the ratio of
# whole-US septic fraction in 2019 (16.3%; from the American Housing Survey) to whole-US septic fraction in 1990 (24.1%). 
#
# The EPA industrial WW emissions come mainly from meat & poultry
# These industries have their on-site treatment systems, and so are not included in the CWNS
# Some of these emissions are reported to GHGRP - we can use those here
# Emissions from non-reporters in this category are not currently included in this inventory
#
# Note that actually some of the emissions in the EPA inventory (15% domestic, 7% industrial) come from effluent, not treatment
# These may not be located entirely at the WWTPs, but it's really hard to know where to put them otherwise
#

Wastewater <- function(){
  ################################################################################
  #Manually defined variables
  
  DMR_file <- file.path(input_directory,'DMR_2022_from_8_10_2023.csv')
  # DMR_file <- file.path(Input_directory,'DMR_2012_from_8_10_2023.csv')
  #Discharge Monitoring Report (DMR) from
  #(https://echo.epa.gov/trends/loading-tool/water-pollution-search) for all
  #facilities in the US.
  
  CWNS_file <- file.path(input_directory,'CWNS_merged_data_2012_KH.xlsx')
  # ACCESS database from (https://www.epa.gov/cwns) that converted to xlsx
  
  low_int_rasterfile <- list.files(pattern=glob2rx("*low_int_regridded.nc"),path=output_directory,full.names = T)
  open_rasterfile <- list.files(pattern=glob2rx("*open_regridded.nc"),path=output_directory,full.names = T)
  #output from NLCD_fractions_by_state after reprojecting
  
  ################################################################################
  #quickly ensure that the state data is all in the same order, alphabetical
  
  nlcd_state_total_areas <- read.table(file.path(output_directory,"nlcd_state_total_areas.csv"),header=T,sep=",")
  #output from NLCD_fractions_by_state.R
  
  low_int_rasterfile <- sort(low_int_rasterfile)
  open_rasterfile <- sort(open_rasterfile)
  Wastewater_State_info <- Wastewater_State_info[order(Wastewater_State_info$State),]
  nlcd_state_total_areas <- nlcd_state_total_areas[order(nlcd_state_total_areas$X),]
  
  ################################################################################
  # First load in and prep the flow data
  
  if(Wastewater_Municipal_file == "CWNS"){
    cwns_2012 <- read_xlsx(CWNS_file)
    
    #ID any that are in the western or southern hemisphere (- coordinates)
    Western_hemis <- grep("W",cwns_2012$LONGITUDE)
    Southern_hemis <- grep("S",cwns_2012$LATITUDE)
    #remove the hemisphere text so we can make numeric
    cwns_2012$LATITUDE <- gsub("N|S","",cwns_2012$LATITUDE)
    cwns_2012$LONGITUDE <- gsub("W|E","",cwns_2012$LONGITUDE)
    cwns_2012$LATITUDE <- as.numeric(cwns_2012$LATITUDE)
    cwns_2012$LONGITUDE <- as.numeric(cwns_2012$LONGITUDE)
    #make those in the S or W hemispheres the appropriate negative coordinates
    cwns_2012$LATITUDE[Southern_hemis] <- cwns_2012$LATITUDE[Southern_hemis]*-1
    cwns_2012$LONGITUDE[Western_hemis] <- cwns_2012$LONGITUDE[Western_hemis]*-1
    
    #Pick only those entries that have lat and lon coordinates
    cwns_2012_filt <- subset(cwns_2012,!is.na(LATITUDE) & !is.na(LONGITUDE))
    
    # Nearly all the entries are NAD83, but some aren't
    # Convert everything over to WGS84
    # Assume blank or unknown entries are NAD83
    cwns_2012_wgs84 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM=="World Geodetic System of 1984")
    cwns_2012_nad27 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM=="North American Datum of 1927")
    cwns_2012_nad83 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM!="North American Datum of 1927" & HORIZONTAL_COORDINATE_DATUM!="World Geodetic System of 1984")
    
    cwns_2012_wgs84 <- vect(cwns_2012_wgs84,geom=c("LONGITUDE","LATITUDE"))
    cwns_2012_nad27 <- vect(cwns_2012_nad27,geom=c("LONGITUDE","LATITUDE"))
    cwns_2012_nad83 <- vect(cwns_2012_nad83,geom=c("LONGITUDE","LATITUDE"))
    
    crs(cwns_2012_wgs84) <- "EPSG:4326"  # WGS84
    crs(cwns_2012_nad27) <- "EPSG:4267"  # NAD27
    crs(cwns_2012_nad83) <- "EPSG:4269"  # NAD83
    
    cwns_2012_nad27_trans <- project(cwns_2012_nad27,crs(cwns_2012_wgs84))
    cwns_2012_nad83_trans <- project(cwns_2012_nad83,crs(cwns_2012_wgs84))
    
    Municipal_flow <- rbind(cwns_2012_wgs84,cwns_2012_nad27_trans,cwns_2012_nad83_trans)
    
    tot_flow <- sum(Municipal_flow$EXIST_MUNICIPAL, na.rm=T)
    
  }else if(Wastewater_Municipal_file=="DMR"){
    DMR_data <- read.csv(DMR_file,skip=3)
    colnames(DMR_data) <- gsub("\\.","\\_",colnames(DMR_data))
    Municipal_flow <- subset(DMR_data,!is.na(Facility_Latitude) & !is.na(Facility_Longitude))
    Municipal_flow <- vect(Municipal_flow,geom=c("Facility_Longitude","Facility_Latitude"))
    crs(Municipal_flow) <- "EPSG:4326"
    tot_flow <- sum(DMR_data$Average_Flow__MGD_, na.rm=T)
  }
  
  ################################################################################
  # distribute EPA emissions across the CWNS facilities
  
  if(Wastewater_Municipal_method=="GHGI"){
    # Take total emissions for each category from the 2021 EPA report (values for 2019 in kt)
    central_EPA_emiss <- GHGI_national_wastewater_nonseptic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
    
    if(Wastewater_Municipal_file=="CWNS"){
      Municipal_flow$emiss <- central_EPA_emiss*Municipal_flow$EXIST_MUNICIPAL/tot_flow   # in mol/s
    }else if(Wastewater_Municipal_file=="DMR"){
      Municipal_flow$emiss <- central_EPA_emiss*Municipal_flow$Average_Flow__MGD_/tot_flow   # in mol/s
    }
  }else if(Wastewater_Municipal_method=="Moore_EF"){
    if(Wastewater_Municipal_file=="CWNS"){
      stop("CWNS data does not include the BOD, so the EF approach isn't an option")
    }
    #small medium and large means (on a lognormal distribution).  Need to
    #understand the BOD5 data before I can use these EFs
    exp(-2.6+(1.3^2)/2)
    exp(-4.1+(1.2^2)/2)
    exp(-3.4+(1^2)/2)
    
  }else if(Wastewater_Municipal_method=="Moore_linear"){
    #convert from million gallons/day to m3/s
    if(Wastewater_Municipal_file=="CWNS"){
      Municipal_flow$EXIST_MUNICIPAL <- Municipal_flow$EXIST_MUNICIPAL*3785.41178/(24*60*60)  
      #Apply the log-log linear relationship from Figure 2A of Moore et al.
      Municipal_flow$emiss <- 1.2*log10(Municipal_flow$EXIST_MUNICIPAL)+1
    }else if(Wastewater_Municipal_file=="DMR"){
      Municipal_flow$Average_Flow__MGD_ <- Municipal_flow$Average_Flow__MGD_*3785.41178/(24*60*60)
      Municipal_flow$emiss <- 1.2*log10(Municipal_flow$Average_Flow__MGD_)+1
    }
    #convert from log10(g/s) to mol/s
    Municipal_flow$emiss <- (10^(Municipal_flow$emiss))/(12.011+1.008*4)
  }
  
  # Rasterise
  Municipal_flow_crop <- crop(Municipal_flow,domain)
  Municipal_flow_crop_filt <- subset(Municipal_flow_crop,!is.na(Municipal_flow_crop$emiss))
  central_rast <- rasterize(Municipal_flow_crop_filt, domain, "emiss", fun=sum)
  
  central_flux <- central_rast*1e9/(cellSize(central_rast,unit="m"))  # Calculate flux in nmol/m2/s
  central_flux[is.na(central_flux)]<-0
  
  # Save point sources as csv files - first just the raw dataframe
  write.csv(Municipal_flow_crop_filt, file.path(output_directory,"WWTP_municipal_all.csv"),row.names = F)
  
  # Now just the names, coordinates and emissions
  if(Wastewater_Municipal_file=="CWNS"){
    Municipal_flow_crop_output <- data.frame(Municipal_flow_crop_filt$FACILITY_NAME,
                                             geom(Municipal_flow_crop_filt)[,"x"],geom(Municipal_flow_crop_filt)[,"y"],
                                             Municipal_flow_crop_filt$emiss)
  }else if(Wastewater_Municipal_file=="DMR"){
    Municipal_flow_crop_output <- data.frame(Municipal_flow_crop_filt$Facility_Name,
                                             geom(Municipal_flow_crop_filt)[,"x"],geom(Municipal_flow_crop_filt)[,"y"],
                                             Municipal_flow_crop_filt$emiss)
  }
  colnames(Municipal_flow_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
  write.csv(Municipal_flow_crop_output,file.path(output_directory,'WWTP_municipal.csv'),row.names=FALSE)
  
  ################################################################################
  #Now septic systems.  Load reprojected output  - this is divided by state and
  #open vs low intensity land use.
  
  septic_EPA_emiss <- GHGI_national_wastewater_septic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
  tot_nlcd_area <- Total_national_open_or_low_int_area  # all states in km2
  
  # blank output to combine the states
  domain_nlcd_frac <- rast(open_rasterfile[1])
  septic_emiss2 <- rast(open_rasterfile[1])
  values(septic_emiss2) <- 0
  values(domain_nlcd_frac) <- 0
  
  for(A in 1:length(open_rasterfile)){
    open <- rast(open_rasterfile[A])
    low_int <- rast(low_int_rasterfile[A])
    
    # Method 1:
    # Calculate fractional area (of national total) in each grid cell
    nlcd_frac <- open + low_int
    domain_nlcd_frac <- mosaic(nlcd_frac,domain_nlcd_frac,fun=sum)
    
    # Method 2:
    # Calculate state-by-state totals and disaggregate within each state
    states_trans <- project(State_Tigerlines,crs(nlcd_frac))
    
    state_poly <- subset(states_trans, states_trans$STUSPS==Wastewater_State_info[A,1])
    Tot_area <- sum(nlcd_state_total_areas[which(Wastewater_State_info[A,1]==nlcd_state_total_areas[,1]),c(2,3)]) # total area of both classes in km2 from nlcd_state_total_areas.csv
    pop <- Wastewater_State_info[A,2]
    if(Wastewater_State_info[A,4]=="scaled"){
      septic_frac <- Wastewater_State_info[A,3]*National_wastewater_info[2,2]/National_wastewater_info[1,2]
    }else if(Wastewater_State_info[A,4]=="reported"){
      septic_frac <- Wastewater_State_info[A,3]
    }else{
      stop("State info's method needs to be \"scaled\" or \"reported\" ")
    }
    state_tot_emiss <- pop*septic_frac*GHGI_septic_EF/(16.043*24*60*60)  #in mol/s (EF is in g/capita/day)
    state_emiss <- state_tot_emiss*nlcd_frac*cellSize(nlcd_frac,unit="km")/Tot_area #gridded and distributed equally in mol/s
    
    septic_emiss2 <- mosaic(septic_emiss2,state_emiss,fun=sum)
    #add this state's emissions in
    
    Wastewater_State_info$total_emissions_mol_per_s[A] <- state_tot_emiss
    cat("Finished",Wastewater_State_info[A,1],"\n")
  }
  
  #calculate some info to compare the 2 methods.  The actual calculation is the
  #same, it's just the emissions per area that changes.
  Wastewater_State_info$total_septic_area_km2 <- rowSums(nlcd_state_total_areas[,c(2,3)])
  Wastewater_State_info$emission_per_area <- Wastewater_State_info$total_emissions/Wastewater_State_info$total_septic_area
  Wastewater_State_info$State_to_national_method_ratio <- Wastewater_State_info$emission_per_area/(septic_EPA_emiss/tot_nlcd_area)
  
  # Method 1:
  # Now multiply by total EPA emissions
  septic_emiss <- septic_EPA_emiss*domain_nlcd_frac*cellSize(domain_nlcd_frac,unit="km")/tot_nlcd_area  # in mol/s
  septic_flux <- septic_emiss*1e9/(cellSize(septic_emiss,unit="km")*1e6)  # Calculate flux in nmol/m2/s
  septic_flux[is.na(septic_flux)]<-0
  
  # Method 2:
  # Now converting the totals to a per/area gridded product
  septic_flux2 <- septic_emiss2*1e9/(cellSize(septic_emiss2,unit="km")*1e6)  # Calculate flux in nmol/m2/s
  septic_flux2[is.na(septic_flux2)]<-0
  
  
  ################################################################################
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
  
  #delete all tempfiles and clean up working environment
  rm(A,ghgrp_all_data,ghgrp_facility_info,ghgrp_data,data_URLs)
  ################################################################################
  # Now rasterize and save the data
  
  ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(ghgrp) <- "epsg:4326"
  ghgrp_crop <- crop(ghgrp,domain)
  ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60) #MT CH4/yr to mol/s

  # Now rasterise
  ghgrp_rast <- rasterize(ghgrp_crop, domain, "emiss", fun=sum)
  ghgrp_flux <- ghgrp_rast*1e9/(cellSize(ghgrp_rast,unit="m"))  # Calculate flux in nmol/m2/s
  ghgrp_flux[is.na(ghgrp_flux)]<-0
  
  # Save point sources as csv files - first just the raw dataframe
  write.csv(ghgrp_crop, file.path(output_directory,"WWTP_industrial_all.csv"))
  
  # Now just the names, coordinates and emissions
  ghgrp_crop_output <- data.frame(ghgrp_crop$facility_name.x,
                                  geom(ghgrp_crop)[,"x"],geom(ghgrp_crop)[,"y"],
                                  ghgrp_crop$emiss)
  names(ghgrp_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
  write.csv(ghgrp_crop_output, file.path(output_directory,"WWTP_industrial.csv"),row.names = F)
  
  #now save the comparison across the methods
  write.csv(Wastewater_State_info, file.path(output_directory,"WWTP_septic_method_comparison.csv"),row.names = F)
  
  ################################################################################
  # Write the rasters
  writeCDF(central_flux,
           file.path(output_directory,'Wastewater_dom_central.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants',
           missval=-9999,
           overwrite=TRUE)
  
  writeCDF(septic_flux,
           file.path(output_directory,'Wastewater_dom_septic_national.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on calculations at the state level',
           missval=-9999,
           overwrite=TRUE)
  
  writeCDF(septic_flux2,
           file.path(output_directory,'Wastewater_dom_septic_bystate.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on EPA national values',
           missval=-9999,
           overwrite=TRUE)
  
  writeCDF(ghgrp_flux,
           file.path(output_directory,'Wastewater_ind.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from industrial wastewater treatment plants',
           missval=-9999,
           overwrite=TRUE)
  
  ################################################################################
  #Finally, load up some functions/datasets and plot up this output nicely
  
  if(verbose){
    not_log_plot(septic_flux2,filename="Wastewater_dom_septic_bystate",
                 "Domestic Wastewater - Septic v2\n estimated state septic distributed using \ndeveloped open space/low intensity land cover",
                 global(min(septic_flux,septic_flux2),min),
                 global(max(septic_flux,septic_flux2),max))
    
    not_log_plot(septic_flux,filename="Wastewater_dom_septic_national",
                 "Domestic Wastewater - Septic\n national EPA septic distributed using \ndeveloped open space/low intensity land cover",
                 global(min(septic_flux,septic_flux2),min),
                 global(max(septic_flux,septic_flux2),max))
    
    log_plot(central_flux,filename="Wastewater_dom_central",
             "Domestic Wastewater -\n EPA total distributed using \nClean Watersheds Needs Survey")
    
    log_plot(ghgrp_flux,filename="Wastewater_ind",
             "Industrial Wastewater -\n GHGRP Reporters")
    
    Summed_wastewater_treatment = central_flux+septic_flux+ghgrp_flux
    
    log_plot(Summed_wastewater_treatment,
             "Wastewater Treatment Sector\nGHGI total distributed with CWNS (Domestic facilities) and GHGRP (industrial)\nand developed open space/low intensity NLCD land cover (Septic)")
  }
}
