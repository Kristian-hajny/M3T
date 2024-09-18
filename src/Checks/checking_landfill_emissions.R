
################################################################################
#User input

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/"

ghgrp_file <- "US_GHGRP_Landfills_only_all_years.xls"
LMOP_file <- "lmopdata(Aug_22)_landfill_only.xlsx"
year <- "2019"

GHGI_value <- 3943 #Gg CH4/yr
#total national municipal landfill emissions from the GHGI

# 1 site stopped reporting without a valid reason, as ID'd from an error message
# in the code - instead using the most recent reported value for that facility
LMOP_update <- data.frame("Facility"=c("AL TURI LANDFILL & LFGTE FACILITY",
                                       "BEULAH SANITARY LANDFILL",
                                       "Kearny 1-D"),
                          "GHGRP.ID"=c(1004823,1000331,1011381),
                          "CH4.Data"=c(41284,9909.3,85308),
                          "latest.year"=c(2016,2016,2016),
                          "latitude"=c(41.40646,38.65194,40.74986),
                          "longitude"=c(-74.37313,-75.91445,-74.13357),
                          "STATE"=c("MD","NY","NJ"))

domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long

inventory_year=2019
state_name_list <- sort(c("NJ","NY","PA","MD","DE"))
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","terra","jsonlite","dplyr")
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
# First load all data and prep to compare the GHGRP download vs the API download

{
  ghgrp_old <- read_xls(file.path(Input_directory,ghgrp_file),sheet=year,col_names = T,skip = 5)
  #read the appropriate year.  
  
  #incorporate the manually ID'd ones that stopped reporting without a valid
  #reason (was just Kearney)
  LMOP_update <- LMOP_update[order(LMOP_update$GHGRP.ID),]
  colnames(LMOP_update) <- c("FACILITY NAME","GHGRP ID","GHG QUANTITY (METRIC TONS CO2e)","REPORTING YEAR","LATITUDE","LONGITUDE","STATE")
  
  #add Kearney into the GHGRP and GHGRP crop data
  ghgrp_old <- merge(ghgrp_old,LMOP_update,by=c("GHGRP ID","FACILITY NAME","GHG QUANTITY (METRIC TONS CO2e)","REPORTING YEAR","LATITUDE","LONGITUDE","STATE"),all=T)

  #convert to a spatial object, crop to d03, convert units
  coordinates(ghgrp_old) <- ~LONGITUDE + LATITUDE
  proj4string(ghgrp_old) <- CRS(SRS_string="EPSG:4326")  # WGS84
  ghgrp_crop_old <- crop(ghgrp_old, domain)
  ghgrp_crop_old$emiss <- ghgrp_crop_old$`GHG QUANTITY (METRIC TONS CO2e)`*1e6/(25*16.043*365*24*60*60)   # MT CO2e/yr to mol/s of CH4
  
  # Now calculate national totals
  ghgrp_old_national <- (sum(ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`)-sum(LMOP_update$`GHG QUANTITY (METRIC TONS CO2e)`))/25000   # MT CO2e/yr to Gg CH4/yr
  EPA_total_old <- GHGI_value 
  non_ghgrp_total_old <- EPA_total_old - ghgrp_old_national
  
  old_count <- nrow(ghgrp_old)-nrow(LMOP_update)
}

{
  ################################################################################
  #Download the relevant emissions data using the API
  #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant landfill-sector data
  #(https://www.epa.gov/enviro/greenhouse-gas-model).  Must download the relevant
  #data for each possible sector separately as emissions are split by sector
  #(i.e., gas capture for electricity is subpart D, flaring is C, and landfill
  #emissions HH - all of which can occur at the same landfill)
  ghgrp_landfill_only_emissions <- fromJSON("https://data.epa.gov/efservice/HH_SUBPART_LEVEL_INFORMATION/JSON")
  # ghgrp_landfill_emissions2 <- fromJSON("https://data.epa.gov/dmapservice/ghg.hh_subpart_level_information/json")
  ghgrp_combustion_emissions <- fromJSON("https://data.epa.gov/efservice/C_SUBPART_LEVEL_INFORMATION/json")
  # ghgrp_electricity_emissions <- fromJSON("https://data.epa.gov/efservice/D_SUBPART_LEVEL_INFORMATION/json")
  # ghgrp_industrial_landfill_emissions <- fromJSON("https://data.epa.gov/efservice/tt_subpart_ghg_info/json")
  
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
  
  ghgrp_landfill_only_emissions <- make_consistent(ghgrp_landfill_only_emissions)
  ghgrp_combustion_emissions <- make_consistent(ghgrp_combustion_emissions)
  # ghgrp_electricity_emissions <- make_consistent(ghgrp_electricity_emissions)
  # ghgrp_industrial_landfill_emissions <- make_consistent(ghgrp_industrial_landfill_emissions)
  
  #rename so the columns are different
  colnames(ghgrp_landfill_only_emissions) <- gsub("ghg_quantity","HH_emissions",colnames(ghgrp_landfill_only_emissions))
  colnames(ghgrp_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(ghgrp_combustion_emissions))
  # colnames(ghgrp_electricity_emissions) <- gsub("ghg_quantity","D_emissions",colnames(ghgrp_electricity_emissions))
  # colnames(ghgrp_industrial_landfill_emissions) <- gsub("ghg_quantity","TT_emissions",colnames(ghgrp_industrial_landfill_emissions))
  
  #combine all 4 into 1 dataframe - using landfill emissions as the base to get
  #ID/year matches from
  ghgrp_landfill_emissions=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id","year","facility_name","ghg_name"), all.x = TRUE)},
                                  list(ghgrp_landfill_only_emissions,
                                       ghgrp_combustion_emissions))#,
  # ghgrp_electricity_emissions,
  # ghgrp_industrial_landfill_emissions))
  
  #convert the relevant columns to numeric class
  ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")] <- apply(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")],
                                                                      2,FUN = function(x){as.numeric(x)})
  ghgrp_landfill_emissions$ghg_quantity <- rowSums(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")],na.rm=T)
  # ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","D_emissions","TT_emissions")] <- apply(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","D_emissions","TT_emissions")],
  #                                                                                                  2,FUN = function(x){as.numeric(x)})
  # ghgrp_landfill_emissions$ghg_quantity <- rowSums(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","D_emissions","TT_emissions")],na.rm=T)
  
  
  # subpart d is only 1 facility and is NOT included in GHGRP flight.  
  # subpart C is many and IS included in GHGRP flight
  # subpart TT is only 1 facility and is NOT included in GHGRP flight.
  
  #Calculate national total in the GHGRP for the year of interest
  ghgrp_national <- sum(as.numeric(ghgrp_landfill_emissions$ghg_quantity[ghgrp_landfill_emissions$year==inventory_year]))/1000   # MT CH4/yr to Gg CH4/yr
  new_count <- nrow(ghgrp_landfill_emissions[ghgrp_landfill_emissions$year==inventory_year,])
  
  rm(ghgrp_landfill_only_emissions,ghgrp_combustion_emissions,make_consistent)
  ################################################################################
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.epa.gov/enviro/envirofacts-data-service-api
  # data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_name_list,"/JSON")
  data_URLs <- "https://data.epa.gov/efservice/PUB_DIM_FACILITY/JSON"
  
  #initialize output
  ghgrp_facility_info <- fromJSON(data_URLs)
  # ghgrp_facility_info <- data.frame()
  # for(A in 1:length(state_name_list)){
  #   # download data and read/combine in an R dataframe
  #   ghgrp_facility_info <- rbind(ghgrp_facility_info,fromJSON(data_URLs[A]))
  # }
  
  #combine the datasets by ID, and year
  ghgrp_all_data <- merge(ghgrp_facility_info,ghgrp_landfill_emissions,
                          by=c("facility_id","year"), all=F)
  
  #keep only data for the year of interest
  ghgrp <- ghgrp_all_data[ghgrp_all_data$year==inventory_year,]
  
  #identify facilities that stopped reporting without a valid reason, then subset
  #to only landfill facilities
  nonreporting_facilities <- unique(ghgrp_facility_info$facility_id[ghgrp_facility_info$reporting_status=="STOPPED_REPORTING_UNKNOWN_REASON" & ghgrp_facility_info$year<=inventory_year])
  nonreporting_landfills <- nonreporting_facilities[which(nonreporting_facilities %in% unique(ghgrp_landfill_emissions$facility_id))]
  nonreporting_landfills <- nonreporting_landfills[!(nonreporting_landfills %in% unique(ghgrp$facility_id))]
  
  # find the closest data available for those that stopped reporting
  nonreporting_landfill_data <- ghgrp_all_data[ghgrp_all_data$facility_id %in% nonreporting_landfills,]
  nonreporting_landfill_data=tapply(nonreporting_landfill_data,
         INDEX=nonreporting_landfill_data$facility_id,
         FUN=function(x){x[which.min(abs(x$year-inventory_year)),]})
  nonreporting_landfill_data=do.call(rbind, nonreporting_landfill_data)
  
  #add this most recent data to the GHGRP dataset
  if(all(!nonreporting_landfill_data$facility_id %in% unique(ghgrp$facility_id))){
    ghgrp <- rbind(nonreporting_landfill_data,ghgrp)
  }else{
    stop("need to recode to handle something or some landfills will be duplicated")
  }
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  
  #delete all tempfiles and clean up working environment
  # rm(A,ghgrp_all_data,ghgrp_facility_info,
  #    nonreporting_landfill_data,nonreporting_facilities,nonreporting_landfills)
  rm(ghgrp_all_data,ghgrp_facility_info,
     nonreporting_landfill_data,nonreporting_facilities,nonreporting_landfills)
  ################################################################################
  #Now convert to spatial and load/convert LMOP.  Assign GHGI_national -
  #GHGRP_national to all LMOP facilities equally.
  
  #convert to a spatial object, crop to d03, convert units
  ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(ghgrp) <- "epsg:4326"
  ghgrp <- project(ghgrp,crs(domain))
  ghgrp_crop <- crop(ghgrp, domain)
  ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)   # MT CH4/yr to mol/s of CH4
  
  # Now calculate national totals
  EPA_total <- GHGI_value 
  non_ghgrp_total <- EPA_total - ghgrp_national
}

################################################################################
#Now compare the old and new input data

cat("GHGRP national total (Gg CH4/yr) differs by (new - old) =",ghgrp_national - ghgrp_old_national,"\nrelative to a total of",ghgrp_national,"\nor",(ghgrp_national - ghgrp_old_national)/ghgrp_national*100,"%")
cat("GHGRP via API =",new_count,"facilities \nold download =",old_count,"facilities")

ghgrp_old <- vect(ghgrp_old)
ghgrp <- ghgrp[order(ghgrp$facility_id),]
ghgrp_old <- ghgrp_old[order(ghgrp_old$`GHGRP ID`),]

plot(ghgrp)
points(ghgrp_old,col="red",pch=16)

plot(ghgrp_old)
points(ghgrp,col="red",pch=16)

# View(as.data.frame(ghgrp[which(!(ghgrp$facility_id %in% ghgrp_old$`GHGRP ID`)),]))
unadjusted_ghgrp <- ghgrp[which((ghgrp$facility_id %in% ghgrp_old$`GHGRP ID`)),]

plot(distance(unadjusted_ghgrp,ghgrp_old,pairwise=T))
offset <- (as.data.frame(cbind(unadjusted_ghgrp$facility_name.x,unadjusted_ghgrp$ghg_quantity,crds(unadjusted_ghgrp),
                               ghgrp_old$`FACILITY NAME`,ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25,crds(ghgrp_old),
                               distance(unadjusted_ghgrp,ghgrp_old,pairwise=T))[distance(unadjusted_ghgrp,ghgrp_old,pairwise=T)>500,]))
offset$V9 <- as.numeric(offset$V9)
View(offset)

plot(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)
offset <- (as.data.frame(cbind(unadjusted_ghgrp$facility_name.x,unadjusted_ghgrp$ghg_quantity,
                                ghgrp_old$`FACILITY NAME`,ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)[abs(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)>10,]))
View(offset)


cat('per facility mean delta (MT CH4/yr) =',mean(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25),
    "\nper facility median delta =",median(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25),
    "\nper facility max delta =",max(abs(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)))

cat('After removing top 3, per facility mean delta (MT CH4/yr) =',mean((unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)[which(abs(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)<10,)]),
    "\nper facility median delta =",median((unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)[which(abs(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)<10,)]),
    "\nper facility max delta =",max((abs(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25))[which(abs(unadjusted_ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)<10,)]),
    "\nAPI facility median = ",median(unadjusted_ghgrp$ghg_quantity))













ghgrp_old <- ghgrp_old[ghgrp_old$STATE %in% c("MD","NY","NJ","PA","DE"),]
ghgrp <- ghgrp[ghgrp$state %in% c("MD","NY","NJ","PA","DE"),]

ghgrp <- ghgrp[order(ghgrp$facility_id),]
ghgrp_old <- ghgrp_old[order(ghgrp_old$`GHGRP ID`),]

cat("GHGRP 5-state total (MT CH4/yr) differs by (new - old) =",sum(ghgrp$ghg_quantity,na.rm=T) - sum(ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25),
    "\nrelative to the new value of",sum(ghgrp$ghg_quantity,na.rm=T),
    "\nor",(sum(ghgrp$ghg_quantity,na.rm=T) - sum(ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25))/sum(ghgrp$ghg_quantity,na.rm=T)*100,"%")
cat("GHGRP via API =",nrow(ghgrp),"facilities \nold download =",nrow(ghgrp_old),"facilities")
cat("GHGRP via API found that Al Turi and Beulah landfills stopped reporting without a valid reason in addition to Kearney and added them in.  Adding to GHGRP_old now, though it wasn't done originally.")


plot(ghgrp)
points(ghgrp_old,col="red",pch=16)

plot(ghgrp_old)
points(ghgrp,col="red",pch=16)

plot(distance(ghgrp,ghgrp_old,pairwise=T))
View(cbind(ghgrp$facility_name.x,ghgrp$ghg_quantity,crds(ghgrp),
           ghgrp_old$`FACILITY NAME`,ghgrp_old$ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25,crds(ghgrp_old))[distance(ghgrp,ghgrp_old,pairwise=T)>500,])

plot(ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)
cat('per facility mean delta (MT CH4/yr) =',mean(ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25),
    "\nper facility median delta =",median(ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25),
    "\nper facility max delta =",max(abs(ghgrp$ghg_quantity - ghgrp_old$`GHG QUANTITY (METRIC TONS CO2e)`/25)))


################################################################################
# First load all data and convert as necessary

# Read in LMOP and remove those in GHGRP
LMOP <- read_xlsx(file.path(Input_directory,LMOP_file),sheet="LMOP Database",col_names = T)
LMOP_non_ghgrp <- LMOP[!(LMOP$`GHGRP ID` %in% ghgrp$facility_id),]

# #the approach used now
# LMOP_non_ghgrp_new <- LMOP[!(LMOP$`GHGRP ID` %in% ghgrp_landfill_emissions$facility_id[ghgrp_landfill_emissions$year==inventory_year]),]
# 
# #comparing the 2 approaches - all of them were facilities identified as stopped
# #reporting without a valid reason
# View(as.data.frame(ghgrp[!(ghgrp$facility_id %in% ghgrp_landfill_emissions$facility_id[ghgrp_landfill_emissions$year==inventory_year]),]))

#This has some nans in, remove those
LMOP_filt <- subset(LMOP_non_ghgrp,!is.na(Latitude))
coordinates(LMOP_filt) <- ~Longitude + Latitude
proj4string(LMOP_filt) <- CRS(SRS_string="EPSG:4326")  # WGS84
LMOP_crop <- crop(LMOP_filt, domain)

# Find avg emission per non-GHGRP LMOP landfill (including the ones with no coordinates)
avg_non_ghgrp <- non_ghgrp_total/nrow(LMOP_non_ghgrp)
# For comparison, calculate avg ghgrp
avg_ghgrp <- ghgrp_national/nrow(ghgrp)

#sort them by GHGRP ID so that matches can be made more easily
LMOP_crop <- LMOP_crop[order(LMOP_crop$`GHGRP ID`),]

# Assign the avg emissions to LMOP landfills
LMOP_crop$emiss <- avg_non_ghgrp*1e9/(16.043*365*24*60*60)   #Gg CH4/yr to mol/s of CH4

# rm(ghgrp_old,ghgrp_one_year)
################################################################################
# Now rasterise and save

ghgrp_crop_old <- ghgrp_crop_old[order(ghgrp_crop_old$`GHGRP ID`),]
ghgrp_crop <- ghgrp_crop[order(ghgrp_crop$facility_id),]

ghgrp_rast <- rasterize(ghgrp_crop_old, domain, "emiss", fun=sum)
# ghgrp_flux_old <- ghgrp_rast*1e9/(area(ghgrp_rast)*1e6)  # Calculate flux, mol/s to nmol/m2/s
ghgrp_flux_old <- raster(rast(ghgrp_rast)*1e9/(cellSize(rast(ghgrp_rast),unit="m")))  # Calculate flux, mol/s to nmol/m2/s
ghgrp_flux_old[is.na(ghgrp_flux_old)]<-0

LMOP_rast <- rasterize(LMOP_crop, domain, field="emiss", fun=sum)
# LMOP_flux_old <- LMOP_rast*1e9/(area(LMOP_rast)*1e6)  # Calculate flux, mol/s to nmol/m2/s
LMOP_flux_old <- raster(rast(LMOP_rast)*1e9/(cellSize(rast(LMOP_rast),unit="m")))  # Calculate flux, mol/s to nmol/m2/s
LMOP_flux_old[is.na(LMOP_flux_old)]<-0

ghgrp_rast <- rasterize(ghgrp_crop, rast(domain), "emiss", fun=sum)
ghgrp_flux <- ghgrp_rast*1e9/(cellSize(ghgrp_rast,unit="m"))  # Calculate flux, mol/s to nmol/m2/s
ghgrp_flux[is.na(ghgrp_flux)]<-0

LMOP_rast <- rasterize(vect(LMOP_crop), rast(domain), field="emiss", fun=sum)
LMOP_flux <- LMOP_rast*1e9/(cellSize(LMOP_rast,unit="m"))  # Calculate flux, mol/s to nmol/m2/s
LMOP_flux[is.na(LMOP_flux)]<-0

plot(rast(LMOP_flux_old) - LMOP_flux)
plot(rast(ghgrp_flux_old) - ghgrp_flux)

ghgrp_count <- rasterize(ghgrp_crop, rast(domain), "emiss", fun="count")
ghgrp_count <- rasterize(vect(ghgrp_crop_old), rast(domain), "emiss", fun="count")

plot(ghgrp_crop$emiss - ghgrp_crop_old$emiss)
range(range(ghgrp_crop$emiss - ghgrp_crop_old$emiss)*
        1e9/unlist(rep(global(cellSize(rast(ghgrp_rast),unit="m"),range),each=2)))




