#Compare locations for all LMOP and GHGRP landfill sites.  

################################################################################
#User input

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
Output_directory <- "~/../../Kristian/Desktop/"

LMOP_file <- "lmopdata(Mar_24)_landfill_only.xlsx"
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","httr","dplyr","jsonlite","pracma")
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


rm(ghgrp_landfill_only_emissions,ghgrp_combustion_emissions,make_consistent)
################################################################################
#Download the relevant facility (e.g., location) data using the API and merge

#see https://www.epa.gov/enviro/envirofacts-data-service-api
data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/JSON")

#initialize output
ghgrp_facility_info <- fromJSON(data_URLs)

#combine the datasets by ID, and year
ghgrp_all_data <- merge(ghgrp_facility_info,ghgrp_landfill_emissions,
                        by=c("facility_id","year"), all=F)

#convert the relevant columns to numeric class
ghgrp_all_data[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp_all_data[,c("latitude","longitude","ghg_quantity")],
                                                                   2,FUN = function(x){as.numeric(x)})

#remove duplicates - only need 1 year to get locations
ghgrp_all_data <- ghgrp_all_data[!duplicated(ghgrp_all_data$facility_id),]

#delete all tempfiles and clean up working environment
rm(ghgrp_facility_info)
################################################################################
#Now load/convert LMOP

# Read in LMOP and keep only those in GHGRP
LMOP <- read_xlsx(file.path(Input_directory,LMOP_file),sheet="LMOP Database",col_names = T)
LMOP_ghgrp <- LMOP[(LMOP$`GHGRP ID` %in% ghgrp_all_data$facility_id),] 

Combined_dataset <- merge(ghgrp_all_data,LMOP_ghgrp,by.x="facility_id",by.y="GHGRP ID",all=F)

#a few facilities in GHGRP are NOT in LMOP GHGRP IDs:
#1003046,1005141,1006442,1006557,1007103,1010830. Ignoring these here.  No clear
#consistent attribute across them (vary regionally, emissions, age, etc.)

#2 facilities had no lat/long in LMOP.  Remove now.
Combined_dataset <- Combined_dataset[!is.na(Combined_dataset$Latitude),]
Combined_dataset <- Combined_dataset[!is.na(Combined_dataset$Longitude),]

#Keep only the most relevant info
Combined_dataset <- Combined_dataset[,c("facility_id","Landfill ID",
                                        "parent_company","Landfill Owner Organization(s)",
                                        "latitude","longitude","Latitude","Longitude",
                                        "state","State",
                                        "facility_name.x","Landfill Name","Landfill Alias",
                                        "HH_emissions","C_emissions")]

colnames(Combined_dataset) <- c("GHGRP_ID","LMOP_ID",
                                "GHGRP_parent_company","LMOP_owner_organization",
                                "GHGRP_lat","GHGRP_long","LMOP_lat","LMOP_long",
                                "GHGRP_state","LMOP_state",
                                "GHGRP_facility_name","LMOP_facility_name","LMOP_facility_Alias",
                                "GHGRP_landfill_emissions","GHGRP_combustion_emissions")

################################################################################
#Calculate the distance from LMOP to GHGRP coordinates using haversine

Distances <- vector(length=nrow(Combined_dataset))
for(A in 1:nrow(Combined_dataset)){
  Distances[A] <- haversine(c(Combined_dataset$GHGRP_lat[A],Combined_dataset$GHGRP_long[A]),
                            c(Combined_dataset$LMOP_lat[A],Combined_dataset$LMOP_long[A]))
}
Combined_dataset$Distance <- Distances

png(file.path(Output_directory,"LMOP vs GHGRP Distances.png"))
plot(density(Combined_dataset$Distance),main = "Distance (km)")
dev.off()

sum(Combined_dataset$Distance>1);sum(Combined_dataset$Distance>1)/nrow(Combined_dataset)*100
sum(Combined_dataset$Distance>5);sum(Combined_dataset$Distance>5)/nrow(Combined_dataset)*100
sum(Combined_dataset$Distance>10);sum(Combined_dataset$Distance>10)/nrow(Combined_dataset)*100
sum(Combined_dataset$Distance>50);sum(Combined_dataset$Distance>50)/nrow(Combined_dataset)*100
sum(Combined_dataset$Distance>100);sum(Combined_dataset$Distance>100)/nrow(Combined_dataset)*100


