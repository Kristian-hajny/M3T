## NG_distribution_emissions_r3.R
## In use: 2022-01-28 17:00
#
# Spatially allocate the various NG distribution (and residential post-meter) emission subsectors
# using sectoral CO2 emissions from either Vulcan or ACES as a spatial proxy.
# For both Vulcan and ACES, produce three maps by disaggregating emissions from the:
#     - individual company total
#     - state total
#     - domain total

NG_distribution <- function(){
  ################################################################################
  #User input
  
  # NG_distribution_by_LDC <- TRUE
  # #TRUE or FALSE, just whether to calculate everything at the local distribution
  # #company scale, or only the state and domain scale.  LDC scale calculations
  # #require manually matching facilities between the input files (EIA, PHMSA,
  # #HIFLD, GHGRP)
  # 
  # input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
  # output_directory <- 'G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/'
  # HIFLD_shapefile <- file.path(input_directory,"Natural__Gas__Service__Territories/NG_Service_Terr.shp")
  if(NG_distribution_by_LDC){
    HIFLD_file <- file.path(input_directory,"Natural__Gas__Service__Territories_edit.xlsx")
    EIA_file <- file.path(input_directory,"176 Type of Operations and Sector Items_edit.xlsx")
    PHMSA_file <- file.path(input_directory,"annual_gas_distribution_2010_present/annual_gas_distribution_2019_edit.xlsx")
    GHGRP_file <- file.path(input_directory,"US_GHGRP_NG_Local_Distribution_Companies_only_all_years_edit.xls")
  }else{
    # HIFLD_file <- file.path(input_directory,"Natural__Gas__Service__Territories.csv")
    EIA_file <- file.path(input_directory,"176 Type of Operations and Sector Items.xlsx")
    PHMSA_file <- file.path(input_directory,"annual_gas_distribution_2010_present/annual_gas_distribution_2019.xlsx")
    # GHGRP_file <- file.path(input_directory,"US_GHGRP_NG_Local_Distribution_Companies_only_all_years.xls")
  }
  # #GHGRP data was subset to only CH4 and only the NG local distribution companies
  # 
  # #Note the code assumes the filetypes are as above (all xlsx or xls, except HIFLD
  # #if not calculating by LDC)
  # 
  # #all 4 files (HIFLD, PHMSA, EIA, GHGRP) were edited to ensure the ID's were
  # #consistent and named as below if calculating by LDC
  # 
  # # HIFLD=https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::natural-gas-service-territories/explore?location=38.521197%2C-86.048965%2C7.00
  # # PHMSA=https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids
  # # EIA=https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name
  # # GHGRP=https://ghgdata.epa.gov/ghgp/main.do
  
  EPA_file <- file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx")
  EPA_EF_sheet <- "3.6-2"
  EPA_Activity_sheet <- "3.6-7"
  #which sheets are the needed ones in the EPA file.  We want Average CH4 Emission
  #Factors (kg/unit activity) for Natural Gas Systems Sources, for All Years
  #AND Activity Data for Natural Gas Systems Sources, for All Years
  
  # # EPA=https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2020-ghg
  # 
  # #Several emission factors (meter and regulating stations, services, meters, and
  # #maintenance) are pulled from the EPA file in a section on line 420.
  # 
  # Vulcan_residential_file <- 'G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0/Sectoral/Vulcan_v3_US_annual_1km_residential_mn.nc4'
  # Vulcan_commercial_file <- 'G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0/Sectoral/Vulcan_v3_US_annual_1km_commercial_mn.nc4'
  # 
  # ACES_residential_file <- 'G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0/Sectoral/2017_Annual_ACES_Residential.nc'
  # ACES_commercial_file <- 'G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0/Sectoral/2017_Annual_ACES_Commercial.nc'
  # 
  # vulcan_band <- 6
  # #year of Vulcan data.  Assuming Vulcan v3.0, 1 - 6 corresponding to years 2010 -
  # #2015
  # 
  # Use_Vulcan <- TRUE
  # Use_ACES <- TRUE
  # #which inventory to use in the spatial disaggregation?  Can be both, has to be
  # #at least 1 to finish processing.
  # 
  # year="2019"
  # county_outline_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_county/tl_2022_us_county.shp"
  # #census outlines for states
  # Cartographic_boundary_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/cb_2021_us_state_500k/cb_2021_us_state_500k.shp"
  # #Slightly different census outlines for states, excluding water areas
  # state_name_list <- c("DE","MD","NJ","NY","PA")
  # #which states to process
  # 
  temp_location <- "~/../../Desktop/"
  # #a place to save a downloaded HTML temporarily.  Due to permission errors that
  # #claim R is still using the file, it is unable to automatically delete it some
  # #of the time...
  # 
  # pipeline_emission_factors <- data.frame("Leaks_per_mile"=
  #                                           c(0.51,1,0.61,0.43),
  #                                         "Avg_emissions_mol_per_s"=
  #                                           c(2.24,1.72,2,2.03)/(16.043*60)) #converting from g/min to mol/s
  # rownames(pipeline_emission_factors) <- c("Bare_Steel",
  #                                          "Cast_Iron",
  #                                          "Coated_steel",
  #                                          "Plastic")
  # #pipeline emission factors and activity data from Weller et al., 2020 (doi:
  # #10.1021/acs.est.0c00437)
  # 
  # post_meter_emission_factor <- 7850/401*0.005/(16.043*60*60*24*365) 
  # #whole-house residential post-meter emission factor from Fischer et al., 2018
  # #(doi:https://doi.org/10.1021/acs.est.8b03217).  Reported as 0.5% of residential
  # #consumption in a region with 401 Giga cubic feet ~= 7850 giga grams NG consumed
  # #/ yr.  This is used as a conversion factor from cubic feet to grams here.  Then
  # #convert from g/yr to mol/s.
  # 
  # source("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Inventory_based_disaggregation.R")
  # #Load in a function to disaggregate total emissions using ACES/Vulcan or both
  # #within sub-domains (LDC, state, entire domain)
  # 
  # XESMF_check <- TRUE
  # #use xesmf to reproject (TRUE), or projectraster (FALSE)
  # 
  # d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
  # resolution <- 0.01
  # #Will be used to generate a blank raster for the output to be built onto, WGS84
  # #CRS.  Resolution in deg, bounding box in lat/long.  Only needed if using
  # #projectraster
  # 
  # #Important manual notes - 
  # 
  # #line 210 is a section to manually compare the HIFLD shapefile to the GHGRP one
  # #to identify any changes needed
  # 
  # #line 373 also has a manually defined value for a single PA utility (UGI
  # #Utilities) that should be removed if no longer relevant
  # 
  # #line 860 has manual adjustments for several LDCs in NY and PA that had
  # #different shapefiles in HFILD and the GHGRP.  These should be commented out if
  # #no longer relevant.
  # 
  # ################################################################################
  # #load packages
  # i <- 1
  # packagecheck <- c("raster","ncdf4","sf","readxl","ggplot2","fBasics")
  # 
  # while(i<=length(packagecheck)){
  #   if(length(find.package(packagecheck[i],quiet = TRUE))<1){
  #     install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  #   }
  #   i <- i+1
  # }
  # 
  # invisible(suppressPackageStartupMessages(lapply(packagecheck, require, character.only=TRUE)))
  # rm(packagecheck,i)
  # 
  # #raster + ncdf4 = raster and .nc filetype functionalities
  # #sf = spatial dataset functionalities
  # #readxl = better excel reading/writing
  # #ggplot2 = plotting options
  # #fbasics = timpallete colorscale (matlab-style rainbow, broader than R's rainbow) and rowprods function
  ################################################################################
  #Quit ASAP if neither ACES or Vulcan are set to be used.  Need one of them
  if(!(Use_Vulcan | Use_ACES)){
    stop("We need ACES, Vulcan or both to disaggregate emissions.")
  }
  if(!(NG_distribution_by_domain | NG_distribution_by_LDC | NG_distribution_by_state)){
    stop("We need to disaggregate natural gas distribution emissions by domain, state, or local distribution company or some combination thereof.")
  }
  
  ################################################################################
  #load in and filter the various files, excluding the EPA one for now
  
  # Load in HIFLD shapefile containing the LDC service territories
  HIFLD_shp <- vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Service_Territories/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  # HIFLD_shp_old <- vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Service_Territories/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  # HIFLD_shp_new <- vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Local_Distribution_Company_Service_Territories/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  if(NG_distribution_by_LDC){
    # Load in the HIFLD csv file containing the clean company IDs that we'll use for cross-referencing
    HIFLD_csv <- read.csv(HIFLD_file)
    rm(HIFLD_file)
  }else{
    #note that dates are not translated well from this
    HIFLD_csv <- as.data.frame(HIFLD_shp)
  }
  
  # Load the EIA company-level data for 2019 - this may have been edited to add a
  # dummy 'OTHER' entry
  EIA_csv <- read_xlsx(EIA_file,skip=1,col_names = T)
  # Load the PHMSA data for 2019 - this file may have had company ID's edited to
  # be consistent with EIA for the states that we will use
  PHMSA_csv <- read_xlsx(PHMSA_file,skip=2,col_names = T)
  
  # Filter the PHMSA file by commodity
  PHMSA_csv_NG <- PHMSA_csv[which(PHMSA_csv$COMMODITY == 'Natural Gas'),]
  
  #filter to only those for the relevant states and those with a company ID in
  #HIFLD (present at all).
  PHMSA_csv_NG <- PHMSA_csv_NG[which(PHMSA_csv_NG$STOP%in%state_name_list),]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$LDC_STATE %in% state_name_list | HIFLD_csv$COMPID=="OTHER",]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$COMPID!="NOT AVAILABLE",]
  HIFLD_csv <- HIFLD_csv[!is.na(HIFLD_csv$COMPID),]
  
  #add the LDC state abbreviation if the user hasn't done so manually (irrelevant
  #if not calculating by LDC)
  HIFLD_check <- substr(HIFLD_csv$COMPID,start = nchar(HIFLD_csv$COMPID)-1,stop = nchar(HIFLD_csv$COMPID))
  HIFLD_check2 <- sapply(gregexpr("[[:alpha:]]",HIFLD_check),FUN=function(x){x==-1}[1])
  for(A in 1:length(HIFLD_check)){
    if(HIFLD_check2[A]){
      HIFLD_csv[A,"COMPID"] <- paste0(HIFLD_csv[A,"COMPID"],HIFLD_csv[A,"LDC_STATE"])
    }
  }
  
  rm(PHMSA_csv,EIA_file,HIFLD_check,HIFLD_check2,PHMSA_file,A)
  
  if(NG_distribution_by_LDC){
    GHGRP_csv <- read_xls(GHGRP_file,sheet=inventory_year,col_names = T,skip = 5)
  }else{
    ################################################################################
    #Download the relevant ghgrp emissions data using the API
    #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
    #facility and emission data appropriately
    
    #download the relevant LDC-sector data
    #(https://www.epa.gov/enviro/greenhouse-gas-model).  
    ghgrp_w_only_emissions <- fromJSON("https://data.epa.gov/efservice/ef_w_emissions_source_ghg/JSON")
    
    #because we're getting sub-facility level information for transmission
    #compressor, first need to aggregate.  Subsetting to only the year of interest
    #now instead of later.
    ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$reporting_year==inventory_year,]
    ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$industry_segment=="Natural gas distribution [98.230(a)(8)]",]
    ghgrp_w_only_emissions <- aggregate(ghgrp_w_only_emissions$total_reported_ch4_emissions,
                                        by=list(ghgrp_w_only_emissions$facility_id,
                                                ghgrp_w_only_emissions$facility_name),
                                        sum,na.rm=T)
    
    #Now name the aggregated columns for clarity
    colnames(ghgrp_w_only_emissions) <- c("facility_id","facility_name","Reported_CH4")
    #and remove those that have 0 emissions for this category
    ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$Reported_CH4>0,]
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
    
    #subset to the desired year
    ghgrp_facility_info <- ghgrp_facility_info[ghgrp_facility_info$year==inventory_year,]
    
    #combine the datasets by ID
    GHGRP_csv <- merge(ghgrp_facility_info,ghgrp_w_only_emissions,
                       by="facility_id")
    
    #convert the relevant columns to numeric class
    GHGRP_csv[,c("latitude","longitude","Reported_CH4")] <- apply(GHGRP_csv[,c("latitude","longitude","Reported_CH4")],
                                                                  2,FUN=function(x){as.numeric(x)})
    
    #delete all tempfiles and clean up working environment
    rm(A,ghgrp_facility_info,ghgrp_w_only_emissions)
  }
  
  ################################################################################
  #do some webscraping to add a few additional variables for GHGRP facilities
  
  download_dest <- tempfile(fileext = ".html")
  GHGRP_csv[,c("Miles_of_Mains","N_of_above_grade_T-D_transfer_stations","N_of_above_grade_non_T-D_MR_stations",
               "N_of_below_grade_T-D_transfer_stations","N_of_below_grade_non_T-D_MR_stations")] <- 0
  #save to the temp file destination.  Add several new variables to GHGRP_csv
  
  for(A in 1:nrow(GHGRP_csv)){
    counter = 0
    repeat{
      counter=counter+1
      info=tryCatch(
        #the url is build from the GHGRP ID, the desired year, and a common url.
        #This file contains more information about the facility that isn't in the
        #downloaded file.
        download.file(paste0("https://ghgdata.epa.gov/ghgp/service/html/",inventory_year,"?id=",GHGRP_csv$facility_id[A],"&et=undefined"),
                      destfile=download_dest,quiet = T),
        warning = function(w) {
          Sys.sleep(1)
          NA
        },
        error = function(e) {
          Sys.sleep(1)
          NA
        }
      )
      if(!is.na(info)) {
        break
      }
      if(counter>=10){
        stop("Failed to download ",GHGRP_csv$facility_name.x[A]," data from\n",
             paste0("https://ghgdata.epa.gov/ghgp/service/html/",inventory_year,"?id=",GHGRP_csv$facility_id[A],"&et=undefined\n"),
             "The links used may no longer be accurate.  Check the GHGRP FLIGHT website.")
      }
    }
    #try to download the url, and retry up to 10x with 1s between runs as the link
    #seems to fail on occasion.
    #from https://stackoverflow.com/a/60880960
    
    HTML_data <- readChar(download_dest,file.info(download_dest)$size)
    #Now read in the whole html as text
    
    text_loc <- gregexpr("Distribution Mains, Gas Service",text = HTML_data)
    answer <- 0
    #initialize an output and locate some text near data we want (amount of
    #pipeline of various pipe types)
    for(B in 1:length(text_loc[[1]])){
      #should have found 1 value for each type of pipeline
      text <- substr(HTML_data,text_loc[[1]][B],text_loc[[1]][B]+attributes(text_loc[[1]])$match.length[B]+200)
      sub_answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
      #see https://www.debuggex.com/cheatsheet/regex/pcre
      answer <- answer+as.numeric(substr(text,sub_answer+1,sub_answer+attributes(sub_answer)$match.length-6))
      #first subset to the located text + buffer, then regex to find a number
      #with/without a decimal in it as formatted html text, then grab just this
      #value and add it to the answer (we only want the total across all pipeline
      #types)
    }
    GHGRP_csv$Miles_of_Mains[A] <- answer
    
    #now repeat the same type of process for various other variables
    text_loc <- regexpr("Number of above grade T-D transfer stations at the facility",text = HTML_data)
    text <- substr(HTML_data,text_loc,text_loc+attributes(text_loc)$match.length+50)
    answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
    answer <- substr(text,answer+1,answer+attributes(answer)$match.length-6)
    GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'[A] <- as.numeric(answer)
    
    text_loc <- regexpr("Number of above grade metering-regulating stations that are not T-D transfer stations",text = HTML_data)
    text <- substr(HTML_data,text_loc,text_loc+attributes(text_loc)$match.length+50)
    answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
    answer <- substr(text,answer+1,answer+attributes(answer)$match.length-6)
    GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations'[A] <- as.numeric(answer)
    
    text_loc <- gregexpr("Below Grade T-D Station, Gas Service, Inlet Pressure ",text = HTML_data)
    answer <- 0
    for(B in 1:length(text_loc[[1]])){
      text <- substr(HTML_data,text_loc[[1]][B],text_loc[[1]][B]+attributes(text_loc[[1]])$match.length[B]+200)
      sub_answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
      answer <- answer+as.numeric(substr(text,sub_answer+1,sub_answer+attributes(sub_answer)$match.length-6))
    }
    GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'[A] <- answer
    
    text_loc <- gregexpr("Below Grade M-R Station, Gas Service, Inlet Pressure",text = HTML_data)
    answer <- 0
    for(B in 1:length(text_loc[[1]])){
      text <- substr(HTML_data,text_loc[[1]][B],text_loc[[1]][B]+attributes(text_loc[[1]])$match.length[B]+200)
      sub_answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
      answer <- answer+as.numeric(substr(text,sub_answer+1,sub_answer+attributes(sub_answer)$match.length-6))
    }
    GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations'[A] <- answer
    
    cat("\rFinished downloading GHGRP data for",A,"of",nrow(GHGRP_csv),"                 ")
    #user update
  }
  
  #attempt to remove the downloaded html file
  unlink(download_dest)
  
  if(NG_distribution_by_LDC){
    GHGRP_csv$'Miles_of_Mains(PHMSA)' <- sapply(GHGRP_csv$`Company ID`,
                                                FUN=function(x){sum(PHMSA_csv_NG$MMILES_TOTAL[which(x==PHMSA_csv_NG$Company_ID)])})
    #copy the corresponding PHMSA total miles to the GHGRP file for comparison and
    #simpler calculations
    GHGRP_csv$'Miles_of_Mains(PHMSA)'[GHGRP_csv$`FACILITY NAME`=="UGI Utilities, Inc."] <- 12028
    #manually correct this one.  It's set to other in PHMSA as it corresponds to a
    #few facilities, but also varying shapes depending on the datasource.
    
    GHGRP_csv$above_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                                          GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations')/
      GHGRP_csv$`Miles_of_Mains(PHMSA)`
    GHGRP_csv$below_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                                          GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
      GHGRP_csv$`Miles_of_Mains(PHMSA)`
    GHGRP_csv$stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                              GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations'+
                                              GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                              GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
      GHGRP_csv$`Miles_of_Mains(PHMSA)`
    #calculate a few ratios
    
    GHGRP_PHMSA_comparison <- abs(GHGRP_csv$'Miles_of_Mains(PHMSA)' - GHGRP_csv$Miles_of_Mains)/mean(c(GHGRP_csv$'Miles_of_Mains(PHMSA)',GHGRP_csv$Miles_of_Mains))*100
    if(max(GHGRP_PHMSA_comparison)>5){
      View(GHGRP_csv[GHGRP_PHMSA_comparison>5,c("FACILITY NAME","Miles_of_Mains","Miles_of_Mains(PHMSA)")])
      stop("Double check the GHGRP facilities:\n",paste(GHGRP_csv$`FACILITY NAME`[GHGRP_PHMSA_comparison>5],collapse = "\n"),"\n\nas the miles of mains was >5% different than the corresponding PHMSA facility.  One of them is likely wrong.")
    }
    #user update check - PHMSA and GHGRP should agree very well.  Any that differ a
    #lot could be due to mislabeling.
    rm(GHGRP_PHMSA_comparison)
  }else{
    #same process, but using GHGRP Miles of mains
    GHGRP_csv$above_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                                          GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations')/
      GHGRP_csv$Miles_of_Mains
    GHGRP_csv$below_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                                          GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
      GHGRP_csv$Miles_of_Mains
    GHGRP_csv$stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                              GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations'+
                                              GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                              GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
      GHGRP_csv$Miles_of_Mains
  }
  
  rm(A,B,text_loc,answer,download_dest,sub_answer,HTML_data,text,info,counter)
  ################################################################################
  #Pull the EPA data we'll need later and save it to a few dataframes
  
  first_col <- which(read_xlsx(EPA_file,sheet = EPA_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  EPA_p1 <- read_xlsx(EPA_file,sheet = EPA_Activity_sheet,skip=first_col,col_names = T)
  
  first_col <- which(read_xlsx(EPA_file,sheet = EPA_EF_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  EPA_p2 <- read_xlsx(EPA_file,sheet = EPA_EF_sheet,skip=first_col,col_names = T)
  #p2 = emission factors, p1 = activity data.  Columns = year, rows = various
  #types of sources.  First col is just to identify the first column of useable
  #data
  
  Data_list <- c("M&R >300","M&R 100-300","M&R <100","Reg >300","R-Vault >300",
                 "Reg 100-300","R-Vault 100-300","Reg 40-100","R-Vault 40-100",
                 "Reg <40")
  #all the sources we're looking for, written exactly as in the EPA file
  
  EPA_MnR <- data.frame("Type"=Data_list,
                        "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                          1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                        "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p1[EPA_p1[,1]==x,as.character(inventory_year)]}))),
                        row.names = NULL)
  #use sapply to find the row using data list, specify the column as the year and
  #grab the relevant EF and activity data into a dataframe.
  
  #repeat for several other source types
  Data_list <- c("Services - Unprotected steel",
                 "Services Protected steel",
                 "Services - Plastic",
                 "Services - Copper")
  
  EPA_Services <- data.frame("Type"=Data_list,
                             "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                               1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                             row.names = NULL)
  
  Data_list <- c("Residential",
                 "Commercial",
                 "Industrial")
  
  EPA_meters <- data.frame("Type"=Data_list,
                           "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[which(EPA_p2[,1]==x)[1],as.character(inventory_year)]})))*
                             1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                           row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  EPA_maintenance <- data.frame("Type"=Data_list,
                                "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                                  1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  EPA_maintenance <- data.frame("Type"=Data_list,
                                "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                                  1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                row.names = NULL)
  
  rm(EPA_p1,EPA_p2,Data_list,EPA_file,first_col)
  ################################################################################
  ## Calculate emissions (all in mol/s):
  
  PHMSA_csv_NG$bare_steel_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_BARE","MMILES_STEEL_CP_BARE","MMILES_CU")],
                                               na.rm=T)*
                                         GHGI_natural_gas_pipeline_emission_factors[1,"Leaks_per_mile"]*
                                         GHGI_natural_gas_pipeline_emission_factors[1,"Avg_emissions_mol_per_s"])
  PHMSA_csv_NG$iron_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_CI","MMILES_DI","MMILES_RCI")],
                                         na.rm=T)*
                                   GHGI_natural_gas_pipeline_emission_factors[2,"Leaks_per_mile"]*
                                   GHGI_natural_gas_pipeline_emission_factors[2,"Avg_emissions_mol_per_s"])
  PHMSA_csv_NG$coat_steel_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_COATED","MMILES_STEEL_CP_COATED","MMILES_OTHER")],
                                               na.rm=T)*
                                         GHGI_natural_gas_pipeline_emission_factors[3,"Leaks_per_mile"]*
                                         GHGI_natural_gas_pipeline_emission_factors[3,"Avg_emissions_mol_per_s"])
  PHMSA_csv_NG$plastic_mains_ER <- (PHMSA_csv_NG$MMILES_PLASTIC*
                                      GHGI_natural_gas_pipeline_emission_factors[4,"Leaks_per_mile"]*
                                      GHGI_natural_gas_pipeline_emission_factors[4,"Avg_emissions_mol_per_s"])
  #Mains using EFs from Weller et al., or as specified at the top of the code
  
  PHMSA_csv_NG$UNP_steel_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_UNP_COATED","NUM_SRVS_STEEL_UNP_BARE")],
                                             na.rm=T)*
                                       EPA_Services$EF[1])
  PHMSA_csv_NG$CP_steel_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_CP_BARE","NUM_SRVS_STEEL_CP_COATED","NUM_SRVS_OTHER")],
                                            na.rm=T)*
                                      EPA_Services$EF[2])
  PHMSA_csv_NG$plastic_serv_ER <- (PHMSA_csv_NG$NUM_SRVS_PLASTIC*
                                     EPA_Services$EF[3])
  PHMSA_csv_NG$copper_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_CU","NUM_SRVS_CI","NUM_SRVS_DI","NUM_SRVS_RCI")],
                                          na.rm=T)*
                                    EPA_Services$EF[4])
  # Services using EFs from the EPA national inventory report
  
  # M&R stations - can't use GHGRP data without matching facilities, so estimate
  # based on avg stations per mile for reporters in each state. Then split by
  # pressure and function assuming the same split as at the national level (from
  # the EPA national inventory report).
  
  main_miles_ghgrp <- aggregate(GHGRP_csv$Miles_of_Mains,
                                list(State=GHGRP_csv$state),
                                sum,
                                na.rm=TRUE)
  above_grade_MnR <- aggregate((GHGRP_csv$`N_of_above_grade_T-D_transfer_stations` +
                                  GHGRP_csv$`N_of_above_grade_non_T-D_MR_stations`),
                               list(State=GHGRP_csv$state),
                               sum,
                               na.rm=TRUE)
  below_grade_MnR <- aggregate((GHGRP_csv$`N_of_below_grade_non_T-D_MR_stations` +
                                  GHGRP_csv$`N_of_below_grade_T-D_transfer_stations`),
                               list(State=GHGRP_csv$state),
                               sum,
                               na.rm=TRUE)
  above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
  below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
  # Calculate average stations per mile in each state
  
  if(!NG_distribution_by_LDC){
    state_indx <- match(PHMSA_csv_NG$STOP,above_grade_MnR$State)
    PHMSA_csv_NG$MnR_above <- PHMSA_csv_NG$MMILES_TOTAL*above_grade_MnR$stations_per_mile[state_indx]
    PHMSA_csv_NG$MnR_below <- PHMSA_csv_NG$MMILES_TOTAL*below_grade_MnR$stations_per_mile[state_indx]
  }
  # allocate average stations per mile in each state to all facilities if not
  # calculating by LDC
  
  ################################################################################
  #prep to merge the many files, excluding the EPA for now, calculate a few
  #additional variables
  
  PHMSA_csv_NG$Miles_main_and_serv <- PHMSA_csv_NG$MMILES_TOTAL + PHMSA_csv_NG$NUM_SRVCS_TOTAL*PHMSA_csv_NG$AVERAGE_LENGTH/5280
  # We're going to need the total miles of pipeline (inc. services) later -
  # calculate that here from AVERAGE_LENGTH (in ft)
  
  # Then select the columns we need and aggregate the entries which share the same company ID or state
  PHMSA_cols_to_keep <- c('MMILES_STEEL_UNP_BARE',
                          'MMILES_STEEL_UNP_COATED',
                          'MMILES_STEEL_CP_BARE',
                          'MMILES_STEEL_CP_COATED',
                          'MMILES_PLASTIC',
                          'MMILES_CI',
                          'MMILES_DI',
                          'MMILES_CU',
                          'MMILES_OTHER',
                          'MMILES_RCI',
                          'MMILES_TOTAL',
                          'NUM_SRVS_STEEL_UNP_BARE',
                          'NUM_SRVS_STEEL_UNP_COATED',
                          'NUM_SRVS_STEEL_CP_BARE',
                          'NUM_SRVS_STEEL_CP_COATED',
                          'NUM_SRVS_PLASTIC',
                          'NUM_SRVS_CI',
                          'NUM_SRVS_DI',
                          'NUM_SRVS_CU',
                          'NUM_SRVS_OTHER',
                          'NUM_SRVS_RCI',
                          'NUM_SRVCS_TOTAL',
                          'Miles_main_and_serv',
                          "bare_steel_mains_ER",
                          "iron_mains_ER",
                          "coat_steel_mains_ER",
                          "plastic_mains_ER",
                          "UNP_steel_serv_ER",
                          "CP_steel_serv_ER",
                          "plastic_serv_ER",
                          "copper_serv_ER")
  
  if(!NG_distribution_by_LDC){
    PHMSA_cols_to_keep <- c(PHMSA_cols_to_keep,
                            'MnR_above',
                            'MnR_below')
  }
  #if not calculating by LDC, these variables are in the PHMSA.  Otherwise,
  #they're in the ghgrp data
  
  EIA_cols_to_keep <- c("Residential Total Volume (Mcf)",
                        "Residential Total Customers",
                        'Commercial Total Volume (Mcf)',
                        'Commercial Total Customers',
                        'Industrial Total Volume (Mcf)',
                        'Industrial Total Customers',
                        'Electric Total Volume (Mcf)',
                        'Electric Total Customers')
  
  if(NG_distribution_by_LDC){
    cols_to_keep <- c('Company',
                      'Company Name',
                      'SVCTERID',
                      'State',
                      EIA_cols_to_keep,
                      PHMSA_cols_to_keep,
                      'MMiles_PHMSA_GHGRP',
                      'MnR_above',
                      'MnR_below')
  }else{
    cols_to_keep <- c('State',
                      EIA_cols_to_keep,
                      PHMSA_cols_to_keep,
                      'Miles_of_Mains')
  }
  #No HIFLD or GHGRP data if not calculating by LDC
  
  if(NG_distribution_by_LDC){
    PHMSA_csv_NG_agg <- aggregate(PHMSA_csv_NG[PHMSA_cols_to_keep],
                                  list(COMPANY_ID=PHMSA_csv_NG$Company_ID,
                                       STOP=PHMSA_csv_NG$STOP),
                                  sum,na.rm=T)
    #combine by ID
    
    
    EIA_PHMSA_merge <- merge(EIA_csv, PHMSA_csv_NG_agg, by.x='Company', by.y='COMPANY_ID')
    EIA_PHMSA_HIFLD_merge <- merge(EIA_PHMSA_merge, HIFLD_csv, by.x='Company', by.y='COMPID')
    all_merge <- merge(EIA_PHMSA_HIFLD_merge, GHGRP_csv, by.x='Company', by.y='Company ID', all.x=TRUE)
    # Now merge csv stuff together
    
    all_merge$State <- all_merge$STOP  # rename for clarity
    all_merge$MMiles_PHMSA_GHGRP <- all_merge$`Miles_of_Mains(PHMSA)`# Essentially the same as MMILES_TOTAL, but slightly different due to the way we've combined some PHMSA entries
    all_merge$MnR_above <- all_merge$`N_of_above_grade_T-D_transfer_stations`+ all_merge$`N_of_above_grade_non_T-D_MR_stations`
    all_merge$MnR_below <- all_merge$`N_of_below_grade_T-D_transfer_stations` + all_merge$`N_of_below_grade_non_T-D_MR_stations`
    # Clean up
    
    all_merge_clean <- all_merge[cols_to_keep]
    
    EIA_state_totals <- aggregate(EIA_csv[EIA_cols_to_keep],
                                  list(State=EIA_csv$State),
                                  sum,
                                  na.rm = TRUE)
    # Calculate residual EIA values from state totals
    
    EIA_merge_state_totals <-  aggregate(all_merge_clean[EIA_cols_to_keep],
                                         list(State=all_merge_clean$State),
                                         sum,
                                         na.rm = TRUE)
    
    for(a_state in unique(all_merge_clean$State)){
      residuals <- (EIA_state_totals[which(EIA_state_totals$State == a_state),-1] -
                      EIA_merge_state_totals[which(EIA_merge_state_totals$State == a_state),-1])
      all_merge_clean[which(all_merge_clean$Company == 'OTHER' & all_merge_clean$State == a_state), EIA_cols_to_keep] <- residuals
    }
    # Loop through states and assign residual EIA values to OTHER
    
    # M&R stations - can use GHGRP data for those stations that report, otherwise estimate based on avg stations per mile
    # for reporters in each state. Then split by pressure and function assuming the same split as at the national level
    # (from the EPA national inventory report).
    
    # Use the original GHGRP_csv df here - it includes UGI data in PA, which we had to exclude from all_merge_clean because there
    # was no good shapefile for it, but the underlying activity data is fine.
    # Note that for PA this means the average stations_per_mile value for reporters included here does not equal the default
    # stations_per_mile value assigned to non-reporters below.
    main_miles_ghgrp <- aggregate(GHGRP_csv$`Miles_of_Mains(PHMSA)`,
                                  list(State=GHGRP_csv$STATE),
                                  sum,
                                  na.rm=TRUE)
    above_grade_MnR <- aggregate((GHGRP_csv$`N_of_above_grade_T-D_transfer_stations` +
                                    GHGRP_csv$`N_of_above_grade_non_T-D_MR_stations`),
                                 list(State=GHGRP_csv$STATE),
                                 sum,
                                 na.rm=TRUE)
    below_grade_MnR <- aggregate((GHGRP_csv$`N_of_below_grade_non_T-D_MR_stations` +
                                    GHGRP_csv$`N_of_below_grade_T-D_transfer_stations`),
                                 list(State=GHGRP_csv$STATE),
                                 sum,
                                 na.rm=TRUE)
    
    above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
    below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
    # Calculate average stations per mile in each state
    
    non_ghgrp_indx <- which(is.na(all_merge_clean$MnR_above))
    non_ghgrp_state <- all_merge_clean$State[non_ghgrp_indx]
    state_indx <- match(non_ghgrp_state,above_grade_MnR$State)
    all_merge_clean$MnR_above[non_ghgrp_indx] <- all_merge_clean$MMILES_TOTAL[non_ghgrp_indx]*above_grade_MnR$stations_per_mile[state_indx]
    all_merge_clean$MnR_below[non_ghgrp_indx] <- all_merge_clean$MMILES_TOTAL[non_ghgrp_indx]*below_grade_MnR$stations_per_mile[state_indx]
    # Estimate number of stations for non-reporters
  }else{
    PHMSA_csv_NG_agg <- aggregate(PHMSA_csv_NG[PHMSA_cols_to_keep],
                                  list(STOP=PHMSA_csv_NG$STOP),
                                  sum,na.rm=T)
    EIA_csv_agg <- aggregate(EIA_csv[EIA_cols_to_keep],
                             list(State=EIA_csv$State),
                             sum,na.rm=T)
    GHGRP_csv_agg <- aggregate(GHGRP_csv[,"Miles_of_Mains"],
                               list(STATE=GHGRP_csv$state),
                               sum,na.rm=T)
    colnames(GHGRP_csv_agg) <- c("STATE","Miles_of_Mains")
    EIA_PHMSA_merge <- merge(EIA_csv_agg, PHMSA_csv_NG_agg, by.x='State', by.y='STOP')
    all_merge <- merge(EIA_PHMSA_merge, GHGRP_csv_agg, by.x='State', by.y='STATE', all.x=TRUE)
    # Now merge csv stuff together
    
    all_merge_clean <- all_merge[cols_to_keep]
    # Clean up
  }
  
  # Calculate the total mains emissions to be distributed according to residential and commercial CO2 emissions
  # This is calculated for each company according to the ratio of residential:commercial customers
  # Industrial customer numbers are much smaller, so we ignore these here
  all_merge_clean$mains_ER_total_res <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$"Residential Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$mains_ER_total_com <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$"Commercial Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$serv_ER_total_res <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$"Residential Total Customers"/
                                          (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$serv_ER_total_com <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$"Commercial Total Customers"/
                                          (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  ################################################################################
  #Calculate a few additional emissions
  
  EPA_MnR_above <- sum(EPA_MnR$Total_stations[-grep('Vault', EPA_MnR$Type)])
  EPA_MnR_below <- sum(EPA_MnR$Total_stations[grep('Vault', EPA_MnR$Type)])
  #split by function/pressure
  
  # Estimate emissions by function/pressure
  all_merge_clean$MnR_HiP_ER <- (all_merge_clean$MnR_above*                                                    # Abv grade stations
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R >300')]/EPA_MnR_above* # Type fraction
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'M&R >300')]) # Emission factor
  
  all_merge_clean$MnR_MidP_ER <- (all_merge_clean$MnR_above*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R 100-300')]/EPA_MnR_above*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'M&R 100-300')])
  
  all_merge_clean$MnR_LoP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R <100')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'M&R <100')])
  
  all_merge_clean$Reg_HiP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg >300')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg >300')])
  
  all_merge_clean$Reg_MidP_ER <- (all_merge_clean$MnR_above*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg 100-300')]/EPA_MnR_above*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'Reg 100-300')])
  
  all_merge_clean$Reg_LoP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg 40-100')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg 40-100')])
  
  all_merge_clean$Reg_VLP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg <40')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg <40')])
  
  all_merge_clean$RegV_HiP_ER <- (all_merge_clean$MnR_below*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault >300')]/EPA_MnR_below*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault >300')])
  
  all_merge_clean$RegV_MidP_ER <- (all_merge_clean$MnR_below*
                                     EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault 100-300')]/EPA_MnR_below*
                                     EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault 100-300')])
  
  all_merge_clean$RegV_LoP_ER <- (all_merge_clean$MnR_below*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault 40-100')]/EPA_MnR_below*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault 40-100')])
  
  all_merge_clean$MnR_ER_total_res <- ((all_merge_clean$MnR_HiP_ER + 
                                          all_merge_clean$MnR_MidP_ER +
                                          all_merge_clean$MnR_LoP_ER +
                                          all_merge_clean$Reg_HiP_ER +
                                          all_merge_clean$Reg_MidP_ER +
                                          all_merge_clean$Reg_LoP_ER +
                                          all_merge_clean$Reg_VLP_ER +
                                          all_merge_clean$RegV_HiP_ER +
                                          all_merge_clean$RegV_MidP_ER +
                                          all_merge_clean$RegV_LoP_ER)*
                                         all_merge_clean$"Residential Total Customers"/
                                         (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$MnR_ER_total_com <- ((all_merge_clean$MnR_HiP_ER + 
                                          all_merge_clean$MnR_MidP_ER +
                                          all_merge_clean$MnR_LoP_ER +
                                          all_merge_clean$Reg_HiP_ER +
                                          all_merge_clean$Reg_MidP_ER +
                                          all_merge_clean$Reg_LoP_ER +
                                          all_merge_clean$Reg_VLP_ER +
                                          all_merge_clean$RegV_HiP_ER +
                                          all_merge_clean$RegV_MidP_ER +
                                          all_merge_clean$RegV_LoP_ER)*
                                         all_merge_clean$"Commercial Total Customers"/
                                         (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  # Consumer meters - use emission factors from the EPA national inventory report
  all_merge_clean$Res_meter_ER <- all_merge_clean$"Residential Total Customers"*EPA_meters$EF[1]
  all_merge_clean$Com_meter_ER <- all_merge_clean$"Commercial Total Customers"*EPA_meters$EF[2]
  all_merge_clean$Ind_meter_ER <- all_merge_clean$"Industrial Total Customers"*EPA_meters$EF[3]
  
  # We could allocate the industrial meter emissions by ACES and Vulcan industrial sector
  # But this sector is dominated by a handful of large point sources, many of which don't even use natural gas
  # So instead, share these emissions out between the residential and commercial CO2 maps
  # Split according to the ratio of Res_meter_ER:Com_meter_ER - could equally have split according to the number of
  # customers, but that would shift the ratio of total meter emissions towards residential, which doesn't seem desirable
  # Keep the same naming convention as for the other subsectors (i.e. _total_res) even though it makes less sense here
  all_merge_clean$meter_ER_total_res <- (all_merge_clean$Res_meter_ER +
                                           all_merge_clean$Ind_meter_ER*
                                           all_merge_clean$Res_meter_ER/
                                           (all_merge_clean$Res_meter_ER + all_merge_clean$Com_meter_ER))
  
  all_merge_clean$meter_ER_total_com <- (all_merge_clean$Com_meter_ER +
                                           all_merge_clean$Ind_meter_ER*
                                           all_merge_clean$Com_meter_ER/
                                           (all_merge_clean$Res_meter_ER + all_merge_clean$Com_meter_ER))
  
  # Maintenance and upsets
  all_merge_clean$Relief_valve_ER <- all_merge_clean$MMILES_TOTAL*EPA_maintenance$EF[1]
  all_merge_clean$Blowdown_ER <- all_merge_clean$Miles_main_and_serv*EPA_maintenance$EF[2]
  all_merge_clean$Mishap_ER <- all_merge_clean$Miles_main_and_serv*EPA_maintenance$EF[3]
  
  all_merge_clean$upset_ER_total_res <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$"Residential Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$upset_ER_total_com <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$"Commercial Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  # Post-meter
  all_merge_clean$post_meter_ER_total_res <- all_merge_clean$"Residential Total Volume (Mcf)"*1000*natural_gas_post_meter_emission_factor
  #McF = thousand cubic ft
  
  ################################################################################
  #Start working with the shape files if calculating by LDC
  
  ## Now we have the emissions by company as a dataframe
  # Next do some pre-processing of the HIFLD shapefile so we can sptially allocate these emissions:
  # - SVCTERID number LDC360007 needs splitting into three bits based on NYS county polygons
  # - SVCTERID numbers LDC420001 and LDC420022 need merging (these companies merged so the 2019 data is combined)
  
  if(NG_distribution_by_LDC){
    LI_shp <- County_Tigerlines[which(County_Tigerlines$COUNTYNS %in% c('00974149', '00974128')),]
    LI_shp <- aggregate(LI_shp)
    NYC_shp <- County_Tigerlines[which(County_Tigerlines$COUNTYNS %in% c('00974122', '00974139', '00974141', '00974129', '00974101')),]
    NYC_shp <- aggregate(NYC_shp_temp)
    #combine a few LDCs
    
    state_shp_trans <- project(State_Tigerlines,crs(HIFLD_shp))
    LI_shp_trans <- project(LI_shp, crs(HIFLD_shp))
    NYC_shp_trans <- project(NYC_shp, crs(HIFLD_shp))
    # Move everything onto the target crs
    
    NGrid_shp <- HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007'),]
    NGrid_LI <- terra::intersect(LI_shp_trans,NGrid_shp)
    NGrid_LI <- NGrid_shp*LI_shp_trans
    
    NGrid_LI <- st_intersection(st_as_sf(NGrid_shp), st_as_sf(LI_shp_trans))
    NGrid_NYC <- st_intersection(st_as_sf(NGrid_shp), st_as_sf(NYC_shp_trans))
    NGrid_other <- st_difference(NGrid_shp, st_union(LI_shp_trans, NYC_shp_trans))
    # # Split up the National Grid LDC polygon
    
    HIFLD_shp <- rbind(HIFLD_shp, HIFLD_shp[rep(which(HIFLD_shp$SVCTERID == 'LDC360007'), 3),])
    # Add new entries for the shapefile containing the new split NGrid polygon
    
    HIFLD_shp$SVCTERID[nrow(HIFLD_shp)-2] <- 'LDC360007a'
    HIFLD_shp$SVCTERID[nrow(HIFLD_shp)-1] <- 'LDC360007b'
    HIFLD_shp$SVCTERID[nrow(HIFLD_shp)] <- 'LDC360007c'
    
    st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007a'),]) <- st_geometry(NGrid_LI)
    st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007b'),]) <- st_geometry(NGrid_NYC)
    st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007c'),]) <- st_geometry(NGrid_other)
    
    PPL_NG_combined <- st_combine(HIFLD_shp[which(HIFLD_shp$SVCTERID %in% c('LDC420001','LDC420022')),])
    st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC420001'),]) <- PPL_NG_combined
    # Merge the Peoples Natural Gas LDC geometries
    
    HIFLD_shp[which(HIFLD_shp$SVCTERID=="LDC240007"),] <- st_intersection(HIFLD_shp[which(HIFLD_shp$SVCTERID=="LDC240007"),],
                                                                          state_shp_trans[which(state_shp_trans$STUSPS == "MD"),])[names(HIFLD_shp)]
    #this shapefile bleeds over into VA, even though GHGRP and the other inputs
    #separate the data by state.  Removing the VA portion here
    
    all_merge_with_poly <- merge(all_merge_clean, HIFLD_shp[c('SVCTERID', 'geometry')], all.x=TRUE)
    # Now merge the geometries from HIFLD_shp with the entries in all_merge_clean
    
    all_merge_sf <- st_as_sf(all_merge_with_poly, sf_column_name='geometry', crs=crs(HIFLD_shp))
    # Turn into sf object
    
    for(a_state in unique(all_merge_sf$State)){
      other_indx <- which(all_merge_sf$State == a_state & all_merge_sf$Company == 'OTHER')
      if(length(other_indx)){  # if there is an 'OTHER' entry for this state
        state_poly <- state_shp_trans[which(state_shp_trans$STUSPS == a_state),]
        st_geometry(all_merge_sf[other_indx,]) <- st_geometry(st_difference(state_poly, st_union(all_merge_sf)))
        all_merge_sf[other_indx,'SVCTERID'] <- paste0('DUMMY_', a_state)
      }
    }
    # Go through each state and get the geometry of the OTHER entry for all_merge_clean (i.e. areas not covered by all_merge_sf)
    # Also change SVCTERID from DUMMY to a unique value
    
    ################################################################################
    #plot the updated LDCs
    
    for(A in 1:length(state_name_list)){
      png(file.path(output_directory,paste0('/Updated_',state_name_list[A],'_LDC_shapefile.png')),)
      par(oma = c(0, 0, 0, 4))
      current_state <- state_name_list[A]
      plot(all_merge_sf[all_merge_sf$State==current_state,1],key.length=0.9,
           key.pos=4,main=paste0(current_state," SVCTERID"),
           pal=timPalette(n=nrow(all_merge_sf[all_merge_sf$State==current_state,1])))
      graphics.off()
    }
    
  }
  
  ################################################################################
  #Load in ACES/Vulcan and use them to redistribute residential/commercial emissions
  
  res_totals <- c('mains_ER_total_res',
                  'serv_ER_total_res',
                  'MnR_ER_total_res',
                  'meter_ER_total_res',
                  'upset_ER_total_res',
                  'post_meter_ER_total_res')
  
  com_totals <- c('mains_ER_total_com',
                  'serv_ER_total_com',
                  'MnR_ER_total_com',
                  'meter_ER_total_com',
                  'upset_ER_total_com')
  #the various subsectors
  
  if(Use_ACES){
    aces_res <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Residential.nc'))
    aces_res <- flip(aces_res)
    crs(aces_res) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    
    aces_com <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Commercial.nc'))
    aces_com <- flip(aces_com)
    crs(aces_com) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  }
  if(Use_Vulcan){
    vu_res <- rast(paste0(vulcan_directory,"/Sectoral/","Vulcan_v3_US_annual_1km_residential_mn.nc4"), subds='carbon_emissions', lyrs=vulcan_band)
    vu_com <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)
  }
  # Load in ACES and Vulcan sectors - these are in different units, but it
  # doesn't matter as we'll only use fractions
  
  if(NG_distribution_by_LDC){
    all_merge_sf_LCC <- st_transform(all_merge_sf, crs(crs_to_use))
    # Transform to ACES/Vulcan CRS
    
    all_merge_sp_LCC <- as(all_merge_sf_LCC, 'Spatial')
    # Convert all_merge_sf_LCC to Spatial so we can use it with raster more
    # easily.  
    
    if(Use_ACES){
      cover_all <- cellFromPolygon(aces_res, all_merge_sp_LCC, weights = TRUE)
      # Get the fraction of each cell covered by each polygon - this is much quicker
      # that rasterize(getCover=T) although it does have strange bug (as of
      # raster_3.4-5) that calculates weights that are exactly a factor of 100 too
      # low i.e. they give 0.01 when the whole cell is covered
      
      # do this 1 time now instead of doing it 1x per sector (com and res).
      
      disaggregation(aces_res,res_totals,agg_level="LDC",sf_input=all_merge_sf_LCC)
      disaggregation(aces_com,com_totals,agg_level="LDC",sf_input=all_merge_sf_LCC)
    }
    if(Use_Vulcan){
      cover_all <- cellFromPolygon(vu_res, all_merge_sp_LCC, weights = TRUE)
      disaggregation(vu_res,res_totals,agg_level="LDC",sf_input=all_merge_sf_LCC)
      disaggregation(vu_com,com_totals,agg_level="LDC",sf_input=all_merge_sf_LCC)
    }
  }
  
  ################################################################################
  ## Now aggregate emissions at the state level and repeat
  # Side note - splitting into residential/commercial emissions at the company
  # level, then aggregating (as we do here) is probably more logical than
  # aggregating total emissions at the state level, then splitting into
  # residential/commercial This is obvious if you think of a situation where one
  # company dominates emissions, but another dominates consumers. In that case the
  # residential/commercial split should closely match that of the high emitting
  # company, not the high consumer company.
  
  all_merge_state <- aggregate(all_merge_clean[!(names(all_merge_clean) %in% c('SVCTERID', 'Company', 'Company Name', 'State'))],
                               list(State=all_merge_clean$State),
                               sum,na.rm=T)
  
  # Merge the geometries
  all_merge_state_poly <- terra::merge(State_Tigerlines, all_merge_state, by.y='State', by.x='STUSPS')
  
  if(Use_ACES){
    #convert state scale version to the proper crs
    all_merge_LCC_state <- project(all_merge_state_poly,aces_res)
    
    cover_all <- all_merge_LCC_state %>% 
      split(f=all_merge_LCC_state$STATEFP) %>%
      lapply(function(x){extract(aces_res,x,weights=T,exact=T,cells=T)})
    
    disaggregation(aces_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
    disaggregation(aces_com,com_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
  }
  if(Use_Vulcan){
    all_merge_LCC_state <- project(all_merge_state_poly,vu_res)
    
    cover_all <- all_merge_LCC_state %>% 
      split(f=all_merge_LCC_state$STATEFP) %>%
      lapply(function(x){extract(vu_res,x,weights=T,exact=T,cells=T)})
    
    disaggregation(vu_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
    disaggregation(vu_com,com_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
  }
  
  ################################################################################
  #Repeat when aggregated to the domain total.
  
  all_merge_domain <- as.data.frame(colSums(all_merge_clean[!(names(all_merge_clean) %in% c('SVCTERID', 'Company', 'Company Name', 'State'))]))
  
  all_merge_domain_poly <- aggregate(State_Tigerlines)
  values(all_merge_domain_poly) <- t(all_merge_domain)
  
  if(Use_ACES){
    #convert domain scale version to the proper crs
    all_merge_LCC_domain <- project(all_merge_domain_poly,aces_res)
    cover_all <- list(extract(aces_res,all_merge_LCC_domain,weights=T,exact=T,cells=T))

    disaggregation(aces_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    disaggregation(aces_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
  }
  if(Use_Vulcan){
    all_merge_LCC_domain <- project(all_merge_domain_poly,vu_res)
    cover_all <- list(extract(vu_res,all_merge_LCC_domain,weights=T,exact=T,cells=T))
    
    disaggregation(vu_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    disaggregation(vu_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
  }
  
  ################################################################################
  #write a function to save, dependent on whether or not we use XESMF
  if(XESMF){
    save_data <- function(input){
      input_name <- deparse(substitute(input))
      #pull the input name (e.g., vu_com_ch4_bydomain[[total]])
      disaggregation_level <- substring(text = input_name,regexpr("by",input_name),
                                        regexpr("\\[",input_name)-1)
      inventory_name <- strsplit(input_name,"_")[[1]][1]
      #pull the bydomain/bystate/byldc and vu/aces parts
      writeRaster(input,
                  paste0(output_directory,'/',inventory_name,'_',disaggregation_level,'_NG_dist_',total,'.nc'),
                  force_v4=TRUE,
                  varname='methane_emissions',
                  varunit='mol/km2/s',
                  longname=paste0(inventory_name,'_',disaggregation_level,'_NG_dist_',total),
                  NAflag=-9999,
                  overwrite=TRUE)
    }
  }else{
    #project with terra
    save_data <- function(input){
      input_name <- deparse(substitute(input))
      disaggregation_level <- substring(text = input_name,regexpr("by",input_name),
                                        regexpr("\\[",input_name)-1)
      inventory_name <- strsplit(input_name,"_")[[1]][1]
      #project to a grid with the exact right resolution, extent and origin.
      input <- project(input,domain)
      #convert from mol/km2s to nmol/m2s
      input <- input*1000
      
      #grab some text for the longname
      if(grepl("_res",total)){
        sector_name <- "residential"
      }else if(grepl("_com",total)){
        sector_name <- "commercial"
      }
      
      if(grepl("mains",total)){
        subsector_name <- "mains pipelines"
      }else if(grepl("serv",total)){
        subsector_name <- "service pipelines"
      }else if(grepl("MnR",total)){
        subsector_name <- "metering and regulating stations"
      }else if(grepl("^meter",total)){
        subsector_name <- "consumer meters"
      }else if(grepl("upset",total)){
        subsector_name <- "upsets and maintenance"
      }else if(grepl("post_meter",total)){
        subsector_name <- "post-meter residential leakage and usage"
      }
      
      if(grepl("ldc",disaggregation_level)){
        disaggregation_name <- "local distribution company"
      }else if(grepl("state",disaggregation_level)){
        disaggregation_name <- "individual-state"
      }else if(grepl("domain",disaggregation_level)){
        disaggregation_name <- "domain"
      }
      
      if("aces"==inventory_name){
        inventory_name <- "aces"
      }else if("vu"==inventory_name){
        inventory_name <- "vulcan"
      }
      

      writeCDF(input,
               paste0(output_directory,'/',"NG_dist_",sub("_ER_total","",total),
                      "_",disaggregation_level,"_",inventory_name,'.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname=paste0('Methane emissions from natural gas distribution ',subsector_name,
                               ', spatially allocated from ',disaggregation_name,
                               ' totals using ',inventory_name,' ',sector_name,' CO2 emissions'),
               missval=-9999,
               overwrite=TRUE)
    }
  }
  ################################################################################
  #Save the output
  
  # Now save the rasters for each subsector
  for(total in res_totals){
    if(NG_distribution_by_LDC){
      if(Use_ACES){
        save_data(aces_res_ch4_byLDC[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_res_ch4_byLDC[[total]])
      }
    }
    if(NG_distribution_by_state){
      if(Use_ACES){
        save_data(aces_res_ch4_bystate[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_res_ch4_bystate[[total]])
      }
    }
    if(NG_distribution_by_domain){
      if(Use_ACES){
        save_data(aces_res_ch4_bydomain[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_res_ch4_bydomain[[total]])
      }
    }
  }
  
  for(total in com_totals){
    if(NG_distribution_by_LDC){
      if(Use_ACES){
        save_data(aces_com_ch4_byLDC[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_com_ch4_byLDC[[total]])
      }
    }
    if(NG_distribution_by_state){
      if(Use_ACES){
        save_data(aces_com_ch4_bystate[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_com_ch4_bystate[[total]])
      }
    }
    if(NG_distribution_by_domain){
      if(Use_ACES){
        save_data(aces_com_ch4_bydomain[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_com_ch4_bydomain[[total]])
      }
    }
  }
  ################################################################################
  # Some sanity checks
  
  res_data_objects <- as.list(ls(pattern=glob2rx("*_res_ch4*")))
  #get the length, convert from a list of the names to the actual rasters
  res_data_length <- length(res_data_objects)
  res_data_list <- sapply(res_data_objects,get,envir=environment())
  #get the domain total for each raster, put into an organized df
  res_data <- as.data.frame(matrix(sapply(res_data_list,global,sum),
                                   ncol=res_data_length))
  #properly name it's dimensions
  names(res_data) <- gsub("res_ch4","by",unlist(res_data_objects))
  rownames(res_data) <- names(res_data_list[,1])
  
  com_data_objects <- as.list(ls(pattern=glob2rx("*_com_ch4*")))
  com_data_length <- length(com_data_objects)
  com_data_list <- sapply(com_data_objects,get,envir=environment())
  com_data <- as.data.frame(matrix(sapply(com_data_list,global,sum),
                                   ncol=com_data_length))
  names(com_data) <- gsub("com_ch4","by",unlist(com_data_objects))
  rownames(com_data) <- names(com_data_list[,1])
  
  if(NG_distribution_by_LDC){
    input_totals_LDC <- st_drop_geometry(all_merge_sf_LCC[,grep(glob2rx("*_ER*"),colnames(all_merge_sf_LCC))])
    input_totals_LDC <- colSums(input_totals_LDC)
  }
  input_totals_state <- all_merge_clean[,grep(glob2rx("*_ER*"),colnames(all_merge_clean))]
  input_totals_domain <- all_merge_domain[grep(glob2rx("*_ER*"),rownames(all_merge_domain)),1]
  names(input_totals_domain) <- rownames(all_merge_domain)[grep(glob2rx("*_ER*"),rownames(all_merge_domain))]
  input_totals_state <- colSums(input_totals_state)
  #original data that was distributed in the rasters.  The totals should still
  #match.
  
  ch4_totals_df <- rbind(res_data,com_data)
  if(NG_distribution_by_LDC){
    ch4_totals_df <- data.frame(ch4_totals_df,
                                "byLDC_input"=input_totals_LDC[rownames(ch4_totals_df)],
                                "bystate_input"=input_totals_state[rownames(ch4_totals_df)],
                                "bydomain_input"=input_totals_domain[rownames(ch4_totals_df)])
  }else{
    ch4_totals_df <- data.frame(ch4_totals_df,
                                "bystate_input"=input_totals_state[rownames(ch4_totals_df)],
                                "bydomain_input"=input_totals_domain[rownames(ch4_totals_df)])
  }
  
  
  ch4_totals_df <- apply(ch4_totals_df,2,FUN=function(x){as.numeric(x)})
  
  if(!all(as.vector(round(ch4_totals_df,7)==round(ch4_totals_df[,1],7)))){
    #round each to 7 digits and compare every column to the first column.  Check
    #that all are TRUE
    View(ch4_totals_df)
    stop("Something has gone wrong - the total across the domain when disaggregated by LDC vs by state vs by domain or by using ACES vs Vulcan disagree")
  }
  
}
