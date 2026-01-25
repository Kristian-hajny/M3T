## code to prepare 'ACES' data.  Download hourly files for each year and
## annualize. Note this is extremely time consuming and memory intensive to run
## as it's processing thousands of hours of CONUS data. A significant number of
## files constituting hundreds of GB must be downloaded before running.


output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"

#URLs for manual download - requires login
sectors <- c("Commercial","Elec","Industrial","Residential") #required sectors
# sectors <- c("Air","Commercial","Elec","Industrial","Marine","Nonroad","Oilgas",
#              "Onroad","Rail","Residential","Total") #all sectors
paste0("http://data.ornldaac.earthdata.nasa.gov/protected/nacp/NACP_ACES_V2/data/aces_",
       sectors,"_[2012:2017][01:12].nc4")

#Firefox extension DownloadThemAll! was used to simplify downloading these in
#batches rather than individually

#individual URLs
# Months <- sprintf("%02d",1:12)
# paste0("http://data.ornldaac.earthdata.nasa.gov/protected/nacp/NACP_ACES_V2/data/aces_",
#        rep(sectors,each=length(Months)),"_",rep(2012:2017,each=length(sectors)*length(Months)),
#        rep(Months,length(sectors)),".nc4")

################################################################################
#save partial data given the significant processing time/memory needed

ACES_output_directory <- file.path(output_directory,"ACES")
dir.create(ACES_output_directory,showWarnings = F,recursive = T)

ACES_years <- 2012:2017

ACES_sectors <- c("Commercial","Elec","Industrial","Residential") #required sectors
Months <- sprintf("%02d",1:12)


for(A in 1:length(ACES_years)){
  #output directory and names
  monthly_output_list <- paste0("aces_",rep(ACES_sectors,each=12),"_",ACES_years[A],Months,".nc4")
  annual_output_list <- paste0("ACES_annual_",ACES_sectors,"_",ACES_years[A],".nc")
  
  #keep only those that have monthly data in the folder - likely done in batches
  #for memory.
  annual_output_list <- annual_output_list[file.exists(file.path(ACES_output_directory,monthly_output_list))[seq(1,by=12,to=length(monthly_output_list))]]
  monthly_output_list <- monthly_output_list[file.exists(file.path(ACES_output_directory,monthly_output_list))]
  
  #no data for this year
  if(length(monthly_output_list)==0){
    cat("No ACES data for year",ACES_years[A],"trying next one\n")
    next
  }
  
  #keep only those that haven't already been annualized
  monthly_output_list <- monthly_output_list[!rep(file.exists(file.path(ACES_output_directory,annual_output_list)),each=12)]
  annual_output_list <- annual_output_list[!file.exists(file.path(ACES_output_directory,annual_output_list))]
  
  monthly_output_list <- file.path(ACES_output_directory,monthly_output_list)
  annual_output_list <- file.path(ACES_output_directory,annual_output_list)
  
  if(length(monthly_output_list) %% 12 != 0){
    stop("Assumes entire year of files for at least 1 sector are available and the number of ACES files is not divisible by 12, stopping")
  }
  
  ################################################################################
  #prep a template to work with - all 0, but still NA outside land areas
  
  Annual_ACES <- terra::rast(monthly_output_list[1])[[1]]
  Annual_ACES <- Annual_ACES*0
  monthly_ACES <- Annual_ACES

  ################################################################################
  #loop through each sector, and each monthly file
  
  sectors <- sapply(strsplit(basename(annual_output_list),"_"),"[[",3)
  
  for(Sector_indx in 1:(length(sectors))){
    subset_monthly_output <- monthly_output_list[grep(sectors[Sector_indx],basename(monthly_output_list))]
    
    for(File_indx in 1:12){
      cat("\rProcessing",basename(subset_monthly_output)[File_indx],
          "which is ACES file number",File_indx+12*(Sector_indx-1),
          "of",length(monthly_output_list),
          "for",ACES_years[A],"at",format(Sys.time(),"%H:%M\n                   "))

      #compared against simply using sum/mean; this was slightly faster (1.05
      #min vs 1.16 min using ACES total)
      monthly_data <- terra::rast(subset_monthly_output[File_indx])
      
      total_hrs <- terra::nlyr(monthly_data)
      #go through each hour of the file and add to the monthly total.  Note this
      #is several times faster than simply sum(monthly_ACES).
      for(hr_indx in 1:total_hrs){
        monthly_ACES <- monthly_ACES+monthly_data[[hr_indx]]
        cat("\rHour",hr_indx,"of",total_hrs,"       ")
      }

      #add this month to the annual total
      Annual_ACES <- Annual_ACES+monthly_ACES
      
      #reset the monthly_aces raster
      monthly_ACES <- monthly_ACES*0
    }
    
    #now that every monthly file has been processed and summed into the annual
    #one, convert that to an average (per hr units)
    Annual_ACES <- Annual_ACES/8760
    
    #save and reset the annual_aces raster
    terra::writeCDF(Annual_ACES,
                    annual_output_list[Sector_indx],
                    force_v4=TRUE,
                    varname="flux_co2",
                    unit="kg km-2 hr-1",
                    longname=paste0(sectors[Sector_indx],"_sector_annual_average_combustion_CO2_emissions"),
                    missval=-9999,
                    time=as.character(ACES_years[A]),
                    overwrite=TRUE)
    Annual_ACES <- Annual_ACES*0
    
    #delete the monthly files to minimize how much storage space is needed
    #while running the code (each file is >1 Gb, so >100 Gb would be needed
    #otherwise)
    invisible(gc())
    invisible(closeAllConnections())
    invisible(unlink(subset_monthly_output))
  }
}

################################################################################
#comparing sum of annualized sectors to annualized total for 1 year as a check

# Annuals <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/ACES",full.names=T,pattern=".nc")
# First_months <- list.files("C:/Users/Kristian/Downloads/firstsonly",full.names=T,pattern=".nc")
# 
# Annuals <- terra::rast(Annuals)
# First_months <- terra::rast(First_months)
# 
# total_indx <- grep("Total",terra::sources(Annuals))
# 
# Delta <- sum(Annuals[[-total_indx]],na.rm=T) - Annuals[[total_indx]]
# Delta[Delta==0] <- NA
# terra::plot(Delta,colNA="black")
# Delta_range <- terra::global(Delta,range,na.rm=T)
# Annual_range <- terra::global(Annuals[[total_indx]],range,na.rm=T)
# 
# Ratio <- sum(Annuals[[-total_indx]],na.rm=T)/Annuals[[total_indx]]
# Ratio[Ratio==1] <- NA
# terra::plot(Ratio,colNA="black")
# Ratio_range <- terra::global(Ratio,range,na.rm=T)
# 
# 
# 
# hour_indx <- 1
# raw_rast <- terra::rast(First_months[1])[[hour_indx]]
# for(A in 2:length(First_months)){
#   raw_rast <- c(raw_rast,
#                 rast(First_months[A])[[hour_indx]])
# }
# 
# raw_Delta <- sum(raw_rast[[-total_indx]],na.rm=T) - raw_rast[[total_indx]]
# raw_Delta[raw_Delta==0] <- NA
# terra::plot(raw_Delta,colNA="black")
# raw_Delta_range <- terra::global(raw_Delta,range,na.rm=T)
# raw_Annual_range <- terra::global(raw_rast[[total_indx]],range,na.rm=T)
# 
# raw_Ratio <- sum(raw_rast[[-total_indx]],na.rm=T)/raw_rast[[total_indx]]
# raw_Ratio[raw_Ratio==1] <- NA
# terra::plot(raw_Ratio,colNA="black")
# raw_Ratio_range <- terra::global(raw_Ratio,range,na.rm=T)
# 
# 
# #The annualized data shows as good of agreement between the sum of sectors and
# #the total as the original data does for a random selection of hours.
# 
# #tested for hour_indx 1, 39, and 400
# #Calculated ranges
# #raw ratio (all) =  0.9999998,1
# #Annualized ratio = 0.9999998,1
# #
# #raw range (1) =    0,2502741
# #Annualized range = 0,2514872
# #
# #raw delta (1) =    -0.3838750,0.1305559
# #raw delta (39) =   -0.2664308,0.1828993
# #raw delta (400) =  -0.1721419,0.2238539
# #Annualized delta = -0.1199687,0.1094103
# 
