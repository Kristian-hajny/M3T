#'@title Package Options for M3T
#'
#'@description M3T stores many user-configurable options in a private
#'  environment. These control which sectors to process, how to access required
#'  datasets, and variations to run. You can get and set these options using the
#'  accessor functions: \code{\link{M3T_get_config}} and
#'  \code{\link{M3T_set_config}}.
#'
#'
#'@details The options are described by sector to simplify navigating.  All
#'  relevant datasets have a Source_X option that can generally be set to "M3T"
#'  to rely on preprocessed data built into the package or its
#'  \href{https://zenodo.org/}{companion Zenodo}, "download" to have the package
#'  download the data from the source, or a filepath pointing to a local copy of
#'  the properly formatted data file.  As datasets can move, change format, etc.
#'  over time, the download option may fail over time.
#'
#'
#'  \bold{Across Sectors}
#' \itemize{
#'   \item{\bold{Terra_datatype} - character describing how to save raster data. See '?terra::terraOptions'. This is temporarily applied while running 'CH4_inventory_build'. Default "FLT8S".}
#'   \item{\bold{Terra_progress} - integer describing when to use progress bars when processing raster data. See '?terra::terraOptions'. This is temporarily applied while running 'CH4_inventory_build'. Default 0 (none).}
#'   \item{\bold{Base_timeout} - integer describing the maximum time to attempt a download in seconds. See '?options'. This is temporarily applied while running 'CH4_inventory_build' and may be critical if any Source_X is set to "download". Default 20 minutes.}
#'
#' Method Variations
#' \itemize{
#'   \item{\bold{Use_ACES} - logical stating whether the \href{https://doi.org/10.3334/ORNLDAAC/1943}{ACES CO2 inventory} should be used to downscale the stationary combustion and natural gas distribution emissions. Default TRUE.}
#'   \item{\bold{Use_Vulcan} - logical stating whether the \href{https://earth.gov/ghgcenter/data-catalog/vulcan-ffco2-yeargrid-v4}{Vulcan CO2 inventory} should be used to downscale the stationary combustion and natural gas distribution emissions. Default TRUE.}
#'   \item{\bold{Combine_sectors} - logical stating whether sectors should be summed to create all possible unique inventory combinations across variations. Be aware that with many variations this can create >1,000 inventories. Default TRUE.}
#'   \item{\bold{Separate_thermo} - logical stating whether combined inventories should also be calculated separately for thermogenic (i.e., natural gas) and non-thermogenic sources. Be aware that this will triple the number of inventories created. Default FALSE.}
#'   }
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_Tigerlines_data} - character stating "M3T", "download", or a vector of filepaths to the needed files. Includes state, county, and urban tigerlines from the \href{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}{U.S. Census Bureau}. Used throughout. Default "M3T".}
#'   \item{\bold{Source_Cartographic_Boundaries_data} - character stating "M3T", "download", or a filepath pointing to the needed file. The State Cartographic Boundary file is similar to the state tigerlines, but excludes water areas and is also produced by the \href{https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html}{U.S. Census Bureau}. Used throughout for visualizations only and used to interactively define the domain if set to "custom". Default "M3T".}
#'   \item{\bold{Source_GHGRP_facility_data} - character stating "M3T", "download", or a filepath pointing to the needed file. Data on facility locations from the \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/pub_dim_facility}{EPA Greenhouse Gas Reporting Program's Envirofacts API}. Used for the landfill, natural gas distribution, natural gas transmission, and wastewater sectors as they rely in part on GHGRP data. Default "M3T".}
#'   \item{\bold{Source_GHGRP_combustion} - character stating "M3T", "download", or a filepath pointing to the needed file. Data on facility combustion emissions (subpart C) from the \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/c_subpart_level_information}{EPA Greenhouse Gas Reporting Program's Envirofacts API}. Used for the landfill and natural gas transmission sectors as these sources report to subpart C. Default "M3T".}
#'   \item{\bold{Source_GHGRP_NG} - character stating "M3T", "download", or a filepath pointing to the needed file. Data on petroleum and natural gas system facility-level emissions (subpart W) from the \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/w_subpart_level_information}{EPA Greenhouse Gas Reporting Program's Envirofacts API}. Used for the natural gas distribution and natural gas transmission sectors as these sources report to subpart W. Default "M3T".}
#'   \item{\bold{Source_GHGI} - character stating "M3T", "download", or a filepath pointing to a directory with the needed files. The main text and annex tables from the \href{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2022}{EPA Greenhouse Gas Inventory}. Optionally used for the landfill, natural gas distribution, natural gas transmission, and stationary combustion sectors to get the national activity and emission factor data used. Default "M3T".}
#'   \item{\bold{Source_ACES} - character stating "M3T", "download", or a filepath pointing to a directory with the needed files. Annual rasters of the industrial, residential, electric production, and commercial sectors from the \href{https://doi.org/10.3334/ORNLDAAC/1943}{ACES CO2 inventory}. Optionally used to downscale stationary combustion and natural gas distribution methane emissions. Code to convert the hourly data to annual is available on the \href{https://zenodo.org/}{companion Zenodo}. Default "M3T".}
#'   \item{\bold{Source_Vulcan} - character stating "M3T", "download", or a filepath pointing to a directory with the needed files. Annual rasters of the industrial, residential, electric production, and commercial sectors from the \href{https://earth.gov/ghgcenter/data-catalog/vulcan-ffco2-yeargrid-v4}{Vulcan v4.0 CO2 inventory}. Optionally used to downscale stationary combustion and natural gas distribution methane emissions. Default "M3T".}
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'  \bold{Landfills}
#' \itemize{
#'   \item{\bold{Process_landfills} - logical stating if this sector should be run. Default TRUE.}
#'
#' Method Variations:
#' \itemize{
#' Landfills have 2 methods to estimate their emissions - a first order decay
#' model, and an assumed collection efficiency of a gas collection system.
#' Either can be used as the "reported" value and this can change over time for
#' a given landfill. You can use the one they report, or force it to one method.
#' Note landfills without gas collection systems will still be included using
#' the modeled emission estimate if forcing to the collection efficiency
#' estimate.
#'   \item{\bold{landfill_ghgrp_reported} - logical stating if the "reported" estimate should be used. Default TRUE.}
#'   \item{\bold{landfill_ghgrp_modeled} - logical stating if the decay model estimate should be used. Default TRUE.}
#'   \item{\bold{landfill_ghgrp_collection_efficiency} - logical stating if the collection efficiency estimate should be used. Default TRUE.}
#'   }
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_GHGRP_landfills} - character stating "M3T", "download", or a filepath pointing to a directory with the needed file. The EPA GHGRP tables \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/hh_subpart_level_information}{Envirofacts subpart level info} and \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/hh_gas_collection_system_detls}{Envirofacts gas collection system details} that include GHGRP modeled, collection efficiency, and "reported" emissions. Default "M3T".}
#'   \item{\bold{Source_LMOP} - character stating "M3T", "download", or a filepath pointing to a directory with the needed file. The \href{https://www.epa.gov/lmop/landfill-technical-data}{EPA Landfill Methane Outreach Program} is a voluntary program with some details including location, but no emissions information. Used to distribute the residual betwen the GHGI and GHGRP estimates to landfills to small to report to the GHGRP (25,000 MT CO2e). Default "M3T".}
#'   }
#'
#' Emission Factors and similar
#' \itemize{
#'   \item{\bold{GHGI_landfill_total} - character or numeric listing either "GHGI" to pull the value from the GHGI or an integer value in gigagrams CH4/yr (or kilotons CH4/yr). Represents the net municipal landfill (MSW) emissions that can be pulled from GHGI table "CH4 emissions from Landfills (kt)". Default "GHGI".}
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'  \bold{Natural gas distribution - including residential post-meter}
#' \itemize{
#'   \item{\bold{Process_natural_gas_distribution} - logical stating if this sector should be run. Default TRUE.}
#'
#' Method Variations
#' \itemize{
#' Disaggregate emissions from a local distribution company (LDC), state, or
#' domain total to pixels. By LDC requires use of a function that requires
#' manual editing and comparison available in the
#' \href{https://zenodo.org/}{companion Zenodo}.
#'   \item{\bold{NG_distribution_by_LDC} - logical. Default FALSE.}
#'   \item{\bold{NG_distribution_by_state} - logical. Default TRUE.}
#'   \item{\bold{NG_distribution_by_domain} - logical. Default TRUE.}
#'   }
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_EIA_NG_file} - character stating "M3T", "download", or a filepath pointing to the needed file. \href{https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name}{EIA Form 176} - the Annual Report of Natural and supplemental Gas Supply and Disposition that includes sold natural gas volume by company and customer type. Can be downloaded using a button in the topright on the webpage. Default "M3T".}
#'   \item{\bold{Source_PHMSA_file} - character stating "M3T", "download", or a filepath pointing to the needed file. The \href{https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids}{Pipeline and Hazardous Material Safety Administration (PHMSA)} - the Gas Distribution Annual Data that includes miles of pipe by company and type. Default "M3T".}
#'   \item{\bold{Source_GHGRP_LDC} - character stating "M3T", "download", or a filepath pointing to a directory with the needed file. The EPA GHGRP table \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/w_local_dist_companies_details}{Envirofacts local distribution companies details} if before 2015 or \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/ef_w_equip_leaks_ngdist_leaks}{Envirofacts natural gas distribution leaks} and \href{https://enviro.epa.gov/envirofacts/metadata/table/ghg/ef_w_equip_leaks_pop_count}{Envirofacts population counts} otherwise that include the number of metering and regulating (MR) stations and transmission-distribution transfer (TD) stations for each GHGRP reporting company. Default "M3T".}
#'   }
#'
#' Emission Factors and similar
#' \itemize{
#'   \item{\bold{natural_gas_pipeline_emission_factors} - data.frame with columns "Leaks_per_mile" and "Avg_emissions_mol_per_s" and named rows "Bare_Steel", "Cast_Iron", "Coated_steel", and "Plastic". Default uses \href{https://doi.org/10.1021/acs.est.0c00437}{Weller et al. 2020}.}
#'   \item{\bold{natural_gas_res_post_meter_emission_factor} - numeric providing the whole-house residential post-meter emission factor in mol/s. Default uses \href{https://doi.org/10.1021/acs.est.8b03217}{Fischer et al. 2018} who reported 0.5% of residential consumption with 401 Giga cubic feet ~= 7850 giga grams of natural gas consumed per year. This is used as a conversion factor from cubic feet to grams.}
#'   \item{\bold{natural_gas_com_post_meter_emission_factor} - numeric providing the whole-building commercial post-meter emission factor in mol/s. Default is 0 as we are unaware of any published estimates.}
#'   \item{\bold{GHGI_MnR} - data.frame with columns "Type", "EF", and "Total_stations" with data for metering and regulating (MR) stations or "GHGI" to indicate this data should be pulled automatically from GHGI files. Emission factors (EF) should be in kg/activity and type should include "M&R >30", "M&R 100-300", "M&R <100", "Reg >300", "R-Vault >300", "Reg 100-300", R-Vault 100-300", "Reg 40-100", "R-Vault 40-100", and "Reg <40" where the number refers to the operating pressure. Default is "GHGI", which pulls this information from the \href{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}{EPA Greenhouse Gas Inventory annex files}.}
#'   \item{\bold{GHGI_maintenance} - data.frame with columns "Type" and "EF" with data for maintenance events or "GHGI" to indicate this data should be pulled automatically from GHGI files. Emission factors (EF) should be in kg/activity and type should include "Pressure Releief Valve Releases", "Pipeline Blowdown", and "Mishaps (Dig-ins)". Default is "GHGI", which pulls this information from the \href{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}{EPA Greenhouse Gas Inventory annex files}.}
#'   \item{\bold{GHGI_meters} - data.frame with columns "Type" and "EF" with data for meters or "GHGI" to indicate this data should be pulled automatically from GHGI files. Emission factors (EF) should be in kg/unit and type should include "Residential", "Commercial", and "Industrial". Default is "GHGI", which pulls this information from the \href{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}{EPA Greenhouse Gas Inventory annex files}.}
#'   \item{\bold{GHGI_services} - data.frame with columns "Type" and "EF" with data for service lines or "GHGI" to indicate this data should be pulled automatically from GHGI files. Emission factors (EF) should be in kg/mile and type should include "Services - Unprotected steel", "Services Protected steel", "Services - Plastic", "Services - Copper". Default is "GHGI", which pulls this information from the \href{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}{EPA Greenhouse Gas Inventory annex files}.}
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'  \bold{Natural gas transmission}
#' \itemize{
#'   \item{\bold{Process_natural_gas_transmission} - logical stating if this sector should be run. Default TRUE.}
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_HIFLD_compressor_file} - character stating "M3T" or a filepath pointing to the needed file. \bold{Cannot} be set to "download" as the data is no longer available online. This is a dataset formerly hosted by \href{www.dhs.gov/gmo/hifld}{the Homeland Infrastructure Foundation-Level Data} that has been deprecated and removed from the site. It includes the location of transmission compressors, including those too small to report to the GHGRP, and has been edited to map to the GHGRP. Default "M3T".}
#'   \item{\bold{Source_EIA_transmission_file} - character stating "M3T", "download", or a filepath pointing to the needed file. Inter and intrastate transmission pipeline map from the \href{https://atlas.eia.gov/}{Energy Information Administration (EIA) energy atlas} accessed from \href{https://www.arcgis.com/home/item.html?id=9833ca6c8103490b8ad145a30f0522ee}{ArcGIS} for the moment. Default "M3T".}
#'   }
#'
#' Emission Factors and similar
#' \itemize{
#'   \item{\bold{GHGI_Pipeline} - data.frame with columns "Type", "Emissions", and "Total_stations" with data for pipelines and transmission metering and regulating (M&R) stations or "GHGI" to indicate this data should be pulled automatically from GHGI files. Emissions should be in kt/yr, total stations should be in counts or miles, and type should include "Pipeline Leaks", "M&R (Trans. Co. Interconnect)", "M&R (Farm Taps + Direct Sales)", and "Pipeline venting". Default is "GHGI", which pulls this information from the \href{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}{EPA Greenhouse Gas Inventory annex files}}
#'   \item{\bold{GHGI_transmission_compressors} - data.frame with columns "Type", "Emissions", and "Total_stations" with data for transmission compressors or "GHGI" to indicate this data should be pulled automatically from GHGI files. Emissions (EF) should be in kt/yr, total stations should be in counts, and type should include "Station Total Emissions", "Dehydrator vents (Transmission)", "Flaring (Transmission)", "Engines (Transmission)", "Turbines (Transmission)", "Engines (Storage)", "Turbines (Storage)", "Generators (Engines)", "Generators (Turbines)", "Pneumatic Devices Transmission", and "Station Venting Transmission". Default is "GHGI", which pulls this information from the \href{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}{EPA Greenhouse Gas Inventory annex files}}
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' \bold{Stationary combustion}
#' \itemize{
#'   \item{\bold{Process_stationary_combustion} - logical stating if this sector should be run. Default TRUE.}
#'   \item{\bold{EIA_API_key} - API key to access the State Energy Data System (SEDS) data API at the \href{https://www.eia.gov/opendata/}{Energy Information Administration (EIA)}. One can register for a key using their email on the right hand side of the page.}
#'
#' Method Variations
#' \itemize{
#' Disaggregate emissions from a state or domain total to counties.
#'   \item{\bold{stationary_combustion_by_state} - logical. Default TRUE.}
#'   \item{\bold{stationary_combustion_by_domain} - logical. Default TRUE.}
#'   }
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_EIA_SEDS_data} - character stating "M3T", "download", or a filepath pointing to the needed file. State Energy Data System (SEDS) fuel consumption by economic sector and state from the \href{https://www.eia.gov/state/seds/seds-data-complete.php?sid=US}{Energy Information Administration (EIA)}. Default "M3T".}
#'   \item{\bold{Source_NEI_data} - character stating "M3T", "download", or a filepath pointing to the needed file. National Emissions Inventory (NEI) CO emissions data by county, fuel, and economic sector from the \href{https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei}{Environmental Protection Agency (EPA)}. Default "M3T".}
#'   }
#'
#' Emission Factors and similar
#' \itemize{
#'   \item{\bold{stationary_combustion_GHGI_data} - data.frame with columns "State", "com_coal", "ind_coal", "elec_coal", "res_petr", "com_petr", "ind_petr", "elec_petr", "com_gas", "ind_gas", "elec_gas", "res_wood", "com_wood", "ind_wood", and "elec_wood" or "GHGI" to indicate this data should be pulled automatically from GHGI files. State should be "US_EPA" and each column should list the Tera BTU of energy consumed for the economic sector - fuel combination. Res = residential, com = commercial, ind = industrial, elec = electric, and petr = petroleum. Res_coal is 0 in the US and res_gas is handled separately. Default is "GHGI", which pulls this information from the \href{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}{EPA Greenhouse Gas Inventory annex files}.}
#'   \item{\bold{stationary_combustion_emission_factors} - data.frame with columns "com_coal", "ind_coal", "elec_coal", "res_petr", "com_petr", "ind_petr", "elec_petr", "com_gas", "ind_gas", "elec_gas", "res_wood", "com_wood", "ind_wood", and "elec_wood". Each column should list the emission factor in g/Giga Joule for the economic sector - fuel combination. Res = residential, com = commercial, ind = industrial, elec = electric, and petr = petroleum. Res_coal is 0 in the US and res_gas is handled separately. Default is \href{https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html}{Intergovernmental Panel on Climate Change (IPCC)} 2006 volume 2, energy tables 2.2 through 2.5, except the natural gas electric sector which comes from \href{https://doi.org/10.1021/acs.est.9b01875}{Hajny et al., 2019}.}
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' \bold{Wastewater}
#' \itemize{
#'   \item{\bold{Terra_datatype} - }
#'   \item{\bold{Terra_progress} - }
#'   \item{\bold{Base_timeout} - }
#'
#' Method Variations
#' \itemize{
#'   \item{\bold{Use_ACES} - .}
#'   \item{\bold{Use_Vulcan} - .}
#'   \item{\bold{Combine_sectors} - }
#'   \item{\bold{Separate_thermo} - }
#'   }
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_Tigerlines_data} - }
#'   \item{\bold{Source_GHGRP_facility_data} - }
#'   \item{\bold{Source_GHGRP_combustion} - }
#'   \item{\bold{Source_GHGI} - }
#'   \item{\bold{Source_GHGRP_NG} - }
#'   \item{\bold{Source_Cartographic_Boundaries_data} - }
#'   \item{\bold{Source_ACES} - }
#'   \item{\bold{Source_Vulcan} - }
#'   }
#'
#' Emission Factors and similar
#' \itemize{
#'   \item{\bold{Source_Tigerlines_data} - }
#'   \item{\bold{Source_GHGRP_facility_data} - }
#'   \item{\bold{Source_GHGRP_combustion} - }
#'   \item{\bold{Source_GHGI} - }
#'   \item{\bold{Source_GHGRP_NG} - }
#'   \item{\bold{Source_Cartographic_Boundaries_data} - }
#'   \item{\bold{Source_ACES} - }
#'   \item{\bold{Source_Vulcan} - }
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' \bold{Wetlands and inland waters}
#' \itemize{
#'   \item{\bold{Terra_datatype} - }
#'   \item{\bold{Terra_progress} - }
#'   \item{\bold{Base_timeout} - }
#'
#' Method Variations
#' \itemize{
#'   \item{\bold{Use_ACES} - .}
#'   \item{\bold{Use_Vulcan} - .}
#'   \item{\bold{Combine_sectors} - }
#'   \item{\bold{Separate_thermo} - }
#'   }
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_Tigerlines_data} - }
#'   \item{\bold{Source_GHGRP_facility_data} - }
#'   \item{\bold{Source_GHGRP_combustion} - }
#'   \item{\bold{Source_GHGI} - }
#'   \item{\bold{Source_GHGRP_NG} - }
#'   \item{\bold{Source_Cartographic_Boundaries_data} - }
#'   \item{\bold{Source_ACES} - }
#'   \item{\bold{Source_Vulcan} - }
#'   }
#'
#' Emission Factors and similar
#' \itemize{
#'   \item{\bold{Source_Tigerlines_data} - }
#'   \item{\bold{Source_GHGRP_facility_data} - }
#'   \item{\bold{Source_GHGRP_combustion} - }
#'   \item{\bold{Source_GHGI} - }
#'   \item{\bold{Source_GHGRP_NG} - }
#'   \item{\bold{Source_Cartographic_Boundaries_data} - }
#'   \item{\bold{Source_ACES} - }
#'   \item{\bold{Source_Vulcan} - }
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' \bold{Remaining sectors from the gridded EPA inventory}
#' \itemize{
#'   \item{\bold{Terra_datatype} - }
#'   \item{\bold{Terra_progress} - }
#'   \item{\bold{Base_timeout} - }
#'
#' Method Variations
#' \itemize{
#'   \item{\bold{Use_ACES} - .}
#'   \item{\bold{Use_Vulcan} - .}
#'   \item{\bold{Combine_sectors} - }
#'   \item{\bold{Separate_thermo} - }
#'   }
#'
#' Accessing Datasets
#' \itemize{
#'   \item{\bold{Source_Tigerlines_data} - }
#'   \item{\bold{Source_GHGRP_facility_data} - }
#'   \item{\bold{Source_GHGRP_combustion} - }
#'   \item{\bold{Source_GHGI} - }
#'   \item{\bold{Source_GHGRP_NG} - }
#'   \item{\bold{Source_Cartographic_Boundaries_data} - }
#'   \item{\bold{Source_ACES} - }
#'   \item{\bold{Source_Vulcan} - }
#'   }
#'
#' Emission Factors and similar
#' \itemize{
#'   \item{\bold{Source_Tigerlines_data} - }
#'   \item{\bold{Source_GHGRP_facility_data} - }
#'   \item{\bold{Source_GHGRP_combustion} - }
#'   \item{\bold{Source_GHGI} - }
#'   \item{\bold{Source_GHGRP_NG} - }
#'   \item{\bold{Source_Cartographic_Boundaries_data} - }
#'   \item{\bold{Source_ACES} - }
#'   \item{\bold{Source_Vulcan} - }
#'   }
#' }






#'
#' @name M3T_config
#' @aliases M3T_config
#' @export
#' @seealso
#' * [M3T_get_config] Get config options
#' * [M3T_set_config] Set config options for the current R session
#' * [CH4_inventory_build] Calculates methane inventory using settings provided in config.
#' * [terra::terraOptions] Options for the terra package
#' * [base::options] Options in base R
NULL






