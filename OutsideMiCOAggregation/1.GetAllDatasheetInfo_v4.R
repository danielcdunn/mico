library(tidyverse)
library(sf)
library(readxl)
library(crayon)

sf_use_s2(FALSE)

##This script is meant to pull all MiCO datasheets from a species folder in Data, and get and save
##the site and connection/route info. The resulting site and connection files can then be error checked more easily,
##and will be used in the site aggregation script.


#Specify species & taxa
spp <- "MAHA"  ###CHANGE THIS
common_name <- "Northern Giant Petrel"  ###CHANGE THIS
taxa <- "bird"  ###CHANGE THIS to one of: mammal, turtle, bird, fish
AntarcticSpp <- TRUE ###CHANGE THIS: Is this a circumpolar species that is not limited by continental landmasses (TRUE or FALSE)

sppfolder <- paste("Data/", spp, "/", sep="")
missing_coords_file <- paste("Data/Georeferencing/site_details_", taxa, ".csv", sep="")
missing_coords <- read.csv(missing_coords_file, header=TRUE)

#Get list of datasheets
folder_names <-  list.dirs(sppfolder, full.names = FALSE, recursive = FALSE) #Looks for MiCO datasheet folders in the Data folder

all_sites <- data.frame() ##Creates a blank data.frame to hold all site information across all data sheets read
all_connections <- data.frame()  ##Creates a blank data.frame to hold all route and connection information across all data sheets read

#Load Reference metadata
inputreffile <- "Data/MiCOReferenceMetadata20210308.csv"
refmetadata <- read.csv(inputreffile)
if("Key" %in% colnames(refmetadata)){refmetadata <- refmetadata %>% rename(ZoteroID = "Key")}
colnames(refmetadata) <- str_replace_all(colnames(refmetadata),c(" " = "", "\\." = "", "#" = "", "-" = ""))
refinfo <- subset(refmetadata, select = c("ZoteroID", "Title", "PublicationYear", "Author")) %>%
  distinct(ZoteroID, .keep_all = TRUE) #gets rid of redundant references

#i=folder_names[1]  #Used for testing

for (i in folder_names) {  ##iterates through all folders in the Data folder
  print(i)
  ds_name <- list.files(paste(sppfolder, i, "/", sep=""), pattern = "xls", include.dirs = FALSE)  #pulls out any Excel files in the folder
  
  if (length(ds_name) > 1) {  ##Checks to make sure there is not more than one Excel file in the folder.
    cat(red("There is more than one Excel file in the following data sheet folder:\n"), 
        red(i), sep = "")
  } else {
  }
  
  j <- unlist(str_split(i, "_")) ##Splits the folder name to into items in a list
  zotero_id <- last(j)  ##grabs the ZoteroID to use later

  
  site_data <- read_excel(path=paste(sppfolder, i, "/", ds_name, sep=""), sheet = "TelemetryMarkRecaptureSites") %>%
    slice(-1) ## Reads the Sites sheet and removes the first row, which has info about the columns
  site_data <- site_data %>% mutate(Species = spp, CommonName = common_name, Taxa = taxa, ZoteroID = zotero_id, UniqueID = paste(ZoteroID, SiteID, sep = "_"), .before=SiteID)  ##Adds the species code and ZoteroID to the output data.frame

  #Fix column names
  #Cleanup the column names in the datasheet
  colnames(site_data) <- str_replace_all(colnames(site_data),c(" " = "", "\\." = "", "#" = "", "-" = "", "Site" = ""))
  colnames(site_data) <- str_replace(colnames(site_data),"\\s*\\([^\\)]+\\)","") #removes the "(s)" in some column names
  
  site_data <- site_data %>% select(Species:SamplingTechnology,Individuals,Lifestage:Activity) %>% 
    rename(SiteID = ID, NumIndividuals = Individuals, Activities = Activity)

  
  ###grab old georeferencing info for datasheets that didn't include that info
  if (ncol(site_data) < 18) { #Old datasheets have 18 not 21 columns after adding in Spp and ZoteroID.  They are missing the georeferencing information.  We add them in from a spearate file.
    spp_missing_data <- missing_coords %>% filter(Species == spp, Zotero.ID == zotero_id) %>%
      transmute(SiteID = Site.ID, Long = Longitude, Lat = Latitude, Radius = Radius) %>% unique()#added unique because sometimes T and MR are listed separately for the same site in the georeferencing datasheet
    site_data <- inner_join(site_data, spp_missing_data, by = c("SiteID" = "SiteID")) %>% relocate(c(Long, Lat, Radius), .after = Location)
  } else {
      site_data <- site_data %>% rename(Long = Longitude, Lat = Latitude)
    }
    
  all_sites <- rbind(all_sites, site_data)  ##Adds the site info to the data.frame containing all of the sites from every data sheet in the Data folder
  
  conn_data <- read_excel(path=paste(sppfolder, i, "/", ds_name, sep=""), sheet = "RoutesORConnections") %>%
    slice(-1)## Reads the Routes sheet and removes the first row, which has info about the columns
  conn_data <- conn_data %>% mutate(Species = spp, ZoteroID = zotero_id, .before=ConnectionID)  ##Adds the species code and ZoteroID to the output data.frame

  #Fix differences in how the column names were exported
  colnames(conn_data) <- str_replace_all(colnames(conn_data),c(" " = "", "\\." = "", "#" = "", "-" = "", "\\/" = "", "\\?" = ""))
  colnames(conn_data) <- str_replace(colnames(conn_data),"\\s*\\([^\\)]+\\)","") #removes the "(s)" in some column names
  
  conn_data <- conn_data %>% transmute(Species,
                                       CommonName = common_name,
                                       ZoteroID,
                                       Type = RouteConnection,
                                       ID = ConnectionID,
                                       Method = SampleMethod,
                                       SiteFrom = TagFromSiteIDSIOrigin,
                                       SiteTo = TagToSiteIDSISampled,
                                       SitesBetween = AssociatedConnectedSiteIDs, 
                                       NumIndividuals = RouteConnectionIndividuals,
                                       MonthsStart = RouteStartMonths,
                                       MonthsEnd = RouteEndMonths)
  
  conn_data$NumIndividuals <- conn_data$NumIndividuals %>% na_if("Unk") %>% as.numeric()
  
  all_connections <- rbind(all_connections, conn_data)  ##Adds the route and connection info to the data.frame containing all of the sites from every data sheet in the Data folder
} #End loop after retreiving all datasheets from all folders


#Finish up with the connections by creating an output file
exportroutefile <- paste("Data/route_details_", spp, ".csv", sep="")
write.csv(all_connections, exportroutefile, row.names = FALSE)


##Add more info to the sites files before exporting

##Add Methods info to sites
telemetry = c("ARGOS", "SONIC TAG", "RADIO TAG", "POP-UP ARCHIVAL (PAT)", "SATELLITE TAG (ARGOS)", "GLS", "GEOLOCATOR (GLS)", "GPS",
              "SATELLITE RELAY DATA LOGGER (SRDL)", "ADVANCED DIVE BEHAVIOR TAG", "3D DATA LOGGERS AND CRITTER CAMS", "GPS DATALOGGERS",
              "POP-UP SATELLITE ARCHIVAL (PSAT)", "SATELLITE TAG (GPS)", "PTT", "ACOUSTIC/SONIC TAG (TELEMETRY)", "GPS DATALOGGER",
              "VHF RADIO TRANSMITTERS", "SATELLITE TELEMETRY (UNKNOWN)")

mark_recap = c("BANDING", "FLIPPER", "PASSIVE INTEGRATED TRANSPONDER (PIT)", "PHOTO-ID", "PIT", "FLIPPER TAG", "MARK-RECAPTURE")

observation = c("SIGHTING", "SIGHTINGS", "SIGHTINGS (NO TAG/SAMPLE)")

all_sites$Method <- ""
all_sites$SamplingTechnology <- toupper(all_sites$SamplingTechnology)

for (k in 1:nrow(all_sites)) {
  print(k)
  k_tel <- sum(str_detect(all_sites$SamplingTechnology[k],telemetry))
  k_mr <- sum(str_detect(all_sites$SamplingTechnology[k],mark_recap))
  k_obs <- sum(str_detect(all_sites$SamplingTechnology[k],observation))
  if(sum(is.na(c(k_tel,k_mr,k_obs))) == 3) next
  
  if(k_tel > 0) {
    k_val <- "T"
  }
  if(k_mr > 0){
    k_val <- if_else(k_val == "", "MR", paste(k_val, "MR", sep=";"))
  }
  if(k_obs > 0) {
    k_val <- if_else(k_val == "", "O", paste(k_val, "O", sep=";"))
  }
  all_sites$Method[k] <- k_val
  k_val=""
} 

#Add Reference metadata
sites_count <- nrow(all_sites)
input_refs <- unique(all_sites$ZoteroID)
ref_sites <- left_join(all_sites, refinfo, by = "ZoteroID") #joins ref metadata to the site file to aid in aggregating sites in the next step

##Check for loss of sites during join
missing_refs <- ref_sites %>% filter(is.na(Author) & is.na(Title)) %>% select(ZoteroID) %>% unique()
if (length(missing_refs) > 0) {paste("Missing Refs: ", missing_refs)}


#Create dummy versions of the UniqueID for now for the anti join later
#Note- you can join on different field names but using one field name (even if redundant) reduces errors
ref_sites$UniqueID_from <- ref_sites$UniqueID
ref_sites$UniqueID_to <- ref_sites$UniqueID

#Get rid of any sites without georeferenced locations
sites_pts <- ref_sites %>% subset(!(is.na(Long) | is.na(Lat))) 
#Generate output files for each species selected
exportfile <- paste("Data/georeferenced_sites_", spp, "_draft.csv", sep="")
write.csv(sites_pts, exportfile, row.names = FALSE)


#Identify the ocean basin of the site to eliminate aggregation across basins in the aggregation script 
if (AntarcticSpp == TRUE) {#Use for Southern Ocean species.  Don't want the aggregation to be affected by ocean basin when it is all southern ocean data...
  sites_pts <- sites_pts %>% mutate(Basin = "ANT") 
} else {
  sites_pts <- st_as_sf(sites_pts,coords=c("Long","Lat"), crs=4326) #make an sf object of the sites for spatial overlays 
  IHO_file <- "Data/GIS/World_Seas_IHO_v3/World_Seas_IHO_v3_simplified_and_smoothed_fixed.shp"
  IHO_basins <- st_read(IHO_file)
  sites_pts$Basin <- IHO_basins$Basin[st_nearest_feature(sites_pts, IHO_basins)] #Finds the nearest polygon to each point and adds the vector as a new field

  sites_pts <- sites_pts %>% mutate(Long = sf::st_coordinates(.)[,1], Lat = sf::st_coordinates(.)[,2])
  st_geometry(sites_pts) <- NULL
}

#Generate output files for each species selected
exportfile <- paste("Data/georeferenced_sites_", spp, "_draft.csv", sep="")
write.csv(sites_pts, exportfile, row.names = FALSE)


#If sites were lost because they were missing coordinates, write a log file with the missing sites
ungeoreferenced_sites <- all_sites %>% subset(Species == spp & (is.na(Long) | is.na(Lat))) %>% select(UniqueID) %>% mutate(Reason = "Missing Coords")
if (nrow(ungeoreferenced_sites > 0)) {
  logfile <- paste("Data/georeferenced_sites_", spp, "_missing_sites.csv", sep="")
  write.csv(ungeoreferenced_sites, logfile)
}

###NOTE: This results in a different number of sites than the output from the MiCO system contains, because the MiCO system creates 
###duplicate dummy sites when a site has both (T)elemetry and (MR)Mark-recapture data.

