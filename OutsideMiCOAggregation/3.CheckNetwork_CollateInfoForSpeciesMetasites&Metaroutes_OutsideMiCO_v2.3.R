
# load required libraries 
library(tidyverse)
library(assertthat)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(geosphere)
library(readxl)


#Specify species
spp <- "MAHA"

sitesfile <- paste("Data/georeferenced_sites_", spp, "_unsupervised_outside_v14_meter_projection.csv", sep="") #note: I have changed the name to go with the standardised version control
routesfile <- paste("Data/route_for_metaconnections_", spp, ".csv", sep="")

# load georeferenced sites & routes
sites <-  read.csv(sitesfile, header = T, sep = ",", dec = ".", stringsAsFactors = F, colClasses=c(Method="character"))
#sites <- read_excel(sitesfile, sheet=NULL, range = NULL, col_names=TRUE, col_types=NULL, trim_ws=TRUE)
routes <- read.csv(routesfile, header=T, sep = ",", dec = ".", stringsAsFactors = F) 
routes <- routes %>% subset(Species == spp) #select only routes associated with this species

#Cleanup sites file
if("Commonname" %in% colnames(sites)){sites <- sites %>% rename(CommonName = "Commonname")}
species <- sites$CommonName[1]

##Clean up NumIndividuals and Activites fields 
sites$NumIndividuals <- sites$NumIndividuals %>% na_if("Unk") %>% as.numeric() 
sites$Activities <- str_replace_all(sites$Activities, " ", "")

##update sites file UniqueID to include species info
sites$UniqueID <- paste(sites$Species, sites$ZoteroID, sites$SiteID, sep = "_")

#Cleanup routes file
##standardise column names
colnames(routes) <- str_replace_all(colnames(routes),c("[.]" = "", "Xanimals" = "NumIndividuals"))
routes$NumIndividuals <- routes$NumIndividuals %>% as.numeric() 

##generate ZoteroID and Unique_IDs for routes file
routes$UniqueID_from <- paste(routes$ZoteroID, routes$SiteFrom, sep = "_")
routes$UniqueID_to <- paste(routes$ZoteroID, routes$SiteTo, sep = "_")

#Generate new unique ID info for metasites info... and redundant "to" and "from" IDs to decrease errors during joins later on
sites$MetaUniqueID <- paste(sites$MetaID, sites$MetasiteID, sep = "_")
sites$MetaUniqueID_from <- sites$MetaUniqueID
sites$MetaUniqueID_to <- sites$MetaUniqueID


##RECALL THAT ALL SITES HAVE HAD A FIELD ADDED ("MetaSiteID") OUTSIDE OF R THAT ALLOWS THIS SCRIPT TO AGGREGATE THEM.
#Aggregate sites
metasites <- sites %>%
  group_by(MetaUniqueID) %>%
  mutate(val =  ifelse(n() > 1, toString(geomean(cbind(MetasiteLong,MetasiteLat))),toString(cbind(MetasiteLong,MetasiteLat))),
         MaxRadius = max(Radius, na.rm=TRUE), 
         IndividualSites = paste(unique(UniqueID), collapse=";"),
         NumSites = length(unique(UniqueID)),
         Methods = paste(unique(Method), collapse=";"), 
         SamplingTech = paste(unique(SamplingTechnology), collapse=";"), 
         SiteLocations = paste(unique(Location), collapse=";"),
         SiteType = paste0(Activities, collapse=";"),
         SiteYears = paste(unique(na.omit(Year)), collapse=";")) %>%
  separate(val, c("MeanLon", "MeanLat"), sep = ",") %>%
  mutate_at(c("MeanLon", "MeanLat"), as.numeric)

metasites <- metasites %>% mutate(SiteYears = str_extract_all(SiteYears, "\\d{4}") %>%  #extracts all patterns of digits ("\\d") that appear 4 times in a row (4)
                                    unlist() %>%
                                    unique() %>%
                                    as.numeric() %>%
                                    sort() %>%
                                    paste(collapse = ";"))

#Code to aggregate and carry forward ALL Activities
fullactivity <- c("Spawning", "Nesting", "Breeding", "Chick-Rearing", "Fledging", "Feeding/Foraging", "Staging", 
                  "Stopover", "Migrating", "Wintering", "Nonbreeding", "Observations", "")


##Clean up SiteType
metasites$SiteType <- metasites$SiteType %>%
  str_to_lower() %>%
  str_replace_all(c(" "="","likely"="","overwintering"="wintering")) %>%
  str_to_title() %>%
  str_split(";") 
metasites <- metasites %>%  mutate(SiteType = map_chr(SiteType, ~ paste(unique(unlist(.)), collapse=";")))

metasites <- ungroup(metasites)


##WORK ON ROUTES
#Remove routes associated with duplicate records
metaroutes <- inner_join(routes,subset(metasites, select = c(UniqueID_from, MetaUniqueID_from)), by = "UniqueID_from")
dplyr::setdiff(routes$UniqueID_from,metaroutes$UniqueID_from)
metaroutes <- inner_join(metaroutes,subset(metasites, select = c(UniqueID_to, MetaUniqueID_to)), by = "UniqueID_to")

#Remove routes that now start and end at the same site
metaroutes <- metaroutes %>% subset(!MetaUniqueID_from == MetaUniqueID_to)

#aggregate routes between the same two nodes
metaroutes <- metaroutes %>% group_by(MetaUniqueID_from, MetaUniqueID_to) %>%
  mutate(SumNumIndividuals = sum(NumIndividuals, na.rm = TRUE)) %>%
  distinct(MetaUniqueID_from, MetaUniqueID_to, .keep_all = TRUE)


#Remove duplicate records (i.e., records that used the same data to describe the same sites)
#NOTE: we can't do this before the previous code working on routes or it eliminates routes
#that are associated with duplicate sites, even if they are not duplicate routes
metasites$Duplicate <- as.character(metasites$Duplicate)
metasites <- metasites %>% replace_na(list(Duplicate = "FALSE"))
metasites <- metasites %>% filter(!Duplicate == TRUE)


##Aggregate Sex info
### Split out sex data
metasites <- metasites %>% mutate(Sex_Female = NA, Sex_Male = NA, Sex_Unknown = NA, .after = Sex)

# Convert "Sex" column to list and extract counts for "M", "U", and "F"
for (i in seq_along(metasites$Sex)) {
    sex_data <- metasites$Sex[i]
    sex_types <- sex_data %>% str_remove(" ") %>% str_split(";") %>% unlist() #str_extract_all("[A-Z]+")
    for (j in sex_types){
      sex_num <- as.numeric(str_extract_all(j, "[0-9]+")[[1]])
      metasites$Sex_Female[i] <- ifelse(str_ends(j,"F"), sex_num, metasites$Sex_Female[i])
      metasites$Sex_Male[i] <- ifelse(str_ends(j,"M"), sex_num, metasites$Sex_Male[i])
      metasites$Sex_Unknown[i] <- ifelse(str_ends(j,"U"), sex_num, metasites$Sex_Unknown[i])
  }
}

### Split out life stage data
metasites <- metasites %>% mutate(LS_AIN = NA, LS_AB = NA, LS_ACR = NA, LS_ABG = NA,
                                  LS_APG = NA, LS_J = NA, LS_A = NA, LS_C = NA, LS_AS = NA, 
                                  LS_I = NA, LS_SUB = NA, LS_NH = NA, LS_UNK = NA, .after = Lifestage)

# Convert "life stage" column to list and extract counts
for (i in seq_along(metasites$Lifestage)) {
  tryCatch({
    ls_data <- metasites$Lifestage[i]
    ls_types <- ls_data %>% str_remove(" ") %>% str_split(";") %>% unlist() 
    for (j in ls_types){
      j <- str_to_upper(j) %>% str_remove(";")
      ls_num <- as.numeric(str_extract_all(j, "[0-9]+")[[1]])
      metasites$LS_AIN[i] <- ifelse(str_ends(j,"AIN"), ls_num, metasites$LS_AIN[i])
      metasites$LS_AB[i] <- ifelse(str_ends(j,"AB"), ls_num, metasites$LS_AB[i])
      metasites$LS_ACR[i] <- ifelse(str_ends(j,"ACR"), ls_num, metasites$LS_ACR[i])
      metasites$LS_ABG[i] <- ifelse(str_ends(j,"ABG"), ls_num, metasites$LS_ABG[i])
      metasites$LS_APG[i] <- ifelse(str_ends(j,"APG"), ls_num, metasites$LS_APG[i])
      metasites$LS_A[i] <- ifelse(str_ends(j,"A"), ls_num, metasites$LS_A[i])
      metasites$LS_J[i] <- ifelse(str_ends(j,"J"), ls_num, metasites$LS_J[i])
      metasites$LS_C[i] <- ifelse(str_ends(j,"C"), ls_num, metasites$LS_C[i])
      metasites$LS_AS[i] <- ifelse(str_ends(j,"AS"), ls_num, metasites$LS_AS[i])
      metasites$LS_NH[i] <- ifelse(str_ends(j,"NH"), ls_num, metasites$LS_NH[i])
      metasites$LS_SUB[i] <- ifelse(str_ends(j,"SUB"), ls_num, metasites$LS_SUB[i])
      metasites$LS_I[i] <- ifelse(str_ends(j,c("AI|I")), ls_num, metasites$LS_I[i])      
      metasites$LS_UNK[i] <- ifelse(str_ends(j,"U|UNK"), ls_num, metasites$LS_UNK[i])
    }
  }, error = function(e) {
    # Handle invalid strings here (e.g., print a warning or log the row numbers)
    warning(paste("Invalid Lifestage string (", j, "in row", i), sep="")
  })
}



#Now that the routes have been aggregated, we can finish aggregating the sites
metasites <- metasites %>% group_by(MetaUniqueID) %>%
  mutate(MinNumIndividuals = sum(NumIndividuals, na.rm = TRUE), #THIS SEEMS TO BE NOT COUNTING WHEN THERE ARE NAs
         Sex_Female_Total = sum(Sex_Female, na.rm = TRUE),
         Sex_Male_Total = sum(Sex_Male, na.rm = TRUE),
         Sex_Unknown_Total = sum(Sex_Unknown, na.rm = TRUE),
         LS_AIN_Total = sum(LS_AIN, na.rm = TRUE),
         LS_AB_Total = sum(LS_AB, na.rm = TRUE),
         LS_ACR_Total = sum(LS_ACR, na.rm = TRUE),
         LS_ABG_Total = sum(LS_ABG, na.rm = TRUE),
         LS_APG_Total = sum(LS_APG, na.rm = TRUE),
         LS_A_Total = sum(LS_A, na.rm = TRUE),
         LS_J_Total = sum(LS_J, na.rm = TRUE),
         LS_C_Total = sum(LS_C, na.rm = TRUE),
         LS_AS_Total = sum(LS_AS, na.rm = TRUE),
         LS_NH_Total = sum(LS_NH, na.rm = TRUE),
         LS_SUB_Total = sum(LS_SUB, na.rm = TRUE),
         LS_I_Total = sum(LS_C, na.rm = TRUE),
         LS_UNK_Total = sum(LS_UNK, na.rm = TRUE)) %>%
  distinct(MetaUniqueID, .keep_all = TRUE)


#PREPARE AND EXPORT METASITE AND METAROUTE FILES
#get a subset of fields to export
metasites_exported <- ungroup(metasites) %>% transmute(prod_id = paste("METASITE_", spp, "_", MetasiteID, sep = ""),
                                           cntr_lv = NA,
                                           description = Location,
                                           type = "METASITE",
                                           cntrd_lt = MetasiteLat,
                                           cntrd_ln = MetasiteLong,
                                           n_ndvdl = MinNumIndividuals,
                                           radius = Radius,
                                           method = Method,
                                           came_from = "LIT_REVIEW",
                                           species_code = spp,
                                           release_stage = 0,
                                           zotero_id = IndividualSites,
                                           num_sites = NumSites,
                                           site_names = SiteLocations,
                                           review_id = NA,
                                           activity_code = SiteType,
                                           metasite_id = paste(MetaID,"_",MetasiteID, sep=""),
                                           children = NA,
                                           radius_m = radius * 111319.5,
                                           sampling_technology = SamplingTech,
                                           population_identifier = NA,
                                           years = SiteYears,
                                           #months = Monthssampled,
                                           #LS_AIN_Total, LS_AB_Total, LS_ACR_Total, LS_ABG_Total, LS_APG_Total, LS_A_Total, 
                                           #LS_J_Total, LS_C_Total, LS_AS_Total, LS_NH_Total, LS_Sub_Total, LS_I_Total, LS_UNK_Total,
                                           #Sex_Female_Total, Sex_Male_Total, Sex_Unknown_Total,
                                           version = "OutsideMiCO",
                                           CommonName = CommonName,
                                           last_update = NA)
metasites <- ungroup(metasites)

#Get metasite location info for metaroutes
metasites_from <- metasites %>% transmute(MetaUniqueID_from, from_long = MetasiteLong, from_lat = MetasiteLat)
metasites_to <- metasites %>% transmute(MetaUniqueID_to, to_long = MetasiteLong, to_lat = MetasiteLat)

metaroutes_from <- inner_join(metaroutes, metasites_from, by = "MetaUniqueID_from")
metaroutes_to <- inner_join(metaroutes_from, metasites_to, by = "MetaUniqueID_to")

metaroutes_exported <- metaroutes_to %>% transmute(source = MetaUniqueID_from, 
                                                   target = MetaUniqueID_to,
                                                   type = NA,
                                                   weight = NA,
                                                   start_x = from_long,
                                                   end_x = to_long,
                                                   start_y = from_lat,
                                                   end_y = from_lat,
                                                   method = Method,
                                                   came_from = "LITERATURE_REVIEW",
                                                   num_trips = NA,
                                                   num_individuals = SumNumIndividuals,
                                                   species_code = spp,
                                                   direction = NA)

spp_metasite_file <- paste(c("Data/",spp,"_metasites_outsideMiCO_v14_meter_projection.csv"), collapse = "")
spp_metaroute_file <- paste(c("Data/",spp,"_metaroutes_outsideMiCO_v14_meter_projection.csv"), collapse = "")

write.csv(metasites_exported, spp_metasite_file, row.names = FALSE)
write.csv(metaroutes_exported, spp_metaroute_file, row.names = FALSE)




