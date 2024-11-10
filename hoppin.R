# Goals

# Libraries
library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices

# Load libraries, in particular utils for the OSM download module
rscript_folder <- "C:/projects/pgn-data-airflow/rscripts/"
source(paste0(rscript_folder,"utils_updated_check_protoanchors.R"))
source(paste0(rscript_folder,"utils.R"))

log_folder <- "C:/temp/logs/"
local_folder <- "C:/temp/"



# Parameters
distance_threshold <- ??

# 1. Download all the hoppin points


# WFS download as explained on https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/


  
# Define the base WFS URL
wfs_server <- "https://geoserver.gis.cloud.mow.vlaanderen.be/geoserver/ows"

# Build the request URL
build_request_url <- function(start_index) {
  url <- parse_url(wfs_server)
  url$query <- list(
    service = "wfs",
    request = "GetFeature",
    typename = "hoppin:hoppinzuil",
    srsName = "EPSG:31370",
    startIndex = start_index,
    maxFeatures = 10000,
    outputFormat = "application/json"
  )
  return(build_url(url))
}

# Initialize variables
all_features <- list()
start_index <- 0
batch_size <- 10000
has_more_features <- TRUE

# Loop to fetch data in batches
while (has_more_features) {
  # Build the request URL for the current batch
  request_url <- build_request_url(start_index)
  
  # Fetch the data
  batch <- read_sf(request_url)
  
  # Check if there are no more features to fetch
  if (nrow(batch) == 0) {
    has_more_features <- FALSE
  } else {
    # Append the fetched features to the list
    all_features <- append(all_features, list(batch))
    # Increment the start index for the next batch
    start_index <- start_index + batch_size
  }
}

# Combine all fetched features into a single data frame
hoppin_official <- do.call(rbind, all_features)

hoppin_official_raw<-hoppin_official


# 2. Transform to OSM data model
## for now: just a reference of the point, a name and a main tag
## could be extended with info about local sharing systems and other info from the polygons (hoppin:hoppinpunt)

hoppin_official<-hoppin_official %>% 
  mutate(
    "ref:hoppin_column"=as.character(ID),
    name=Hoppinpunt,
    network='Hoppin',
    "network:wikidata"='Q124310711',
    amenity='mobility_hub',
    fixme='imported from official source, geometry might be off. Remove this fixme when location improved',
    tourism='information',
    information=case_when(
      Type=='Interactief' ~ 'terminal',
      Type=='Analoog (grote versie)' | Type=='Analoog (kleine versie)'~'board',
      TRUE~'board'
    ))
    
# TODO: remove stray spaces from names
hoppin_official<-hoppin_official %>% 
  mutate(name=trimws(name))

# 3 . Download OSM hopping data

features_list <- list("amenity" = "mobility_hub")

# If default server fails, set to TRUE to use mail.ru server (older data)
alternative_overpass_server<-FALSE
# Define extra tags to use as columns for properties
extra_columns <- c("amenity","network","network:wikidata","ref","ref:hoppin_column","ref:hoppin_polygon","type")
# Choose which datatypes are needed, as a list of datatypes, using any of "points", "lines", "mpolygons" (this is polygons+multipolygons together)
datatypes <- c("points")



### Actual OSM download & transformation ----

tryCatch({
  # Call the large function
  osm_1<-download_osm_process(features_list, datatypes, extra_columns, alternative_overpass_server, keep_region=TRUE )
  print("OSM data downloaded & processes succesfully")
}, error = function(e) {
  # Print error message
  print(paste("Something went wrong:", e$message))
})
osm_1<-osm_1 %>% filter(amenity=='mobility_hub')


# TODO: find a way to deal with the site relations automatically

# select points with expected ref:hoppin_column and remove them from hoppin_official
osm_already_mapped <- as.data.frame(osm_1) %>% 
 select("ref_hoppin_column",osm_id) %>%
  filter(!is.na(ref_hoppin_column)) %>%
  mutate(ref_hoppin_column = as.character(ref_hoppin_column)) %>%
  rename("ref:hoppin_column"=ref_hoppin_column)



# TODO: select points without these refs and make a subset of hoppin_official that could be linked to them

# 4. Improve OSM
# Based on exploration of OSM data, first clean it up before attempting to do an update

# 5. Create geojson to upload to OSM

# remove records already mapped
# TODO: check if hoppin_column is as stable as said by the data owner
hoppin_official_new<-hoppin_official %>% 
  left_join(osm_already_mapped, by="ref:hoppin_column")

hoppin_official_new <- hoppin_official_new %>% 
  filter(is.na(osm_id)) %>%
  select("ref:hoppin_column", name, network, "network:wikidata", amenity, information, fixme, tourism)
  

# reproject to wgs84
hoppin_official_new<-st_transform(hoppin_official_new, 4326)

# write to geojson
st_write(hoppin_official_new, paste0(log_folder, "hoppin_upload_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".geojson"))


# open in JOSM
# check for incomplete data with https://overpass-turbo.eu/s/1TU0
# import the rest

# TODO: compare old import file with new import file to see what attributes have changed. Compare that to OSM to identify which OSM objects should be updated

