#Citizen Science practical

#Installing and running packages ---- 

install.packages("rgbif")
library(rgbif)

#Adding in species data ----

#key <- name_suggest(q = "Caprimulgidae", rank="family")$data["key"] # Nightjar

key <- name_suggest(q = "Loxia", rank="genus")$data["key"][1,1] # Crossbill
cntry_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]
sx <- occ_search(taxonKey = key, country = cntry_code)

# To only return only if geographic coords available
sx <- occ_search(taxonKey = key, country = cntry_code, hasCoordinate = TRUE,
                 eventDate = '1990,2020', limit=2500)

unique(sx$data$scientificName)

#We can see some duplicate scientific names, some appear to lack ‘authorities’ (Author and year), and one lacks the actual species. 
#We will remove these:

sx$data <- sx$data[sx$data$scientificName != "Loxia curvirostra curvirostra",]
sx$data <- sx$data[sx$data$scientificName != "Loxia leucoptera bifasciata (C.L.Brehm, 1827)",]
sx$data <- sx$data[sx$data$scientificName != "Loxia Linnaeus, 1758",]

unique(sx$data$scientificName)

#Much better now

#Sometimes data can be uploaded with issues in the recordings
#We can view the issues with the data by using the commands:

problems <- gbif_issues()
View(problems)

#We can clean up the data we have downloaded by removing the basis of record is invalid
#(bri), invalid coordinate (cdiv), or geographic coordinate is out of range (cdout), 
#or the taxon match can only be done to a higher rank and not the scientific name (txmathi)

sx <- sx %>% occ_issues(-bri, -cdiv, -cdout, -txmathi) # Pipe syntax
# sx <- occ_issues(sx, -bri, -cdiv, -cdout, -txmathi) # Standard syntax

#Exploring and visualising the data ----

#We shall begin by plotting the number of records per year, easily carried out
#by using the tidyverse packages 

library(ggplot2)
library(dplyr)

ggplot(sx$data, aes(x=year)) +
  geom_histogram()

records_per_yr <- sx$data %>% 
  group_by(year) %>% 
  summarise(count_per_year = n())

ggplot(records_per_yr, aes(x = year, y=count_per_year)) +
  geom_line()

ggplot(sx$data, aes(x = year)) +
  geom_bar()

#Mapping the data ----

library(leaflet)
library(leafem)

# Create map 
# We will use different colour codes for each species
unique_spp <- unique(sx$data$scientificName) # Unique list of species
marker_col <- rainbow(length(unique_spp))    # Define set of rainbow colours
base_col <- rep("red", nrow(sx$data))        # Create a vector with default red

# Next bit less complicated than it looks. We go through each row of your data
# and compare the sx$data$scientificName with the unique_spp name. If they
# match we allocate the relevant rainbow colour in marker_col to the base_col
# vector
for(i in 1:nrow(sx$data)){
  for(j in 1:length(unique_spp)){
    if(sx$data$scientificName[i] == unique_spp[j]){
      base_col[i] <- marker_col[j]
    }
  }
}

m <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>% 
  ## add markers
  addCircleMarkers(sx$data$decimalLongitude, sx$data$decimalLatitude,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col=base_col, popup = sx$data$scientificName) %>% 
  addLegend(colors = marker_col, opacity=1, labels=unique_spp) 

m

#Citizen science data from Inaturalist ----

#We can access citizen science data from Inaturalist by using the rinat package 

install.packages("rinat")
library(rinat)

#Now we can get the crossbill data

sx <- get_inat_obs(query = "crossbill")

sx$datetime
sx$scientific_name

sx <- get_inat_obs(query = "crossbill", place_id=6857, maxresults=2500)
nrow(sx)

gb_ll <- readRDS("gb_simple.RDS")
plot(gb_ll)

sx2 <- get_inat_obs(query = "crossbill", bounds=gb_ll, maxresults=2500)
nrow(sx2)

#We can check the quality of the data being used with a few simple functions

sx <- sx[sx$quality_grade == "research",]
nrow(sx)

sx2 <- sx2[sx2$quality_grade =="research",]
nrow(sx2)

unique(sx$scientific_name)
unique(sx2$scientific_name)

#Again some errors here, but we can change them 

sx <- sx[sx$scientific_name != "Loxia", ]
sx <- sx[sx$scientific_name != "Carduelis carduelis", ]
sx <- sx[sx$scientific_name != "Chloris chloris", ]
unique(sx$scientific_name)

sx2 <- sx2[sx$scientific_name != "Loxia", ]
sx2 <- sx2[sx$scientific_name != "Carduelis carduelis", ]
sx2 <- sx2[sx$scientific_name != "Chloris chloris", ]
unique(sx2$scientific_name)

#We can now plot by date (need lubridate)

library(lubridate)

summary(sx$datetime)

# Convert datetime from character into a proper date format
sx$datetime <- sx$datetime %>%
  ymd_hms()
summary(sx$datetime) # Now correctly coded as a date and time

# Use the year() function to create a column with calendar year
sx <- sx %>% 
  mutate(year = year(datetime))

ggplot(sx, aes(x=year)) +
  geom_histogram()

records_per_yr <- sx %>% 
  group_by(year) %>% 
  summarise(count_per_year = n())

ggplot(records_per_yr, aes(x = year, y=count_per_year)) +
  geom_line()

for(i in 1:nrow(sx)){  
  for(j in 1:length(unique_spp)){
    if(sx$scientific_name[i] == unique_spp[j]){
      base_col[i] <- marker_col[j]
    }
  }
}

m <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>% 
  ## add markers
  addCircleMarkers(sx$longitude, sx$latitude,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col=base_col, popup = sx$scientific_name) %>% 
  addLegend(colors = marker_col, opacity=1, labels=unique_spp)
m

#Additional info for Inaturalist ----

#Get images
sx$image_url[1]

#Sound
sx$sound_url[sx$sound_url != ""] # Look for where it does not equal (!=) a blank

#Citizen science data from National Biodiversity Network

nbn <- readRDS("nbn_loxia.RDS")
nbn <- nbn[nbn$identificationVerificationStatus.processed == "Accepted",]

ggplot(nbn, aes(x=year.processed)) +
  geom_histogram()


#Using the NBN4 package to automate this 
install.packages("devtools")
library(devtools)
install_github("fozy81/NBN4R")


library(NBN4R)
nbn_reasons()

nbn_config(download_reason_id = 4)

nbn_config(warn_on_empty=TRUE)

#Searching and visualising taxonomies: crossbills, true finches

tx <- taxinfo_download("rk_family:Fringillidae",
                       fields=c("guid","rk_genus","scientificName","rank"))
# View(tx[])  # List structure in R so need empty square brackets to access the data

install.packages("phytools")
library(phytools)

tx <- tx[tx$rank %in% c("species","subspecies"),] ## restrict to species and subspecies

## as.phylo requires the taxonomic columns to be factors
tx$genus <- as.factor(tx$genus)
tx$scientificName <- as.factor(tx$scientificName)

## create phylo object of scientific Latin name nested within Genus
ax <- as.phylo(~genus/scientificName, data=tx)
plotTree(ax, type="fan", fsize=0.6)

#Displaying the occurence of crossbills and truefinches
nbn_config(caching="off")
nbn_config(verbose=TRUE)

loxia_recs <- occurrences(taxon="Loxia scotica", download_reason_id=10,
                          email="W.robson1@newcastle.ac.uk", verbose=TRUE)

loxia_recs <- ALA4R::occurrences(taxon="Loxia", download_reason_id=10,
                                 email="W.robson1@newcastle.ac.uk", verbose=TRUE)

loxia_recs <- readRDS("nbn_Loxia.RDS")

# Remove unconfirmed records
unique(loxia_recs$data$identificationVerificationStatus)

# Remove records only identified to genus Loxia
loxia_recs$data <- loxia_recs$data[loxia_recs$data$scientificName != "Loxia",]
# Loxia curvirostra curvirostra is the subspecies for Loxia curvirostra
# (common crossbill). Generally recorded the same so may be better to merge
# Use a conditional search with == inside square brackets to replace
# subspecies with species
loxia_recs$data$scientificName[loxia_recs$data$scientificName == "Loxia curvirostra curvirostra"] <- "Loxia curvirostra"

unique(loxia_recs$data$scientificName)

# If you only keep Loxia scotica for simplicity for now
loxia_scotica <- loxia_recs
loxia_scotica$data <- loxia_scotica$data[loxia_scotica$data$scientificName == "Loxia scotica",]
nrow(loxia_scotica$data)

ggplot(loxia_scotica$data, aes(x=startDateYear)) +
  geom_histogram()

records_per_yr <- loxia_scotica$data %>% 
  group_by(startDateYear) %>% 
  summarise(count_per_year = n())

ggplot(records_per_yr, aes(x = startDateYear, y=count_per_year)) +
  geom_line()

m <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>% 
  ## add markers
  addCircleMarkers(loxia_scotica$data$longitudeWGS84, loxia_scotica$data$latitudeWGS84,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", popup = loxia_scotica$data$scientificName) %>% 
  addLegend(colors = "red", opacity=1, labels="Scottish Crossbill")
m

