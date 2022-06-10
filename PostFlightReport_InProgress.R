library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(webshot)

#webshot::install_phantomjs()



setwd("C:\\Users\\oobrien\\Documents\\R_Work_Directory\\PostFlightReport")
target_dir <- paste(getwd(), "/output/", sep="")
filenames <- list.files(".//data//", pattern="*RAW.CSV")
date <- substr(filenames, 1, 8)
date <- max(date)

filename <- paste(".//data//", date, "_RAW.CSV", sep="")
dat <- read.csv(filename)

#dat <- read.csv("20220520_RAW.CSV")
dat <- dat %>%
  filter(dat$long < 0) #%>%
  #filter(dat$legtype != 0) 

min_long <- min(dat$long)
min_lat <- min(dat$lat)
max_long <- max(dat$long)
max_lat <- max(dat$lat)
mid_lat <- mean(c(min(dat$lat), max(dat$lat)))
mid_long <- mean(c(min(dat$long), max(dat$long)))



## use this url: https://apps-nefsc.fisheries.noaa.gov/cgi-bin/mammalmaps/xmlgenDMA.pl
## copy any relevant DMAs below

dma1_lat <- c(0, 
              0, 
              0, 
              0) %>%
  as.data.frame() %>%
  rename("lat" = ".")

dma1_long <- c(0, 
               0, 
               0, 
               0)
dma1 <- cbind(dma1_lat, dma1_long) %>%
  as.data.frame() #

dma2_lat <- c(0, 
              0, 
              0, 
              0) %>%
  as.data.frame() %>%
  rename("lat" = ".")

dma2_long <- c(0, 
               0, 
               0, 
               0)
dma2 <- cbind(dma2_lat, dma2_long) %>%
  as.data.frame() 

## replace Kenney SPECCODE with common names and NO scientific names
kenney2commonNoLatin <- function(SPECCODE){
  str_replace_all(SPECCODE, 
                  c("BLWH" = "Blue Whale", 
                    "BOWH" = "Bowhead Whale", 
                    "BRWH" = "Bryde's Whale", 
                    "FIWH" = "Fin Whale",
                    "GRWH" = "Gray Whale", 
                    "HUWH" = "Humpback Whale",
                    "RIWH" = "Right Whale", 
                    "SEWH" = "Sei Whale",
                    "SPWH" = "Sperm Whale", 
                    "SRWH" = "Southern Right Whale",
                    "UNBA" = "Unidentified *Balaenoptera*",
                    "UNBS" = "Unidentified Bryde's or Sei Whale", 
                    "UNFS" = "Unidentified Fin or Sei Whale", 
                    "UNLW" = "Unidentified Large Whale", 
                    "UNRO" = "Unidentified Rorqual", 
                    "UNWH" = "Unidentified Whale",
                    "BEWH" = "Beaked Whale", 
                    "BLBW" = "Blainville's Beaked Whale", 
                    "GEBW" = "Gervais' Beaked Whale", 
                    "GOBW" = "Cuvier's Beaked Whale", 
                    "KIWH" = "Killer Whale", 
                    "MIWH" = "Minke Whale", 
                    "NBWH" = "Northern Bottlenose Whale", 
                    "SOBW" = "Sowerby's Beaked Whale", 
                    "TRBW" = "True's Beaked Whale", 
                    "UNBW" = "Unidentified Beaked Whale", 
                    "UNMW" = "Unidentified Medium Whale", 
                    "ASDO" = "Atlantic Spotted Dolphin", 
                    "BELU" = "Beluga",
                    "BODO" = "Bottlenose Dolphin", 
                    "CLDO" = "Clymene Dolphin", 
                    "DSWH" = "Dwarf Sperm Whale", 
                    "FKWH" = "False Killer Whale", 
                    "FRDO" = "Fraser's Dolphin", 
                    "GRAM" = "Risso's Dolphin",
                    "HAPO" = "Harbor Porpoise", 
                    "LFPW" = "Long-finned Pilot Whale", 
                    "MHWH" = "Melon-Headed Whale", 
                    "OBDO" = "Offshore Bottlenose Dolphin", 
                    "PIWH" = "Pilot Whale", 
                    "PSDO" = "Pan-Tropical Spotted Dolphin", 
                    "PSWH" = "Pygmy Sperm Whale",
                    "PYKW" = "Pygmy Killer Whale", 
                    "RTDO" = "Rough-Toothed Dolphin", 
                    "SADO" = "Common Dolphin", 
                    "SFPW" = "Short-finned Pilot Whale", 
                    "SNDO" = "Spinner Dolphin", 
                    "SPDO" = "Spotted Dolphin", 
                    "STDO" = "Striped Dolphin", 
                    "UNBD" = "Unidentified Beaked Dolphin",
                    "UNBF" = "Unidentified Blackfish",
                    "UNCW" = "Unidentified Common or White-sided Dolphin",
                    "UNDO" = "Unidentified Dolphin/Porpoise",
                    "UNGD" = "Spotted or Bottlenose Dolphin",
                    "UNKO" = "Pygmy or Dwarf Sperm Whale", 
                    "UNLD" = "Unidentified *Lagenorhynchus*",
                    "UNSB" = "Unidentified Small Blackfish", 
                    "UNST" = "Unidentified *Stenella*",
                    "WBDO" = "White-Beaked Dolphin",
                    "WSDO" = "Atlantic White-Sided Dolphin", 
                    "BESE" = "Bearded Seal", 
                    "GRSE" = "Gray Seal", 
                    "HASE" = "Harbor Seal", 
                    "HGSE" = "Harp or Gray Seal",
                    "HOSE" = "Hooded Seal", 
                    "HPSE" = "Harp Seal", 
                    "MANA" = "Manatee", 
                    "PINN" = "Unidentified Pinniped", 
                    "POBE" = "Polar Bear", 
                    "RISE" = "Ringed Seal", 
                    "UNSE" = "Unidentified Seal", 
                    "WALR" = "Walrus", 
                    "GRTU" = "Green Turtle", 
                    "HATU" = "Hawksbill Turtle", 
                    "LETU" = "Leatherback Turtle", 
                    "LOTU" = "Loggerhead Turtle", 
                    "ORTU" = "Olive Ridley Sea Turtle", 
                    "RITU" = "Kemp's Ridley Turtle", 
                    "UNTU" = "Unidentified Turtle", 
                    "ANSH" = "Angel Shark", 
                    "BASH" = "Basking Shark", 
                    "BLSH" = "Blue Shark", 
                    "DUSH" = "Dusky Shark", 
                    "GHSH" = "Great Hammerhead Shark", 
                    "HHSH" = "Hammerhead Shark", 
                    "LMSH" = "Long-finned Mako Shark", 
                    "SDOG" = "Spiny Dogfish", 
                    "SMSH" = "Short-finned Mako Shark",
                    "THSH" = "Thresher Shark", 
                    "TISH" = "Tiger Shark", 
                    "UNSH" = "Unidentified/Other Shark", 
                    "WHSH" = "Whale Shark", 
                    "WTSH" = "White Shark", 
                    "BFTU" = "Bluefin Tuna", 
                    "BLFI" = "Bluefish", 
                    "CDRA" = "Chilean Devil Ray", 
                    "CNRA" = "Cow-Nosed Ray",  
                    "FLFI" = "Flying Fish", 
                    "MAHI" = "Mahi-mahi/Dolphin-fish", 
                    "MARA" = "Manta Ray", 
                    "MOBU" = "Mobulid ray, not identified to species", 
                    "OCSU" = "Ocean Sunfish", 
                    "OTBI" = "Other Billfish", 
                    "SCFI" = "Fish School", 
                    "SCRA" = "School of Rays", 
                    "SWFI" = "Swordfish", 
                    "TUNS" = "Unidentified Tuna", 
                    "UNFI" = "Unidentified/Other Fish", 
                    "UNRA" = "Unidentified/Other Ray", 
                    "WHMA" = "White Marlin", 
                    "YFTU" = "Yellowfin Tuna", 
                    "AMAL" = "American Alligator", 
                    "JELL" = "Jellyfish", 
                    "LMJE" = "Lion's-Mane Jellyfish",
                    "PMOW" = "Portuguese Man of War", 
                    "UNCE" = "Unidentified Cetacean", 
                    "UNID" = "Unidentified Animal", 
                    "UNMM" = "Unidentified Marine Mammal"))
}
riwh_surv1 <- dat %>%
  filter(speccode == "RIWH" | speccode == "HUWH" | speccode == "MIWH" |
           speccode == "UNLW" | speccode == "SADO" | speccode == "WSDO" |
           speccode == "UNDO" | speccode == "UNSE" | speccode == "FIWH" | 
           speccode == "BEWH" | speccode == "BLWH" | speccode == "BODO" | 
           speccode == "GRAM" | speccode == "GRSE" | speccode == "HAPO" |
           speccode == "HASE" | speccode == "KIWH" | speccode == "LETU" |
           speccode == "LFPW" | speccode == "MANA" | speccode == "OBDO" |
           speccode == "PINN" | speccode == "PIWH" | speccode == "SEWH" |
           speccode == "SFPW" | speccode == "SPWH" | speccode == "UNFS" ) %>%
  mutate(new.speccode = kenney2commonNoLatin(speccode)) %>%
  arrange(desc(number))




mm_levs <-  
  c(
    'Right Whale',
    'Humpback Whale',
    'Fin Whale',
    'Minke Whale',
    'Sei Whale',
    'Sperm Whale',
    'Blue Whale',
    'Unidentified Large Whale',
    'Unidentified Fin or Sei Whale',
    'Common Dolphin', 
    'Bottlenose Dolphin',
    'Atlantic White-sided Dolphin',
    'Pilot Whale',
    'Harbor Porpoise',
    'Striped Dolphin',
    'Rissos Dolphin',
    'Unidentified Dolphin/Porpoise', 
    'Gray Seal', 
    'Harbor Seal',
    'Unidentified Seal',
    'Leatherback Turtle',
    'Loggerhead Turtle',
    'Kemps Ridley Turtle',
    'Unidentified Turtle')
mm_pal <-  
  c("#FF0B0B",
    "#0156FF",
    "#068100",
    "#E3FA20",
    "#D089F4",
    "#8dd3c7",
    "#80b1d3",
    "#252525",
    "#252525",
    "#FF850B",
    "#b2df8a",
    "#ffff99",
    "#6a3d9a",
    "#b15928",
    "#fb9a99",
    "#fb8072",
    "#f0f0f0",
    "#33FFFF",
    "#662506",
    "#C0BDC1",
    "#99d8c9",
    "#ae017e",
    "#9ebcda",
    "#111011" 
    
  )


sightColors <-colorFactor(palette = mm_pal, domain = mm_levs, ordered = TRUE) 


statesf <- st_read("C://Users//oobrien//Documents//GIS//States_DOT_unkwn.shp")
#graticule <- st_read("C://Users//oobrien//Documents//R_Work_Directory//PostFlightReport//GraticuleDegrees.shp")
#base <- st_read(
#  "./images/MA_Coastal_Zone/MA_Coastal_Zone_Boundary.shp")


#my_title <- ("Active SMAs in red, active DMAs or Slow Zones in yellow")
#my_title <- tags$p(tags$style("p {color: black; font-size:14px}"),
#                   tags$b("Active SMAs in red, active DMAs or Slow Zones in yellow
#                         "))

surveymap <- 
  leaflet(options = leafletOptions(zoomSnap = 0.05, zoomDelta = 0.10)) %>%
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # add another layer with place names
  
    addPolygons(data = statesf, color = "tan", fill = "grey", weight = 0.5, fillOpacity=1) %>% 
    addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
fitBounds(
  lng1 = min_long,
  lat1 = min_lat,
  lng2 = max_long,
  lat2 = max_lat
) %>%
    addPolylines(data = dat, 
                 lng = dat$long,
                 lat = dat$lat,
                 fillColor = "black",
                 weight = 0.75,
                 opacity = 0.5, 
                 stroke = TRUE,
                 color = "black") %>%  
    addCircleMarkers(data = riwh_surv1, 
                     lng = riwh_surv1$long, 
                     lat = riwh_surv1$lat,
                     weight = 0.5,
                     fillColor = ~sightColors(new.speccode),
                     color = "black",
                     radius = 4,
                     fillOpacity = 1,
                     stroke = TRUE,
                     # radius = NUMBER,
                     label = ~paste0(number, ' ', new.speccode),
                     group = 'Points') %>%
    addPolygons(lng=dma1$dma1_long, lat=dma1$lat, color="yellow", weight=0.5, fillColor="yellow")%>%
  addScaleBar(position = 'bottomright') %>%
  
    
 #   addControl(my_title, position = "bottomright" ) %>%
    addLegend("bottomleft", pal = sightColors, values = riwh_surv1$new.speccode 
              ) %>%
  setView(lng = (mid_long-.5), lat = mid_lat, zoom = 8) 

 # mapshot(surveymap, file = paste0(target_dir, "/", date, "map.png")#,
    #      remove_controls = c("homeButton", "layersControl",  "easyButton", "drawToolbar")
  #        )


