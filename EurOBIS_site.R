library(vegan)
library(readr)
library(Hmisc)
require(leaflet)
library(dplyr)


#import files downloaded from EurOBIS IPT
imr_fish_eggs_survey_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_fish_eggs_survey-v1.1/occurrence.txt",
                                              "\t", escape_double = FALSE, trim_ws = TRUE)
imr_fish_eggs_survey_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_fish_eggs_survey-v1.1/measurementorfact.txt",
                                                     "\t", escape_double = FALSE, trim_ws = TRUE)
imr_juvenile_fish_monitoring_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_juveline_fish_monitoring-v1.1/occurrence.txt",
                                                      "\t", escape_double = FALSE, trim_ws = TRUE)
imr_juvenile_fish_monitoring_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_juveline_fish_monitoring-v1.1/measurementorfact.txt",
                                                             "\t", escape_double = FALSE, trim_ws = TRUE)
imr_mareano_beamtrawl_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_mareano_beamtrawl-v1.1/occurrence.txt",
                                               "\t", escape_double = FALSE, trim_ws = TRUE)
imr_mareano_beamtrawl_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_mareano_beamtrawl-v1.1/measurementorfact.txt",
                                                      "\t", escape_double = FALSE, trim_ws = TRUE)
imr_mareano_grab_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_mareano_grab-v1.1/occurrence.txt",
                                          "\t", escape_double = FALSE, trim_ws = TRUE)
imr_mareano_grab_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_mareano_grab-v1.1/measurementorfact.txt",
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
imr_mareano_rpsledge_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_mareano_rpsledge-v1.1/occurrence.txt",
                                              "\t", escape_double = FALSE, trim_ws = TRUE)
imr_mareano_rpsledge_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_mareano_rpsledge-v1.1/measurementorfact.txt",
                                                     "\t", escape_double = FALSE, trim_ws = TRUE)
imr_zoopl_northsea_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_zoopl_north_sea-v1.1/occurrence.txt",
                                            "\t", escape_double = FALSE, trim_ws = TRUE)
imr_zoopl_northsea_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_zoopl_north_sea-v1.1/measurementorfact.txt",
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
imr_zoopl_norwsea_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_zoopl_norw_sea-v1.1/occurrence.txt",
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
imr_zoopl_norwsea_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-imr_zoopl_norw_sea-v1.1/measurementorfact.txt",
                                                  "\t", escape_double = FALSE, trim_ws = TRUE)
# nsbp_cochrane_occurrence <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-nsbp_cochrane-v1.1/occurrence.txt",
#                                        "\t", escape_double = FALSE, trim_ws = TRUE)
# nsbp_cochrane_measurementorfact <- read_delim("~/OBIS/MBONs/GEOBON MBON/SDG14Project/dwca-nsbp_cochrane-v1.1/measurementorfact.txt",
#                                               "\t", escape_double = FALSE, trim_ws = TRUE)

occurrence_combined <- rbind(imr_fish_eggs_survey_occurrence, imr_juvenile_fish_monitoring_occurrence, imr_mareano_beamtrawl_occurrence, 
                             imr_mareano_grab_occurrence, imr_mareano_rpsledge_occurrence, imr_zoopl_northsea_occurrence, 
                             imr_zoopl_norwsea_occurrence)

# Separating out occurrences that are not identified to at lease the genus level and then grabbing the unique scientific names
occurrence_combined_speciesonly <- occurrence_combined[!is.na(occurrence_combined$genus),]
occurrence_combined_genusonly_unique <- occurrence_combined_speciesonly[!duplicated(occurrence_combined_speciesonly$scientificName),]

# Grouping occurrences by phylum and determining the number of observations, then creating a nice looking table of the info
o = occurrence_combined

o %>% 
  group_by(phylum) %>%
  summarize(
    n = n()) %>%
  arrange(desc(n)) %>%
  knitr::kable()

# Combining all the measurement or fact files into one file
mof_combined <- rbind(imr_fish_eggs_survey_measurementorfact, imr_juvenile_fish_monitoring_measurementorfact, imr_mareano_beamtrawl_measurementorfact, 
                             imr_mareano_grab_measurementorfact, imr_mareano_rpsledge_measurementorfact, imr_zoopl_northsea_measurementorfact, 
                             imr_zoopl_norwsea_measurementorfact)
mof_abundanceonly <- mof_combined[which(mof_combined$measurementType== "Abundance"),]

# Merging the measurement or fact abundance only table with the occurrences
oc_mof <- left_join(occurrence_combined, mof_abundanceonly, by = "id")

# Plot over time the abundance for each taxonomic group, just a test at first to see what it looks like need to keep in mind units
annelida_abundance <- oc_mof[which(oc_mof$phylum == "Annelida"),]

annelida_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

arthropoda_abundance <- oc_mof[which(oc_mof$phylum == "Arthropoda"),]

arthropoda_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

mollusca_abundance <- oc_mof[which(oc_mof$phylum == "Mollusca"),]

mollusca_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

enchinodermata_abundance <- oc_mof[which(oc_mof$phylum == "Echinodermata"),]

enchinodermata_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

# Break up Chordata to smaller taxonomic levels
chordata_abundance <- oc_mof[which(oc_mof$phylum == "Chordata"),]
chor <- chordata_abundance
chor %>% 
  group_by(class) %>%
  summarize(
    n = n()) %>%
  arrange(desc(n)) %>%
  knitr::kable()

# Try to make abundances equivalent (so convert 100 m to 1 m units)
# Try to take into account level of effort somehow? Number of collection days?
  







