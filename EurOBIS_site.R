library(vegan)
library(Hmisc)
require(leaflet)
library(tidyverse) # includes readr, dplyr
library(stringr)

# set host-specific path to data files that are too big for Github
dir_data = switch(
  R.utils::System$getHostname(),
  'Ben-Bests-Macbook-Pro.local' = '~/Google Drive/projects/mbon/products/infographics/mbon_sdg14_eurobis')

#import files downloaded from EurOBIS IPT
d = tibble(
  dir     = list.dirs(dir_data, recursive=F),
  occ_txt = file.path(dir, 'occurrence.txt'),
  name    = str_replace(basename(dir), 'dwca-imr_(.*)-v1.1', '\\1'),
  data    = map(occ_txt, function(x) read_delim(file=x, delim='\t', escape_double=F, trim_ws=T))) %>%
  unnest()
# Error in bind_rows_(x, .id) : Column `eventDate` can't be converted from POSIXct, POSIXt to character

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
mof_combined <- rbind(imr_juvenile_fish_monitoring_measurementorfact, imr_mareano_beamtrawl_measurementorfact, 
                             imr_mareano_grab_measurementorfact, imr_mareano_rpsledge_measurementorfact, imr_zoopl_northsea_measurementorfact, 
                             imr_zoopl_norwsea_measurementorfact)
mof_abundanceonly <- mof_combined[which(mof_combined$measurementType== "Abundance"),]

# Merging the measurement or fact abundance only table with the occurrences
oc_mof <- left_join(occurrence_combined, mof_abundanceonly, by = "id")

## To make the abundances more equivalent converted: 
# number per 100 square meter to number per square meter
oc_mof$measurementValue <- ifelse (oc_mof$measurementUnit == "Number per 100 square meter", oc_mof$measurementValue/100, oc_mof$measurementValue*1)
# oc_mof$measurementUnit <- ifelse(oc_mof$measurementUnit == "Number per 100 square meter", "Number per square meter", ) # not sure what to put in the else
# number per nautical mile to number per meter
oc_mof$measurementValue <- ifelse(oc_mof$measurementUnit == "number per nautic mile with Pelagic trawl, Opening: 18x18m", oc_mof$measurementValue/1852,
                                  oc_mof$measurementValue*1)
oc_mof$measurementValue <- ifelse(oc_mof$measurementUnit == "number per nautic mile with Pelagic trawl, Opening: 29x29m", oc_mof$measurementValue/1852,
                                  oc_mof$measurementValue*1)

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

mollusca_abundance <- oc_mof[which(oc_mof$phylum == "Mollusca"),] #should I break up mollusca like I did with chordata?
mollusca_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

enchinodermata_abundance <- oc_mof[which(oc_mof$phylum == "Echinodermata"),] #echinoderms/sea stars/sea urchins/sand dollars/sea cucumbers
enchinodermata_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

chaetognatha_abundance <- oc_mof[which(oc_mof$phylum == "Chaetognatha"),] # arrow worms
chaetognatha_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

cnidaria_abundance <- oc_mof[which(oc_mof$phylum == "Cnidaria"),] #jellyfish/corals/hydrozoans/box jellies
cnidaria_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

porifera_abundance <- oc_mof[which(oc_mof$phylum == "Porifera"),] #sponges
porifera_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

sipuncula_abundance <- oc_mof[which(oc_mof$phylum == "Sipuncula"),] #peanut worms
sipuncula_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

bryozoa_abundance <- oc_mof[which(oc_mof$phylum == "Bryozoa"),] #sea mats
bryozoa_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

brachiopoda_abundance <- oc_mof[which(oc_mof$phylum == "Brachiopoda"),] #lamp shells
brachiopoda_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

nemertea_abundance <- oc_mof[which(oc_mof$phylum == "Nemertea"),] #ribbon worms
nemertea_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

nematoda_abundance <- oc_mof[which(oc_mof$phylum == "Nematoda"),] #round worms
nematoda_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

hemichordata_abundance <- oc_mof[which(oc_mof$phylum == "Hemichordata"),] #hemichordates (acorn worms)
hemichordata_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

ctenophora_abundance <- oc_mof[which(oc_mof$phylum == "Ctenophora"),] #comb jellies
ctenophora_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

sarcomastigophora_abundance <- oc_mof[which(oc_mof$phylum == "Sarcomastigophora"),] #protozoans
sarcomastigophora_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

foraminifera_abundance <- oc_mof[which(oc_mof$phylum == "Foraminifera"),] #forams (amoeboid protists)
foraminifera_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

radiozoa_abundance <- oc_mof[which(oc_mof$phylum == "Radiozoa"),] #radiolarians
radiozoa_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

platyhelminthes_abundance <- oc_mof[which(oc_mof$phylum == "Platyhelminthes"),] #flatworms
platyhelminthes_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

haptophyta_abundance <- oc_mof[which(oc_mof$phylum == "Haptophyta"),] #algae
haptophyta_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

rotifera_abundance <- oc_mof[which(oc_mof$phylum == "Rotifera"),] #rotifers (wheel animals)
rotifera_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

cephalorhyncha_abundance <- oc_mof[which(oc_mof$phylum == "Cephalorhyncha"),]
cephalorhyncha_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()


# Break up Chordata to smaller taxonomic level (class), leaving out Holocephali and Myxini because too few observations
chordata_abundance <- oc_mof[which(oc_mof$phylum == "Chordata"),]
chor <- chordata_abundance
chor %>% 
  group_by(class) %>%
  summarize(
    n = n()) %>%
  arrange(desc(n)) %>%
  knitr::kable()

actinopterygii_abundance <- oc_mof[which(oc_mof$class == "Actinopterygii" | oc_mof$class == "Actinopteri"),] #ray finned fish
actinopterygii_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

tunicate_abundance <- chordata_abundance[which(chordata_abundance$class == "Appendicularia" | chordata_abundance$class == "Ascidiacea" | 
                                                 chordata_abundance$class == "Thaliacea"),] #tunicates
tunicate_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()

elasmobranchiii_abundance <- oc_mof[which(oc_mof$class == "Elasmobranchii"),]
elasmobranchiii_abundance %>%
  group_by(year) %>%
  summarize(avg = mean(measurementValue)) %>%
  arrange(avg) %>%
  knitr::kable()




# Try to take into account level of effort somehow? Number of collection days?
  







