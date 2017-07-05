# load libraries
library(vegan)
library(Hmisc)
require(leaflet)
library(tidyverse) # includes readr, dplyr
library(stringr)

# set host-specific path to data files that are too big for Github
dir_data = switch(
  R.utils::System$getUsername(),
  'bbest'    = '~/Google Drive/projects/mbon/products/infographics/mbon_sdg14_eurobis',
  'albenson' = '~/OBIS/MBONs/GEOBON MBON/SDG14Project')

# import files downloaded from EurOBIS IPT
d = tibble(
  dir      = list.dirs(dir_data, recursive=F),
  occ_txt  = file.path(dir, 'occurrence.txt'),
  mof_txt  = file.path(dir, 'measurementorfact.txt'),
  name     = str_replace(basename(dir), 'dwca-imr_(.*)-v1.1', '\\1'),
  occ_data = map(occ_txt, function(x) read_delim(file=x, delim='\t', escape_double=F, trim_ws=T)),
  mof_data = map(mof_txt, function(x) read_delim(file=x, delim='\t', escape_double=F, trim_ws=T)))
# TODO: sort measurementorfact.txt Warning: 538 parsing failures... measurementValue no trailing characters
# d[,-c(1:3)] # d0 = d # d = d0

# combine occurrence data
occ = d %>%
  mutate(
    # fix for unnest Error in bind_rows_(x, .id) : Column `eventDate` can't be converted from POSIXct, POSIXt to character
    occ_data = map(occ_data, function(x) mutate(x, eventDate = as.character(eventDate)))) %>%
  select(name, occ_data) %>%
  unnest()

# combine measurement or fact data
mof = d %>%
  select(name, mof_data) %>%
  unnest()

# merge occurrences with measurements
occ_mof = occ %>%
  # establish group to default by phylum
  mutate(
    group = sprintf('phylum %s', phylum)) %>%
  # add group for ray-finned fishes
  bind_rows(
    occ %>%
      filter(
        class %in% c('Actinopterygii','Actinopteri')) %>%
      mutate(
        group = 'class Actinopterygii')) %>%
  # add group for tunicates
  bind_rows(
    occ %>%
      filter(
        class %in% c('Appendicularia','Ascidiacea','Thaliacea')) %>%
      mutate(
        group = 'subphylum Tunicata')) %>%
  # join measurement data
  left_join(
    mof %>%
      filter(measurementType =='Abundance'), 
    by='id')

# summarize by group and year
occ_mof %>%
  group_by(group, year) %>%
  summarize(
    avg = mean(measurementValue, na.rm=T),
    n   = n()) %>%
  #arrange(phylum, desc(avg)) %>% # NOTE: group_by(group, year) takes care of sensible ordering of rows
  knitr::kable()


# TODO: figure out measurementUnit -> measurementValue conversions
# TODO: Try to take into account level of effort somehow? Number of collection days?
table(mof$measurementUnit)
# table(mof$measurementUnit) %>% names() %>% paste(collapse='\n') %>% cat
#   Gram biomass per 100 square meter
#   Number per 100 square meter
#   number per nautic mile with Pelagic trawl, Opening: 18x18m
#   number per nautic mile with Pelagic trawl, Opening: 29x29m
#   number per square meter

# measurementUnit -> measurementValue, original ----
# oc_mof$measurementValue <- ifelse (oc_mof$measurementUnit == "Number per 100 square meter", oc_mof$measurementValue/100, oc_mof$measurementValue*1)
# # oc_mof$measurementUnit <- ifelse(oc_mof$measurementUnit == "Number per 100 square meter", "Number per square meter", ) # not sure what to put in the else
# # number per nautical mile to number per meter
# oc_mof$measurementValue <- ifelse(oc_mof$measurementUnit == "number per nautic mile with Pelagic trawl, Opening: 18x18m", oc_mof$measurementValue/1852,
#                                   oc_mof$measurementValue*1)
# oc_mof$measurementValue <- ifelse(oc_mof$measurementUnit == "number per nautic mile with Pelagic trawl, Opening: 29x29m", oc_mof$measurementValue/1852,
#                                   oc_mof$measurementValue*1)

# # original, not used ----
# # Separating out occurrences that are not identified to at lease the genus level and then grabbing the unique scientific names
# occurrence_combined_speciesonly <- occurrence_combined[!is.na(occurrence_combined$genus),]
#   
# # Separating out occurrences that are not identified to at lease the genus level and then grabbing the unique scientific names
# occurrence_combined_speciesonly <- occurrence_combined[!is.na(occurrence_combined$genus),]
# occurrence_combined_genusonly_unique <- occurrence_combined_speciesonly[!duplicated(occurrence_combined_speciesonly$scientificName),]
