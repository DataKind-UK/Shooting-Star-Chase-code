FROM_FOLDER <- "data/second-round-anonymised-DO-NOT-DISTRIBUTE"
TO_FOLDER <- "data/third-round-anonymised"
CENSUS_HOUSEHOLD_COMPOSITION <- "data/census_2011_household_composition_by_postcode_sector.csv"
MIN_HOUSEHOLDS_WITH_CHILDREN_IN_POSTCODE_SECTOR <- 200

library(dplyr)
options(stringsAsFactors = FALSE)

# empty/create target folder
unlink(TO_FOLDER, recursive = TRUE)
dir.create(TO_FOLDER)

# postcodes referenced in SCC's data
referenced_postcode_sectors <- read.csv(paste0(FROM_FOLDER, "/ChildDemographic.csv")$Postcode
# remove spaces
referenced_postcode_sectors <- gsub("\\s", "", referenced_postcode_sectors)
# drop empty postcodes
referenced_postcode_sectors <- referenced_postcode_sectors[referenced_postcode_sectors != ""]
# to upper cases and unique values only 
referenced_postcode_sectors <- unique(toupper(referenced_postcode_sectors))

# Census 2011's household composition data, by postcode sector
# source: http://www.nomisweb.co.uk/census/2011/ks105ew
household_composition <- read.csv(CENSUS_HOUSEHOLD_COMPOSITION)
# remove spaces
household_composition$geography <- gsub("\\s","", household_composition$geography)
# drop empty postcodes (likely unnecessary here)
household_composition <- household_composition[household_composition$geography != "", ]
# to upper cases  (likely unnecessary here) 
household_composition$geography <- toupper(household_composition$geography)
# keep just the ones references in the SSC data
household_composition <- household_composition[household_composition$geography %in% referenced_postcode_sectors, ]

# calculate the total number of households with children across the more 
# detailed categories the Census uses
household_composition$no_of_households_with_children <- household_composition$Household.Composition..One.family.only..Married.or.same.sex.civil.partnership.couple..Dependent.children..measures..Value + household_composition$Household.Composition..One.family.only..Married.or.same.sex.civil.partnership.couple..All.children.non.dependent..measures..Value + household_composition$Household.Composition..One.family.only..Cohabiting.couple..Dependent.children..measures..Value + household_composition$Household.Composition..One.family.only..Cohabiting.couple..All.children.non.dependent..measures..Value + household_composition$Household.Composition..One.family.only..Lone.parent..Dependent.children..measures..Value + household_composition$Household.Composition..One.family.only..Lone.parent..All.children.non.dependent..measures..Value + household_composition$Household.Composition..Other.household.types..With.dependent.children..measures..Value 

# drop the other columns
household_composition <- household_composition[, c("geography", "no_of_households_with_children")]

# postcode sectors to remove from the data
postcode_sectors_to_remove <- household_composition[household_composition$no_of_households_with_children < MIN_HOUSEHOLDS_WITH_CHILDREN_IN_POSTCODE_SECTOR, ]$geography

child_demographic <- read.csv(paste0(FROM_FOLDER, "/ChildDemographic.csv"))
# remove spaces from the postcodes and change to upper case
child_demographic$Postcode <- toupper(gsub("\\s","", child_demographic$Postcode))
# identify the child ids to remove
family_ids_to_remove <- unique(child_demographic[child_demographic$Postcode %in% postcode_sectors_to_remove, ]$Family.Id)

# **************************************************************************** #
# ChildDemographic.csv
# **************************************************************************** #

child_demographic <- read.csv(paste0(FROM_FOLDER, "/ChildDemographic.csv"))
child_demographic_clean <- child_demographic[!(child_demographic$Family.Id %in% family_ids_to_remove), ]
write.csv(child_demographic_clean, paste0(TO_FOLDER, "/ChildDemographic.csv"), row.names = FALSE)
rm(child_demographic, child_demographic_clean)

# **************************************************************************** #
# FamilyMembers.csv
# **************************************************************************** #

family_members <- read.csv(paste0(FROM_FOLDER, "/FamilyMembers.csv"))
family_members_clean <- family_members[!(family_members$Family.Id %in% family_ids_to_remove), ]
write.csv(family_members_clean, paste0(TO_FOLDER, "/FamilyMembers.csv"), row.names = FALSE)
rm(family_members, family_members_clean)

# **************************************************************************** #
# Files that do not need processing
# **************************************************************************** #

file.copy(sapply(setdiff(dir(FROM_FOLDER), dir(TO_FOLDER)), function (x) paste0(FROM_FOLDER, "/", x)), TO_FOLDER)
