library(dplyr)

postcodes_enumeration <- read.csv("GitHub/data/other-sources/Postcodes_(Enumeration)_(2011)_to_parishes_(2011)_to_wards_(2011)_to_local_authority_districts_(2011)_E+W_lookup/pcd11_par11_wd11_lad11_ew_lu.csv")
postcodes_enumeration$Postcode <- gsub("\\s", "", postcodes_enumeration$pcd7)
postcodes_enumeration$Postcode <- toupper(postcodes_enumeration$Postcode)
postcodes_enumeration$Postcode <- substr(postcodes_enumeration$Postcode, 1, 4)
postcodes_enumeration <- postcodes_enumeration[, c('Postcode', 'lad11cd', 'lad11nm')]
postcodes_enumeration <- unique(postcodes_enumeration)

child_demographic <- read.csv("GitHub/data/anonymised/ChildDemographic.csv")
child_demographic$Postcode <- gsub("\\s", "", child_demographic$Postcode)
child_demographic$Postcode <- toupper(child_demographic$Postcode)
child_demographic$Postcode <- substr(child_demographic$Postcode, 1, 4)
child_demographic$Postcode <- ifelse(child_demographic$Postcode %in% unique(postcodes_enumeration$Postcode), child_demographic$Postcode, NA)

child_demographic_with_LA <- left_join(child_demographic, postcodes_enumeration)
