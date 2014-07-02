FROM_FOLDER <- "data/first-round-anonymised-submission-2-DO-NOT-DISTRIBUTE"
TO_FOLDER <- "data/second-round-anonymised-DO-NOT-DISTRIBUTE"
options(stringsAsFactors = FALSE)

# empty/create target folder
unlink(TO_FOLDER, recursive = TRUE)
dir.create(TO_FOLDER)

# **************************************************************************** #
# Files that will be dropped, e.g. because after anonymisation they contain
# no relevant information 
# **************************************************************************** #

files_to_drop <- c(
    "FamilyID.csv",
    "FamilyIDTable.csv"
)

# **************************************************************************** #
# ChildDemographic.csv
# **************************************************************************** #

child_demographic <- read.csv(paste0(FROM_FOLDER, "/ChildDemographic.csv"))
# drop the columns that are risky or not useful
child_demographic_clean <- child_demographic[, !(colnames(child_demographic) %in% c(
    'Borough',
    'First.Language', 
    'Second.Language',
    'Interpreter.needed',
    'DateofBirth', 
    'DateOfDeath'
))]
# create new columns with only the year of birth and death
child_demographic_clean$YearOfBirth <- as.numeric(sapply(child_demographic$DateofBirth, function (x) strsplit(x, "/")[[1]][2]))
child_demographic_clean$YearOfDeath <- as.numeric(sapply(child_demographic$DateOfDeath, function (x) strsplit(x, "/")[[1]][2]))
# save
write.csv(child_demographic_clean, paste0(TO_FOLDER, "/ChildDemographic.csv"), row.names = FALSE)
rm(child_demographic, child_demographic_clean)

# **************************************************************************** #
# FamilyMembers.csv
# **************************************************************************** #

family_members <- read.csv(paste0(FROM_FOLDER, "/FamilyMembers.csv"))
# drop the columns that are risky or not useful
family_members_clean <- family_members[, !(colnames(family_members) %in% c(
    'DateOfBirth',
    'Gender', # the same information is already implied by the "Relationship" column,
    'Postcode'
))]
# replace the postcode information with less revealing information about 
# the family member living or not with the child
family_members_clean$lives_with_child <- family_members$Postcode == "With Child"
# save
write.csv(family_members_clean, paste0(TO_FOLDER, "/FamilyMembers.csv"), row.names = FALSE)
rm(family_members, family_members_clean)

# **************************************************************************** #
# Referrals.csv
# **************************************************************************** #

referrals <- read.csv(paste0(FROM_FOLDER, "/Referrals.csv"))
referrals_clean <- referrals
# identify reasons for withdrawn/closed referrals with occurrence <= 10, e.g.
# there is only 1 "Moved to [some place name]"
withdrawn_closed_reasons <- data.frame(reason = unique(referrals_clean$Reason.Case.Withdrawn.closed))
withdrawn_closed_reasons$occurrences <- sapply(withdrawn_closed_reasons$reason, function (r) nrow(referrals[referrals$Reason.Case.Withdrawn.closed == r,]))
withdrawn_closed_reasons <- withdrawn_closed_reasons[withdrawn_closed_reasons$occurrences <= 10, ]$reason
# replacing with "Other" all cases above 
referrals_clean$Reason.Case.Withdrawn.closed <- ifelse(referrals_clean$Reason.Case.Withdrawn.closed %in% withdrawn_closed_reasons, "Other", referrals_clean$Reason.Case.Withdrawn.closed)
# save
write.csv(referrals_clean, paste0(TO_FOLDER, "/Referrals.csv"), row.names = FALSE)
rm(referrals, referrals_clean)

# **************************************************************************** #
# Files that do not need processing
# **************************************************************************** #

file.copy(sapply(setdiff(dir(FROM_FOLDER), union(files_to_drop, dir(TO_FOLDER))), function (x) paste0(FROM_FOLDER, "/", x)), TO_FOLDER)
