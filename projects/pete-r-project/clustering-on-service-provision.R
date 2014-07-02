# This script loads the patient activity detail, creates an aggregated features
# table and then produces a clustering model based on service provision


# libraries
library(stringr)
library(reshape2)
library(stats)


# Load the data
ActivityInvitees <- read.csv("../../data/anonymised/ActivityInvitees.csv", stringsAsFactors=FALSE)
ActivityTable <- read.csv("../../data/anonymised/ActivityTable.csv", stringsAsFactors=FALSE)
ChildDemographic <- read.csv("../../data/anonymised/ChildDemographic.csv", stringsAsFactors=FALSE)
FamilyMembers <- read.csv("../../data/anonymised/FamilyMembers.csv", stringsAsFactors=FALSE)
ChristophersVisitRecords <- read.csv("../../data/anonymised/ChristophersVisitRecords.csv", stringsAsFactors=FALSE)
Referrals <- read.csv("../../data/anonymised/Referrals.csv", stringsAsFactors=FALSE)
HospiceVisitBreakdown <- read.csv("../../data/anonymised/HospiceVisitBreakdown.csv")
CP_Dependency <- read.csv("../../data/anonymised/CP_Dependency.csv")


# Finding unique activity titles:
uniqueActivities <- as.data.frame(tolower(ActivityTable$Title), stringsAsFactors=FALSE)    # Make everything lower case
uniqueActivities[uniqueActivities[[1]]=='',1] <- 'unknown'  #Turn blanks into unknown values
uniqueActivities[[1]] <- gsub("[0-9]+[a-zA-Z][a-zA-Z] +[a-zA-Z]+ +[0-9][0-9]+", "", uniqueActivities[[1]])# Turn written dates into blanks
uniqueActivities[[1]] <- gsub("[0-9]+\\.[0-9]+\\.[0-9]+", "", uniqueActivities[[1]]) # Turn [0?.0?.0?] dates into blanks
uniqueActivities[[1]] <- gsub("[0-9]+\\.[0-9][0-9] *: *[0-9]+\\.[0-9][0-9]", "", uniqueActivities[[1]])# Turn [00.00 : 00.00] time ranges into blanks
uniqueActivities[[1]] <- gsub("[0-9]+\\.[0-9][0-9] *- *[0-9]+\\.[0-9][0-9]", "", uniqueActivities[[1]])# Turn [00.00 - 00.00] time ranges into blanks
uniqueActivities[[1]] <- gsub("[0-9]+:[0-9][0-9] *: *[0-9]+:[0-9][0-9]", "", uniqueActivities[[1]])# Turn [00:00 : 00:00] time ranges into blanks
uniqueActivities[[1]] <- gsub("[0-9]+:[0-9][0-9] *- *[0-9]+:[0-9][0-9]", "", uniqueActivities[[1]])# Turn [00:00 - 00:00] time ranges into blanks
uniqueActivities[[1]] <- gsub("[0-9]+\\.[0-9][0-9]", "", uniqueActivities[[1]]) # Turn [00.00] times into blanks
uniqueActivities[[1]] <- gsub("[0-9]+:[0-9][0-9]", "", uniqueActivities[[1]]) # Turn [00:00] times into blanks
uniqueActivities[[1]] <- gsub("20[0-9][0-9]", "", uniqueActivities[[1]])# Turn [20xx] years into blanks
uniqueActivities[[1]] <- gsub("-", "", uniqueActivities[[1]]) # Turn hyphens into blanks
uniqueActivities[[1]] <- gsub("\'", "", uniqueActivities[[1]]) # Turn apostrophes into blanks
uniqueActivities[[1]] <- str_trim(uniqueActivities[[1]], side="both") # Remove leading and trailing blanks


# When first ten characters are the same, make both entries the shortest entry <<<<< NOT CHECKED VALUE OF 10, just inspection
# When levenshtein <= 3 make both entries the shortest entry
colnames(uniqueActivities) <- c('title')
postLeven <- list()
mylength <- length(uniqueActivities[[1]])
comparisonSet <- unique(uniqueActivities[[1]])
for (i in 1:mylength) {
  
  # Create the current set for comparison(i.e. not itself!)
  currComparisonSet <- comparisonSet[comparisonSet!=uniqueActivities[i,1]]
  currentValue <- uniqueActivities[i,1]
  
  for (j in 1:length(currComparisonSet)) {
  
    # If levenshtein distance less than 4, replace
    if (adist(currentValue, currComparisonSet[j]) < 4) {
      shortestEntry <- character()
      if (nchar(currentValue) <= nchar(currComparisonSet[j])) {
        shortestEntry <- currentValue
      } else {shortestEntry <- currComparisonSet[j]}
      currentValue <- shortestEntry
    
    # If first ten characters match, replace 
    } else if (substr(currentValue,1,10) == substr(currComparisonSet[j],1,10)) {
      shortestEntry <- character()
      if (nchar(currentValue) <= nchar(currComparisonSet[j])) {
        shortestEntry <- currentValue
      } else {shortestEntry <- currComparisonSet[j]}
      currentValue <- shortestEntry 
    }
     
  }
  postLeven <- c(postLeven, currentValue)
  print(i)

}
uniqueActivities<- as.data.frame(as.character(postLeven), stringsAsFactors=FALSE)


# Append this back to the activities Table
colnames(uniqueActivities) <- c('cleaned_title')
ActivityTable <- cbind(ActivityTable, uniqueActivities)


# Group up the activities by child
ActivityMerged <- merge(ActivityInvitees, ActivityTable)
ActivityMergedColsForSum <- ActivityMerged[,c(2,7,21)]
ActivityCountByChild <- aggregate(ActivityMergedColsForSum[[2]], by=list(ChildID=ActivityMergedColsForSum[[1]],Title=ActivityMergedColsForSum[[3]]), FUN=sum, na.rm=TRUE)
ActivityCountByChildSorted <- ActivityCountByChild[order(ActivityCountByChild$x, decreasing=TRUE),]


# Cast the data frame to produce vectors 
ActivityCountByChild_Flat <- dcast(ActivityCountByChild, ActivityCountByChild$ChildID~ActivityCountByChild$Title, fill=0)


# Now do the same for the hospice visits data
# Include total time spent in hospice (hospice visit breakdown - sum by visit and merge onto visit records)
MinutesByVisit <- aggregate(HospiceVisitBreakdown$Minutes, by=list(Visit.Record=HospiceVisitBreakdown$Visit.Record), FUN=sum, na.rm=TRUE)
colnames(MinutesByVisit) <- c('Visit.Record', 'AggregateMinutes')
HospiceVisitAggregation <- merge(ChristophersVisitRecords, MinutesByVisit)
# Create a count variable for confirmed visits
ConfirmedVisitVals <- data.frame(confirmedFlag=integer())
ConfirmedVisitVals <- (HospiceVisitAggregation$StatusDescription == 'Confirmed') * 1
HospiceVisitAggregation <- cbind(HospiceVisitAggregation, ConfirmedVisitVals)
# CREATE ONE CAST FOR EACH FACTOR, THEN AGGREGATE FOR MINUTES AND VISITS, THEN JOIN TOGETHER?
# Cast out to get sums of values and category for each of priority and visit desc
HospiceCast_Priority <- HospiceVisitAggregation[,c(2,6,33)]
HospiceCast_Priority <- dcast(HospiceCast_Priority, HospiceCast_Priority$Child.ID~HospiceCast_Priority$Priority, sum, fill=0)
HospiceCast_Priority <- HospiceCast_Priority[,1:3]
colnames(HospiceCast_Priority) <- c('ChildID', 'Priority1','Priority2')
HospiceCast_Description <- HospiceVisitAggregation[,c(2,9,33)]
HospiceCast_Description <- dcast(HospiceCast_Description, HospiceCast_Description$Child.ID~HospiceCast_Description$Description, sum, fill=0)
colnames(HospiceCast_Description) <- c('ChildID', colnames(HospiceCast_Description)[2:length(HospiceCast_Description)])
HospiceCast_Visits <- aggregate(HospiceVisitAggregation$ConfirmedVisitVals, by=list(ChildID=HospiceVisitAggregation$Child.ID), FUN=sum, na.rm=TRUE)
colnames(HospiceCast_Visits) <- c('ChildID', 'Visits')
HospiceCast_Minutes <- aggregate(HospiceVisitAggregation$AggregateMinutes, by=list(ChildID=HospiceVisitAggregation$Child.ID), FUN=sum, na.rm=TRUE)
colnames(HospiceCast_Minutes) <- c('ChildID', 'AggregateMinutes')

#Yo lets merge!!! XD
merge1 <- merge(HospiceCast_Priority,HospiceCast_Description)
merge2 <- merge(HospiceCast_Visits,HospiceCast_Minutes)
HospiceCast_Combined <- merge(merge1, merge2)


### ACTIVITY ONLY CLUSTERING
# Scale the data for clustering, and remove the ChildID variable
ActivityCountByChild_FlatNoID <- ActivityCountByChild_Flat[,2:length(ActivityCountByChild_Flat)]
ActivityCountByChild_FlatNoID <-scale(ActivityCountByChild_FlatNoID) # standardize variables
ActivityCountByChild_FlatNoID <- as.data.frame(ActivityCountByChild_FlatNoID[ , colSums(is.na(ActivityCountByChild_FlatNoID)) == 0], 
                                               stringsAsFactors=FALSE) # Remove the NaN columns

# Determine number of clusters
wss <- (nrow(ActivityCountByChild_FlatNoID)-1)*sum(apply(ActivityCountByChild_FlatNoID,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ActivityCountByChild_FlatNoID, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


### HOSPICE ONLY CLUSTERING
HospiceCast_NoIDs <- HospiceCast_Combined[,2:length(HospiceCast_Combined)]
HospiceCast_Scaled <- as.data.frame(scale(HospiceCast_NoIDs), stringsAsFactors=FALSE)
wss <- (nrow(HospiceCast_Scaled)-1)*sum(apply(HospiceCast_Scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(HospiceCast_Scaled, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


### ACTIVITY AND HOSPICE CLUSTERING
colnames(ActivityCountByChild_Flat) <- colnames(ActivityCountByChild_Flat) <- c('ChildID', colnames(ActivityCountByChild_Flat)[2:length(ActivityCountByChild_Flat)])
CombinedClusteringTable <- merge(HospiceCast_Combined, ActivityCountByChild_Flat)
CombinedClusteringTable_NoIDs <- CombinedClusteringTable[,2:length(CombinedClusteringTable)]
CombinedClusteringTable_NoIDs <- scale(CombinedClusteringTable_NoIDs)
CombinedClusteringTable_NoIDs <- as.data.frame(CombinedClusteringTable_NoIDs[ , colSums(is.na(CombinedClusteringTable_NoIDs)) == 0], 
                                               stringsAsFactors=FALSE) # Remove the NaN columns
wss <- (nrow(CombinedClusteringTable_NoIDs)-1)*sum(apply(CombinedClusteringTable_NoIDs,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(CombinedClusteringTable_NoIDs, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


### NUMERICAL ATTRIBUTE ONLY CLUSTERING
HospiceCast_Numerical <- HospiceCast_Combined[,2:length(HospiceCast_Combined)]
HospiceCast_Numerical <- cbind(HospiceCast_Numerical$Priority1, HospiceCast_Numerical$Priority2, 
                               HospiceCast_Numerical$Visits, HospiceCast_Numerical$AggregateMinutes)
HospiceCast_Numerical <- scale(HospiceCast_Numerical)
HospiceCast_Numerical <- as.data.frame(HospiceCast_Numerical[ , colSums(is.na(HospiceCast_Numerical)) == 0], 
                                       stringsAsFactors=FALSE) # Remove the NaN columns
wss <- (nrow(HospiceCast_Numerical)-1)*sum(apply(HospiceCast_Numerical,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(HospiceCast_Numerical, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


### OK! LETS USE NUMERICAL ONLY - GIVES THE BEST RESULT WHEN SCALED

# K-Means Cluster Analysis
fit <- kmeans(HospiceCast_Numerical, 4) 
# append cluster assignment
HospiceCast_Clustered <- data.frame(HospiceCast_Numerical, fit$cluster)
HospiceCast_Clustered <- as.data.frame(cbind(HospiceCast_Combined$ChildID, HospiceCast_Clustered$fit.cluster), stringsAsFactors=FALSE)
colnames(HospiceCast_Clustered) <- c('ChildID', 'ClusterID')

# Map clusters to:
# CP_Dependency - Overall Criteria
FinalCluster_Combined <- merge(x = HospiceCast_Clustered, y = CP_Dependency, by = "ChildID", all.x=TRUE)
# Referrals - ReferralSource
Referrals_Source <- Referrals[,c(1,8)]
colnames(Referrals_Source) <- c('ChildID', 'ReferralSource')
FinalCluster_Combined <- merge(x = FinalCluster_Combined, y = Referrals_Source, by = "ChildID", all.x=TRUE)

# Export the dataset
write.csv(FinalCluster_Combined, "clustered_metrics.csv")

# Profile the clusters! 
# We want number of visits, hours spent there, avg priority 1, avg priority 2
FinalCluster_AttributesReadded <- merge(HospiceCast_Clustered, HospiceCast_Combined)
FinalCluster_AttributesReadded <- merge(FinalCluster_AttributesReadded,ActivityCountByChild_Flat)
colnames(ChildDemographic) <- c('ChildID', colnames(ChildDemographic)[2:length(ChildDemographic)])
FinalCluster_AttributesReadded <- merge (FinalCluster_AttributesReadded, ChildDemographic)
write.csv(FinalCluster_AttributesReadded, "output-clusters-with-correlations.csv")

