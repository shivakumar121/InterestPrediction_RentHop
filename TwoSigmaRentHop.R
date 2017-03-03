source("https://bioconductor.org/biocLite.R")
#biocLite("BiocParallel")
library ("rjson")
library ("gdata")
library ("ggplot2")
library ("caret")
library ("lubridate")
library ("outliers")
library ("stringr")
library ("stringdist")
library ("BiocParallel")
MyData <- fromJSON(file = "train.json")
#MyData <- MyData[[]][1]
#### Function to Return NA when data is missing ######
returnNAData <- function (x, var)
{
  if (!is.na (x[[var]]))
  {
    return (trim (x[[var]]))
  } else
  {
    return (NA)
  }
}
##### Function to Convert JSON List to DataFrame ########
grabInfo <- function (var)
{
  print (paste0 ("Variable", var))
  sapply (MyData, function (x) {returnNAData(x,var)})
}
#### Do the Deed
MyDataInDataFrame <- data.frame (sapply(1:2, grabInfo), stringsAsFactors = F)
########Just testing below ##############
########################################
MyDataInDataFrame = matrix (nrow = 100, ncol = 15)
#trownames (MyDataInDataFrame) = names(geno(MyVCF)$GT[1,])
colnames (MyDataInDataFrame) = c("bathrooms","bedrooms","building_id","created","description","display_address","features","latitude","listing_id","longitude","manager_id","photos","price","street_address","interest_level")
#MyDataInDataFrame <- NULL
MyDataInDataFrame <- as.data.frame (MyDataInDataFrame)
MyDataInDataFrameTemp <- NULL
MyDataInDataFrameTemp <- as.data.frame (MyDataInDataFrameTemp)
#ncol(MyDataInDataFrame)
for (i in 1:ncol(MyDataInDataFrame))
{
  for (j in 1:length(MyData[[1]]))
  {
    if ((!is.na(MyData[[i]][j]) | length (MyData[[i]][j]) == 0) & (lengths(MyData[[i]][j]) > 0))
    {
      MyDataInDataFrame[j,i] <- paste0(unlist(lapply (MyData[[i]][j], trim)), collapse = ",")
    } else 
    {
      MyDataInDataFrame[j,i] <- "NA"
    }
    
  }
  
}
#saveRDS (object = MyDataInDataFrame, compress = T, file = "MyDataInDataFrame.rds")
MyDataInDataFrame <- readRDS (file = "MyDataInDataFrame.rds")
##### Set Classes for all Columns ########
MyDataInDataFrame$bathrooms <- as.numeric(MyDataInDataFrame$bathrooms)
MyDataInDataFrame$bedrooms <- as.numeric(MyDataInDataFrame$bedrooms)
MyDataInDataFrame$building_id <- as.character(MyDataInDataFrame$building_id)
MyDataInDataFrame$created <- ymd_hms(MyDataInDataFrame$created)
MyDataInDataFrame$description <- as.character(MyDataInDataFrame$description)
MyDataInDataFrame$display_address <- as.character(MyDataInDataFrame$display_address)
MyDataInDataFrame$features <- as.character(MyDataInDataFrame$features)
MyDataInDataFrame$latitude <- as.numeric(MyDataInDataFrame$latitude)
MyDataInDataFrame$listing_id <- as.character(MyDataInDataFrame$listing_id)
MyDataInDataFrame$longitude <- as.numeric(MyDataInDataFrame$longitude)
MyDataInDataFrame$manager_id <- as.character(MyDataInDataFrame$manager_id)
MyDataInDataFrame$photos <- as.character(MyDataInDataFrame$photos)
MyDataInDataFrame$price <- as.numeric(MyDataInDataFrame$price)
MyDataInDataFrame$street_address <- as.character(MyDataInDataFrame$street_address)
MyDataInDataFrame$interest_level <- as.factor(MyDataInDataFrame$interest_level)
MyDataInDataFrame$PriceByBedrooms <- as.numeric (MyDataInDataFrame$price / (MyDataInDataFrame$bedrooms + 1))
MyDataInDataFrame$CreatedDayOfWeek <- wday (MyDataInDataFrame$created)
##### Remove Wild Outliers ########
MyQuantiles <- quantile(MyDataInDataFrame$price,probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,.99,1))
IndexToRemove <- which (MyDataInDataFrame$price > MyQuantiles[11])
MyDataInDataFrame <- MyDataInDataFrame[-IndexToRemove,]
MyDataInDataFrame <- MyDataInDataFrame[-(which(MyDataInDataFrame$latitude < 25)),]
LocClusters <- kmeans (x = MyDataInDataFrame[,c("latitude","longitude")], iter.max = 50000, centers = 10, nstart = 500)
MyDataInDataFrame$Cluster <- as.factor(LocClusters$cluster)
##### Create Validation Set #########
ValidtionSet = createDataPartition(MyDataInDataFrame$interest_level,p=0.75,list = F)
FinalDataTableValidation_TRAIN = MyDataInDataFrame[ValidtionSet,]
FinalDataTableValidation_TEST = MyDataInDataFrame[-ValidtionSet,]
procValues = preProcess(FinalDataTableValidation_TRAIN[,c("bathrooms","bedrooms","price","PriceByBedrooms")], method = c("center","scale","YeoJohnson"))
FinalDataTableValidation_TRAIN_scaled <- predict (procValues, FinalDataTableValidation_TRAIN[,c("bathrooms","bedrooms","price","PriceByBedrooms")])
FinalDataTableValidation_TEST_scaled <- predict (procValues, FinalDataTableValidation_TEST[,c("bathrooms","bedrooms","price","PriceByBedrooms")])
FinalDataTableValidation_TRAIN_scaled$Cluster <- FinalDataTableValidation_TRAIN$Cluster
FinalDataTableValidation_TRAIN_scaled$CreatedDayOfWeek <- FinalDataTableValidation_TRAIN$CreatedDayOfWeek
FinalDataTableValidation_TEST_scaled$Cluster <- FinalDataTableValidation_TEST$Cluster
FinalDataTableValidation_TEST_scaled$CreatedDayOfWeek <- FinalDataTableValidation_TEST$CreatedDayOfWeek
ModelsToTry <- c("treebag", "rmda", "AdaBoost.M1")
detach("package:lubridate", unload=TRUE)
MyPredictedValues <- bplapply (ModelsToTry, FUN = MyTrainFunction, FinalDataTableValidation_TRAIN_scaled = FinalDataTableValidation_TRAIN_scaled, TargetValues = FinalDataTableValidation_TRAIN$interest_level)

TempTest <- train (x = FinalDataTableValidation_TRAIN_scaled, y = FinalDataTableValidation_TRAIN$interest_level, method = "AdaBoost.M1")
confusionMatrix (reference = (FinalDataTableValidation_TEST$interest_level), data = predict (TempTest, FinalDataTableValidation_TEST_scaled), positive = "TRUE")
######### Testing Below ###############
ModelsToTry <- c("treebag", "rmda", "AdaBoost.M1")
MyTrainFunction <- function (CurrentMethodToUse, FinalDataTableValidation_TRAIN_scaled, TargetValues)
{
  TempTest <- train (x = FinalDataTableValidation_TRAIN_scaled, y = FinalDataTableValidation_TRAIN$interest_level, method = CurrentMethodToUse)
  MyPrdictedValues <- predict (TempTest, FinalDataTableValidation_TEST_scaled)
  return (MyPrdictedValues)
}

MyDataInDataFrame <- MyDataInDataFrame[-(which(MyDataInDataFrame$latitude < 35)),]
tt = kmeans (x = MyDataInDataFrame[,c(8,10)], iter.max = 50000, centers = 25, nstart = 500)
ptest <- ggplot (data = MyDataInDataFrame, aes (latitude, longitude, color = factor (tt$cluster)))
ptest <- ptest + geom_point(alpha = 0.3)
ptest <- ptest + xlim (40.3,41) + ylim (-74.2, -73.5)
ptest
tt = kmeans (x = MyDataInDataFrame[,c(8,10)], iter.max = 100, centers = 3)
Features <- MyDataInDataFrame$features
Features <-  unlist (str_split (Features, pattern = ","))
Features <-  unlist (str_split (Features, pattern = "\\*"))
TopFeatures <- rownames (table (Features)[which(table(Features) > 100)])
TopFeatures <- TopFeatures[-(which(str_length(TopFeatures) < 3))]
if (length (which (length(TopFeatures) < 2)) > 1)
{
  TopFeatures <- TopFeatures[-(which(length(TopFeaturesDist) < 2))]
}
#TopFeatures <- TopFeatures[grepl(TopFeatures, pattern = "")]
TopFeaturesDist <- adist (TopFeatures, ignore.case = T, partial = T)
rownames(TopFeaturesDist) <- TopFeatures
colnames(TopFeaturesDist) <- TopFeatures


which ((TopFeaturesDist < 3) & (TopFeaturesDist > 0), arr.ind = T)
which (length(TopFeaturesDist) < 2)
unlist (sapply (TopFeatures, FUN = stringdist))
for (i in 1:length(TopFeatures))
{
  for (j in i:length (TopFeatures))
  {
    TopFeaturesDist[i,j] <- stringdist(a = TopFeatures[i], b = TopFeatures[j], method = "dl")
    TopFeaturesDist[j,i] <- TopFeaturesDist[i,j]
  }
}

TempTest <- train (x = FinalDataTableValidation_TRAIN[,c(1,2,13)], y = FinalDataTableValidation_TRAIN$interest_level, method = "treebag")
confusionMatrix (reference = (FinalDataTableValidation_TEST$interest_level), data = predict (TempTest, FinalDataTableValidation_TEST[,c(1,2,13)]) , positive = "TRUE")

featurePlot(x = MyDataInDataFrame[,c(1,2,13)], y = MyDataInDataFrame$interest_level, plot = "pairs",auto.key = list(columns = 2))
p1 <- ggplot (data = MyDataInDataFrame, aes (bathrooms, interest_level))
p1 <- p1 + geom_point()
p1
              p1 <- ggplot (data = tab, aes(EV1,EV2))
              p1 <- p1 + geom_point(size = 5, aes (color = factor (CollectionCenter)))





fmNames<-sapply(MyData, function(x) x[[15]])
head(fmNames)
