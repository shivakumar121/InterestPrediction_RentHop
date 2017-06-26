#source("https://bioconductor.org/biocLite.R")
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
library ("plyr")
library ("BatchJobs")
library ("Rmpi")
#library ("pROC")
library ("e1071")
library(doMC)
registerDoMC(2)
######################################
MyDataInDataFrame_TRAIN <- readRDS (file = "MyDataInDataFrame.rds")
MyDataInDataFrame_TEST <- readRDS("MyDataInDataFrame_TEST.rds")
##### Set Classes for all Columns in TRAIN ########
MyDataInDataFrame_TRAIN$bathrooms <- as.numeric(MyDataInDataFrame_TRAIN$bathrooms)
MyDataInDataFrame_TRAIN$bedrooms <- as.numeric(MyDataInDataFrame_TRAIN$bedrooms)
MyDataInDataFrame_TRAIN$building_id <- as.character(MyDataInDataFrame_TRAIN$building_id)
MyDataInDataFrame_TRAIN$created <- ymd_hms(MyDataInDataFrame_TRAIN$created)
MyDataInDataFrame_TRAIN$description <- as.character(MyDataInDataFrame_TRAIN$description)
MyDataInDataFrame_TRAIN$display_address <- as.character(MyDataInDataFrame_TRAIN$display_address)
MyDataInDataFrame_TRAIN$features <- as.character(MyDataInDataFrame_TRAIN$features)
MyDataInDataFrame_TRAIN$latitude <- as.numeric(MyDataInDataFrame_TRAIN$latitude)
MyDataInDataFrame_TRAIN$listing_id <- as.character(MyDataInDataFrame_TRAIN$listing_id)
MyDataInDataFrame_TRAIN$longitude <- as.numeric(MyDataInDataFrame_TRAIN$longitude)
MyDataInDataFrame_TRAIN$manager_id <- as.character(MyDataInDataFrame_TRAIN$manager_id)
MyDataInDataFrame_TRAIN$photos <- as.character(MyDataInDataFrame_TRAIN$photos)
MyDataInDataFrame_TRAIN$price <- as.numeric(MyDataInDataFrame_TRAIN$price)
MyDataInDataFrame_TRAIN$street_address <- as.character(MyDataInDataFrame_TRAIN$street_address)
MyDataInDataFrame_TRAIN$interest_level <- as.factor(MyDataInDataFrame_TRAIN$interest_level)
MyDataInDataFrame_TRAIN$PriceByBedrooms <- as.numeric (MyDataInDataFrame_TRAIN$price / (MyDataInDataFrame_TRAIN$bedrooms + 1))
MyDataInDataFrame_TRAIN$CreatedDayOfWeek <- wday (MyDataInDataFrame_TRAIN$created)
##### Set Classes for all Columns in TEST ########
MyDataInDataFrame_TEST$bathrooms <- as.numeric(MyDataInDataFrame_TEST$bathrooms)
MyDataInDataFrame_TEST$bedrooms <- as.numeric(MyDataInDataFrame_TEST$bedrooms)
MyDataInDataFrame_TEST$building_id <- as.character(MyDataInDataFrame_TEST$building_id)
MyDataInDataFrame_TEST$created <- ymd_hms(MyDataInDataFrame_TEST$created)
MyDataInDataFrame_TEST$description <- as.character(MyDataInDataFrame_TEST$description)
MyDataInDataFrame_TEST$display_address <- as.character(MyDataInDataFrame_TEST$display_address)
MyDataInDataFrame_TEST$features <- as.character(MyDataInDataFrame_TEST$features)
MyDataInDataFrame_TEST$latitude <- as.numeric(MyDataInDataFrame_TEST$latitude)
MyDataInDataFrame_TEST$listing_id <- as.character(MyDataInDataFrame_TEST$listing_id)
MyDataInDataFrame_TEST$longitude <- as.numeric(MyDataInDataFrame_TEST$longitude)
MyDataInDataFrame_TEST$manager_id <- as.character(MyDataInDataFrame_TEST$manager_id)
MyDataInDataFrame_TEST$photos <- as.character(MyDataInDataFrame_TEST$photos)
MyDataInDataFrame_TEST$price <- as.numeric(MyDataInDataFrame_TEST$price)
MyDataInDataFrame_TEST$street_address <- as.character(MyDataInDataFrame_TEST$street_address)
MyDataInDataFrame_TEST$PriceByBedrooms <- as.numeric (MyDataInDataFrame_TEST$price / (MyDataInDataFrame_TEST$bedrooms + 1))
MyDataInDataFrame_TEST$CreatedDayOfWeek <- wday (MyDataInDataFrame_TEST$created)
##### Remove Wild Outliers ########
MyQuantiles <- quantile(MyDataInDataFrame_TRAIN$price,probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,.99,1))
IndexToRemove <- which (MyDataInDataFrame_TRAIN$price > MyQuantiles[11])
MyDataInDataFrame_TRAIN <- MyDataInDataFrame_TRAIN[-IndexToRemove,]
MyDataInDataFrame_TRAIN <- MyDataInDataFrame_TRAIN[-(which(MyDataInDataFrame_TRAIN$latitude < 25)),]
LocClusters <- kmeans (x = MyDataInDataFrame_TRAIN[,c("latitude","longitude")], iter.max = 50000, centers = 25, nstart = 500)
#LocClusters_TEST <- kmeans (x = MyDataInDataFrame_TEST[,c("latitude","longitude")], iter.max = 50000, centers = 25, nstart = 500)
MyDataInDataFrame_TRAIN$Cluster <- as.numeric(LocClusters$cluster)
#### Find TEST Clusters ########
TestCluster_List <- NULL
for (i in 1:nrow (MyDataInDataFrame_TEST))
{
  Current_Position <- MyDataInDataFrame_TEST[i,c("latitude","longitude")]
  rownames (Current_Position) <- "26"
  Current_Distances <- as.matrix (dist(rbind(Current_Position, LocClusters$centers), method = "euclidean", diag = F))
  ShortestDist <- sort (Current_Distances["26",], decreasing = F)[2]
  TestCluster_List <- c(TestCluster_List, names (which (Current_Distances["26",] == ShortestDist)[1]))
}
MyDataInDataFrame_TEST$Cluster <- as.numeric (TestCluster_List)
######################################
####### Add feature columns ########
#PopFeatures <- names (sort(table(unlist (str_split(MyDataInDataFrame_TRAIN$features, pattern = ","))), decreasing = T)[1:50])
PopFeatures <- unlist (str_split(tolower(MyDataInDataFrame_TRAIN$features), pattern = ","))
PopFeatures <- unlist(str_split(PopFeatures, pattern = "\\*"))
PopFeatures <- names (sort(table(PopFeatures), decreasing = T)[1:250])
PopFeatures <- PopFeatures[which(str_length(PopFeatures) > 0)]
PopFeatures <- PopFeatures[-grep(PopFeatures, pattern = "na")]
PopFeatures[grep(PopFeatures, pattern = "walk in closet")] <- "walk in closet"
PopFeatures[grep(PopFeatures, pattern = "pre-war")] <- "prewar"
PopFeatures[grep(PopFeatures, pattern = "playroom")] <- "playroom"
PopFeatures <- unique(trim(PopFeatures))
for (CurrentFeature in PopFeatures)
{
  if (CurrentFeature == "prewar")
  {
    Index <- grep (MyDataInDataFrame_TRAIN$features, pattern = CurrentFeature, ignore.case = T)
    Index <- c(Index, grep (MyDataInDataFrame_TRAIN$features, pattern = "pre-war", ignore.case = T))
    Index <- sort(unique(Index))
  }
  Index <- grep (MyDataInDataFrame_TRAIN$features, pattern = CurrentFeature, ignore.case = T)
  FeatureLogical <- rep("0", times = nrow(MyDataInDataFrame_TRAIN))
  FeatureLogical[Index] <- "1"
  MyDataInDataFrame_TRAIN <- cbind (MyDataInDataFrame_TRAIN,as.numeric(FeatureLogical))
  CurrentColnames <- colnames (MyDataInDataFrame_TRAIN)
  CurrentColnames[length(CurrentColnames)] <- CurrentFeature
  colnames (MyDataInDataFrame_TRAIN) <- CurrentColnames
  ## Now for Test set
  Index <- grep (MyDataInDataFrame_TEST$features, pattern = CurrentFeature, ignore.case = T)
  FeatureLogical <- rep("0", times = nrow(MyDataInDataFrame_TEST))
  FeatureLogical[Index] <- "1"
  MyDataInDataFrame_TEST <- cbind (MyDataInDataFrame_TEST,as.numeric(FeatureLogical))
  CurrentColnames <- colnames (MyDataInDataFrame_TEST)
  CurrentColnames[length(CurrentColnames)] <- CurrentFeature
  colnames (MyDataInDataFrame_TEST) <- CurrentColnames
}
Pics <- MyDataInDataFrame_TRAIN$photos
NumPics <- str_count(Pics, pattern = ".jpg")
MyDataInDataFrame_TRAIN$NumPics <- as.numeric(NumPics)
Pics <- MyDataInDataFrame_TEST$photos
NumPics <- str_count(Pics, pattern = ".jpg")
MyDataInDataFrame_TEST$NumPics <- as.numeric(NumPics)
ColNames <- colnames (MyDataInDataFrame_TRAIN)
ColNames <- ColNames[-grep(ColNames, pattern = "interest_level")]
TrainAndTest.df <- rbind(MyDataInDataFrame_TRAIN[,ColNames],MyDataInDataFrame_TEST)
procValues = preProcess(TrainAndTest.df[,c("bathrooms","bedrooms","price","PriceByBedrooms", "Cluster", "CreatedDayOfWeek", "NumPics")], method = c("center","scale","YeoJohnson"))
MyDataInDataFrame_TRAIN_scaled <- predict (procValues, MyDataInDataFrame_TRAIN[,c("bathrooms","bedrooms","price","PriceByBedrooms", "Cluster", "CreatedDayOfWeek", "NumPics")])
MyDataInDataFrame_TEST_scaled <- predict (procValues, MyDataInDataFrame_TEST[,c("bathrooms","bedrooms","price","PriceByBedrooms", "Cluster", "CreatedDayOfWeek", "NumPics")])
for (CurrentFeature in PopFeatures)
{
  MyDataInDataFrame_TRAIN_scaled <- cbind (MyDataInDataFrame_TRAIN_scaled,MyDataInDataFrame_TRAIN[,c(CurrentFeature)]) 
  CurrentColnames <- colnames (MyDataInDataFrame_TRAIN_scaled)
  CurrentColnames[length(CurrentColnames)] <- CurrentFeature
  colnames (MyDataInDataFrame_TRAIN_scaled) <- CurrentColnames
  ## Now for TEST
  MyDataInDataFrame_TEST_scaled <- cbind (MyDataInDataFrame_TEST_scaled,MyDataInDataFrame_TEST[,c(CurrentFeature)]) 
  CurrentColnames <- colnames (MyDataInDataFrame_TEST_scaled)
  CurrentColnames[length(CurrentColnames)] <- CurrentFeature
  colnames (MyDataInDataFrame_TEST_scaled) <- CurrentColnames
}
AllManager_ids <- levels(as.factor(MyDataInDataFrame_TRAIN$manager_id))
ManagerId_TrainIndex <- rep.int(0, times = nrow(MyDataInDataFrame_TRAIN))
ManagerId_TESTIndex <- rep.int(0, times = nrow(MyDataInDataFrame_TEST))
for (i in 1:length(AllManager_ids))
{
  Index <- MyDataInDataFrame_TRAIN$manager_id %in% AllManager_ids[i]
  Index <- which(Index == "TRUE")
  ManagerId_TrainIndex[Index] <- i
  Index <- MyDataInDataFrame_TEST$manager_id %in% AllManager_ids[i]
  Index <- which(Index == "TRUE")
  if (length(Index) > 0)
  {
    ManagerId_TESTIndex[Index] <- i
  }
  
}
MyDataInDataFrame_TRAIN_scaled$manager_id <- as.numeric(ManagerId_TrainIndex)
MyDataInDataFrame_TEST_scaled$manager_id <- as.numeric(ManagerId_TESTIndex)
MyDataInDataFrame_TRAIN_scaled$interest_level <- MyDataInDataFrame_TRAIN$interest_level
saveRDS(object = MyDataInDataFrame_TRAIN_scaled, file = "MyDataInDataFrame_TRAIN_scaled_Apr25_2017_2.rds")
saveRDS(object = MyDataInDataFrame_TEST_scaled, file = "MyDataInDataFrame_TEST_scaled_Apr25_2017_2.rds")
#saveRDS(object = MyDataInDataFrame_TRAIN_scaled, file = "MyDataInDataFrame_TRAIN_scaled_Apr252017.rds")
#saveRDS(object = MyDataInDataFrame_TEST_scaled, file = "MyDataInDataFrame_TEST_scaled_Apr252017.rds")
ModelsToTry <- c("xgbTree", "xgbLinear", "gbm", "treebag", "ctree")
###########
##### Log Loss Function ##########
LogLosSummary <- function (data, lev = NULL, model = NULL) {
  LogLos <- function(actual, pred, eps = 1e-15) {
    stopifnot(all(dim(actual) == dim(pred)))
    pred[pred < eps] <- eps
    pred[pred > 1 - eps] <- 1 - eps
    -sum(actual * log(pred)) / nrow(pred) 
  }
  if (is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  pred <- data[, "pred"]
  obs <- data[, "obs"]
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  data <- data[!isNA, ]
  cls <- levels(obs)
  
  if (length(obs) + length(pred) == 0) {
    out <- rep(NA, 2)
  } else {
    pred <- factor(pred, levels = levels(obs))
    require("e1071")
    out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag",                                                                                                                                                             "kappa")]
    
    probs <- data[, cls]
    actual <- model.matrix(~ obs - 1)
    out2 <- LogLos(actual = actual, pred = probs)
  }
  out <- c(out, out2)
  names(out) <- c("Accuracy", "Kappa", "LogLoss")
  
  if (any(is.nan(out))) out[is.nan(out)] <- NA 
  
  out
}
##### Initiate parallel comouting ########
funs = makeClusterFunctionsSGE("./VerySimple.tmpl")
param = BatchJobsParam(workers = 100, resources = list(job.delay = TRUE),cluster.function = funs)
#######################
#### Function for training different models in parallel #############
MyTrainFunction <- function (CurrentMethodToUse, MyDataInDataFrame_TRAIN_scaled, LogLosSummary, MyPredictors)
{
  library ("caret")
  #library(doMC)
  #registerDoMC(8)
  print (paste0("Started with ", CurrentMethodToUse))
  fitControl <- trainControl(method = "LGOCV", number = 2, p = 0.75, allowParallel = F,
                             classProbs = TRUE, summaryFunction = LogLosSummary)
  if (CurrentMethodToUse == "ctree")
  {
    TempTest <- train(y = as.factor(MyDataInDataFrame_TRAIN_scaled$interest_level), x = MyDataInDataFrame_TRAIN_scaled[,MyPredictors], 
                      method = CurrentMethodToUse, 
                      trControl = fitControl,
                      ## This last option is actually one
                      ## for gbm() that passes through
                      
                      metric = "LogLoss",
                      maximize = FALSE)
  } else
  {
    TempTest <- train(y = as.factor(MyDataInDataFrame_TRAIN_scaled$interest_level), x = MyDataInDataFrame_TRAIN_scaled[,MyPredictors], 
                      method = CurrentMethodToUse, 
                      trControl = fitControl,
                      ## This last option is actually one
                      ## for gbm() that passes through
                      verbose = TRUE,distribution = "multinomial",
                      metric = "LogLoss",
                      maximize = FALSE)
  }
  
  #TempTest <- train (x = MyDataInDataFrame_TRAIN_scaled, y = TargetValues, method = CurrentMethodToUse)
  #MyPrdictedValues <- predict (TempTest, MyDataInDataFrame_TEST_scaled)
  OutRDSName <- paste0("Apr252017.", CurrentMethodToUse, ".",length(MyPredictors), "Preds.rds")
  if (!file.exists(OutRDSName))
  {
    saveRDS(object = TempTest, file = OutRDSName)
  }
  print (paste0("Finished doing ", CurrentMethodToUse))
  #return (MyPrdictedValues)
}
########################################
MyPredictors <- colnames(MyDataInDataFrame_TRAIN_scaled)
MyPredictors <- MyPredictors[-which(MyPredictors == "interest_level")]
bplapply (ModelsToTry, FUN = MyTrainFunction, MyDataInDataFrame_TRAIN_scaled = MyDataInDataFrame_TRAIN_scaled, MyPredictors = MyPredictors, LogLosSummary = LogLosSummary, BPPARAM = param)
PopFeatures <- c("bathrooms", "bedrooms", "price", "PriceByBedrooms", "dishwasher", "laundry in building", "no fee", "hardwood", "laundry", "prewar", "furnished")
MyPredictors <- PopFeatures
bplapply (ModelsToTry, FUN = MyTrainFunction, MyDataInDataFrame_TRAIN_scaled = MyDataInDataFrame_TRAIN_scaled, MyPredictors = MyPredictors, LogLosSummary = LogLosSummary, BPPARAM = param)





AllRDSFiles <- list.files(pattern = "^Apr252017")
AllRDSFiles <- AllRDSFiles[grep(AllRDSFiles, pattern = ".rds")]
PredDataFrame <- NULL
for ( i in c(1:4,6:length(AllRDSFiles)))
{
  print(AllRDSFiles[i])
  CurrentTrainedModel <- readRDS(AllRDSFiles[i])
  MyPredictors <- colnames(MyDataInDataFrame_TRAIN_scaled)
  MyPredictors <- MyPredictors[-which(MyPredictors == "interest_level")]
  CurrentPredictions <- predict(CurrentTrainedModel, MyDataInDataFrame_TEST_scaled[,MyPredictors], type = "prob")
  NewColNames <- paste0 (AllRDSFiles[i], ".",colnames(CurrentPredictions))
  colnames(CurrentPredictions) <- NewColNames
  if (i == 1)
  {
    PredDataFrame <- CurrentPredictions
  } else
  {
    PredDataFrame <- cbind(PredDataFrame, CurrentPredictions)
  }
  
  print(CurrentTrainedModel)
  remove(CurrentTrainedModel)
  gc()
}
##### Now train stacked model ##########
print (paste0("Started with stacking model xgbLinear"))
fitControl <- trainControl(method = "LGOCV", number = 10, p = 0.75, allowParallel = F,
                           classProbs = TRUE, summaryFunction = LogLosSummary)
StackedModel <- train(y = PredDataFrame$interest_levels, x = PredDataFrame[,1:(ncol(PredDataFrame)-1)], 
                      method = "xgbLinear", 
                      trControl = fitControl,
                      ## This last option is actually one
                      ## for gbm() that passes through
                      verbose = TRUE,distribution = "multinomial",
                      metric = "LogLoss",
                      maximize = FALSE)
StackedModelOutFile <- "StackedModel_Apr252017_2.rds"
if (!(file.exists(StackedModelOutFile)))
{
  saveRDS(object = StackedModel, file = StackedModelOutFile)
}
### Now apply stacking model ####
#StackedModel <- readRDS("StackedModel_Apr252017_1.rds")
MyFinalPreds <- predict(StackedModel, PredDataFrame[,], type = "prob")
FinalPreds_OutFileName <- "MyFinalPreds_Apr252017_2.csv"
MyFinalPreds$listing_id <- MyDataInDataFrame_TEST$listing_id
MyFinalPreds <- MyFinalPreds[,c(4,1,3,2)]
if (!file.exists(FinalPreds_OutFileName))
{
  write.csv(x = MyFinalPreds, file = FinalPreds_OutFileName, row.names = F, quote = F, col.names = T)
}





