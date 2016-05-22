library(caret)

# Download file and read in the data
fn <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
# For Mac use download.file(fn, "pml-training.csv", method="curl")
download.file(fn, "pml-training.csv")

trndata <- read.csv("pml-training.csv")

## Cleaning Data

# Remove the columns with NA; Attaches FALSE if col has no NA and 
# TRUE if column has NA
nacol  <- apply(trndata, 2, anyNA)
# Eliminate the columns that wer marked as TRUE.
trndata <- trndata[, !names(trndata) %in% names(trndata)[nacol]]

# Remove columns that have blanks; sets TRUE to columns which have blanks
blnkcol <- apply(trndata, 2, function(x) sum(x=="")!=0)
trndata <- trndata[, !names(trndata) %in% names(trndata)[blnkcol]]

# Remove columns that are not required for analysis
trndata$raw_timestamp_part_1 <- NULL
trndata$raw_timestamp_part_2 <- NULL
trndata$cvtd_timestamp <- NULL
trndata$new_window <- NULL
trndata$num_window <- NULL
trndata$X <- NULL

## Create validation data from training data. Split is 80/20.
set.seed (0007)
inTrain <- createDataPartition(y=trndata$classe,
                               p=0.8, list=FALSE)
train.trndata <- trndata[inTrain,]
val.trndata <- trndata[-inTrain,]

# Create a model using Random Forest and test accuracy on the validation set.
# Remove the observed classification from the validation set.
obsvdresult <- val.trndata$classe
val.trndata <- val.trndata[1:53]

# Use Random Forest to build a model and predict on the validation set
crsvalFitRf <- randomForest(classe ~ ., data = train.trndata, ntree = 10)
crsvalFitPred <- predict (crsvalFitRf, newdata = val.trndata)

# Check the accuracy of the prediction; Accuracy is 99%  
accuracy <- sum(obsvdresult == crsvalFitPred)/length(obsvdresult)
accuracy 

# Repartition the data training data into training and validation data
# Use 60/40 split, ntree = 30  and build and simualte model 5 times.
for (i in 1:5) {
        inTrain <- createDataPartition(y=trndata$classe,
                                       p=0.6, list=FALSE)
        train.trndata <- trndata[inTrain,]
        val.trndata <- trndata[-inTrain,]
        obsvdresult <- val.trndata$classe
        val.trndata <- val.trndata[1:53]
        crsvalFitRf <- randomForest(classe ~ ., 
                                    data = train.trndata, ntree = 30)
        crsvalFitPred <- predict (crsvalFitRf, newdata = val.trndata)
        x <- sum(obsvdresult == crsvalFitPred)/length(obsvdresult)
        accuracy[i] <- x
}
accuracy
# Accuracy of the prediction was 99% and hence, this model is a good 
#predictor.

## Prediction on actual test set.
# Download file and read in the data
fn <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fn, "pml-testing.csv")
tstdata <- read.csv("pml-testing.csv")

## Cleaning Data

# Remove the columns with NA; Attaches FALSE if col has no NA and 
# TRUE if column has NA
nacol  <- apply(tstdata, 2, anyNA)
# Eliminate the columns that wer marked as TRUE.
tstdata <- tstdata[, !names(tstdata) %in% names(tstdata)[nacol]]

# Remove columns that have blanks; sets TRUE to columns which have blanks
blnkcol <- apply(tstdata, 2, function(x) sum(x=="")!=0)
tstdata <- tstdata[, !names(tstdata) %in% names(tstdata)[blnkcol]]

# Remove columns that are not required for analysis
tstdata$raw_timestamp_part_1 <- NULL
tstdata$raw_timestamp_part_2 <- NULL
tstdata$cvtd_timestamp <- NULL
tstdata$new_window <- NULL
tstdata$num_window <- NULL
tstdata$X <- NULL

# Remove the problem_id column from the testing set.
tstdata <- tstdata[1:53]
modelFitPred <- predict (crsvalFitRf, newdata = tstdata)
modelFitPred
