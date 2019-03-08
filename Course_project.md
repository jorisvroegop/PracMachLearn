Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

Libraries
---------

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

Loading data
------------

``` r
TrainData <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=TRUE)
dim(TrainData)
```

    ## [1] 19622   160

``` r
TestData <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=TRUE)
dim(TestData)
```

    ## [1]  20 160

### Cleaning data

If we look at the data, many of the 160 variables are (nearly) empty, therefore we remove the columns with more than 80% NAs in the training set . We remove the same columns in the test set.

``` r
indColToRemove <- which(colSums(is.na(TrainData) |TrainData=="")>0.8*dim(TrainData)[1]) 
TrainDataClean <- TrainData[,-indColToRemove]
TrainDataClean <- TrainDataClean[,-c(1:7)]
dim(TrainDataClean)
```

    ## [1] 19622    53

``` r
TestDataClean <- TestData[,-indColToRemove]
TestDataClean <- TestDataClean[,-c(1:7)]
dim(TestDataClean)
```

    ## [1] 20 53

Model building
--------------

### Train/test split for cross validation

We split the training set into another training and test set which we can use to determine our (out of sample) accuracy.

``` r
set.seed(11)
inTrain1 <- createDataPartition(TrainDataClean$classe, p=0.7, list=FALSE)
Train1 <- TrainDataClean[inTrain1,]
Test1 <- TrainDataClean[-inTrain1,]
dim(Train1) 
```

    ## [1] 13737    53

Let's use the well-known random forest machine learnig algorithm to predict **classe**. During the training we use cross-validation to find the optimal model within computational/time limits.

``` r
trControl <- trainControl(method="cv", number=5)
model_rf <- train(classe~., data=Train1, method="rf", trControl=trControl, verbose=FALSE)
```

Now we check how good the model actually performs by checking how accurate it predicts on our created test set.

``` r
testpred <- predict(model_rf,newdata=Test1)
confMatRF <- confusionMatrix(Test1$classe,testpred)

# display confusion matrix and model accuracy
confMatRF$table
```

    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1674    0    0    0    0
    ##          B    2 1135    2    0    0
    ##          C    0    9 1016    1    0
    ##          D    0    0   15  948    1
    ##          E    0    0    2    1 1079

``` r
acc <- confMatRF$overall[1]
acc
```

    ##  Accuracy 
    ## 0.9943925

That is a very high accuracy of 0.9943925 reached by using a 5-fold cross-validation. Therefore the expected out of sample error is 0.0056075.

Now let's check the most important variables in the model:

``` r
imp_var <- varImp(model_rf)
imp_var
```

    ## rf variable importance
    ## 
    ##   only 20 most important variables shown (out of 52)
    ## 
    ##                   Overall
    ## roll_belt          100.00
    ## yaw_belt            75.46
    ## magnet_dumbbell_z   69.26
    ## magnet_dumbbell_y   60.95
    ## pitch_belt          57.18
    ## pitch_forearm       53.60
    ## magnet_dumbbell_x   48.78
    ## roll_forearm        48.28
    ## accel_belt_z        42.35
    ## magnet_belt_z       41.09
    ## accel_dumbbell_y    40.63
    ## magnet_belt_y       40.48
    ## roll_dumbbell       39.77
    ## roll_arm            35.48
    ## accel_dumbbell_z    34.07
    ## accel_forearm_x     32.20
    ## gyros_belt_z        30.67
    ## yaw_dumbbell        28.24
    ## gyros_dumbbell_y    27.62
    ## accel_dumbbell_x    27.01

Conclusion
----------

Since we built a good working model, we can now predict the **classe** of our real test set (20 obs).

``` r
FinalTestPred <- predict(model_rf,newdata=TestDataClean)
FinalTestPred
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E
