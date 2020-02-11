url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
colnames(data) <- c(
"age",
"sex",# 0 = female, 1 = male
"cp", # chest pain
# 1 = typical angina,
# 2 = atypical angina,
# 3 = non-anginal pain,
# 4 = asymptomatic
"trestbps", # resting blood pressure (in mm Hg)
"chol", # serum cholestoral in mg/dl
"fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
"restecg", # resting electrocardiographic results
# 1 = normal
# 2 = having ST-T wave abnormality
# 3 = showing probable or definite left ventricular hypertrophy
"thalach", # maximum heart rate achieved
"exang",   # exercise induced angina, 1 = yes, 0 = no
"oldpeak", # ST depression induced by exercise relative to rest
"slope", # the slope of the peak exercise ST segment
# 1 = upsloping
# 2 = flat
# 3 = downsloping
"ca", # number of major vessels (0-3) colored by fluoroscopy
"thal", # this is short of thalium heart scan
# 3 = normal (no cold spots)
# 6 = fixed defect (cold spots during rest and exercise)
# 7 = reversible defect (when cold spots only appear during exercise)
"hd" # (the predicted attribute) - diagnosis of heart disease
# 0 if less than or equal to 50% diameter narrowing
# 1 if greater than 50% diameter narrowing
)
head(data) # now we have data and column names
str(data) # this shows that we need to tell R which columns contain factors
# it also shows us that there are some missing values. There are "?"s
# in the dataset.
## First, replace "?"s with NAs.
data[data == "?"] <- NA
## Now add factors for variables that are factors and clean up the factors
## that had missing data...
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca) # since this column had "?"s in it (which
# we have since converted to NAs) R thinks that
# the levels for the factor are strings, but
# we know they are integers, so we'll first
# convert the strings to integiers...
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)
## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
str(data) ## this shows that the correct columns are factors and we've replaced
## "?"s with NAs because "?" no longer appears in the list of factors
## for "ca" and "thal"
library(randomForest)
library(MASS)
library(tree)
library(gbm)
library(e1071)
set.seed(1)
train = sample (1: nrow(data), nrow(data)/2)
tree.data <- tree(hd~., data = data, subset=train,method="class")
summary(tree.data)
plot(tree.data)
text(tree.wine)
text(tree.data)
data.test=data[-train,"hd"]
data.pred=predict(tree.data,data[-train,],type="class")
tab<-table(data.test,data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
# Random Forests
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)
bag.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=13,importance=TRUE,type="class")
bag.data
data.pred=predict(bag.data,data[-train,],type="class")
tab<-table(data.test,data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(bag.data)
varImpPlot(bag.data)
#Random Forests
set.seed(1)
rf.data=randomForest(hd~.,data=data,subset=train,mtry=3,importance=TRUE,type="class")
rf.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=3,importance=TRUE,type="class")
rf.data
data.pred=predict(rf.data,data[-train,],type="class")
tab<-table(data.test, data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(bag.data)
varImpPlot(bag.data)
importance(rf.data)
varImpPlot(rf.data)
rf.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=2,importance=TRUE,type="class")
rf.fata
rf.data
data.pred.rf=predict(rf.data,data[-train,],type="class")
tab<-table(data.test,data.pred.rf)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(rf.data)
varImpPlot(rf.data)
set.seed(1)
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
summary(boost.data)
train = sample (1: nrow(data), nrow(data)/2)
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
summary(boost.data)
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=5000,distribution="multinomial",type="response")
class.pred<-rep(0,152)
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=458,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag #....this is very good
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=900,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.034 ...no change
classAgreement(tab)$crand
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=1000,interaction.depth=1,shrinkage=0.01)
summary(boost.wine)
summary(boost.data)
gbm.perf(boost.data)
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=412,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=450,distribution="multinomial",type="response")
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
data <- read.csv(url, header=FALSE)
# it also shows us that there are some missing values. There are "?"s
# in the dataset.
## First, replace "?"s with NAs.
data[data == "?"] <- NA
## Now add factors for variables that are factors and clean up the factors
## that had missing data...
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca) # since this column had "?"s in it (which
# we have since converted to NAs) R thinks that
# the levels for the factor are strings, but
# we know they are integers, so we'll first
# convert the strings to integiers...
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)
## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
str(data) ## this shows that the correct columns are factors and we've replaced
## "?"s with NAs because "?" no longer appears in the list of factors
## for "ca" and "thal"
library(randomForest)
library(MASS)
library(tree)
library(gbm)
library(e1071)
set.seed(1)
train = sample (1: nrow(data), nrow(data)/2)
tree.data <- tree(hd~., data = data, subset=train,method="class")
summary(tree.data)
plot(tree.data)
text(tree.wine)
text(tree.data)
data.test=data[-train,"hd"]
data.pred=predict(tree.data,data[-train,],type="class")
tab<-table(data.test,data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
# Random Forests
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)
bag.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=13,importance=TRUE,type="class")
bag.data
data.pred=predict(bag.data,data[-train,],type="class")
tab<-table(data.test,data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(bag.data)
varImpPlot(bag.data)
#Random Forests
set.seed(1)
rf.data=randomForest(hd~.,data=data,subset=train,mtry=3,importance=TRUE,type="class")
rf.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=3,importance=TRUE,type="class")
rf.data
data.pred=predict(rf.data,data[-train,],type="class")
tab<-table(data.test, data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(bag.data)
varImpPlot(bag.data)
importance(rf.data)
varImpPlot(rf.data)
rf.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=2,importance=TRUE,type="class")
rf.fata
rf.data
data.pred.rf=predict(rf.data,data[-train,],type="class")
tab<-table(data.test,data.pred.rf)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(rf.data)
varImpPlot(rf.data)
set.seed(1)
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
summary(boost.data)
train = sample (1: nrow(data), nrow(data)/2)
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
summary(boost.data)
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=5000,distribution="multinomial",type="response")
class.pred<-rep(0,152)
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=458,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag #....this is very good
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=900,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.034 ...no change
classAgreement(tab)$crand
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=1000,interaction.depth=1,shrinkage=0.01)
summary(boost.wine)
summary(boost.data)
gbm.perf(boost.data)
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=412,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=450,distribution="multinomial",type="response")
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
colnames(data) <- c(
"age",
"sex",# 0 = female, 1 = male
"cp", # chest pain
# 1 = typical angina,
# 2 = atypical angina,
# 3 = non-anginal pain,
# 4 = asymptomatic
"trestbps", # resting blood pressure (in mm Hg)
"chol", # serum cholestoral in mg/dl
"fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
"restecg", # resting electrocardiographic results
# 1 = normal
# 2 = having ST-T wave abnormality
# 3 = showing probable or definite left ventricular hypertrophy
"thalach", # maximum heart rate achieved
"exang",   # exercise induced angina, 1 = yes, 0 = no
"oldpeak", # ST depression induced by exercise relative to rest
"slope", # the slope of the peak exercise ST segment
# 1 = upsloping
# 2 = flat
# 3 = downsloping
"ca", # number of major vessels (0-3) colored by fluoroscopy
"thal", # this is short of thalium heart scan
# 3 = normal (no cold spots)
# 6 = fixed defect (cold spots during rest and exercise)
# 7 = reversible defect (when cold spots only appear during exercise)
"hd" # (the predicted attribute) - diagnosis of heart disease
# 0 if less than or equal to 50% diameter narrowing
# 1 if greater than 50% diameter narrowing
)
head(data) # now we have data and column names
str(data) # this shows that we need to tell R which columns contain factors
# it also shows us that there are some missing values. There are "?"s
# in the dataset.
## First, replace "?"s with NAs.
data[data == "?"] <- NA
## Now add factors for variables that are factors and clean up the factors
## that had missing data...
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca) # since this column had "?"s in it (which
# we have since converted to NAs) R thinks that
# the levels for the factor are strings, but
# we know they are integers, so we'll first
# convert the strings to integiers...
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)
## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
str(data) ## this shows that the correct columns are factors and we've replaced
## "?"s with NAs because "?" no longer appears in the list of factors
## for "ca" and "thal"
library(randomForest)
library(MASS)
library(tree)
library(gbm)
library(e1071)
set.seed(1)
train = sample (1: nrow(data), nrow(data)/2)
tree.data <- tree(hd~., data = data, subset=train,method="class")
summary(tree.data)
plot(tree.data)
text(tree.wine)
text(tree.data)
data.test=data[-train,"hd"]
data.pred=predict(tree.data,data[-train,],type="class")
tab<-table(data.test,data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
# Random Forests
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)
bag.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=13,importance=TRUE,type="class")
bag.data
data.pred=predict(bag.data,data[-train,],type="class")
tab<-table(data.test,data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(bag.data)
varImpPlot(bag.data)
#Random Forests
set.seed(1)
rf.data=randomForest(hd~.,data=data,subset=train,mtry=3,importance=TRUE,type="class")
rf.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=3,importance=TRUE,type="class")
rf.data
data.pred=predict(rf.data,data[-train,],type="class")
tab<-table(data.test, data.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(bag.data)
varImpPlot(bag.data)
importance(rf.data)
varImpPlot(rf.data)
rf.data=randomForest(hd~.,data=data.imputed,subset=train,mtry=2,importance=TRUE,type="class")
rf.fata
rf.data
data.pred.rf=predict(rf.data,data[-train,],type="class")
tab<-table(data.test,data.pred.rf)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(rf.data)
varImpPlot(rf.data)
set.seed(1)
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
summary(boost.data)
train = sample (1: nrow(data), nrow(data)/2)
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
summary(boost.data)
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=5000,distribution="multinomial",type="response")
class.pred<-rep(0,152)
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=458,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag #....this is very good
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=900,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.034 ...no change
classAgreement(tab)$crand
boost.data=gbm(hd~.,data=data[train,],distribution="multinomial",n.trees=1000,interaction.depth=1,shrinkage=0.01)
summary(boost.wine)
summary(boost.data)
gbm.perf(boost.data)
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=412,distribution="multinomial",type="response")
for(i in 1:152){
which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=450,distribution="multinomial",type="response")
tab<-table(data.test,class.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
