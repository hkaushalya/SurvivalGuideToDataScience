rm(list=ls())
load('data//weather_140220.RData')

library(rattle)       # fancyRpartPlot()
library(rpart)        # rpart()
library(randomForest) # randomForest()
library(ada)          # ada()
library(ROCR)         # prediction()
library(party)        # ctree() and cforest()
library(ggplot2)      # Frequency plots

# Form the formula
form <- formula(paste0(target, "~ ."))  #targer (rain_tomorrow)
form
# Training and test data prep
set.seed(123)

# Split to 70%-30%
length(train <- sample(nobs, 0.7*nobs))     # just the indices!
length(test <- setdiff(seq_len(nobs), train))

# get the actual data
actual.train <- ds[train, target] # only target (rain_tomorrow) values in train data
actual       <- ds[test, target]  # only target values in test data
risks        <- ds[test, risk]    # risk == 'risk_mm'

# Build -Decision Tree
# rpart 

ctrl <- rpart.control(maxdepth=10)
system.time(model <- m.rp <- rpart(form, ds[train, vars], control=ctrl))
print(model)

opar <- par()
plot.new()
par(mar=c(2,1,2,1))
fancyRpartPlot(model)  #??? could not find fuction????
plot(model)
text(model, use.n=T)
par(opar)  #reset par to default


# Evaluate - Training Accuracy and AUC (area under the curve)

head (cl <- predict(model, ds[train, vars], type="class"))
head (actual.train)

# Compare with the actual class for these observations from the training dataset.
acc <- sum(cl == actual.train, na.rm=TRUE)/length(actual.train)  # do we expect na's is here?
acc

# overrall error rate
err <- sum(cl != actual.train, na.rm=TRUE)/length(actual.train)
err

# Area under the ROC
pr   <- predict(model, ds[train, vars], type="prob")[,2]
pred <- prediction(pr, ds[train, target])
atr  <- attr(performance(pred, "auc"), "y.values")[[1]]
atr

# On test data
cl <- predict(model, ds[test, vars], type="class")
#overall accuracy
acc <- sum(cl==actual, na.rm=TRUE)/length(actual)
acc
#overall err rate
err <- sum(cl!=actual, na.rm=TRUE)/length(actual)
err
# The overall error rate is 16% much higher than on train data as we 
# expect the train data to be biased!

pr   <- predict(model, ds[test, vars], type="prob")[,2]
pred <- prediction(pr, ds[test, target])
ate  <- attr(performance(pred, "auc"), "y.values")[[1]]
ate   # AUC


# Evaluate - Confusion Matrix
# On training dataset
cl <- predict(model, ds[train, vars], type="class")
round(100*table(actual.train, cl, dnn=c("Actual", "Predicted"))/length(actual.train))

# On test dataset
pr <- predict(model, ds[test, vars], type="class")
round(100*table(actual, pr, dnn=c("Actual", "Predicted"))/length(actual))

# Evaluate - Risk Chart
# risk chart == accumulative performance plot
riskchart(pr, ds[test, target], ds[test, risk])
length(pr)
length(ds[test,target])
nrow(ds[test,target])
ds[test, target]
