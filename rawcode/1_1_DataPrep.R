#This is a list of task from 
# http://www.datasciencecentral.com/profiles/blogs/one-page-r-a-survival-guide-to-data-science-with-r
rm(list=ls())
INSTALL <- FALSE

if (INSTALL) {
  install.packages(rattle, randomForest, ggplot2, lubricate, dependencies=T)
}
library(rattle)
library(randomForest)
library(ggplot2)

dspath <- system.file("csv", "weather.csv", package="rattle")
weather <- read.csv(dspath)

dsname <- "weather"
ds <- get(dsname)  #being more genereic. this allows load different datasets to ds

#Meta Data Cleansing
# Normalize Variable Names

names(ds) <- normVarNames(names(ds))

# Data Formats Review
library(lubridate)
sapply(ds, class)
ds$date <- ymd(as.character(ds$date))

# ID variables' role
vars <- names(ds)

target <- "rain_tomorrow"
risk <- "risk_mm"
id <- c("date", "location") 

# Clean - Ignore IDs, Outputs, Missing
ignore <- union(id, if (exists("risk")) risk)

# may want to identify variable that has a uniqie value for every observation
ids <- which(sapply(ds, function(x) length(unique(x)) == nrow(ds) ))
ids

ignore <- union(ignore, ids)

# All Missing??
# count the number of missing values for each variable, and then list
# the names of those variables with only missing values!
mvc <- sapply(ds, function(x) sum(is.na(x)))
mvn <- names(which(mvc==nrow(ds)))
ignore <- union(ignore, mvn)

# Many Missing ?
# If we want to ignore variables with more than 70% of the values missing?
mvn <- names(which(mvc >= 0.7 * nrow(ds)))
ignore <- union(ignore, mvn)


#9: Clean - Ignore MultiLevel, Constants
# Too Many Levels? may want to ignore these variables or regroup
# to smaller number of levels

factors <- which(sapply(ds[vars], is.factor))
lvls <- sapply(factors, function(x) length(levels(ds[[x]])))????
many <- names(which(lvls > 20))

#ignore <- union(ignore, many)

# Constants: Ignore variables with constant values
constants <- names(which(sapply(ds[vars], function(x) all(x == x[1L]))))

# 10: Clean - Remove the variables
# Remove the identified variables to ignore
length(vars)

vars <- setdiff(vars, ignore)
length(vars)

# 11: Clean: Remove Missing Target
# May need to convert (target) variable to correct type, remove missing values

dim(ds)
sum(is.na(ds[target]))
ds <- ds[!is.na(ds[target]),]
sum(is.na(ds[target])) #should be 0
dim(ds)

# 12: Clean: Deal with Missing Values
# randomForest cannot deal with NA
# rpart has the tools

# Using na.roughfix() from randomForest
# To demonstrate the process!!!
ods <- ds
dim(ds[vars])
sum(is.na(ds[vars]))
ds[vars] <- na.roughfix(ds[vars])
sum(is.na(ds[vars]))
dim(ds[vars])
ds <- ods

# 13: Clean - Omitting Observations

ods <- ds
omit <- NULL
dim(ds[vars])
sum(is.na(ds[vars]))

mo <- attr(na.omit(ds[vars]), "na.action") # assigns na.action (or 'omit') 
mo
omit <- union(omit, mo)
if (length(omit)) ds <-ds[-omit,]
sum(is.na(ds[vars]))
dim(ds[vars])

# Restore the dataset
ds <- ods

# Clean - Normalise Factors
# Some variables will have levels with spaces, and mixture of cases, etc
# So we may want to normalize the levels for each of the categorical variables

factors <- which(sapply(ds[vars], is.factor))
for (f in factors) levels(ds[[f]]) <- normVarNames(levels(ds[[f]]))

# Clean - Ensure Target is Categoric
ds[target] <- as.factor(ds[[target]])
table(ds[target])

# Visualize target variable factor

p <- ggplot(ds, aes_string(x=target))
p <- p + geom_bar(width=0.2)
print(p)


# Prepare - Variables
# Identify variables to build the model
inputs <- setdiff(vars, target)

nobs <- nrow(ds)
dim(ds[vars])


# may need to identify numeric and categorical variables
numi <- which(sapply(ds[inputs], is.numeric))
numi
numerics <-names(numi)

cati <- which(sapply(ds[inputs], is.factor))
cati
categorics <- names(cati)

# Prepare - Save Dataset
# Once the data shape is correct save it for future.
dsdate <- paste0("_", format(Sys.Date(), "%y%m%d"))
dsrdata <- paste0("data/",dsname, dsdate, ".RData")
save(ds, dsname, dspath, dsdate, target, risk, id, ignore, vars,
     nobs, omit, inputs, numi, numerics, cati, categorics, file=dsrdata)
