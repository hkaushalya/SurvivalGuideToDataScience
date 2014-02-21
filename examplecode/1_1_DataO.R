
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "DataO"
cat(paste0("\\newcommand{\\Module}{", Module, "}"))


## ----setup, child="mycourse.Rnw"-----------------------------------------

## ----setup_options, include=FALSE----------------------------------------
library(knitr)
library(xtable)

opts_chunk$set(cache=FALSE)

opts_chunk$set(out.width='0.8\\textwidth')
opts_chunk$set(fig.align='center')

opts_chunk$set(src.top=NULL)
opts_chunk$set(src.bot=NULL)
opts_chunk$set(out.lines=4)
opts_chunk$set(out.truncate=80)

opts_chunk$set(fig.path=sprintf("figures/%s/", Module))
opts_chunk$set(cache.path=sprintf("cache/%s/", Module))
opts_chunk$set(bib.file=paste0(Module, ".bib"))

# Leave code as I have formatted it.

opts_chunk$set(tidy=FALSE)

# Hooks

# Truncate long lines and long output

hook_output <- knit_hooks$get("output")
hook_source <- knit_hooks$get("source")
knit_hooks$set(output=function(x, options) 
{
  if (options$results != "asis")
  {
    # Split string into separate lines.
    x <- unlist(stringr::str_split(x, "\n"))
    # Trim to the number of lines specified.
    if (!is.null(n <- options$out.lines)) 
    {
      if (length(x) > n) 
      {
        # Truncate the output.
        x <- c(head(x, n), "....\n")
      }
    }
    # Truncate each line to length specified.
    if (!is.null(m <- options$out.truncate))
    {
      len <- nchar(x)
      x[len>m] <- paste0(substr(x[len>m], 0, m-3), "...")
    }
    # Paste lines back together.
    x <- paste(x, collapse="\n")
    # Replace ' = ' with '=' - my preference. Hopefully won't 
    # affect things inappropriately.
    x <- gsub(" = ", "=", x)
  }
  hook_output(x, options)
},
source=function(x, options)
{
  # Split string into separate lines.
  x <- unlist(stringr::str_split(x, "\n"))
  # Trim to the number of lines specified.
  if (!is.null(n <- options$src.top)) 
  {
    if (length(x) > n) 
    {
      # Truncate the output.
      if (is.null(m <-options$src.bot)) m <- 0
      x <- c(head(x, n+1), "\n....\n", tail(x, m+2)) 
   }
  }
  # Paste lines back together.
  x <- paste(x, collapse="\n")
  hook_source(x, options)
})

# Optionally allow R Code chunks to be environments so we can refer to them.

knit_hooks$set(rcode=function(before, options, envir) 
{
  if (before)
    sprintf('\\begin{rcode}\\label{%s}\\hfill{}', options$label)
  else
    '\\end{rcode}'
})




## ----load_packages, message=FALSE----------------------------------------
library(rattle)       # The weather dataset and normVarNames().
library(randomForest) # Use na.roughfix() to deal with missing data.
library(ggplot2)      # Plots.


## ----common_intro, child='documentation.Rnw', eval=TRUE------------------


## ----help_library, eval=FALSE, tidy=FALSE--------------------------------
## ?read.csv


## ----help_package, eval=FALSE--------------------------------------------
## library(help=rattle)


## ----record_start_time, echo=FALSE---------------------------------------
start.time <- proc.time()


## ----generate_bib, echo=FALSE, message=FALSE, warning=FALSE--------------
# Write all packages in the current session to a bib file
if (is.null(opts_chunk$get("bib.file"))) opts_chunk$set(bib.file="Course.bib")
write_bib(sub("^.*/", "", grep("^/", searchpaths(), value=TRUE)),
          file=opts_chunk$get("bib.file"))
system(paste("cat extra.bib >>", opts_chunk$get("bib.file")))
# Fix up specific issues.
# R-randomForest
system(paste("perl -pi -e 's|Fortran original by Leo Breiman",
             "and Adele Cutler and R port by|Leo Breiman and",
             "Adele Cutler and|'", opts_chunk$get("bib.file")))
# R-C50
system(paste("perl -pi -e 's|. C code for C5.0 by R. Quinlan|",
             " and J. Ross Quinlan|'", opts_chunk$get("bib.file")))
# R-caret
system(paste("perl -pi -e 's|. Contributions from|",
             " and|'", opts_chunk$get("bib.file")))
# Me
system(paste("perl -pi -e 's|Graham Williams|",
             "Graham J Williams|'", opts_chunk$get("bib.file")))




## ----eval=FALSE----------------------------------------------------------
## dspath <- "http://rattle.togaware.com/weather.csv"


## ------------------------------------------------------------------------
dspath <- system.file("csv", "weather.csv", package="rattle")


## ----dataset_load--------------------------------------------------------
weather <- read.csv(dspath)


## ----prepare_the_dataset, out.lines=7------------------------------------
dsname <- "weather"
ds     <- get(dsname)
dim(ds)
names(ds)


## ----alternative_dataset_assignment, eval=FALSE--------------------------
## ds <- weather


## ----dataset_head, out.lines=10------------------------------------------
head(ds)


## ----dataset_tail, out.lines=10------------------------------------------
tail(ds)


## ----dataset_sample, out.lines=10----------------------------------------
ds[sample(nrow(ds), 6),]


## ----dataset_structure, out.lines=30-------------------------------------
str(ds)


## ----dataset_summary, out.lines=43---------------------------------------
summary(ds)


## ----message=FALSE-------------------------------------------------------
names(ds)
names(ds) <- normVarNames(names(ds))
names(ds)


## ------------------------------------------------------------------------
sapply(ds, class)


## ----convert_date, message=FALSE-----------------------------------------
library(lubridate)
head(ds$date)
ds$date <- ymd(as.character(ds$date))
head(ds$date)


## ------------------------------------------------------------------------
sapply(ds, class)


## ----variable_roles, out.lines=NULL--------------------------------------
(vars  <- names(ds))
target <- "rain_tomorrow"
risk   <- "risk_mm"
id     <- c("date", "location")


## ------------------------------------------------------------------------
ignore <- union(id, if (exists("risk")) risk)


## ------------------------------------------------------------------------
(ids   <- which(sapply(ds, function(x) length(unique(x))) == nrow(ds)))
ignore <- union(ignore, ids)


## ----ignore_missing_variables--------------------------------------------
mvc <- sapply(ds[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(ds)))
ignore <- union(ignore, mvn)


## ----ignore_mostly_missing_variables-------------------------------------
mvn <- names(which(mvc >= 0.7*nrow(ds)))
ignore <- union(ignore, mvn)


## ----ignore_factors_with_many_levels-------------------------------------
factors <- which(sapply(ds[vars], is.factor))
lvls    <- sapply(factors, function(x) length(levels(ds[[x]])))
(many   <- names(which(lvls > 20)))
ignore  <- union(ignore, many)


## ----ignore_variables_constant_values------------------------------------
(constants <- names(which(sapply(ds[vars], function(x) all(x == x[1L])))))
ignore     <- union(ignore, constants)


## ------------------------------------------------------------------------
length(vars)
vars <- setdiff(vars, ignore)
length(vars)


## ----remove_missing_target-----------------------------------------------
dim(ds)
sum(is.na(ds[target]))
ds <- ds[!is.na(ds[target]),]
sum(is.na(ds[target]))
dim(ds)


## ------------------------------------------------------------------------
ods <- ds


## ----impute_missing_values-----------------------------------------------
dim(ds[vars])
sum(is.na(ds[vars]))
ds[vars] <- na.roughfix(ds[vars])
sum(is.na(ds[vars]))
dim(ds[vars])


## ------------------------------------------------------------------------
ds <- ods


## ------------------------------------------------------------------------
ods <- ds
omit <- NULL


## ----remove_missing_values-----------------------------------------------
dim(ds[vars])
sum(is.na(ds[vars]))
mo <- attr(na.omit(ds[vars]), "na.action")
omit <- union(omit, mo)
if (length(omit)) ds <- ds[-omit,]
sum(is.na(ds[vars]))
dim(ds[vars])


## ------------------------------------------------------------------------
ds <- ods


## ----normalise_factors---------------------------------------------------
factors <- which(sapply(ds[vars], is.factor))
for (f in factors) levels(ds[[f]]) <- normVarNames(levels(ds[[f]]))


## ----ensure_target_is_categoric------------------------------------------
ds[target] <- as.factor(ds[[target]])
table(ds[target])


## ----fig.height=4--------------------------------------------------------
p <- ggplot(ds, aes_string(x=target))
p <- p + geom_bar(width=0.2)
print(p)


## ----identify_variables--------------------------------------------------
(inputs  <- setdiff(vars, target))
(nobs    <- nrow(ds))
dim(ds[vars])


## ----identify_variable_types---------------------------------------------
numi       <- which(sapply(ds[inputs], is.numeric))
numi
numerics   <- names(numi)
numerics
cati       <- which(sapply(ds[inputs], is.factor))
cati
categorics <- names(cati)
categorics


## ----eval=FALSE----------------------------------------------------------
## dsdate <- paste0("_", format(Sys.Date(), "%y%m%d"))
## dsrdata <- paste0(dsname, dsdate, ".RData")
## save(ds, dsname, dspath, dsdate, target, risk, id, ignore, vars,
##      nobs, omit, inputs, numi, numerics, cati, categorics, file=dsrdata)


## ----echo=FALSE----------------------------------------------------------
# Do this so we know what to load into ModelsO.Rnw
dsdate <- paste0("_", "130704")
dsrdata <- paste0(dsname, dsdate, ".RData")
save(ds, dsname, dspath, dsdate, target, risk, id, ignore, vars,
     nobs, omit, inputs, numerics, categorics, file=dsrdata)


## ------------------------------------------------------------------------
(load(dsrdata))
dsname
dspath
dsdate
dim(ds)
id
target
risk
ignore
vars


## ----review_load, eval=FALSE---------------------------------------------
## # Required packages
## library(rattle)		# normVarNames()
## library(randomForest)	# Impute missing using na.roughfix()
## 
## # Data setup
## dspath     <- system.file("csv", "weather.csv", package="rattle")
## weather    <- read.csv(dspath)
## dsname     <- "weather"
## ds         <- get(dsname)
## names(ds)  <- normVarNames(names(ds)) # Optional lower case variable names.
## vars       <- names(ds)
## target     <- "rain_tomorrow"
## risk       <- "risk_mm"
## id         <- c("date", "location")
## 
## # Summarise
## dim(ds)
## names(ds)
## head(ds)
## tail(ds)
## ds[sample(nrow(ds), 6),]
## str(ds)
## summary(ds)
## 
## # Variables to ignore
## ignore     <- c(id, if (exists("risk")) risk)
## mvc        <- sapply(ds[vars], function(x) sum(is.na(x))) # Missing value count.
## mvn        <- names(ds)[(which(mvc == nrow(ds)))]         # Missing var names.
## ignore     <- union(ignore, mvn)
## 
## factors    <- which(sapply(ds[vars], is.factor))
## lvls       <- sapply(factors, function(x) length(levels(ds[[x]])))
## many       <- names(which(lvls > 20))	# Factors with too many levels.
## ignore     <- union(ignore, many)
## 
## vars       <- setdiff(vars, ignore)
## 
## # Normalise factors
## factors    <- which(sapply(ds[vars], is.factor))
## for (f in factors) levels(ds[[f]]) <- normVarNames(levels(ds[[f]]))
## 
## # Remove all observations with a missing target.
## ds         <- ds[!is.na(ds[target]),]
## 
## # Optionally impute missing values, but do this wisely - understand why missing.
## if (sum(is.na(ds[vars]))) ds[vars] <- na.roughfix(ds[vars])
## 
## # Observations to omit
## omit       <- NULL
## mo         <- attr(na.omit(ds[vars]), "na.action")
## omit       <- union(omit, mo)
## if (length(omit)) ds <- ds[-omit,]	# Optional remove ommited observations.


## ----review_finalise, eval=FALSE-----------------------------------------
## # Finalise
## ds[target] <- as.factor(ds[[target]])
## inputs     <- setdiff(vars, target)
## nobs       <- nrow(ds)
## numi       <- which(sapply(ds[inputs], is.numeric))
## numerics   <- names(numi)
## cati       <- which(sapply(ds[inputs], is.factor))
## categorics <- names(cati)
## 
## # Save the data
## dsdate     <- paste0("_", format(Sys.Date(), "%y%m%d"))
## dsrdata    <- paste0(dsname, dsdate, ".RData")
## save(ds, dsname, dspath, dsdate, target, risk, id, ignore, vars,
##      nobs, omit, inputs, numerics, categorics, file=dsrdata)


## ----common_outtro, child="finale.Rnw", eval=TRUE------------------------


## ----syinfo, child="sysinfo.Rnw", eval=TRUE------------------------------

## ----echo=FALSE, message=FALSE-------------------------------------------
require(Hmisc)
pkg <- "knitr"
pkg.version <- installed.packages()[pkg, 'Version']
pkg.date <- installed.packages(fields="Date")[pkg, 'Date']
pkg.info <- paste(pkg, pkg.version, pkg.date)

rev <- system("bzr revno", intern=TRUE)
cpu <- system(paste("cat /proc/cpuinfo | grep 'model name' |",
                    "head -n 1 | cut -d':' -f2"), intern=TRUE)
ram <- system("cat /proc/meminfo | grep MemTotal: | awk '{print $2}'",
              intern=TRUE)
ram <- paste0(round(as.integer(ram)/1e6, 1), "GB")
user <- Sys.getenv("LOGNAME")
node <- Sys.info()[["nodename"]]
user.node <- paste0(user, "@", node)
gcc.version <- system("g++ -v 2>&1 | grep 'gcc version' | cut -d' ' -f1-3",
                      intern=TRUE)
os <- system("lsb_release -d | cut -d: -f2 | sed 's/^[ \t]*//'", intern=TRUE)






