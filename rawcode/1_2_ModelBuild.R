load('data//weather_140220.RData')

# Form the formula
form <- formula(paste(target, "~ ."))  #targer (rain_tomorrow)

# Training and test data prep
set.seed(123)

# Split to 70%-30%
length(train <- sample(nobs, 0.7*nobs))
length(test <- setdiff(seq_len(nobs), train))