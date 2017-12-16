

# Importing the dataset
dset <- read.csv('TRAIN.csv')
# dataset1 = dataset[1:100]
# t_set <- read.csv('stock_test_data_20171125.csv')
# t_set_id <- t_set$id
tset <- read.csv('test.csv')
tset_id <- tset$portfolio_id
# library("ggplotgui")
# ggplot_shiny(dataset)
unique(unlist(dset$currency, use.names = FALSE))

dset <- dset[-c(573, 574, 7476, 7656,1666), ]
 


# check OUTLIERS 
library(fpmoutliers)
# dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
model <- FPI(dset, minSupport = 0.001)
dataFrame <- dataFrame[order(model$scores, decreasing = TRUE),]
print(dataFrame[1,]) # instance with the highest anomaly score
print(dataFrame[nrow(dataFrame),]) # instance with the lowest anomaly score


#scater plot

library(scatr)
scat(dset, x ='libor_rate', y ='return', line ='linear', se = TRUE)





library(Quandl)
chf <- Quandl("FED/RXI_N_B_SZ", api_key="UJFKNvdn4_uyFcsrT2At", start_date="1999-01-04")
cad <- Quandl("FED/RXI_N_B_CA", api_key="UJFKNvdn4_uyFcsrT2At")
# eur <- Quandl("FED/RXI_US_N_A_EU", api_key="UJFKNvdn4_uyFcsrT2At")
eur <- Quandl("FED/RXI_US_N_B_EU", api_key="UJFKNvdn4_uyFcsrT2At")
jpy <- Quandl("FED/RXI_N_B_JA", api_key="UJFKNvdn4_uyFcsrT2At")
gbp <- Quandl("FED/RXI_US_N_B_UK", api_key="UJFKNvdn4_uyFcsrT2At")
# chf$Date <- as.Date(chf$Date, format = '%Y%m%d')
cad$Date <- format(cad$Date, format = "%Y%m%d")
eur$Date <- format(eur$Date, format = "%Y%m%d")
jpy$Date <- format(jpy$Date, format = "%Y%m%d")
gbp$Date <- format(gbp$Date, format = "%Y%m%d")
chf$Date <- format(chf$Date, format = "%Y%m%d")


#  Convert to date if not already
cad$Date <- as.Date(cad$Date, format = '%Y%m%d')
eur$Date <- as.Date(eur$Date, format = '%Y%m%d')
jpy$Date <- as.Date(jpy$Date, format = '%Y%m%d')
gbp$Date <- as.Date(gbp$Date, format = '%Y%m%d')
chf$Date <- as.Date(chf$Date, format = '%Y%m%d')
#  Get months
cad$Month <- months(cad$Date)
eur$Month <- months(eur$Date)
jpy$Month <- months(jpy$Date)
gbp$Month <- months(gbp$Date)
chf$Month <- months(chf$Date)

#  Get years
cad$Year <- format(cad$Date,format="%Y")
eur$Year <- format(eur$Date,format="%Y")
jpy$Year <- format(jpy$Date,format="%Y")
gbp$Year <- format(gbp$Date,format="%Y")
chf$Year <- format(chf$Date,format="%Y")
 
# cad$aggr <- aggregate( Value ~ Month + Year , cad , mean )
# eur$aggr <- aggregate( Value ~ Month + Year , eur , mean )
# jpy$aggr <- aggregate( Value ~ Month + Year , jpy , mean )
# chf$aggr <- aggregate( Value ~ Month + Year , chf , mean )
# gbp$aggr <- aggregate( Value ~ Month + Year , gbp , mean )

library(reshape2)
library(dplyr)
# for (i in 1:nrow(dset)){

# o <-  aggregate( cad$Value ~ cad$Month + cad$Year, cad, mean )
#aggregated rates data
ocad <-  aggregate( Value ~ Month + Year, cad, mean )
ochf <- aggregate( Value ~ Month + Year, chf, mean )
ogbp <- aggregate( Value ~ Month + Year, gbp, mean )
ojpy <- aggregate( Value ~ Month + Year, jpy, mean )
oeur <- aggregate( Value ~ Month + Year, eur, mean )
# o <- cbind(ocad, ochf, ogbp, ojpy, oeur)
# o <- paste(ocad, ochf, ogbp, ojpy, oeur) 
library(tidyr)
ocad <- unite(ocad, date, c(Year, Month))
ochf <- unite(ochf, date, c(Year, Month))
ogbp <- unite(ogbp, date, c(Year, Month))
ojpy <- unite(ojpy, date, c(Year, Month))
oeur <- unite(oeur, date, c(Year, Month))

rates_merged <- merge(x = ocad, y = ochf, by = "date", suffixes = c('_cad', '_chf'), all = TRUE)
rates_merged <- merge(x = rates_merged, y = ogbp, by = "date", suffixes = c('_chf', '_gbp'), all = TRUE)
rates_merged <- merge(x = rates_merged, y = ojpy, by = "date", suffixes = c('_gbp', '_jpy'), all = TRUE)
rates_merged <- merge(x = rates_merged, y = oeur, by = "date", suffixes = c('_jpy', '_eur'), all = TRUE)
# library(plyr)
# rename(rates_merged, c("Value"="Value_oeur"))
#rename 
names(rates_merged)[names(rates_merged)=="Value"] <- "Value_eur"


# o <-  cbind(o, aggregate( chf$Value ~ chf$Month + chf$Year, chf, mean ))
# melt(o, id = c("Month", "Year"), aggregate( chf$Value ~ Month + Year, chf, mean ))
# dcast(o, Month + Year ~ aggregate(chf$Value ~ Month + Year, chf, mean ))
# o1<-dcast(o, Value ~ Month + Year)
# aggregate( chf$Value ~ Month + Year, chf, mean ))

# o1 <- NULL
#Air quality example
dset$creation_date <- format(dset$creation_date, format = "%YYYY-%mm-%dd")
dset$creation_date <- as.Date(dset$creation_date, format = '%Y%m%d')
dset$start_date <- format(dset$start_date, format = "%YYYY-%mm-%dd")
dset$start_date <- as.Date(dset$start_date, format = '%Y%m%d')
dset$sell_date <- format(dset$sell_date, format = "%YYYY-%mm-%dd")
dset$sell_date <- as.Date(dset$sell_date, format = '%Y%m%d')
dset$Month_creation <- months(dset$creation_date)
dset$Month_sell <- months(dset$sell_date)
dset$Year_creation <- format(dset$creation_date, format='%Y')
dset$Year_sell <- format(dset$sell_date, format='%Y')

tset$creation_date <- format(tset$creation_date, format = "%YYYY-%mm-%dd")
tset$creation_date <- as.Date(tset$creation_date, format = '%Y%m%d')
tset$start_date <- format(tset$start_date, format = "%YYYY-%mm-%dd")
tset$start_date <- as.Date(tset$start_date, format = '%Y%m%d')
tset$sell_date <- format(tset$sell_date, format = "%YYYY-%mm-%dd")
tset$sell_date <- as.Date(tset$sell_date, format = '%Y%m%d')
tset$Month_creation <- months(tset$creation_date)
tset$Month_sell <- months(tset$sell_date)
tset$Year_creation <- format(tset$creation_date, format='%Y')
tset$Year_sell <- format(tset$sell_date, format='%Y')

#merge sell/bought dates in to single column
dset <- unite(dset, sell_d, c(Year_sell, Month_sell))
dset <- unite(dset, buy_d, c(Year_creation, Month_creation))
dset <- unite(dset, desk, c(desk_id, office_id))

tset <- unite(tset, sell_d, c(Year_sell, Month_sell))
tset <- unite(tset, buy_d, c(Year_creation, Month_creation))
tset <- unite(tset, desk, c(desk_id, office_id))

# date$cr <- as.Date(dset$creation_date, format = '%Y-%m-%d')

dset$profit <- dset$sold - dset$bought 
tset$profit <- tset$sold - tset$bought 
# sum(dset$profit, na.rm=TRUE)
# sign(dset[12, 'return']) sign(dset[12, 'profit'])

# # New Variable = days portfolio held before sell
# date <- format(dset$creation_date, format = "%Y-%m-%d")
# date <- data.frame(date)
# 
# date$sell <- format(dset$sell_date, format = "%Y-%m-%d")
# date$days <- difftime(date$sell, date$date, units = c('days'))
dset$days <- difftime(dset$sell_date, dset$start_date, units = c('days'))
tset$days <- difftime(tset$sell_date, tset$start_date, units = c('days'))
# is.na(dset$profit)
# anyNA(dset$profit)

# for (i in 1:nrow(dset)) {
#   if (is.na(dset[i, 'profit']) == TRUE){
#     print(i)
#   } else {
#     o <- rbind(i)
#   }
# }

# transform profit to rates/usd value
for (i in 1:nrow(dset)) {
  
  if (dset[i,"currency"] == 'CAD') {
    dset[i,"profit"] <- dset[i, "profit"] * rates_merged[rates_merged[,'date'] == dset[i,'sell_d'], 'Value_cad']
  } else if (dset[i,"currency"] == 'GBP') {
    dset[i,"profit"] <- dset[i, "profit"] * rates_merged[rates_merged[,'date'] == dset[i,'sell_d'], 'Value_gbp']
  } else if (dset[i,"currency"] == 'EUR') {
    dset[i,"profit"] <- dset[i, "profit"] * rates_merged[rates_merged[,'date'] == dset[i,'sell_d'], 'Value_eur']
  } else if (dset[i,"currency"] == 'JPY') {
    dset[i,"profit"] <- dset[i, "profit"] * rates_merged[rates_merged[,'date'] == dset[i,'sell_d'], 'Value_jpy']
  } else if (dset[i,"currency"] == 'CHF') {
  dset[i,"profit"] <- dset[i, "profit"] * rates_merged[rates_merged[,'date'] == dset[i,'sell_d'], 'Value_chf']
    
  } else {
    NULL
  }
}

for (i in 1:nrow(tset)) {
  
  if (tset[i,"currency"] == 'CAD') {
    tset[i,"profit"] <- tset[i, "profit"] * rates_merged[rates_merged[,'date'] == tset[i,'sell_d'], 'Value_cad']
  } else if (tset[i,"currency"] == 'GBP') {
    tset[i,"profit"] <- tset[i, "profit"] * rates_merged[rates_merged[,'date'] == tset[i,'sell_d'], 'Value_gbp']
  } else if (tset[i,"currency"] == 'EUR') {
    tset[i,"profit"] <- tset[i, "profit"] * rates_merged[rates_merged[,'date'] == tset[i,'sell_d'], 'Value_eur']
  } else if (tset[i,"currency"] == 'JPY') {
    tset[i,"profit"] <- tset[i, "profit"] * rates_merged[rates_merged[,'date'] == tset[i,'sell_d'], 'Value_jpy']
  } else if (tset[i,"currency"] == 'CHF') {
    tset[i,"profit"] <- tset[i, "profit"] * rates_merged[rates_merged[,'date'] == tset[i,'sell_d'], 'Value_chf']
    
  } else {
    NULL
  }
}

#delete unimportant columns 
# dset <- dset[,-c('start_date', 'creation_date', 'sell_date')]
dset$start_date <- NULL
dset$sell_date <- NULL
dset$creation_date <- NULL
dset$portfolio_id <- NULL

tset$start_date <- NULL
tset$sell_date <- NULL
tset$creation_date <- NULL
tset$portfolio_id <- NULL
#encode dates labels
library(CatEncoders)
# dset <- data.frame(dset)
renc <- LabelEncoder.fit(dset$currency)
dset$currency <- transform(renc, dset$currency)
tset$currency <- transform(renc, tset$currency)
renc1 <-LabelEncoder.fit(dset$pf_category)
dset$pf_category <- transform(renc1, dset$pf_category)
tset$pf_category <- transform(renc1, tset$pf_category)
renc2 <- LabelEncoder.fit(dset$desk)
dset$desk <- transform(renc2, dset$desk)
tset$desk <- transform(renc2, tset$desk)
renc3 <- LabelEncoder.fit(dset$country_code)
dset$country_code <- transform(renc3, dset$country_code)
tset$country_code <- transform(renc3, tset$country_code)
renc5 <- LabelEncoder.fit(dset$type)
dset$type <- transform(renc5, dset$type)
tset$type <- transform(renc5, tset$type)
renc6 <- LabelEncoder.fit(dset$sell_d)
dset$sell_d <- transform(renc6, dset$sell_d)
tset$sell_d <- transform(renc6, tset$sell_d)
renc7 <- LabelEncoder.fit(dset$buy_d)
dset$buy_d <-transform(renc7, dset$buy_d)
tset$buy_d <-transform(renc7, tset$buy_d)
# unite 3 binary columns
dset <- unite(dset, indicators, c(indicator_code, hedge_value, status))
tset <- unite(tset, indicators, c(indicator_code, hedge_value, status))

renc8 <- LabelEncoder.fit(dset$indicators)
dset$indicators <-transform(renc8, dset$indicators)
tset$indicators <-transform(renc8, tset$indicators)
#days to numeric
dset$days <- as.numeric(as.character(dset$days))
tset$days <- as.numeric(as.character(tset$days))

is.numeric(dset)

# renc4 <- OneHotEncoder.fit(dset$indicator_code)
# dset$indicator_code <- transform(renc4, dset$indicator_code)



library(xgboost)
dlabel <- dset$return
dlabel <-as.matrix(dlabel)
dset$return <- NULL

data <- as.matrix(dset)
# data <- as.matrix(as.numeric(dset))

# weightdata <- dataset$weight
# dtrain <- xgb.DMatrix(data,weight=weightdata,label = dlabel)
dtrain <- xgb.DMatrix(data,label = dlabel)

# data <- as.numeric(as.character(data))
# data <- as.numeric(dset)

test <- as.matrix(tset)
dtest <- xgb.DMatrix(test)

# params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
# xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

bstSparse <- xgboost(data = data, label = dlabel, max.depth = 4, gamma = 0.001, eta = 0.01, nround = 500, subsample = 0.01, lambda = 0.001, alpha = 0.001, colsample_bytree = 0.1, eval.metric = "rmse", print_every_n = 50, objective = "reg:linear")

pred = predict(bstSparse,test)



final <- data.frame(portfolio_id = tset_id, return = pred);
write.csv(final, paste0("preds.csv"), row.names =F, quote=F)

#scater plot

library(scatr)
scat(dset, x ='libor_rate', y ='return', line ='linear', se = TRUE)




####################################### Gridsearch xgboost

searchGridSubCol <- expand.grid(subsample = c(0.01, 0.1, 0.5), 
                                colsample_bytree = c( 0.1, 0.5),
                                max_depth = c(3, 4, 5),
                                # min_child = seq(1), 
                                # nrounds = c(450, 1000),
                                lambda = c(.001, .01, .1),
                                alpha = c(.001, .01, .1),
                                eta = c(0.01, 0.1)
)

# ntrees <- 5

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
#Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentlambda <- parameterList[["lambda"]]
    currentalpha <- parameterList[["alpha"]]
    # currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = 500, nfold = 2, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta, "lambda" = currentlambda, "alpha" = currentalpha,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentlambda, currentalpha))
    
  }))

output <- as.data.frame(t(rmseErrorsHyperparameters))
head(output)
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentlambda", "currentalpha")
names(output) <- varnames
head(output)
write.csv(output, "xgb_gridsearch.csv")

############################


