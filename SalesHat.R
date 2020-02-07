# ========================================================================================================== #
# ========================================================================================================== # 
#
#
# Title: Simple sales prediction exercise 
# Author: Jakob Everding 
# Date: 16.12.2019 
#
# This script prepares the R environment (e.g. loading packages,  defining functions), 
# imports the data and pre-processes it. Eventually the script also conducts several 
# predictive analyses, including model desin and basc benchmarking. 
#
#
# ========================================================================================================== #
# ========================================================================================================== #
# Remove all objects from the workspace
rm( list=ls() )

library(foreign)
library(reshape2)
library(plyr)
library(dplyr)
library(colorspace)
library(stringr)
library(readxl)
library(stringi)
library(bindrcpp)
library(sandwich)
library(clubSandwich)
library(plm)
library(lmtest)
library(Hmisc)
library(ggplot2)
library(GGally)
library(grf)
library(glmnet)
library(randomForest)
library(gbm)
library(caret)
library(scales)
library(stargazer)
library(xtable)


# setup ------------------------------------------------------------------------------------------------------
main_dir <- getwd()
code_dir <- file.path(main_dir,"01_Code")
data_in_dir <- file.path(main_dir,"02_Data\\01_Raw Data")
data_temp_dir <- file.path(main_dir,"02_Data\\02_Temp Data")
data_out_dir <- file.path(main_dir,"02_Data\\03_Preprocessed Data")
output_dir <- file.path(main_dir,"03_Output")
# set seed 
seed.set <- 191215 


# functions --------------------------------------------------------------------------------------------------


# start ==================================================================================================== # 
# load data --------------------------------------------------------------------------------------------------
data.a <- read.csv(file.path(data_in_dir,"data_pt_a.csv")) #head(data.a)
data.b <- read.csv(file.path(data_in_dir,"data_pt_a.csv")) #View(data.b)

# check data structure, part 1 
summary(data.a)
str(data.a)
str(data.b)

# check for and remove identified duplicates (from data.a)
data.a %>% select(Product, Channel, Week) %>% distinct() %>% dim()
data.b %>% select(Product, Channel, Week) %>% distinct() %>% dim()
data.a <- 
  data.a %>% 
  na.omit() %>% 
  distinct() 
data.b <- 
  data.b %>% 
  na.omit() %>% 
  distinct() 

# (inner) join data on key-columns 
data.a.b <- inner_join(x = data.a, y = data.b, 
                       by = c("Product", "Channel", "Week"))

# format dates 
data.a.b$Week <- as.Date(data.a.b$Week, format = "%Y-%m-%d")
data.a.b$year <- as.factor(format(as.Date(data.a.b$Week),"%Y"))
data.a.b$month <- as.factor(format(as.Date(data.a.b$Week),"%m"))
data.a.b$day <- as.factor(format(as.Date(data.a.b$Week),"%d"))

describe(data.a.b)
names(data.a.b)

# Basic feature engineering 
# One hot encoding (dummies for factor variables)
year.dum <- data.frame(predict(dummyVars( ~ year, data = data.a.b), newdata = data.a.b))
month.dum <- data.frame(predict(dummyVars( ~ month, data = data.a.b), newdata = data.a.b))
day.dum <- data.frame(predict(dummyVars( ~ day, data = data.a.b), newdata = data.a.b))
channel.dum <- data.frame(predict(dummyVars( ~ Channel, data = data.a.b), newdata = data.a.b))
Product.dum <- data.frame(predict(dummyVars( ~ Product, data = data.a.b), newdata = data.a.b))
data.a.b <- cbind(data.a.b, #[,-which(names(data.a.b) %in% c("year", "month", "day"))], 
                  year.dum,
                  month.dum, 
                  day.dum, 
                  channel.dum, 
                  Product.dum)

# Code up additional dummies
data.a.b <- 
  data.a.b %>% 
  mutate(month.start = as.numeric(as.character(day))<=3, 
         month.end = as.numeric(as.character(day))>=23, 
         price.high = Price>mean(Price),
         sharestores.high = ShareStores>mean(ShareStores))

# Another useful option: 
# Generate time lags (I'd generally suggest to at least check if this increases performance, yet do not 
# include this or further feature engineering steps here due to PoC characteristic of the analysis). 
# Another potentially useful option would be to use moving averages 

# Further exploratory data analysis 
# For continuous variables 
ggpairs(data.a.b[,c("NSold","Price","TV","Online","ShareStores")]) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1))
# for categorical/dummy variables (also save/export plotted graph)
png(file = file.path(output_dir,"correl.plot.categorical.png")) 
# could just as well use other format like jpeg or tiff (tiff might be preferable in some situations due to scaling/vector graphic properties)
ggpairs(data.a.b[,c("AmountSold","Channel","Promo","year","month.start","month.end")]) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1))
dev.off()

# Perform additional variable transformation to approximate normal distribution of features 
data.a.b <- 
  data.a.b %>% 
  mutate(price.log = log2(Price+1))

# Exploratory data analysis, part 2
# For continuous variables (also save/export plotted graph)
png(file = file.path(output_dir,"correl.plot.continuous.png"))
ggpairs(data.a.b[,c("AmountSold","price.log","TV","Online","ShareStores")]) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1))
dev.off()

# Generate table with descriptive stats 
# Table combined: Prescriptions, controls, and dummies (year and month; state left out due to space restrictions/large number of states)
table.desc <- data.a.b %>% 
  select(NSold, Price, price.log, price.high, Online, TV, ShareStores, sharestores.high, Promo, month.start,
         month.end,Channel.A,Channel.B,Channel.C,Channel.D,Channel.E,Channel.F,year.2015,year.2016,year.2017,year.2018) %>% 
  mutate(price.high = price.high*100, 
         sharestores.high = sharestores.high*100,
         Promo = ifelse(as.character(Promo) == "True", 100, 0), 
         month.start = month.start*100,
         month.end = month.end*100,
         Channel.A = Channel.A*100, 
         Channel.B = Channel.B*100, 
         Channel.C = Channel.C*100, 
         Channel.D = Channel.D*100, 
         Channel.E = Channel.E*100, 
         Channel.F = Channel.F*100, 
         year.2015 = year.2015*100,
         year.2016 = year.2016*100,
         year.2017 = year.2017*100,
         year.2018 = year.2018*100) %>% 
  stargazer(type = "latex", 
            title = "Descriptive statistics.", 
            covariate.labels = c("Number Sold", 
                                 "Price", 
                                 "Price (log.)", 
                                 "High Price^+", 
                                 "Investment: Online", 
                                 "Investment: TV", 
                                 "Share of stores (availability)", 
                                 "High availability share^+",
                                 "Promotion^+", 
                                 "Start of Month^+", 
                                 "End of Month^+", 
                                 "Channel: A^+", 
                                 "Channel: B^+", 
                                 "Channel: C^+", 
                                 "Channel: D^+", 
                                 "Channel: E^+", 
                                 "Channel: F^+", 
                                 "2015^+",
                                 "2016^+",
                                 "2017^+",
                                 "2018^+"), 
            digits = 2,
            omit = NULL,
            decimal.mark = ".", 
            summary.stat = c("mean", "sd", "min", "max", "n")) #, # "p25", "p75", 
cat(table.desc, sep = '\n', file = file.path(output_dir,"table.desc.tex"))


# Build different models for predicting sales 
# split sample into test and training data sets 
train<-sample(x = 1:nrow(data.a.b), size = round(nrow(data.a.b)*0.8))
Y.train <- data.a.b[train,]$NSold
Y.test <- data.a.b[-train,]$NSold

# 1) Linear model 
# Code up formula for simple linear model 
formula.lm <- formula(NSold ~ price.log + price.high + 
                        Online + TV + 
                        ShareStores + factor(sharestores.high) + 
                        factor(Promo) + factor(month.start) + factor(month.end) + 
                        factor(year) + factor(month) + factor(day) + factor(Channel) + factor(Product))

# Fit model 
fit.lm <- lm(formula.lm,data = data.a.b[train,])
# Get out of sample predictions and goodness of fit (used here: mean squared error)
yhat.lm <- predict(fit.lm,newdata = data.a.b[-train,])
MSE.lm <- summary(lm((Y.test-yhat.lm)^2~1))$coef[1:2]


# Prepare data for regularized regression models 
# Code up formula for regularized regressions (here: lasso, ridge regression, and elastic net) including non-linear transformations, 
# e.g. up to second-order polynomials and two-way interactions (could also introduce other transformations, e.g. splines)
X.flex <-  model.matrix(~(poly(price.log,2) + factor(price.high) + poly(Online,2) + poly(TV,2) + poly(ShareStores,2) + factor(sharestores.high) + 
                            factor(Promo) + factor(month.start) + factor(month.end) + factor(year) + factor(month) + factor(day) + 
                            factor(Channel) + factor(Product))^2, 
                        data = data.a.b)
X.flex.train <-  X.flex[train,]
X.flex.test <-  X.flex[-train,]

# 2) LASSO 
# Fit model using 10-fold cross-validation to choose regularization parameter (lambda)
fit.lasso <- cv.glmnet(X.flex.train, Y.train, family = "gaussian", nfolds=10, alpha=1)
# plot and export cross-validation of tuning parameter lambda
png(file = file.path(output_dir,"lasso.lambda.png"))
plot(fit.lasso) 
dev.off()
yhat.lasso.train <- predict(fit.lasso, newx = X.flex.train)	
lasso.Ym <- mean(yhat.lasso.train)
lasso.Ym
coef(fit.lasso)
summary(fit.lasso)
# (LASSO penalizes a range of control variables to zero due to the L1 norm/its kink at zero) 

# Get out of sample predictions and goodness of fit (used here: mean squared error)
yhat.lasso <- predict(fit.lasso, newx = X.flex.test)	
MSE.lasso <- summary(lm((Y.test-yhat.lasso)^2~1))$coef[1:2]

# 3) Ridge regression 
# Fit model using 10-fold cross-validation to choose regularization parameter (lambda)
fit.ridge <- cv.glmnet(X.flex.train, Y.train, family = "gaussian", nfolds=10, alpha=0)
# plot and export cross-validation of tuning parameter lambda
png(file = file.path(output_dir,"ridge.lambda.png"))
plot(fit.ridge)
dev.off()
yhat.ridge.train <- predict(fit.ridge, newx = X.flex.train)	
ridge.Ym <- mean(yhat.ridge.train)
ridge.Ym
coef(fit.ridge)
summary(fit.ridge)
# (Ridge does not penalize control variables to zero due to the the L2 norm) 

# Get out of sample predictions and goodness of fit (used here: mean squared error)
yhat.ridge <- predict(fit.ridge, newx = X.flex.test)	
MSE.ridge <- summary(lm((Y.test-yhat.ridge)^2~1))$coef[1:2]

# 4) Elastic net 
# Fit model using 10-fold cross-validation to choose regularization parameter (lambda) 
# and set tuning parameter alpha (weight between LASOO and Ridge, here: arbitary/default choices for simplicity) 
fit.elnet <- cv.glmnet(X.flex.train, Y.train, family = "gaussian", nfolds=10, alpha=0.5)
# plot and export cross-validation of tuning parameter lambda
png(file = file.path(output_dir,"elnet.lambda.png"))
plot(fit.elnet)
dev.off()
yhat.elnet.train <- predict(fit.elnet, newx = X.flex.train)	
elnet.Ym <- mean(yhat.elnet.train)
elnet.Ym
coef(fit.elnet)
summary(fit.elnet)
# (Elastic net penalizes a range of control variables to zero) 

# Get out of sample predictions and goodness of fit (used here: mean squared error)
yhat.elnet <- predict(fit.elnet, newx = X.flex.test)	
MSE.elnet <- summary(lm((Y.test-yhat.elnet)^2~1))$coef[1:2]

# 5) Random forest 
# Code up formula for trees (transformations not necessary due to data partitioning trait of the method) 
formula.rf <- formula(NSold ~ price.log + Online + TV + ShareStores + Promo + year + month + day + Channel + Product)

# Fit model and set forest parameters (here: arbitary/default choices for simplicity. ntree) 
fit.rforest<-randomForest(formula = formula.rf, data = data.a.b, subset = train, ntree = 10000, maxnodes = 20, mtry = 3)

# Export Variable importance plot 
png(file = file.path(output_dir,"rf.vimp.png"))
varImpPlot(fit.rforest, 
           main = "Random forest: Variable importance")
dev.off()
# I use 10000 bootstrap replications which is rather at the upper end of commonly used choices to increase the statistical precision 
# From the 10 input control variables, I consider 3 variables (which is the rounded sqrt(10)) for splitting at each partitioning step for tree-building 
# Additionally, I set the maximum number of nodes per tree to 20, which is a reasonable, rather relaxed restriction given a set of only 3 features per split 
# The most important variables appear to be the Channel and Store Availability. All remaining variables seem less important as shown in the variable plot. 

# Get out of sample predictions and goodness of fit (used here: mean squared error)
yhat.rforest<-predict(fit.rforest, newdata = data.a.b[-train,]) 	
MSE.rforest<-summary(lm((Y.test-yhat.rforest)^2~1))$coef[1:2]	


# Table with out of sample predictions: 
table.mse<- matrix(0, nrow = 5, ncol = 2)	
table.mse[1,]<- MSE.lm	
table.mse[2,]<- MSE.lasso	
table.mse[3,]<- MSE.ridge	
table.mse[4,]<- MSE.elnet	
table.mse[5,]<- MSE.rforest
colnames(table.mse)<- c("MSE", "S.E. for MSE")	
rownames(table.mse)<- c("OLS", "LASSO", "Ridge Regression", "Elastic Net",  "Random Forest")
table.mse <- xtable(table.mse, digits = 3)	
print(xtable(table.mse, 
             caption = "Model benchmarking based on mean squared error (MSE)"),
      file = file.path(output_dir,"models.mse.tex"))

# end ====================================================================================================== # 
