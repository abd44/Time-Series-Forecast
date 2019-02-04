library(forecast)
library(tseries)
require(graphics)
library(dplyr)
library(tidyr)

global <- read.csv("Global Superstore.csv")

#Data Preparation and EDA:
str(global)
colSums(is.na(global))
table(global$Market)
table(global$Segment)


# Removing columns we do not need :
cols <- c('Row.ID','Order.ID','Ship.Mode','Ship.Date','Region', 'Customer.ID','Customer.Name','City','State','Discount','Shipping.Cost' ,'Country','Postal.Code','Product.ID','Product.Name')
global[,cols] <- NULL

global$Order.Date <- as.Date(global$Order.Date,format = "%d-%m-%Y")
global$Order.Date <- format(global$Order.Date,"%m-%Y")


#Segment data into 21 segments in a single data frame:
library(tidyr)
Unitee <- unite(global, Market_Segment, c(Market, Segment), remove=FALSE)
Unitee$Market_Segment <- as.factor(Unitee$Market_Segment)

gate <- aggregate(list(Sales=Unitee$Sales,Profit=Unitee$Profit,Quantity=Unitee$Quantity),
                  by=list(Market_Segment = Unitee$Market_Segment,Order_Month = Unitee$Order.Date),FUN=sum)

gate<- gate[order(gate$Market_Segment,gate$Order_Month),]
str(gate)
table(gate$Market_Segment)
#Now we have 21 segments,in the same dataframe.

#Calculating CV for each Segment and Extracting the 2 most Profitable segment
library(dplyr)
gate %>%  group_by(Market_Segment) %>% summarise(cv=sd(Profit)/mean(Profit)) %>% arrange(cv) %>% head(2)


#Thus,2 most profitable segments are : EU_Consumer & APAC_Consumer
# 1 EU_Consumer    0.624
# 2 APAC_Consumer  0.632
#Thus,only using subsets of EU_Consumer and APAC_Consumer for Sales and Quantity forecast

##################                FORECAST SALES on EU_Consumer Segment                   ##################################################

#CLASSICAL DECOMPOSITION:

##########  Separating EU_Consumer Segments and forecasting SALES,using CLASSICAL DECOMPOSITION METHOD using Smoothing as:   #####################################
##     1.Moving Average Smoothing
##     2. Exponential Smoothing
EU_Consumer <- gate %>% filter(Market_Segment=="EU_Consumer")

#We need only monthly sales and quantity 
EU_Consumer_df <-EU_Consumer[,c('Order_Month','Sales','Quantity')]
EU_Consumer_df$Order_Month <- as.Date(paste(EU_Consumer_df$Order_Month,"-01",sep=""),format = "%m-%Y-%d")
EU_Consumer_df$Order_Month <- format(EU_Consumer_df$Order_Month,"%Y-%m")
EU_Consumer_df <- EU_Consumer_df[order(EU_Consumer_df$Order_Month ),] 
EU_Consumer_df$Order_Month <- order(EU_Consumer_df$Order_Month)
nrow(EU_Consumer_df)
train <- EU_Consumer_df[1:42,]
test <- EU_Consumer_df[43:nrow(EU_Consumer_df),]


 #For sales and quantity doing forecast using time series,so dropping rest of the coulmn:

#Converting Sales to time series:            

EU_Consumer_df_totalts_sales <- ts(EU_Consumer_df$Sales)
plot(EU_Consumer_df_totalts_sales)


train_ts_sales <- ts(train$Sales)
#train_ts_sales <- log(train_ts_sales)
plot(train_ts_sales,ylab="Sales")

#############SMOOTHING using Moving Average Smoothing  and Exponential Smoothing ######################


#Smoothing using Moving Average Smoothing  ##############

w <-11
smoothedseries <- stats::filter(train_ts_sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(train_ts_sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- train$Order_Month
lines(smoothedseries, col="blue", lwd=2)


#Building a classical decomposition model on the smoothed time series using Moving Average smoothed series
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')
str(smootheddf)


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(2*Month) * poly(Month,1) + cos(2*Month) * poly(Month,1)
            + Month, data=smootheddf)

summary(lmfit)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- train_ts_sales-global_pred
plot(local_pred, col='red', type = "l")

acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- test$Order_Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,test[,2])[5]
MAPE_class_dec
#MAPE 26.65

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_Consumer_df_totalts_sales, col = "black")
lines(class_dec_pred, col = "red")



## Smoothing TS using Exponential Smoothing Method and checking Accuracy on classical decomposition method :

##########        Creating Classical using Exponential Smoothing      #############################

plot(train_ts_sales,ylab="Sales")

cols <- c("red", "blue", "green","brown","black")
alphas <- c(0.02, 0.1, 0.20,0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp <- HoltWinters(train_ts_sales, alpha=alphas[i],
                                    beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries_exp)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

#Visually estimating alpha, alpha 0.20 seems good smoothing:
smoothedseries_exp <- HoltWinters(train_ts_sales, alpha=0.20,
                                  beta=FALSE, gamma=FALSE)
smoothedseries_exp<-fitted(smoothedseries_exp)[,1]
#Plot the smoothed time series

timevals_in_exp <- train$Order_Month
plot(train_ts_sales)
lines(smoothedseries_exp, col="blue", lwd=2)
smootheddf_exp <- as.data.frame(cbind(timevals_in_exp, as.vector(smoothedseries_exp)))
colnames(smootheddf_exp) <- c('Month', 'Sales')
str(smootheddf_exp)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_exp <- lm(Sales ~ sin(0.5*Month)* poly(Month,1) + cos(0.5*Month)* poly(Month,1) 
                + Month, data=smootheddf_exp)
summary(lmfit_exp)


global_pred_exp <- predict(lmfit_exp, Month=timevals_in)
summary(global_pred_exp)
lines(timevals_in, global_pred_exp, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_exp_exp <- train_ts_sales-global_pred_exp
plot(local_pred_exp_exp, col='red', type = "l")

acf(local_pred_exp_exp)
acf(local_pred_exp_exp, type="partial")
armafit <- auto.arima(local_pred_exp_exp)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred_exp_exp-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

#View(test)
timevals_out_exp <- test$Order_Month

global_pred_exp_out <- predict(lmfit_exp,data.frame(Month =timevals_out_exp))

fcast_exp <- global_pred_exp_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_exp <- accuracy(fcast_exp,test[,2])[5]
MAPE_class_dec_exp
#MAPE 25.37

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred_exp),ts(global_pred_exp_out))
plot(EU_Consumer_df_totalts_sales, col = "black")
lines(class_dec_pred, col = "red")

##########################################################################################
#Thus, for Classical decomposition method:                                               #
#       MAPE using moving Average smoothing:26.65                                        #
#       Mape using Exponential smoothing:25.37                                           #
##########################################################################################







############### Forecasting SALES,using ARIMA fit METHOD on EU_Consumer Segment ################
#So, that was classical decomposition, now let's do an ARIMA fit
###########################################################################################################################
autoarima <- auto.arima(train_ts_sales)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- train_ts_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,test[,2])[5]
MAPE_auto_arima
#MAPE 28.92

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(EU_Consumer_df_totalts_sales, col = "black")
lines(auto_arima_pred, col = "red")


# Thus ,ARIMA fit is giving decent prediction on test data,and MAPE is 28.9226.

#Conclusion:Using Classification Decomposition for SALES Forecast:
                    #with Moving Average Smoothing getting MAPE:26.65
                    #With Exponential Smoothing getting MAPE:25.37

#Using ARIMA Fit Model:28.9226
                 

######################################################################################################################

#########     Forecast Quantity on EU_Consumer Segment  ######################################


#Converting Quantity to time series:            

EU_Consumer_df_totalts_quantity <- ts(EU_Consumer_df$Quantity)
plot(EU_Consumer_df_totalts_quantity,ylab="EU Consumer Quantity")
abline(reg=lm(EU_Consumer_df_totalts_quantity~time(EU_Consumer_df_totalts_quantity)))


train_ts_quantity <- ts(train$Quantity)
plot(train_ts_quantity,ylab="Quantity")


############# SMOOTHING using Moving Average Smoothing  and Exponential Smoothing ###############################################


#Smoothing using Moving Average Smoothing and creating Classical Decompsition  

w <-5
smoothedseries_quantity <- stats::filter(train_ts_quantity, 
                                         filter=rep(1/(2*w+1),(2*w+1)), 
                                         method='convolution', sides=2)

#Smoothing left end of the time series

diff_quantity <- smoothedseries_quantity[w+2] - smoothedseries_quantity[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_quantity[i] <- smoothedseries_quantity[i+1] - diff_quantity
}

#Smoothing right end of the time series

n <- length(train_ts_quantity)
diff_quantity <- smoothedseries_quantity[n-w] - smoothedseries_quantity[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_quantity[i] <- smoothedseries_quantity[i-1] + diff_quantity
}

#Plot the smoothed time series

timevals_in <- train$Order_Month
lines(smoothedseries_quantity, col="blue", lwd=2)


##Building a classical decomposition model on the smoothed time series using Moving Average smoothed series
#First, let's convert the time series to a dataframe

smootheddf_quantity <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_quantity)))
colnames(smootheddf_quantity) <- c('Month', 'Quantity')
str(smootheddf_quantity)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_quantity <- lm(Quantity ~ sin(0.1*Month) * poly(Month,1) + cos(0.1*Month) * poly(Month,1)
                     + Month, data=smootheddf_quantity)

summary(lmfit_quantity)

global_pred_quantity <- predict(lmfit_quantity, Month=timevals_in)
summary(global_pred_quantity)
lines(timevals_in, global_pred_quantity, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_quantity <- train_ts_quantity-global_pred_quantity
plot(local_pred_quantity, col='red', type = "l")

acf(local_pred_quantity)
acf(local_pred_quantity, type="partial")
armafit_quantity <- auto.arima(local_pred_quantity)
tsdiag(armafit_quantity)
armafit_quantity

#We'll check if the residual series is white noise

resi_quantity <- local_pred_quantity-fitted(armafit_quantity)

adf.test(resi_quantity,alternative = "stationary")
kpss.test(resi_quantity)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_quantity <- test$Order_Month

global_pred_out_quantity <- predict(lmfit_quantity,data.frame(Month =timevals_out_quantity))

fcast_quantity <- global_pred_out_quantity
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_quantity <- accuracy(fcast_quantity,test[,3])[5]
MAPE_class_dec_quantity
#Got MAPE as 26.31


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_quantity <- c(ts(global_pred_quantity),ts(global_pred_out_quantity))
plot(EU_Consumer_df_totalts_quantity, col = "black")
lines(class_dec_pred_quantity, col = "red")



###### Smoothing TS using Exponential Smoothing Method and checking Accuracy on classical decomposition   ##############

#Exponential Smoothing

plot(train_ts_quantity,ylab="Quantity")
cols <- c("red", "blue", "green","brown","black")
alphas <- c(0.02, 0.1, 0.20,0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp_quantity <- HoltWinters(train_ts_quantity, alpha=alphas[i],
                                             beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries_exp_quantity)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

#Visually estimating alpha, alpha 0.20 seems good smoothing:
smoothedseries_quantity_exp <- HoltWinters(train_ts_quantity, alpha=0.20,
                                           beta=FALSE, gamma=FALSE)
smoothedseries_quantity_exp<-fitted(smoothedseries_quantity_exp)[,1]
#Plot the smoothed time series

timevals_in_exp <- train$Order_Month
plot(train_ts_quantity)
lines(smoothedseries_quantity_exp, col="blue", lwd=2)
smootheddf_exp_quantity <- as.data.frame(cbind(timevals_in_exp, as.vector(smoothedseries_quantity_exp)))
colnames(smootheddf_exp_quantity) <- c('Month', 'Quantity')
str(smootheddf_exp_quantity)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_exp_quantity <- lm(Quantity ~ sin(0.5*Month)* poly(Month,1) + cos(0.5*Month)* poly(Month,1) 
                         + Month, data=smootheddf_exp_quantity)
summary(lmfit_exp_quantity)


global_pred_exp_quantity <- predict(lmfit_exp_quantity, Month=timevals_in_exp)
summary(global_pred_exp_quantity)
lines(timevals_in_exp, global_pred_exp_quantity, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_exp_quantity <- train_ts_quantity-global_pred_exp_quantity
plot(local_pred_exp_quantity, col='red', type = "l")

acf(local_pred_exp_quantity)
acf(local_pred_exp_quantity, type="partial")
armafit_exp_quantity <- auto.arima(local_pred_exp_quantity)

tsdiag(armafit_exp_quantity)
armafit_exp_quantity

#We'll check if the residual series is white noise

resi_quantity_exp <- local_pred_exp_quantity-fitted(armafit_exp_quantity)

adf.test(resi_quantity_exp,alternative = "stationary")
kpss.test(resi_quantity_exp)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_exp <- test$Order_Month

global_pred_exp_out_quantity <- predict(lmfit_exp_quantity,data.frame(Month =timevals_out_exp))

fcast_exp_quantity <- global_pred_exp_out_quantity
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_exp_quantity <- accuracy(fcast_exp_quantity,test[,3])[5]
MAPE_class_dec_exp_quantity
#Got MAPE as 28.30
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_exp_pred_quantity <- c(ts(global_pred_exp_quantity),ts(global_pred_exp_out_quantity))
plot(EU_Consumer_df_totalts_quantity, col = "black")
lines(class_dec_exp_pred_quantity, col = "red")


##########################################################################################
#Thus, for Classical decomposition method for forecast of QUANTITY:                                               #
#       MAPE using moving Average smoothing:26.31                                        #
#       Mape using Exponential smoothing:28.30                                           #
##########################################################################################




############### Forecasting Quantity,using ARIMA fit METHOD on EU_Consumer Segment ################
###########################################################################################################################

autoarima_quantity <- auto.arima(train_ts_quantity)
autoarima_quantity
tsdiag(autoarima_quantity)
plot(autoarima_quantity$x, col="black")
lines(fitted(autoarima_quantity), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima_quantity <- train_ts_quantity - fitted(autoarima_quantity)

adf.test(resi_auto_arima_quantity,alternative = "stationary")
kpss.test(resi_auto_arima_quantity)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_quantity <- predict(autoarima_quantity, n.ahead = 6)

MAPE_auto_arima_quantity <- accuracy(fcast_auto_arima_quantity$pred,test[,3])[5]
MAPE_auto_arima_quantity
#Got MAPE as 30.13

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_quantity <- c(fitted(autoarima_quantity),ts(fcast_auto_arima_quantity$pred))
plot(EU_Consumer_df_totalts_quantity, col = "black")
lines(auto_arima_pred_quantity, col = "red")



#Conclusion:Using Classification Decomposition for Quantity FORECAST:
#   with Moving Average Smoothing getting MAPE: 26.31
#   With Exponential Smoothing getting MAPE: 28.30

#Using ARIMA Fit Model MAPE: 30.13





######Forecast Sales on APAC_Consumer Segment  ############
#CLASSICAL DECOMPOSITION:

##########  Separating APAC_Consumer Segments and forecasting SALES,using CLASSICAL DECOMPOSITION METHOD using Smoothing as:   #####################################
##     1.Moving Average Smoothing
##     2. Exponential Smoothing
APAC_Consumer <- gate %>% filter(Market_Segment=="APAC_Consumer")

#We need only monthly sales and quantity 
#For sales and quantity doing forecast using time series,so dropping rest of the coulmn:
APAC_Consumer_df <-APAC_Consumer[,c('Order_Month','Sales','Quantity')]
APAC_Consumer_df$Order_Month <- as.Date(paste(APAC_Consumer_df$Order_Month,"-01",sep=""),format = "%m-%Y-%d")
APAC_Consumer_df$Order_Month <- format(APAC_Consumer_df$Order_Month,"%Y-%m")
APAC_Consumer_df <- APAC_Consumer_df[order(APAC_Consumer_df$Order_Month ),] 
APAC_Consumer_df$Order_Month <- order(APAC_Consumer_df$Order_Month)
nrow(APAC_Consumer_df)
train_apac <- APAC_Consumer_df[1:42,]
test_apac <- APAC_Consumer_df[43:nrow(APAC_Consumer_df),]


#Converting Sales to time series:            

APAC_Consumer_df_totalts_sales <- ts(APAC_Consumer_df$Sales)
plot(APAC_Consumer_df_totalts_sales)


train_apac_ts_sales <- ts(train_apac$Sales)
plot(train_apac_ts_sales,ylab="Sales")

#############SMOOTHING using Moving Average Smoothing  and Exponential Smoothing ######################


#Smoothing using Moving Average Smoothing  ##############

w <-7
smoothedseries_apac_ma <- stats::filter(train_apac_ts_sales, 
                                        filter=rep(1/(2*w+1),(2*w+1)), 
                                        method='convolution', sides=2)

#Smoothing left end of the time series

diff_apac_ma <- smoothedseries_apac_ma[w+2] - smoothedseries_apac_ma[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_ma[i] <- smoothedseries_apac_ma[i+1] - diff_apac_ma
}


#Smoothing right end of the time series

n <- length(train_apac_ts_sales)
diff_apac_ma <- smoothedseries_apac_ma[n-w] - smoothedseries_apac_ma[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_ma[i] <- smoothedseries_apac_ma[i-1] + diff_apac_ma
}

#Plot the smoothed time series

timevals_in_apac <- order(train_apac$Order_Month)
lines(smoothedseries_apac_ma, col="blue", lwd=2)


#Building a classical decomposition model on the smoothed time series using Moving Average:
#First, let's convert the time series to a dataframe

smootheddf_apac_ma <- as.data.frame(cbind(timevals_in_apac, as.vector(smoothedseries_apac_ma)))
colnames(smootheddf_apac_ma) <- c('Month', 'Sales')
str(smootheddf_apac_ma)


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_apac_sales_ma <- lm(Sales ~ sin(1*Month) * poly(Month,3) + cos(1*Month) * poly(Month,3)
                          + Month, data=smootheddf_apac_ma)

summary(lmfit_apac_sales_ma)

global_pred_sales_ma <- predict(lmfit_apac_sales_ma, Month=timevals_in_apac)
summary(global_pred_sales_ma)
lines(timevals_in_apac, global_pred_sales_ma, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_apac_sales_ma <- train_apac_ts_sales - global_pred_sales_ma
plot(local_pred_apac_sales_ma, col='red', type = "l")

acf(local_pred_apac_sales_ma)
acf(local_pred_apac_sales_ma, type="partial")
armafit_apac_sales_ma <- auto.arima(local_pred_apac_sales_ma)

tsdiag(armafit_apac_sales_ma)
armafit_apac_sales_ma

#We'll check if the residual series is white noise

resi_apac_sales_ma <- local_pred_apac_sales_ma-fitted(armafit_apac_sales_ma)

adf.test(resi_apac_sales_ma,alternative = "stationary")
kpss.test(resi_apac_sales_ma)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_apac_sales_ma <- test_apac$Order_Month

global_pred_out_apac_sales_ma <- predict(lmfit_apac_sales_ma,data.frame(Month =timevals_out_apac_sales_ma))

fcast_apac_sales_ma <- global_pred_out_apac_sales_ma

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_apac_sales_ma <- accuracy(fcast_apac_sales_ma,test_apac[,2])[5]
MAPE_class_dec_apac_sales_ma
#MAPE 24.02

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_apac_sales_ma <- c(ts(global_pred_sales_ma),ts(global_pred_out_apac_sales_ma))
plot(APAC_Consumer_df_totalts_sales, col = "black")
lines(class_dec_pred_apac_sales_ma, col = "red")

#Thus,we got decent model with MAPE as 24.02 using classical decomposition method with Moving Average Smoothing.




####   Now, Trying CLASSICAL DECOMPOSITION MODEL USING EXPONENTIAL SMOOTHING and Checking ACCURACY #########################

plot(train_apac_ts_sales,ylab="Sales")

cols <- c("red", "blue", "green","brown","black")
alphas <- c(0.02, 0.1, 0.20,0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp_apac_sales <- HoltWinters(train_apac_ts_sales, alpha=alphas[i],
                                               beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries_exp_apac_sales)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

#Visually estimating alpha, alpha 0.20 seems good smoothing:
smoothedseries_exp_apac_sales <- HoltWinters(train_apac_ts_sales, alpha=0.20,
                                             beta=FALSE, gamma=FALSE)
smoothedseries_exp_apac_sales<-fitted(smoothedseries_exp_apac_sales)[,1]
#Plot the smoothed time series

timevals_in_exp_apac_sales <- train_apac$Order_Month
plot(train_apac_ts_sales)
lines(smoothedseries_exp_apac_sales, col="blue", lwd=2)
smootheddf_exp_apac_sales <- as.data.frame(cbind(timevals_in_exp_apac_sales, as.vector(smoothedseries_exp_apac_sales)))
colnames(smootheddf_exp_apac_sales) <- c('Month', 'Sales')
str(smootheddf_exp_apac_sales)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_exp_apac_sales <- lm(Sales ~ sin(0.5*Month)* poly(Month,1) + cos(0.5*Month)* poly(Month,1) 
                           + Month, data=smootheddf_exp_apac_sales)
summary(lmfit_exp_apac_sales)


global_pred_exp_apac_sales <- predict(lmfit_exp_apac_sales, Month=timevals_in_apac)
summary(global_pred_exp_apac_sales)
lines(timevals_in_apac, global_pred_exp_apac_sales, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_exp_apac_sales <- train_apac_ts_sales-global_pred_exp_apac_sales
plot(local_pred_exp_apac_sales, col='red', type = "l")

acf(local_pred_exp_apac_sales)
acf(local_pred_exp_apac_sales, type="partial")
armafit_apac_sales_exp <- auto.arima(local_pred_exp_apac_sales)

tsdiag(armafit_apac_sales_exp)
armafit_apac_sales_exp

#We'll check if the residual series is white noise

resi_apac_sales_exp <- local_pred_exp_apac_sales-fitted(armafit_apac_sales_exp)

adf.test(resi_apac_sales_exp,alternative = "stationary")
kpss.test(resi_apac_sales_exp)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months


timevals_out_exp_apac_sales <- test_apac$Order_Month

global_pred_exp_out_apac_sales <- predict(lmfit_exp_apac_sales,data.frame(Month =timevals_out_exp_apac_sales))

fcast_exp_apac_sales <- global_pred_exp_out_apac_sales
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_exp_apac_sales <- accuracy(fcast_exp_apac_sales,test_apac[,2])[5]
MAPE_class_dec_exp_apac_sales

#Got MAPE 18.80

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_apac_sales_exp <- c(ts(global_pred_exp_apac_sales),ts(global_pred_exp_out_apac_sales))
plot(APAC_Consumer_df_totalts_sales, col = "black")
lines(class_dec_pred_apac_sales_exp, col = "red")

#########################################################################################################################################
#Smoothing using exponential method gave better accuracy than smoothing using Moving Average method in classical decomposition method.
#Thus, for Classical decomposition method sales forecast in APAC Consumer Segment is:
#       MAPE using moving Average smoothing:24.02
#       Mape using Exponential smoothing:18.80
#################################################################################################################################


############### Forecasting SALES,using ARIMA fit METHOD on APAC_Consumer Segment ################

autoarima.exp_apac_sales <- auto.arima(train_apac_ts_sales)
autoarima.exp_apac_sales
tsdiag(autoarima.exp_apac_sales)
plot(autoarima.exp_apac_sales$x, col="black")
lines(fitted(autoarima.exp_apac_sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_exp_apac_sales <- train_apac_ts_sales - fitted(autoarima.exp_apac_sales)

adf.test(resi_auto_arima_exp_apac_sales,alternative = "stationary")
kpss.test(resi_auto_arima_exp_apac_sales)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_exp_apac_sales <- predict(autoarima.exp_apac_sales, n.ahead = 6)

MAPE_auto_arima_exp_apac_sales <- accuracy(fcast_auto_arima_exp_apac_sales$pred,test_apac[,2])[5]
MAPE_auto_arima_exp_apac_sales
#MAPE is 27.68
auto_arima_pred_apac_sales <- c(fitted(autoarima.exp_apac_sales),ts(fcast_auto_arima_exp_apac_sales$pred))
plot(APAC_Consumer_df_totalts_sales, col = "black")
lines(auto_arima_pred_apac_sales, col = "red")


#Conclusion:Using Classification Decomposition for SALES forecast:
#with Moving Average Smoothing getting MAPE:24.01
#With Exponential Smoothing getting MAPE:18.80

#Using ARIMA Fit Model:27.68



##################                      Forecast Quantity on APAC_Consumer Segment  ##############################
#CLASSICAL DECOMPOSITION:

##########  Separating APAC_Consumer Segments and forecasting Quantity,using CLASSICAL DECOMPOSITION METHOD using Smoothing as:   #####################################
##     1.Moving Average Smoothing
##     2. Exponential Smoothing

#Converting Quantity to time series:            

APAC_Consumer_df_totalts_Quantity <- ts(APAC_Consumer_df$Quantity)
plot(APAC_Consumer_df_totalts_Quantity)


train_apac_ts_Quantity <- ts(train_apac$Quantity)
plot(train_apac_ts_Quantity,ylab="Quantity")

#############SMOOTHING using Moving Average Smoothing  and Exponential Smoothing ######################


#Smoothing using Moving Average Smoothing  ##############

w <-7
smoothedseries_apac_ma_quantity <- stats::filter(train_apac_ts_Quantity, 
                                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                                 method='convolution', sides=2)

#Smoothing left end of the time series

diff_apac_ma_quantity <- smoothedseries_apac_ma_quantity[w+2] - smoothedseries_apac_ma_quantity[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_ma_quantity[i] <- smoothedseries_apac_ma_quantity[i+1] - diff_apac_ma_quantity
}


#Smoothing right end of the time series

n <- length(train_apac_ts_Quantity)
diff_apac_ma_quantity <- smoothedseries_apac_ma_quantity[n-w] - smoothedseries_apac_ma_quantity[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_ma_quantity[i] <- smoothedseries_apac_ma_quantity[i-1] + diff_apac_ma_quantity
}

#Plot the smoothed time series

timevals_in_apac_quantity <- train_apac$Order_Month
lines(smoothedseries_apac_ma_quantity, col="blue", lwd=2)


#Building a classical decomposition model on the smoothed time series using Moving Average:
#First, let's convert the time series to a dataframe

smootheddf_apac_ma_quantity <- as.data.frame(cbind(timevals_in_apac_quantity, as.vector(smoothedseries_apac_ma_quantity)))
colnames(smootheddf_apac_ma_quantity) <- c('Month', 'Quantity')
str(smootheddf_apac_ma_quantity)


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_apac_quantity_ma <- lm(Quantity ~ sin(1*Month) * poly(Month,3) + cos(1*Month) * poly(Month,3)
                             + Month, data=smootheddf_apac_ma_quantity)

summary(lmfit_apac_quantity_ma)

global_pred_quantity_ma <- predict(lmfit_apac_quantity_ma, Month=timevals_in_apac_quantity)
summary(global_pred_quantity_ma)
lines(timevals_in_apac_quantity, global_pred_quantity_ma, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_apac_quantity_ma <- train_apac_ts_Quantity - global_pred_quantity_ma
plot(local_pred_apac_quantity_ma, col='red', type = "l")

acf(local_pred_apac_quantity_ma)
acf(local_pred_apac_quantity_ma, type="partial")
armafit_apac_quantity_ma <- auto.arima(local_pred_apac_quantity_ma)

tsdiag(armafit_apac_quantity_ma)
armafit_apac_quantity_ma

#We'll check if the residual series is white noise

resi_apac_quantity_ma <- local_pred_apac_quantity_ma-fitted(armafit_apac_quantity_ma)

adf.test(resi_apac_quantity_ma,alternative = "stationary")
kpss.test(resi_apac_quantity_ma)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_apac_quantity_ma <- test_apac$Order_Month

global_pred_out_apac_quantity_ma <- predict(lmfit_apac_quantity_ma,data.frame(Month =timevals_out_apac_quantity_ma))

fcast_apac_quantity_ma <- global_pred_out_apac_quantity_ma

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_apac_quantity_ma <- accuracy(fcast_apac_quantity_ma,test_apac[,3])[5]
MAPE_class_dec_apac_quantity_ma
#MAPE 22.08

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_apac_quantity_ma <- c(ts(global_pred_quantity_ma),ts(global_pred_out_apac_quantity_ma))
plot(APAC_Consumer_df_totalts_Quantity, col = "black")
lines(class_dec_pred_apac_quantity_ma, col = "red")

#Thus,we got decent model with MAPE as 22.08 using classical decomposition method with Moving Average Smoothing.



####   Now, Trying CLASSICAL DECOMPOSITION MODEL USING EXPONENTIAL SMOOTHING and Checking ACCURACY #########################

plot(train_apac_ts_Quantity,ylab="quantity")

cols <- c("red", "blue", "green","brown","black")
alphas <- c(0.02, 0.1, 0.20,0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp_apac_quantity <- HoltWinters(train_apac_ts_Quantity, alpha=alphas[i],
                                                  beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries_exp_apac_quantity)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

#Visually estimating alpha, alpha 0.20 seems good smoothing:
smoothedseries_exp_apac_quantity <- HoltWinters(train_apac_ts_Quantity, alpha=0.20,
                                                beta=FALSE, gamma=FALSE)
smoothedseries_exp_apac_quantity<-fitted(smoothedseries_exp_apac_quantity)[,1]
#Plot the smoothed time series

timevals_in_exp_apac_quantity <- train_apac$Order_Month
plot(train_apac_ts_Quantity)
lines(smoothedseries_exp_apac_quantity, col="blue", lwd=2)
smootheddf_exp_apac_quantity <- as.data.frame(cbind(timevals_in_exp_apac_quantity, as.vector(smoothedseries_exp_apac_quantity)))
colnames(smootheddf_exp_apac_quantity) <- c('Month', 'Quantity')
str(smootheddf_exp_apac_quantity)


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_exp_apac_quantity <- lm(Quantity ~ sin(0.5*Month)* poly(Month,1) + cos(0.5*Month)* poly(Month,1) 
                              + Month, data=smootheddf_exp_apac_quantity)
summary(lmfit_exp_apac_quantity)


global_pred_exp_apac_quantity <- predict(lmfit_exp_apac_quantity, Month=timevals_in_apac_quantity)
summary(global_pred_exp_apac_quantity)
lines(timevals_in_apac_quantity, global_pred_exp_apac_quantity, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_exp_apac_quantity <- train_apac_ts_Quantity-global_pred_exp_apac_quantity
plot(local_pred_exp_apac_quantity, col='red', type = "l")

acf(local_pred_exp_apac_quantity)
acf(local_pred_exp_apac_quantity, type="partial")
armafit_apac_quantity_exp <- auto.arima(local_pred_exp_apac_quantity)

tsdiag(armafit_apac_quantity_exp)
armafit_apac_quantity_exp


#We'll check if the residual series is white noise

resi_apac_quantity_exp <- local_pred_exp_apac_quantity-fitted(armafit_apac_quantity_exp)

adf.test(resi_apac_quantity_exp,alternative = "stationary")
kpss.test(resi_apac_quantity_exp)

#We confirm with ADF and KPSS test that the residual is White Noise,as 
#for adf test p-value is smaller,so we accept Null Hypothesis
#For kpss p value is greater than .05,hence we failed to accept Null hypothesis and the residual is White noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months


timevals_out_exp_apac_quantity <- test_apac$Order_Month

global_pred_exp_out_apac_quantity <- predict(lmfit_exp_apac_quantity,data.frame(Month =timevals_out_exp_apac_quantity))


fcast_exp_apac_quantity <- global_pred_exp_out_apac_quantity
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_exp_apac_quantity <- accuracy(fcast_exp_apac_quantity,test_apac[,3])[5]
MAPE_class_dec_exp_apac_quantity

#Got MAPE 24.30

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_apac_quantity_exp <- c(ts(global_pred_exp_apac_quantity),ts(global_pred_exp_out_apac_quantity))
plot(APAC_Consumer_df_totalts_Quantity, col = "black")
lines(class_dec_pred_apac_quantity_exp, col = "red")

#########################################################################################################################################
#Thus, for Classical decomposition method for quantity forecast in APAC Consumer Segment is:
#       MAPE using moving Average smoothing:22.08
#       Mape using Exponential smoothing:24.30
#################################################################################################################################



############### Forecasting Quantity,using ARIMA fit METHOD on APAC_Consumer Segment ################

autoarima.exp_apac_quantity <- auto.arima(train_apac_ts_Quantity)
autoarima.exp_apac_quantity
tsdiag(autoarima.exp_apac_quantity)
plot(autoarima.exp_apac_quantity$x, col="black")
lines(fitted(autoarima.exp_apac_quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_exp_apac_quantity <- train_apac_ts_Quantity - fitted(autoarima.exp_apac_quantity)

adf.test(resi_auto_arima_exp_apac_quantity,alternative = "stationary")
kpss.test(resi_auto_arima_exp_apac_quantity)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_exp_apac_quantity <- predict(autoarima.exp_apac_quantity, n.ahead = 6)

MAPE_auto_arima_exp_apac_quantity <- accuracy(fcast_auto_arima_exp_apac_quantity$pred,test_apac[,3])[5]
MAPE_auto_arima_exp_apac_quantity
#MAPE is 26.24
auto_arima_pred_apac_quantity_ <- c(fitted(autoarima.exp_apac_quantity),ts(fcast_auto_arima_exp_apac_quantity$pred))
plot(APAC_Consumer_df_totalts_Quantity, col = "black")
lines(auto_arima_pred_apac_quantity_, col = "red")



#Conclusion:Using Classification Decomposition for QUANTITY forecast:
#with Moving Average Smoothing getting MAPE:22.08
#With Exponential Smoothing getting MAPE:24.30

#Using ARIMA Fit Model:26.24



##############################
# Thus,Selecting Models for Sales and Quantity for both Segment based on LOWER MAPE: 
# For "SALES" in EU_Consumer Segment using model Classification Decomposition With Exponential Smoothing getting MAPE:25.37 
# For "Quantity" in EU_Consumer Segment using model Classification Decomposition with Moving Average Smoothing getting MAPE: 26.31

#For "SALES" in APAC_Consumer Segment using model Classification Decomposition using Exponential Smoothing getting MAPE:18.80
#For "Quantity" in APAC_Consumer Segment using model Classification Decomposition with Moving Average Smoothing getting MAPE:22.08




