df <-read.csv("~/Downloads/commutervan.csv")
df$t <- c(1:64)


#Question 1: Run simple linear regression on daily bookings and completed rides
fit <-lm(rides ~ booked.ride, df)
summary(fit)
#Equation: 204.54863+ 0.84041(booked.ride)
#Adjusted R-squared: 0.958

#Question 2:
fit2 <-lm(revenue ~ rides, df)
summary(fit2)
#Equation: 232.00208 + 3.80737(rides)
#Adjusted R-squared:0.9878

#Question3:

#Best Model:
fit3 <- lm(booked.ride ~ starts.session + tapped.sidebar + viewed.eta, df)
summary(fit3)
#Adjusted R-squared:0.9602 
#removed tapped on stop. Not significant, P value was more than 0.05

#Testing multiple models:
# all variables 
tmodel1 <- lm(booked.ride ~ starts.session + tapped.on.stop + tapped.sidebar + viewed.eta, data = newdata)
summary(tmodel1)

# without tapped on stop, the most significant model 
tmodel2 <- lm(booked.ride ~ starts.session  + tapped.sidebar + viewed.eta, data = newdata)
summary(tmodel2)

# without tapped sidebar 
tmodel3 <- lm(booked.ride ~ starts.session + tapped.on.stop + viewed.eta, data = newdata)
summary(tmodel3)

tmodel4 <- lm(booked.ride ~ starts.session + viewed.eta, data = newdata)
summary(tmodel4)

tmodel5 <- lm(booked.ride ~  tapped.on.stop + tapped.sidebar + viewed.eta, data = newdata)
summary(tmodel5)

tmodel6 <- lm(booked.ride ~ starts.session + tapped.on.stop + tapped.sidebar, data = newdata)
summary(tmodel6)

#Question 4: We used excel instead but tried to calculate the results using R
plot(rides ~ t, data=df)

df$SMA=NA
k = 5
B=nrow(df) - k

for (i in 1:B){
  b=i+(k-1)
  df$SMA[i+k]=mean(df$rides[i:b])
}

df$res=df$rides - df$SMA
df$resS=(df$rides - df$SMA)^2

MSE=mean(df$resS[6:64])
MAD=mean(abs(df$res[6:64]))
MAPE=mean(abs(df$res[6:64]/df$rides[6:64]))

#Question 5: We used excel instead.

#Question6
ggplot(data = df, aes(x=t, y=rides) + geom_line() + geom_point())

#Question 7
Model1 <- lm(rides ~ t, df)
summary(Model1)

#Question 8
df$d1 <- ifelse(df$dayofweek == "Monday", 1,0)
df$d2 <- ifelse(df$dayofweek == "Tuesday", 1,0)
df$d3 <- ifelse(df$dayofweek == "Wednesday", 1,0)
df$d4 <- ifelse(df$dayofweek == "Thursday", 1,0)

Model2 <- lm(rides ~ d1 + d2 + d3 + d4 + t, data=df)
summary(Model2)

#Question 9
df$RegForecast = NA

b0 <- Model2$coefficients[1]
b1 <- Model2$coefficients[2]
b2 <- Model2$coefficients[3]
b3 <- Model2$coefficients[4]
b4 <- Model2$coefficients[5]
b5 <- Model2$coefficients[6]

#In order to calculate the regression models MSE, MAD, and MAPE we predicted every daily completed rides for each day in our dataset.
#MSE: 235026.104
#MAD: 221.793
#MAPE: 0.0535 or 5.35%

#Question10
#The first 5 days were calculate through R. For the full focrecast we used Excel.
(b0 + b1) +b5*66
#Intercept: 2928.705 

#Tuesday Prediction
(b0 + b2) +b5*67
#Intercept: 3405.782

#Wednesday Prediction
(b0 + b3) +b5*68
#Intercept: 3261.013

#Thursday Prediction
(b0 + b4) +b5*69
#Intercept: 3129.474 

#Friday Prediction
b0 + b5*65
#Intercept: 2536.107
