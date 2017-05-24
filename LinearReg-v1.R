install.packages("geosphere")
library(geosphere)

#Read original csv
dataset = read.csv("Train.csv")

#Data Pre-processing
#df$value[is.na(df$value)] <- median(df$value, na.rm=TRUE)
dataset$tolls_amount[is.na(dataset$tolls_amount)]<-median(dataset$tolls_amount,na.rm = TRUE)
dataset$tip_amount[is.na(dataset$tip_amount)]<-median(dataset$tip_amount,na.rm = TRUE)
dataset$mta_tax[is.na(dataset$mta_tax)]<-median(dataset$mta_tax,na.rm = TRUE)
dataset$surcharge[is.na(dataset$surcharge)]<-median(dataset$surcharge,na.rm = TRUE)
dataset$store_and_fwd_flag = factor(dataset$store_and_fwd_flag,
                           levels = c('N', 'Y'),
                           labels = c(0, 1))
dataset$store_and_fwd_flag[is.na(dataset$store_and_fwd_flag)]<-mode(as.numeric(dataset$store_and_fwd_flag))
dataset$distance <- distm (c(dataset$dropoff_longitude, dataset$dropoff_latitude), c(dataset$pickup_longitude, dataset$pickup_latitude), fun = distHaversine)


dataset = read.csv('Train_Changed.csv')
dataset = dataset[1:5000,]

dataset = subset(dataset, select = -c(1,2,6,7))

library(caTools)
 set.seed(123)
 split = sample.split(dataset$fare_amount, SplitRatio = 2/3)
 training_set = subset(dataset, split == TRUE)
 test_set = subset(dataset, split == FALSE)
 
 
 # Fitting Linear Regression to the dataset
 lin_reg = lm(formula =fare_amount ~ .,
              data = training_set)
 
 # Predicting the Test set results
 y_pred = predict(lin_reg, newdata = test_set)
 
 #Predicting on Real Testfare_amount
 
 write.csv(y_pred,"Predicted.csv")
 write.csv(test_set[,-1],"Real.csv")
 # Visualising the Training set results
 install.packages("ggplot2")
 library(ggplot2)
 ggplot() +
   geom_point(aes(x = training_set$surcharge+training_set$tip_amount, y = training_set$fare_amount),
              colour = 'red') +
   geom_line(aes(x = training_set$surcharge+training_set$tip_amount, y = predict(lin_reg, newdata = training_set)),
             colour = 'blue') +
   ggtitle('Salary vs Experience (Training set)') +
   xlab('Years of experience') +
   ylab('Salary')
 
 
 
 # Visualising the Test set results
 library(ggplot2)
 ggplot() +
   geom_point(aes(x = test_set$surcharge+test_set$tip_amount, y = test_set$fare_amount),
              colour = 'red') +
   geom_line(aes(x = training_set$surcharge+test_set$tip_amount, y = predict(lin_reg, newdata = training_set)),
             colour = 'blue') +
   ggtitle('Salary vs Experience (Test set)') +
   xlab('Years of experience') +
   ylab('Salary')
