install.packages("geosphere")
library(geosphere)

#Read original csv
dataset = read.csv("Train.csv")

#Data Pre-processing
#install.packages("rockchalk")
#library(rockchalk)

dataset$store_and_fwd_flag = combineLevels(dataset$store_and_fwd_flag,levs = c("N", ""), newLabel = c("N") )
dataset$store_and_fwd_flag = factor(dataset$store_and_fwd_flag,
                                    levels = c('N', 'Y'),
                                    labels = c(0, 1))

dataset$vendor_id = factor(dataset$vendor_id,
                  levels = c('DST000401', 'DST000532'),
                  labels = c(0, 1))

#dataset$payment_type = combineLevels(dataset$payment_type,levs = c("CRD", "CSH"), newLabel = c("X") )
dataset$payment_type = combineLevels(dataset$payment_type,levs = c("DIS", "NOC","UNK"), newLabel = c("Y") )
dataset$payment_type = factor(dataset$payment_type, levels = c('CRD','CSH','Y'),labels=c(0,1,2))


#Changing date time fields from factor to R dateTime and calculating difference in minutes
dataset$pickup_datetime <- strptime(x = as.character( dataset$pickup_datetime ), format = "%Y-%m-%d %H:%M:%S")
dataset$dropoff_datetime <- strptime(x = as.character( dataset$dropoff_datetime), format = "%Y-%m-%d %H:%M:%S")
dataset$duration<- as.double((dataset$dropoff_datetime - dataset$pickup_datetime)/60)
dataset$duration[is.na(dataset$duration)]<-median(dataset$duration,na.rm = TRUE)


#Substituting missing values with median 
dataset$tolls_amount[is.na(dataset$tolls_amount)]<-median(dataset$tolls_amount,na.rm = TRUE)
dataset$tip_amount[is.na(dataset$tip_amount)]<-median(dataset$tip_amount,na.rm = TRUE)
dataset$mta_tax[is.na(dataset$mta_tax)]<-median(dataset$mta_tax,na.rm = TRUE)
dataset$surcharge[is.na(dataset$surcharge)]<-median(dataset$surcharge,na.rm = TRUE)
dataset$passenger_count[is.na(dataset$passenger_count)]<-median(dataset$passenger_count,na.rm = TRUE)

#dataset[!(is.na(dataset$dropoff_latitude) | is.na(dataset$dropoff_longitude)),]
#dataset[!(is.na(dataset$pickup_latitude) | is.na(dataset$pickup_longitude)),]
#dataset$distance <- sapply(distm (c(dataset$dropoff_longitude, dataset$dropoff_latitude), c(dataset$pickup_longitude, dataset$pickup_latitude), FUN = distHaversine)/1000)


#Calculating the distance and 
dataset$distance <-distHaversine(cbind(dataset$dropoff_longitude, dataset$dropoff_latitude), cbind(dataset$pickup_longitude, dataset$pickup_latitude))/1000 
dataset$distance[is.na(dataset$distance)]<-median(dataset$distance,na.rm = TRUE)

#Checking of the time of ride was less than 5am or greater than 10pm
upperThreshold =  sub(".*\\s+", "",strptime(x = as.character("5/12/2016 22:00"), format = "%m/%d/%Y %H:%M"))
lowerThreshold =  sub(".*\\s+", "",strptime(x = as.character("5/12/2016 5:00"), format = "%m/%d/%Y %H:%M"))
dataset$oddHours<-ifelse(sub(".*\\s+", "",  dataset$pickup_datetime)<lowerThreshold | sub(".*\\s+", "",  dataset$pickup_datetime)>upperThreshold,1,0)

dataset = na.omit(dataset)
dataset = subset(dataset, select = -c(1,3,7,8,10,11,14,15))

#dataset=dataset[1:5000,]

library(caTools)
 set.seed(123)
 split = sample.split(dataset$fare_amount, SplitRatio = 2/3)
 training_set = subset(dataset, split == TRUE)
 test_set = subset(dataset, split == FALSE)
 
 
 # Fitting Linear Regression to the dataset
 lin_reg = lm(formula =fare_amount ~ .,
              data = dataset)
 
 # Predicting the Test set results
 y_pred = predict(lin_reg, newdata = test_set)
 
 #Predicting on Real Testfare_amount
 
 write.csv(y_pred,"Predicted.csv")
 write.csv(test_set[,9],"Real.csv")

 
 
#################################################################################################### 
 
 #Predicting on real test set
 finaltest = read.csv("test.csv")
 
 finaltest$store_and_fwd_flag = combineLevels( finaltest$store_and_fwd_flag,levs = c("N", " "), newLabel = c("N") )
 finaltest$store_and_fwd_flag = factor( finaltest$store_and_fwd_flag,
                                        levels = c('N', 'Y'),
                                        labels = c(0, 1))
 
 finaltest$vendor_id = factor( finaltest$vendor_id,
                            levels = c('DST000481', 'DST000543'),
                            labels = c(0, 1))
 
 #finaltest$payment_type = combineLevels( finaltest$payment_type,levs = c("CRD", "CSH"), newLabel = c("X") )
 finaltest$payment_type = combineLevels( finaltest$payment_type,levs = c("DIS", "NOC","UNK"), newLabel = c("Y") )
 finaltest$payment_type = factor( finaltest$payment_type, levels = c('CRD','CSH','Y'),labels=c(0,1,2))
 
 
 #Changing date time fields from factor to R dateTime and calculating difference in minutes
 finaltest$pickup_datetime <- strptime(x = as.character( finaltest$pickup_datetime ), format = "%m/%d/%Y %H:%M")
 finaltest$dropoff_datetime <- strptime(x = as.character( finaltest$dropoff_datetime), format = "%m/%d/%Y %H:%M")
 finaltest$duration<- as.double((finaltest$dropoff_datetime - finaltest$pickup_datetime)/60)
 finaltest$duration[is.na( finaltest$duration)]<-median( finaltest$duration,na.rm = TRUE)
 
 #Substituting missing values with median 
 finaltest$tolls_amount[is.na(finaltest$tolls_amount)]<-median(finaltest$tolls_amount,na.rm = TRUE)
 finaltest$tip_amount[is.na(finaltest$tip_amount)]<-median(finaltest$tip_amount,na.rm = TRUE)
 finaltest$mta_tax[is.na(finaltest$mta_tax)]<-median(finaltest$mta_tax,na.rm = TRUE)
 finaltest$surcharge[is.na(finaltest$surcharge)]<-median(finaltest$surcharge,na.rm = TRUE)
 finaltest$passenger_count[is.na(finaltest$passenger_count)]<-median(finaltest$passenger_count,na.rm = TRUE)
 
 #finaltest$payment_type = factor(finaltest$payment_type, levels = c('CRD','CSH','DIS','NOC','UNK'),labels=c(0,1,2,3,4))
 
 #finaltest[!(is.na(finaltest$dropoff_latitude) | is.na(finaltest$dropoff_longitude)),]
 #finaltest[!(is.na(finaltest$pickup_latitude) | is.na(finaltest$pickup_longitude)),]
 #finaltest$distance <- sapply(distm (c(finaltest$dropoff_longitude, finaltest$dropoff_latitude), c(finaltest$pickup_longitude, finaltest$pickup_latitude), FUN = distHaversine)/1000)
 #finaltest=finaltest[1:5000,]
 
 #Calculating the distance and 
 finaltest$distance <-distHaversine(cbind(finaltest$dropoff_longitude, finaltest$dropoff_latitude), cbind(finaltest$pickup_longitude, finaltest$pickup_latitude))/1000 
 finaltest$distance[is.na( finaltest$distance)]<-median( finaltest$distance,na.rm = TRUE)
 
 #finaltest = na.omit(finaltest)
 
 
 #Checking of the time of ride was less than 5am or greater than 10pm
 upperThreshold =  sub(".*\\s+", "",strptime(x = as.character("5/12/2016 22:00"), format = "%m/%d/%Y %H:%M"))
 lowerThreshold =  sub(".*\\s+", "",strptime(x = as.character("5/12/2016 5:00"), format = "%m/%d/%Y %H:%M"))
 finaltest$oddHours<-ifelse(sub(".*\\s+", "",  finaltest$pickup_datetime)<lowerThreshold | sub(".*\\s+", "",   finaltest$pickup_datetime)>upperThreshold,1,0)
 
 
 finaltest = subset(finaltest, select = -c(3,7,8,10,11,14,15))
 
 y_pred_new = predict(lin_reg, newdata =  subset(finaltest, select = -c(1)))
 
 
 
 #y_pred_new[is.na( y_pred_new)]<-median( y_pred_new,na.rm = TRUE)
 
 write.csv(y_pred_new,"PredictedF7.csv")
 write.csv(finaltest[,1],"RealF3b.csv")
 