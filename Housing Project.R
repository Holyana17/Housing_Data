
#reading data and import data
Housing_data <- read.csv("D:\\Cases_csv_R\\Project_Final\\Housing_Data.csv");

#generate the numeric summary
summary(Housing_data);
nrow(Housing_data);

#Type converting
Housing_data$Price =as.numeric(Housing_data$Price) 
Housing_data$BuildingArea = as.integer(Housing_data$BuildingArea) 
Housing_data$YearBuilt = as.integer(Housing_data$YearBuilt)

Housing_data$Date =  format(as.Date(Housing_data$Date, format="%d/%m/%Y"),"%Y")
Housing_data$Date = as.numeric(Housing_data$Date)

#Factorizing the categorical variables.
Housing_data$Suburb <- factor(Housing_data$Suburb);
Housing_data$Address <- factor(Housing_data$Address);
Housing_data$Type <- factor(Housing_data$Type);
Housing_data$Method <- factor(Housing_data$Method);
Housing_data$SellerG <- factor(Housing_data$SellerG);
Housing_data$CouncilArea <- factor(Housing_data$CouncilArea);
Housing_data$Regionname <- factor(Housing_data$Regionname);


#Searching for duplicates
anyDuplicated(Housing_data);
duplicated(Housing_data);

Housing_data[duplicated(Housing_data),];


#visualization the duplication
dups <-  duplicated(Housing_data[1:21]) |
  duplicated(Housing_data[1:21], fromLast = T);
dups;
Housing_data[dups,];

# Treat duplicates
Housing_data1 <- Housing_data[!duplicated(Housing_data[1:21]),];
Housing_data1;

#Checking duplication again
nrow(Housing_data1);
nrow(Housing_data);
summary(Housing_data1);
anyDuplicated(Housing_data1);


#Checking the variable to determine independent and dependent variable
summary(Housing_data1);
class(Housing_data1$Price);

#Impute the medianvalues to all NA values of the independent variable.
Housing_data1[is.na(Housing_data1[9]),9] <- median(Housing_data1$Distance, na.rm = T);
Housing_data1[is.na(Housing_data1[14]),14] <- median(Housing_data1$Landsize, na.rm = T);
Housing_data1[is.na(Housing_data1[15]),15] <- median(Housing_data1$BuildingArea, na.rm = T);


#treating dependent variable. As per our assumptions.
#Price depends on house type.
#Rooms depends on house type.
#Bedroom2 depends on house type.
#Bathroom depends on house type.
#Car depends on house type.

df1 <- aggregate(Price ~Type ,data=Housing_data1,FUN = median, na.rm =  T);
df2 <- aggregate(Rooms ~Type ,data=Housing_data1,FUN = median, na.rm =  T);
df3 <- aggregate(Bedroom2 ~Type ,data=Housing_data1,FUN = median, na.rm =  T);
df4 <- aggregate(Bathroom ~Type ,data=Housing_data1,FUN = median, na.rm =  T);
df5 <- aggregate(Car ~Type ,data=Housing_data1,FUN = median, na.rm =  T);

Housing_data2 <- merge(Housing_data1, df1, by = c("Type"));
Housing_data3 <- merge(Housing_data2, df2, by = c("Type")); 
Housing_data4 <- merge(Housing_data3, df3, by = c("Type")); 
Housing_data5 <- merge(Housing_data4, df4, by = c("Type")); 
Housing_data6 <- merge(Housing_data5, df5, by = c("Type")); 

summary(Housing_data6);

#Imputing the NAs in each column with the categorical median values in new added column

Housing_data6[is.na(Housing_data6[5]),5] <- Housing_data6[is.na(Housing_data6[5]),22];
Housing_data6[is.na(Housing_data6[4]),4] <- Housing_data6[is.na(Housing_data6[4]),23];
Housing_data6[is.na(Housing_data6[11]),11] <- Housing_data6[is.na(Housing_data6[11]),24];
Housing_data6[is.na(Housing_data6[12]),12] <- Housing_data6[is.na(Housing_data6[12]),25];
Housing_data6[is.na(Housing_data6[13]),13] <- Housing_data6[is.na(Housing_data6[13]),26];

#Checking for NA value in each column again
sum(is.na(Housing_data6$Price.x));
sum(is.na(Housing_data6$Rooms.x));
sum(is.na(Housing_data6$Bedroom2.x));
sum(is.na(Housing_data6$Bathroom.x));
sum(is.na(Housing_data6$Car.x));

#treating dependent variable.
#Postcode depends on Suburb.
#Lattitude depends on Suburb.
#Longtitude depends on Suburb.

df6 <- aggregate(Postcode ~Suburb ,data=Housing_data1,FUN = median, na.rm =  T);
df7 <- aggregate(Lattitude ~Suburb ,data=Housing_data1,FUN = median, na.rm =  T);
df8 <- aggregate(Longtitude ~Suburb ,data=Housing_data1,FUN = median, na.rm =  T);

Housing_data7 <- merge(Housing_data6, df6, by = c("Suburb"));
Housing_data8 <- merge(Housing_data7, df7, by = c("Suburb"));
Housing_data9 <- merge(Housing_data8, df8, by = c("Suburb"));

summary(Housing_data9);

Housing_data9[is.na(Housing_data9[10]),10] <- Housing_data9[is.na(Housing_data9[10]),27];
Housing_data9[is.na(Housing_data9[18]),18] <- Housing_data9[is.na(Housing_data9[18]),28];
Housing_data9[is.na(Housing_data9[19]),19] <- Housing_data9[is.na(Housing_data9[19]),29];

sum(is.na(Housing_data9$Postcode.x));
sum(is.na(Housing_data9$Lattitude.x));
sum(is.na(Housing_data9$Longtitude.x));

#Subsetting the core dataset to display only the original columns.
Housing_data9 <- Housing_data9[1:21];
summary(Housing_data9);

#Renaming the fixed column.
names(Housing_data9)[4]<-"Rooms";
names(Housing_data9)[5]<-"Price";
names(Housing_data9)[10]<-"Postcode";
names(Housing_data9)[11]<-"Bedroom2";
names(Housing_data9)[12]<-"Bathroom";
names(Housing_data9)[13]<-"Car";
names(Housing_data9)[18]<-"Lattitude";
names(Housing_data9)[19]<-"Longtitude";

Housing_data9$Price =as.numeric(Housing_data9$Price) 
class(Housing_data9$Price) 

summary(Housing_data9);

nrow(Housing_data9);

cor(Housing_data9$Rooms,Housing_data9$Bedroom2)#0.75


#Checking correlation
#How can run correlation btw category variable and numeric variable
cor(Housing_data9$Rooms, Housing_data9$Price, "complete.obs"); #0.4890719
class(Housing_data9$Bedroom2)
cor(Housing_data9$Landsize, Housing_data9$Price, "complete.obs");#0.01224815
cor(Housing_data9$Bedroom2, Housing_data9$Price, "complete.obs"); #0.4054713
cor(Housing_data9$BuildingArea, Housing_data9$Price, "complete.obs");#0.03561516
cor(Housing_data9$Bathroom, Housing_data9$Price, "complete.obs"); #0.3670524
cor(Housing_data9$BuildingArea, Housing_data9$Distance, "complete.obs");#0.06162887
cor(Housing_data9$Distance, Housing_data9$Price, "complete.obs"); #-0.07995916
cor(Housing_data9$Landsize, Housing_data9$Price, "complete.obs"); #0.01224815
cor(Housing_data9$BuildingArea, Housing_data9$Price, "complete.obs");#0.03561516


#1. Does the distance form CBD effect the house price?
#From the correlation, we can see that the outcome is a negative value,Which means there is a negative relation between distance form CBD and price.
#The closer from the CBD, the higher the price. However, if we see the value of the correlation. The value is very low (-0.07995916),which indicates that there is an effect of distance and the price but it is very little.

cor(Housing_data9$Distance, Housing_data9$Price, "complete.obs");#-0.07995916

#2. Does the numbers of rooms have any impact on the house price?
#The correlation shows that there is a positive impact between rooms and price. The more numbers of rooms, the higher the price.
#However, the correlation value is not high (0.4890719). We can conclude that there is an impact but not that high.
cor(Housing_data9$Rooms, Housing_data9$Price, "complete.obs"); #0.4890719

#3. Which suburb has the highest average selling price?
#By using  aggregate()function,we can show the 3 topmost highest average house price are Abbotsford, Aberfeldie and Airport West. 
df1 <- aggregate(Price~Suburb, data = Housing_data9, FUN = mean, "complete.obs");
df1;

df1[order(df1$Price, decreasing = T),];

#4.Which type of property has the highest record of selling price and the lowest record of selling price.?
#By using aggregate() function say can that house type h (house, cottage, villa, semi, terrace) has the highest average selling price.
#Follow by house type t (townhouse) and u (unit, duplex).
aggregate(Price~Type, data = Housing_data9, FUN = mean);
aggregate(Price~Type, data = Housing_data9, FUN = max);
aggregate(Price~Type, data = Housing_data9, FUN = min);

#5.
#plot to visualize the relationship of each variable
Housing_data9;
par(pch = 20, col = 14);
plot(Housing_data9[c(4,11,12,13,14,15,5)]);

plot(Housing_data9[c(1,3,2,5,6,9,20)]);
summary(Housing_data9);



#Basic Plots and visualizations - plot to visualize the relationship of diffrent features of the house. 
#Histogram for the column Rooms
hist(Housing_data9$Rooms, col = "navy blue", xlab = "Rooms", ylab = "Count", xlim = c(1,8), ylim = c(0,10000),main = "Histogram of the Rooms column");

#Histogram for the column Price
hist(Housing_data9$Price, col = "red", xlab = "Price", ylab = "Count" , xlim = c(10^5,6*10^6), ylim = c(0,8000), main = "Histogram for the Price Column");

#Plot for Rooms vs Prices
plot(Housing_data9$Price ~ Housing_data9$Rooms, pch = 19, col = "navy blue", xlim = c(0,12), ylim = c(10^5,10^7), xlab = "Rooms", ylab = "Price", main = "Plot for Rooms vs Price");

#Plot for Type vs Prices
plot(Housing_data9$Price ~ Housing_data9$Type, pch = 19, col = "orange", xlim = c(0,12), ylim = c(10^5,10^7), xlab = "Type", ylab = "Price", main = "Plot for Type vs Price");

##Visualizations

require(ggplot2)
require(scales)

library(ggplot2);
#Barplot showing number of rooms  of the best selling houses.
ggplot(Housing_data9, aes(x = Rooms)) + geom_bar(fill = "brown", color = "black", size = 1.1) + 
  labs(title = " Number of rooms  of the best selling houses.", xlab = "Rooms", ylab = "Count") + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8"));

#Price vs Rooms
ggplot(Housing_data9, aes(Rooms , Price)) + geom_bar(stat = "identity", fill = "navy blue", size = 1.25) + 
  labs(title = "Price vs Rooms", xlab = "Rooms", ylab = "Price") + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8"));

#Most important Region based on number of houses sold
ggplot(Housing_data9, aes(x = Regionname)) + geom_bar(fill = "dark green", color = "black", size = 1.25) + labs(title = "Most important Region based on number of houses sold", xlab = "Region Name", ylab = "Count") + ylim(0,6000);

#Price vs Cars
ggplot(Housing_data9, aes(Car, Price)) + geom_bar(stat = "identity", fill = "red", size = 1.25) + labs(title = "Price vs Cars", xlab = "Cars", ylab = "Price") + xlim(0,8)

#Price vs Bathrooms
ggplot(Housing_data9, aes(Bathroom, Price)) + geom_jitter(color = "navy blue", size = 1.25, size = 3) + labs(title = "Price vs Bathrooms", xlab = "Bathrooms", ylab = "Price") + xlim(0,8) + ylim(1e+05,10e+06)

#Most important Type based on number of houses sold
ggplot(Housing_data9, aes(x = Type)) + geom_bar(fill = "orange", color = "blue", size = 1.1) + 
  labs(title = "Distribution Type on number of houses sold", 
       xlab = "Type", ylab = "Count") + ylim(0,6000);

#Create the model

#Fitting Linear Model

HousingLM <- lm(Housing_data9$Rooms~Housing_data9$Price, data = trainD);
summary(HousingLM); #Adjusted R-squared: 0.239  p-value <2.2e-16 

HousingLM2 <- lm(Housing_data9$Distance~Housing_data9$Price, data = trainD);
summary(HousingLM2); #Adjusted R-squared:  0.0063  p-value: < 2.2e-16

Housing.fit <- lm(Price~Suburb+Rooms+Type+Method+SellerG+Bathroom+Car+Landsize, data=Housing_data9)
summary(Housing.fit) #Adjusted R-squared:  0.5319  p-value: < 2.2e
plot(Housing.fit)

#Create graph
library(ggplot2);

qplot(Housing_data9$Rooms, Housing_data9$Price, data=HousingLM, geom = c("point","smooth"), method = "lm");

qplot(Housing_data9$Distance, Housing_data9$Price, data=HousingLM, geom = c("point","smooth"), method = "lm");

qplot(Housing_data9$Bedroom2, Housing_data9$Rooms, data=HousingLM3, geom = c("point","smooth"), method = "lm");

#Insert numeric column for Suburb
Housing_data9$SuburbNumeric <- as.numeric(Housing_data9$Suburb);
summary(Housing_data9);
Housing_data9$Suburb;

#Suburb and Price
qplot(Price, SuburbNumeric, data = Housing_data9, geom = c("point", "smooth"));

#CREATE MODEL PREDICTIONS

##Creation of prediction Model 1 

#Sampling
sampleIndexes <- sample(nrow(Housing_data9), as.integer(nrow(Housing_data9)*0.70));
Housing_train <- Housing_data9[sampleIndexes,];
Housing_train;
nrow(Housing_data9);
nrow(Housing_train);

Housing_test <- Housing_data9[-sampleIndexes,];
nrow(Housing_test)

#Prediction
predict_housing <- Housing_train[is.na(Housing_train[4]),];
predict_housing;

#Create a model 1 with Housing_train

HousingLM <- lm(Price~Rooms, data = Housing_train);
summary(HousingLM); #Adjusted R-squared:  0.2407   p-value: < 2.2e-16

qplot(Housing_data9$Rooms, Housing_data9$Price, data=HousingLM, geom = c("point","smooth"), method = "lm");

predictions_1 <- predict(HousingLM, newdata = predict_housing, se.fit = F);
predictions_1;

#Subsetting
predict_housing[5] <- predictions_1;
predict_housing;

cleanHouses<-rbind(Housing_train,predict_housing);
cleanHouses;

testLM <- lm(Price~Rooms, data=Housing_train);
testLM;
summary(testLM);    #Adjusted R-squared:  0.2407    p-value: < 2.2e-16

##Creation of prediction Model 3

#Prediction

predict_housing <- Housing_train[is.na(Housing_train[]),];
predict_housing;
str(Housing_train)

#Create a Model 3 with Housing_train

HousingLM3 <- lm(Price~Suburb+Rooms+Type+Method+Bathroom+Car+Landsize, data=Housing_train)
summary(HousingLM3) #Adjusted R-squared:  0.5114  p-value: < 2.2e-16

qplot(Housing_data9$Suburb+Rooms+Type+Method+Bathroom+Car+Landsize, Housing_data9$Price, data=HousingLM3, geom = c("point","smooth"), method = "lm");

predictions_3 <- predict(HousingLM3, newdata = Housing_train, se.fit = F);
predictions_3;

#Subsetting
predict_housing[5] <- predictions_3;
predict_housing;

cleanHouses<-rbind(Housing_train,predict_housing);
cleanHouses;

testLM <- lm(Price~Suburb+Rooms+Type+Method+Bathroom+Car+Landsize, data=Housing_test);
testLM;
summary(testLM)  #Adjusted R-squared:  0.5217  p-value: < 2.2e-16

##CLUSTER  - Analysis

#K-Means Clustering Housing data based on price

Housing_data9;
str(Housing_data9);

n <- nrow(Housing_data9)
data <- data.frame("index" = seq.int(n), "Price" = Housing_data9[,"Price"])
plot(data$index, data$Price, col="blue", pch = 16)

#get the clusters
KMeansmodel <- kmeans(data, 8)
print(KMeansmodel)
#plot K-Means clustering
plot(data, col = KMeansmodel$cluster)
points(KMeansmodel$centers, col = 1:3, pch = c(6, 7, 8), lwd = 5)

##K-Means Clustering Housing data based on Distance

n <- nrow(Housing_data9)
datad <- data.frame("index" = seq.int(n), "Distance" = Housing_data9[,"Distance"])
plot(datad$index, datad$Distance, col="blue", pch = 16)

KMeansmodel_d <- kmeans(datad, 5)
print(KMeansmodel_d)

plot(datad, col = KMeansmodel_d$cluster)
points(KMeansmodel_d$centers, col = 1:3, pch = c(6, 7, 8), lwd = 5)

###K-Means Clustering Housing data based on Buildingarea

n <- nrow(Housing_data9)
datab <- data.frame("index" = seq.int(n), "Regionname" = Housing_data9[,"Regionname"])
plot(datab$index, datab$Regionname, col="blue", pch = 16)

KMeansmodel_b <- kmeans(datab, 5)
print(KMeansmodel_b)

plot(datab, col = KMeansmodel_b$cluster)
points(KMeansmodel_b$centers, col = 1:3, pch = c(6, 7, 8), lwd = 5)
