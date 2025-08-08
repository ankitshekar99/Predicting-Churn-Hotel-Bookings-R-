#install.packages("readr")
#install.packages("caTools")
#install.packages("caret")
#install.packages("tree")
#install.packages("partykit")
#install.packages("randomForest")
#install.packages("glmnet")
#install.packages("factoextra")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("dplyr")

library(readr)
library(caTools)
library(caret)
library(tree)
library(partykit)
library(randomForest)
library(glmnet)
library(factoextra)
library(caret)
library(ggplot2)
library(dplyr)
source("DataAnalyticsFunctions.R")
options(warn=-1) #Switch off warning
data = read.csv("hotel_bookings.csv")
###############################################################
#################DATA UNDERSTANDING/PREPARATION################
###############################################################

summary(data) ### Run to identify and understand data.
###Checking for missing values

### There are 4 missing values for children column.
### Check number of canceled reservations 
table(data$is_canceled)
### we have 44224 canceled reservations and 75166 non-canceled reservations
sum(data$company=='NULL')/nrow(data) ###Checking percentage of NULL in company
### Dropping company because 94% has NULL. 
### Dropping arrival_date_year as we believe year does not affect cancelation, but month does(seasonality)
### Dropping reservation_status because this column has information we get ggploonly on the day of check-in. 
### Dropping reservation_status_date as this does not impact cancelation and we have lead time to track the number of days customer was in system.
drop = c("company","reservation_status_date","reservation_status","arrival_date_year")
data <- data[,!(names(data) %in% drop)]
##Removing all the entries where no people attended
data_1 = data.frame(subset(data, data$adults !=0 | data$children !=0 | data$babies!=0))

data_1$children[is.na(data_1$children)] = 0 ### Making all the NA as 0 for children


##Code to remove all outliers from demographic data
##Total outliers count = 3 ( Babies with count 10,9. Children with count 10)
data_2 = data.frame(subset(data_1, data_1$children < 10))
data_2 = data.frame(subset(data_2, data_2$babies<9))
##Removed all count of row with negative adr(Average Daily Rate)
data_2 = data.frame(subset(data_2, data_2$adr>=0))

### Code to check outliers in data so that splitting of data to test and train can be smoother.
table(data_2$assigned_room_type) ### Room type L = 1
table(data_2$reserved_room_type) ### Room type L = 6
#### Room type L has just 6 values in total, one row has reserved_room_type = assigned_room_type = L
data_2 = subset(data_2, data_2$assigned_room_type!='L')
data_2 = subset(data_2, data_2$reserved_room_type!='L') ###Removed room type L from data


agent_NA <-  ifelse( data_2$agent =='NULL' , 1, 0 )   ### creating a dummy variable for NULL in agent
sum(agent_NA)  ###total sum of NULL = 16280, making ~14% of data
table(data_2$agent) ###Checking how many values with just 1 response = 50. Therefore decided to drop it to avoid overfitting
drop = c("agent")
data_2 <- data_2[,!(names(data_2) %in% drop)]


table(data_2$is_canceled)
###Gives us info about number of cancellation in cleaned data => 44198

sum(data_2$is_canceled)/nrow(data_2)
###Gives us percentage of data with cancellation = 37.07%

new_table = table(data_2$hotel, data_2$is_canceled)
prop.table(new_table,margin=1)
###Gives us the percentage cancellation/successfull bookin in Resort Hotel and City Hotel
### 41.78% cancelation in City Hotel
### 27.76% cancelation in Resort Hotel

dummy =  ifelse( data_2$reserved_room_type == data_2$assigned_room_type , 1, 0 ) ###Creating interation variable for mismatch in rooms
data_2["matching_room"] = dummy

new_table2 = table(data_2$matching_room, data_2$is_canceled) ### Table to understand cancelation & matching rooms.
prop.table(new_table2,margin=1)             ###Probability distribution of cancelation depending on macthing room.
### Probability of cancelation if reserved rooms  = assigned room is 41.56%
### Probability of cancelation if reserved rooms != assigned room is 5.4%

### Changing all countries to continents for better regression model
continent_map <- c(
     'AGO' = 'Africa', 'BWA' = 'Africa', 'CAF' = 'Africa', 'CIV' = 'Africa', 'CPV' = 'Africa', 'DZA' = 'Africa',
     'EGY' = 'Africa', 'ETH' = 'Africa', 'GAB' = 'Africa', 'GHA' = 'Africa', 'KEN' = 'Africa', 'LBY' = 'Africa',
     'LSO' = 'Africa', 'MOZ' = 'Africa', 'MLI' = 'Africa', 'MWI' = 'Africa', 'NAM' = 'Africa', 'NGA' = 'Africa',
     'RWA' = 'Africa', 'SEN' = 'Africa', 'STP' = 'Africa', 'ZAF' = 'Africa', 'ZMB' = 'Africa', 'ZWE' = 'Africa',
     'TGO' = 'Africa', 'TUN' = 'Africa', 'BDI' = 'Africa', 'BEN' = 'Africa', 'BFA' = 'Africa', 'CMR' = 'Africa',
     'COM' = 'Africa', 'DJI' = 'Africa', 'GNB' = 'Africa', 'MAR' = 'Africa', 'MDG' = 'Africa', 'MRT' = 'Africa',
     'MUS' = 'Africa', 'MYT' = 'Africa', 'SLE' = 'Africa', 'SDN' = 'Africa', 'SYC' = 'Africa', 'TZA' = 'Africa',
     'UGA' = 'Africa', 'LCA' = 'Africa',
     
     'ARM' = 'Asia', 'AZE' = 'Asia', 'BHR' = 'Asia', 'BGD' = 'Asia', 'CHN' = 'Asia', 'CYP' = 'Asia', 'GEO' = 'Asia',
     'HKG' = 'Asia', 'IDN' = 'Asia', 'IND' = 'Asia', 'IRN' = 'Asia', 'IRQ' = 'Asia', 'ISR' = 'Asia', 'JPN' = 'Asia',
     'JOR' = 'Asia', 'KAZ' = 'Asia', 'KOR' = 'Asia', 'KWT' = 'Asia', 'LBN' = 'Asia', 'LKA' = 'Asia', 'MAC' = 'Asia',
     'MMR' = 'Asia', 'MDV' = 'Asia', 'MYS' = 'Asia', 'NPL' = 'Asia', 'PAK' = 'Asia', 'PHL' = 'Asia', 'QAT' = 'Asia',
     'SAU' = 'Asia', 'SGP' = 'Asia', 'SYR' = 'Asia', 'THA' = 'Asia', 'TJK' = 'Asia', 'TKM' = 'Asia', 'VNM' = 'Asia',
     'ARE' = 'Asia', 'CN'  = 'Asia', 'KHM' = 'Asia', 'LAO' = 'Asia', 'OMN' = 'Asia', 'TMP' = 'Asia', 'TWN' = 'Asia',
     'UZB' = 'Asia',
     
     'ALB' = 'Europe', 'AND' = 'Europe', 'AUT' = 'Europe', 'BEL' = 'Europe', 'BIH' = 'Europe', 'BGR' = 'Europe',
     'BLR' = 'Europe', 'CHE' = 'Europe', 'CZE' = 'Europe', 'DEU' = 'Europe', 'DNK' = 'Europe', 'ESP' = 'Europe',
     'FIN' = 'Europe', 'FRA' = 'Europe', 'GBR' = 'Europe', 'GRC' = 'Europe', 'HRV' = 'Europe', 'HUN' = 'Europe',
     'IRL' = 'Europe', 'ISL' = 'Europe', 'ITA' = 'Europe', 'LIE' = 'Europe', 'LTU' = 'Europe', 'LUX' = 'Europe',
     'LVA' = 'Europe', 'MLT' = 'Europe', 'MNE' = 'Europe', 'NLD' = 'Europe', 'NOR' = 'Europe', 'POL' = 'Europe',
     'PRT' = 'Europe', 'ROU' = 'Europe', 'RUS' = 'Europe', 'SMR' = 'Europe', 'SRB' = 'Europe', 'SVK' = 'Europe',
     'SVN' = 'Europe', 'SWE' = 'Europe', 'UKR' = 'Europe', 'EST' = 'Europe', 'GGY' = 'Europe', 'FRO' = 'Europe',
     'GIB' = 'Europe', 'JEY' = 'Europe', 'IMN' = "Europe", 'MCO' = 'Europe', 'MKD' = 'Europe', 'TUR' = 'Europe',
     'GBR ' ='Europe',
     
     'ABW' = 'North America', 'AIA' = 'North America', 'ATG' = 'North America', 'BRB' = 'North America',
     'BLZ' = 'North America', 'CAN' = 'North America', 'CYM' = 'North America', 'DMA' = 'North America',
     'DOM' = 'North America', 'GTM' = 'North America', 'HND' = 'North America', 'JAM' = 'North America',
     'MEX' = 'North America', 'NIC' = 'North America', 'PAN' = 'North America', 'PRI' = 'North America',
     'SLV' = 'North America', 'USA' = 'North America', 'VGB' = 'North America', 'VIR' = 'North America',
     'BHS' = 'North America', 'CRI' = 'North America', 'CUB' = 'North America', 'GLP' = 'North America',
     'KNA' = 'North America', 'UMI' = 'North America',
     
     'ARG' = 'South America', 'BOL' = 'South America', 'BRA' = 'South America', 'CHL' = 'South America',
     'COL' = 'South America', 'ECU' = 'South America', 'GUY' = 'South America', 'PER' = 'South America',
     'PRY' = 'South America', 'SUR' = 'South America', 'URY' = 'South America', 'VEN' = 'South America',
     
     'AUS' = 'Oceania', 'FJI' = 'Oceania', 'KIR' = 'Oceania', 'NZL' = 'Oceania', 'PNG' = 'Oceania',
     'SLB' = 'Oceania', 'VUT' = 'Oceania', 'ASM' = 'Oceania', 'NCL' = 'Oceania', 'PLW' = 'Oceania',
     'PYF' = 'Oceania',
     
     'ATA' = 'NULL', 'ATF' = 'NULL', 'NULL' = 'NULL'
)

data_2$continent <- continent_map[data_2$country]
drop = c("country") ###Dropping country as we no longer use country
data_2 <- data_2[,!(names(data_2) %in% drop)]


###Creating factors to handle categorical data from out dataset
data_2$hotel =factor(data_2$hotel)
data_2$arrival_date_month =factor(data_2$arrival_date_month)
data_2$arrival_date_week_number =factor(data_2$arrival_date_week_number)
data_2$arrival_date_day_of_month =factor(data_2$arrival_date_day_of_month)
data_2$meal =factor(data_2$meal)
data_2$market_segment =factor(data_2$market_segment)
data_2$distribution_channel =factor(data_2$distribution_channel)
data_2$reserved_room_type =factor(data_2$reserved_room_type)
data_2$assigned_room_type =factor(data_2$assigned_room_type)
data_2$deposit_type =factor(data_2$deposit_type)
data_2$customer_type =factor(data_2$customer_type)
data_2$continent = factor(data_2$continent)

summary(data_2) ### checking cleamned data 

###At this point we have data with relevant columns and rows. 

###############################################################
#################DATA VISUALIZATION############################
###############################################################

#### plot to see cancelation rate vs lead_time
canceled = data.frame(subset(data_2,data_2$is_canceled == 1))
not_canceled = data.frame(subset(data_2,data_2$is_canceled == 0))
ggplot() +
       geom_histogram(data = canceled, aes(x = lead_time, y = ..density..), 
       bins = 50, fill = "red", alpha = 0.5, position = "identity") +
      geom_histogram(data = not_canceled, aes(x = lead_time, y = ..density..), 
       bins = 50, fill = "blue", alpha = 0.5, position = "identity") +
      geom_density(data = canceled, aes(x = lead_time), color = "red", size = 1) +
      geom_density(data = not_canceled, aes(x = lead_time), color = "blue", size = 1) +
      labs(title = "Lead Time VS Cancelation",
                       x = "Lead Time (days)",
                       y = "Frequency") +
       theme_minimal() +
       scale_fill_manual(values =c("Canceled" = "red", "Not Canceled" = "blue")) +
       scale_color_manual(name = "Booking Status", values = c("Canceled" = "red", "Not Canceled" = "blue")) +
       theme(legend.position = "top")

### plot to see cancelation in City Hotel and Resort Hotel
barplot(new_table, beside = TRUE, names = c("City Hotel", "Resort Hotel;"), main = "Cancelation based on hotel type", xlab= 'Hotel Type', ylab = 'Number of booking', ylim =c(0,50000), legend.text = c("Not-Canceled","Canceled"))

### Plot to see cancelation based on Continent
data_plot <- data.frame(Continent = data_2$continent,Frequency = data_2$is_canceled)
ggplot(data_plot, aes(x = Continent, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Frequency of Cancelation by Continent", x = "Continent",y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))

### Plot to see adr across months
room_prices_monthly <- data_2 %>% 
  select(hotel, arrival_date_month, adr) %>%
  arrange(arrival_date_month)

# Order by month:
ordered_months <- c("January", "February", "March", "April", "May", "June", 
                    "July", "August", "September", "October", "November", "December")

room_prices_monthly$arrival_date_month <- factor(room_prices_monthly$arrival_date_month, 
                                                 levels = ordered_months, ordered = TRUE)

plot(room_prices_monthly$arrival_date_month, room_prices_monthly$adr, type = "l", ylim = c(0,550), xlab = "Months", ylab = "Box Plot of ADR")

### Plot to see cancelation based on total number of special requests. 
new_table18 = table(data_2$total_of_special_requests, data_2$is_canceled)
plot(new_table18, main = "Cancelation based on total number of special requests", xlab= 'Number of special requests', ylab = 'Canceled or Not Canceled')
###############################################################
#################ADDITIONAL DATA EXPLORATION###################
###############################################################

### PCA 
xdata <- model.matrix(is_canceled ~ ., data=data_2)[,-1]
xdata <- scale(xdata)

pca.data <- prcomp(xdata, scale=TRUE)
### Lets plot the variance that each component explains
par(mar=c(4,4,4,4)+0.3)
plot(pca.data,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

loadings <- pca.data$rotation[,1:3]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(xdata)],1]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### Looking at which are large positive and large negative
#### First factor market_segments that are groups or transient party's
####
#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(xdata)],2]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### Second factor is distribution channels through travel agents and travel operators
#### Loading 3
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(xdata)],3]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### the third factor is the number of children and the number of days leading up to the reservation date

#########################################
#### K MEANS
#########################################
xdata1 <- model.matrix(is_canceled ~ ., data=data_2)[,-1]
xdata1 <- scale(xdata1)
twoCenters <- kmeans(xdata1,2,nstart=30)
twoCenters$centers[1,]
twoCenters$centers[2,]
### variation explained with 2 clusters
1 - twoCenters$tot.withinss/twoCenters$totss
### Sizes of clusters
twoCenters$size
### cluser 1 has a cancelation rate of 41.05% and cluster 2 has a cancelation rate of 17.36%
### Cluster 1 has an average lead time of 115.55 days and cluster 2 has an average lead time of 47.38 days
### Cluster 1 has an avereage previous bookings not cancelled as 1.79% and cluster 2 has it at 72.77%
aggregate( data_2$adults ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$is_canceled == "1" ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$children ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$babies ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$previous_cancellations ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$lead_time ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$previous_bookings_not_canceled ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$is_repeated_guest == "1" ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$booking_changes ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$adr ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$total_of_special_requests ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$required_car_parking_spaces ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$stays_in_weekend_nights ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$stays_in_week_nights ~ twoCenters$cluster, FUN = mean )
aggregate( data_2$days_in_waiting_list ~ twoCenters$cluster, FUN = mean )


#######################

threeCenters <- kmeans(xdata1,3,nstart=30)
threeCenters$centers[1,]
threeCenters$centers[2,]
threeCenters$centers[3,]

### variation explained with 3 clusters
1 - threeCenters$tot.withinss/threeCenters$totss
### Sizes of clusters
threeCenters$size

### cluser 1 has an average time on waiting list of 6 days. Cluster 2 has a much lesser wait time, of around 0.08 days. Cluster 3 barely ever waits, with the average at 0.001 days
### Cluster 1 has an average lead time of 161.32 days and cluster 2 has an average lead time of 40.81 days and cluster 3 has an average lead time of 82.20 days
### Cluster 1 has an avereage of 0.163 special requests. cluster 2 has an average of 0.48 and cluster 3 has much more requests at 0.906
### Cluster 1 also has the least average daily rate at 83.13, cluster 2 is a bit more pricey with an adr of 97.44 and cluster 3 has the maximum adr of 116.66
aggregate( data_2$adults ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$is_canceled == "1" ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$children ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$babies ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$previous_cancellations ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$lead_time ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$previous_bookings_not_canceled ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$is_repeated_guest == "1" ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$booking_changes ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$adr ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$total_of_special_requests ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$required_car_parking_spaces ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$stays_in_weekend_nights ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$stays_in_week_nights ~ threeCenters$cluster, FUN = mean )
aggregate( data_2$days_in_waiting_list ~ threeCenters$cluster, FUN = mean )

####################################
fourCenters <- kmeans(xdata1,4,nstart=30)
### Centers
fourCenters$centers[1,]
fourCenters$centers[2,]
fourCenters$centers[3,]
fourCenters$centers[4,]
### Sizes of clusters
fourCenters$size
### variation explained with 4 clusters
1 - fourCenters$tot.withinss/fourCenters$totss

### Cluster 1 has an average lead time of 161.98 days and cluster 2 has an average lead time of 82.45 days and cluster 3 has an average lead time of 40.22 days and cluster 3 has an average lead time of 69.07 days
### Cluster 1 also has the least average daily rate at 83.16, cluster 2 is a bit more pricey with an adr of 115.08, cluster 3 has an adr of 95.08 and cluster 4 has the most expensive daily rate of 171.73
### Cluster 1 had an average of 0.159 special requests. 2 had an average of 0.908, 3 had an average 0f 0.482 and 0.7317 special requests.

aggregate( data_2$adults ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$is_canceled == "1" ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$children ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$babies ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$previous_cancellations ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$lead_time ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$previous_bookings_not_canceled ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$is_repeated_guest == "1" ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$booking_changes ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$adr ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$total_of_special_requests ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$required_car_parking_spaces ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$stays_in_weekend_nights ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$stays_in_week_nights ~ fourCenters$cluster, FUN = mean )
aggregate( data_2$days_in_waiting_list ~ fourCenters$cluster, FUN = mean )


fviz_cluster(twoCenters, data = xdata1, geom = "point", ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal(), main = "K-means Clustering of Booking Data")

fviz_cluster(threeCenters, data = xdata1, geom = "point", ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal(), main = "K-means Clustering of Booking Data")

fviz_cluster(fourCenters, data = xdata1, geom = "point", ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal(), main = "K-means Clustering of Booking Data")

###############################################################
#################DATA MODELING#################################
###############################################################

###############LOGISTIC REGRESSION#############################
set.seed(12)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(is_canceled~., data=train, family="binomial") ###Logistic Regression with all features
summary(result)
p1 = predict.glm(result, newdata = test, type = "response")
p2 = as.factor(ifelse(p1 >0.5, 1, 0))
confusionMatrix(p2, as.factor(test$is_canceled))
##### Accuracy is 81.57%

#####################CART#############################
train_duplicate = data.frame(train)
test_duplicate = data.frame(test)
m.tree <- tree(is_canceled ~. -arrival_date_week_number, data=train_duplicate) 
pred.tree <- predict(m.tree, newdata=test_duplicate, type="vector")
plot(m.tree)
text(m.tree)
#Confusion matrix to check prediction accuracy
tree1 <- predict(m.tree, newdata=test_duplicate, type="vector")
tree3 <- as.factor(ifelse(tree1 >0.5, 1, 0))
confusionMatrix(tree3, as.factor(test_duplicate$is_canceled))
##### Accuracy for OOS is 80.59%
#######Accuracy for LOGISTIC REGRESSION > CART#########

################Model Selection Criteria and handling OVERFITTING#####################
#### BIC to handle Overfitting
### NULL MODEL
null = glm(is_canceled~1, data=train, family="binomial")
full = result
with_BIC = step(full, scope = list(lower = null, upper = full),
                direction = "both", k = log(nrow(train)))

### AIC to handle Overfitting
with_AIC = step(full, scope = list(lower = null, upper = full),
                direction="both", k = 2)


#### OOS Prediction
n1 <- predict.glm(null, newdata = test, type = "response")
n2 <- as.factor(ifelse(n1 > 0.5, 1, 0))
confusionMatrix(n2, as.factor(test$is_canceled))

#### In sample prediction
n0 <- predict.glm(null, newdata = train, type = "response")
n3 <- as.factor(ifelse(n0 > 0.5, 1, 0))
confusionMatrix(n3, as.factor(train$is_canceled))
length(null$coefficients)

#### Logistic regression model
full = result
#### OOS Prediction
f1 <- predict.glm(full, newdata = test, type = "response")
f2 <- as.factor(ifelse(f1 > 0.5, 1, 0))
confusionMatrix(f2, as.factor(test$is_canceled))

#### In sample prediction
f0 <- predict.glm(full, newdata = train, type = "response")
f3 <- as.factor(ifelse(f0 > 0.5, 1, 0))
confusionMatrix(f3, as.factor(train$is_canceled))
length(full$coefficients)



#### Prediction using AIC Model
### OOS Model
x1 <- predict.glm(with_AIC, newdata = test, type = "response")
x2 <- as.factor(ifelse(x1 > 0.5, 1, 0))
confusionMatrix(x2, as.factor(test$is_canceled))

### In sample prediction
x0 <- predict.glm(with_AIC, newdata = train, type = "response")
x3 <- as.factor(ifelse(x0 > 0.5, 1, 0))
confusionMatrix(x3, as.factor(train$is_canceled))
length(with_AIC$coefficients)


#### Prediction using BIC Model
### OOS Model
y1 <- predict.glm(with_BIC, newdata = test, type = "response")
y2 <- as.factor(ifelse(y1 > 0.5, 1, 0))
confusionMatrix(y2, as.factor(test$is_canceled))
### In sample prediction
y0 <- predict.glm(with_BIC, newdata = train, type = "response")
y3 <- as.factor(ifelse(y0 > 0.5, 1, 0))
confusionMatrix(y3, as.factor(train$is_canceled))
length(with_BIC$coefficients)

#### Accuracy with BIC              = 81.48%
#### Accuracy with AIC              = 81.22%
#### Accuracy with Log. Regression  = 81.23%
#### Accuracy with CART             = 80.59%
#### Null                           = 62.87%

summary(with_BIC)  ### Checking BIC Summary to understand data.
varImp(with_BIC)   ### Identify important features
### Feature 1:  Total_of_special_requests(Score:54.61139)
### Feature 2:  deposit_typeNonRefund    (Score:41.75907)
### Feature 3:  previous_cancellation    (Score:39.69796)
### Feature 4:  matching_room            (Score:35.33822)
### Feature 5:  lead_time                (Score:35.10890)

###### Plotting FPR and TPR
##### Plotting FPR and TPR for threshold 0.5
BIC1 = as.factor(ifelse(y1 >0.5, 1, 0))
conf_matrix_1 = confusionMatrix(BIC1, as.factor(test$is_canceled))  
TP1 = conf_matrix_1$table[2,2]
TN1 = conf_matrix_1$table[1,1]
FP1 = conf_matrix_1$table[2,1]
FN1 = conf_matrix_1$table[1,2]

LR.FPR_0.5 <- FP1 / (FP1 + TN1)
LR.TPR_0.5 <- TP1 / (TP1 + FN1)

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( LR.FPR_0.5), c(LR.TPR_0.5))
text( c( LR.FPR_0.5+0.26), c(LR.TPR_0.5-.10), labels=c("BIC (0.5)"))

##### Plotting FPR and TPR for threshold 0.35
BIC2 = as.factor(ifelse(y1 >0.35, 1, 0))
conf_matrix_2 = confusionMatrix(BIC2, as.factor(test$is_canceled))  
TP2 = conf_matrix_2$table[2,2]
TN2 = conf_matrix_2$table[1,1]
FP2 = conf_matrix_2$table[2,1]
FN2 = conf_matrix_2$table[1,2]

LR.FPR_0.35 <- FP2 / (FP2 + TN2)
LR.TPR_0.35 <- TP2 / (TP2 + FN2)
points( c( LR.FPR_0.35), c(LR.TPR_0.35))
text( c( LR.FPR_0.35+0.26), c(LR.TPR_0.35-.10), labels=c("BIC (0.35)"))

##### Plotting FPR and TPR for threshold 0.65
BIC3 = as.factor(ifelse(y1 > 0.65, 1, 0))
conf_matrix_3 = confusionMatrix(BIC3, as.factor(test$is_canceled)) 
TP3 = conf_matrix_3$table[2,2]
TN3 = conf_matrix_3$table[1,1]
FP3 = conf_matrix_3$table[2,1]
FN3 = conf_matrix_3$table[1,2]

LR.FPR_0.65 <- FP3 / (FP3 + TN3)
LR.TPR_0.65 <- TP3 / (TP3 + FN3)
points( c( LR.FPR_0.65), c(LR.TPR_0.65))
text( c( LR.FPR_0.65+0.26), c(LR.TPR_0.65-.10), labels=c("BIC (0.65)"))

##### Plotting FPR and TPR for threshold 1
BIC4 = as.factor(ifelse(y1 >= 0, 1, 0))
conf_matrix_4 = confusionMatrix(BIC4, as.factor(test$is_canceled)) 
TP4 = conf_matrix_4$table[2,2]
TN4 = conf_matrix_4$table[1,1]
FP4 = conf_matrix_4$table[2,1]
FN4 = conf_matrix_4$table[1,2]

LR.FPR_1 <- FP4 / (FP4 + TN4)
LR.TPR_1 <- TP4 / (TP4 + FN4)
points( c( LR.FPR_1), c(LR.TPR_1))
text( c( LR.FPR_1+0.36), c(LR.TPR_1+.10), labels=c("BIC (1)"))

##### Plotting FPR and TPR for threshold 0
BIC5 = as.factor(ifelse(y1 >= 0, 1, 0))
conf_matrix_5 = confusionMatrix(BIC5, as.factor(test$is_canceled)) 
TP5 = conf_matrix_5$table[2,2]
TN5 = conf_matrix_5$table[1,1]
FP5 = conf_matrix_5$table[2,1]
FN5 = conf_matrix_5$table[1,2]

LR.FPR_0 <- FP5 / (FP5 + TN5)
LR.TPR_0 <- TP5 / (TP5 + FN5)
points( c( LR.FPR_0), c(LR.TPR_0))
text( c( LR.FPR_0+0.16), c(LR.TPR_1+.10), labels=c("BIC (0)"))

lines(c(LR.FPR_0,LR.FPR_0.35, LR.FPR_0.5, LR.FPR_0.65, LR.FPR_1), c(LR.TPR_0,LR.TPR_0.35, LR.TPR_0.5, LR.TPR_0.65, LR.TPR_1), col = "blue", lwd = 2)

######### BIC on different sample seed number#######
B <- with_BIC$formula

set.seed(12)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(B, data=train, family=binomial)

y1 <- predict.glm(result, newdata = test, type = "response")

a2 <- as.factor(ifelse(y1 > 0.5, 1, 0))
confusionMatrix(a2, as.factor(test$is_canceled))

# 0.8148`

set.seed(13)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(B, data=train, family=binomial)

y2 <- predict.glm(result, newdata = test, type = "response")

a2 <- as.factor(ifelse(y2 > 0.5, 1, 0))
confusionMatrix(a2, as.factor(test$is_canceled))

# 0.8164

set.seed(14)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(B, data=train, family=binomial)

y3 <- predict.glm(result, newdata = test, type = "response")

a2 <- as.factor(ifelse(y3 > 0.5, 1, 0))
confusionMatrix(a2, as.factor(test$is_canceled))

# 0.8152

set.seed(15)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(B, data=train, family=binomial)

y4 <- predict.glm(result, newdata = test, type = "response")

a2 <- as.factor(ifelse(y4 > 0.5, 1, 0))
confusionMatrix(a2, as.factor(test$is_canceled))

# 0.8122

set.seed(16)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(B, data=train, family=binomial)

y5 <- predict.glm(result, newdata = test, type = "response")

a2 <- as.factor(ifelse(y5 > 0.5, 1, 0))
confusionMatrix(a2, as.factor(test$is_canceled))

# 0.8163

set.seed(17)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(B, data=train, family=binomial)

y6 <- predict.glm(result, newdata = test, type = "response")

a2 <- as.factor(ifelse(y6 > 0.5, 1, 0))
confusionMatrix(a2, as.factor(test$is_canceled))

# 0.8158

set.seed(18)
sample=sample.split(data_2,SplitRatio=0.80)
train=subset(data_2,sample==TRUE)    ###Training dataset(80%)
test=subset(data_2,sample==FALSE)    ###TEST data set(20%)

result <- glm(B, data=train, family=binomial)

y7 <- predict.glm(result, newdata = test, type = "response")

a2 <- as.factor(ifelse(y7 > 0.5, 1, 0))
confusionMatrix(a2, as.factor(test$is_canceled))

# 0.8163

values <- c(0.8148, 0.8164, 0.8152, 0.8122, 0.8163, 0.8158, 0.8163)
bar_names <- paste(1:length(values))

dahata <- data.frame(Sample = bar_names, Accuracy = values)

#### Graph to show model accuracy with different data seed
ggplot(dahata, aes(x = Sample, y = Accuracy)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.9) + scale_y_continuous(breaks = seq(0,1,by = 0.05))

############Business Deployment######################
### Risk assessment
new_test = test  ## Copy of test
testresult = predict.glm(with_BIC, newdata = test, type = "response")

### Adding new column called predicted probability 
new_test$predicted_prob = testresult

#### Making buckets to identify High Risk, Medium Risk and Low Risk individuals based on probability
#### Threshold values based on k-means clusters. 
new_test <- new_test %>% mutate(risk_rating = case_when(
  predicted_prob >= 0.476 ~ "High Risk",   
  predicted_prob >= 0.1596 & predicted_prob < 0.476 ~ "Medium Risk",
  predicted_prob < 0.1596 ~ "Low Risk" ))
head(new_test)

new_table25 <- table(new_test$is_canceled, new_test$risk_rating)

### Plotting graph to understand cancelation in each risk assessment
barplot(new_table25, names = c("High Risk", "Low Risk", "Medium Risk"), main = "Cancelation based on Risk Assignment", xlab= 'Risk Type', ylab = 'Number of booking', ylim =c(0,10000), legend.text = c("Not-Canceled","Canceled"))
prop.table(new_table25, margin = 2) ### Found out percentgae of cancelation in each

#### HIGH RISK = 80.7% (Cancelation Percent)
#### LOW RISK. = 25.4% (Cancelation Percent)
#### MED RISK  =  7.7% (Cancelation Percent)
