##################################
##################################
#### Group9
#### Elizaveta Chaplygina
#### Air France 01/27/2019 ######
####
#### Submitted 02/04/2019 #######
## Importing excel file ###

library(readxl)
air_france <- read_excel("Desktop/R/Cases/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")
View(air_france)

#############################################################################
#############################################################################
#### Descriptive statistics for Amount, Impressions, Cost, Clicks
#############################################################################
#############################################################################

desc_stat <- c("Mean", "Median", "SD", "Min", "Max")
amount_s <- c(mean(air_df$Amount),median(air_df$Amount), sd(air_df$Amount),min(air_df$Amount), max(air_df$Amount))
total_s <- c(mean(air_df$`Total Cost`),median(air_df$`Total Cost`),sd(air_df$`Total Cost`),min(air_df$`Total Cost`),max(air_df$`Total Cost`))
imp_s <- c(mean(air_df$Impressions),median(air_df$Impressions),sd(air_df$Impressions),min(air_df$Impressions),max(air_df$Impressions))
click_s <- c(mean(air_df$Clicks),median(air_df$Clicks),sd(air_df$Clicks),min(air_df$Clicks),max(air_df$Clicks))
summary_table1 <- cbind(desc_stat, amount_s, total_s, imp_s, click_s)


#############################################################################
####DOING TABLE TWO PART 2 with standard, broad and advanced means calculated.
#### Research more of the descriptive statistics
#############################################################################

click_standard <- air_df[which(air_df$`Match Type` == "Standard"),]
click_broad <- air_df[which(air_df$`Match Type` == "Broad"), ]
click_advanced <- air_df[which(air_df$`Match Type` == "Advanced"), ]

table_name <- c("Standard", "Broad", "Advanced")
click_means <- c(mean(click_standard$Clicks),mean(click_broad$Clicks),mean(click_advanced$Clicks))
amount_means <- c(mean(click_standard$Amount), mean(click_broad$Amount),mean(click_advanced$Amount))
vol_booking_mean <- c(mean(click_standard$`Total Volume of Bookings`),mean(click_broad$`Total Volume of Bookings`),
                      mean(click_advanced$`Total Volume of Bookings`))
summary_table2 <- cbind(table_name, click_means, amount_means, vol_booking_mean)


# Descriptive Statistics
summary(air_france)

# Creating a new dataframe 
air_df <- as.data.frame(air_france)
View(air_df)

#Checking for null values column wise
sapply(air_df,function(x)sum(is.na(x)))

# Replacing NAs in Bid Strategy column for No Strategy.
air_df[is.na(air_df)] <- "No Strategy"


#######################################################
#Calculated Fields
#######################################################

#Net Revenue
air_df$`Net Revenue` <- air_df$Amount - air_df$`Total Cost`

#Profit per Click
air_df$Profit_per_Click <- air_df$`Net Revenue` / air_df$Clicks

#Profit per Booking (in case with 0 Bookings we put Total cost as Profit per click)
for (i in 1 : nrow(air_df)) {
  
  if(air_df$`Total Volume of Bookings`[i] == 0) {
    air_df$`Net Revenue_per_Booking`[i] <- air_df$`Total Cost`[i]
  }
  
  else { 
    air_df$`Net Revenue_per_Booking` [i] <- (air_df$`Net Revenue`[i]) / (air_df$`Total Volume of Bookings`[i])
  }
}

#Average Booking Price
for (i in 1 : nrow(air_df)) {
  
  if(air_df$`Total Volume of Bookings`[i] == 0) {
    air_df$Avg_Price_Booking[i] <- air_df$`Total Cost`[i]
  }
  
  else { 
    air_df$Avg_Price_Booking [i] <- (air_df$`Net Revenue`[i]) / (air_df$Amount[i] / air_df$`Total Volume of Bookings`[i])
  }
}

#Cost per Impression

air_df$Cost_per_Impression <- air_df$Impressions / air_df$`Total Cost`

#############################################################
# Using SQL for quick filtering and subsetting
#############################################################
# Filtering with sql Template (counts how many times the word was used, and total volume sold)
library(sqldf)

sqldf('SELECT keyword, 
      COUNT(keyword) as count, 
      SUM(`Total Volume of Bookings`) AS volume 
      FROM air_df WHERE `Total Volume of Bookings` > 0 
      GROUP BY keyword 
      ORDER BY count DESC, volume')



### Campaign biggest Net Revenue per publisher
Campaign1 <- sqldf('SELECT Campaign, `Net Revenue`, `Publisher Name`
                   FROM air_df 
                   ORDER BY `Net Revenue` DESC'
                   )

#The most proofitable click
Campaign_word <- sqldf('SELECT Campaign, Keyword, Profit_per_Click, `Total Volume of Bookings`
                   FROM air_df 
                    WHERE `Total Volume of Bookings`>9
                   ORDER BY Profit_per_Click DESC, `Total Volume of Bookings`')

#### Campaign, key word,  per word, AND it filters by status 

Campaign2 <- sqldf('SELECT Campaign, Keyword, `Publisher Name`, `Net Revenue` 
                   FROM air_df 
                  WHERE Status NOT IN ("Unavailable", "Deactivated")
                   ORDER BY `Net Revenue` DESC')


#Net revenue by broad terms such as 'airline ticket' by Publisher
Broad_entry <- sqldf('SELECT Keyword, `Publisher Name`, SUM(`Net Revenue`) AS net_rev, AVG(`Net Revenue_per_Booking`)
                     FROM air_df
                     WHERE Keyword NOT LIKE "%air%france%"
                      AND `Total Volume of Bookings` > 0
                      GROUP BY `Publisher Name`
                     ORDER BY SUM(`Net Revenue`) DESC')
Broad_entry_net_revenue <- sum(Broad_entry$net_rev)
All_net_revenue <- sum(air_df$`Net Revenue`)
Percent_earned <- Broad_entry_net_revenue/All_net_revenue
sprintf ("%s %f percent", "Here is the percent earned by non airfrance words", Percent_earned*100.0)


#### ROA for key words by Match type in DESC order

ROA_keywords <- sqldf('SELECT Keyword, Amount/`Total Cost` AS ROA, `Match Type`, `Publisher Name`
                      FROM air_df
                      WHERE ROA > 0
                      AND `Total Volume of Bookings`>9
                      ORDER BY `Match Type`, ROA DESC')

View(ROA_keywords)

##### creates ROA by match type

ROA_by_match_type <- sqldf('SELECT Keyword, SUM(Amount) AS newAmount, SUM(`Total Cost`) AS newCost, `Match Type`, `Publisher Name`
                     FROM air_df
                      GROUP BY `Match Type`
                     ORDER BY `Match Type` DESC')
ROA_keywords$ROA <- ROA_keywords$newAmount/ROA_keywords$newCost # this is ROA generating line
View(ROA_keywords)

#### % of clicks over IMpressions
Best_clicks_impessions <- sqldf('SELECT Campaign, `Engine Click Thru %`, `Publisher Name`, `Total Volume of Bookings`
                          FROM air_df
WHERE `Engine Click Thru %` < 100
AND `Total Volume of Bookings` > 0
                           ORDER BY `Engine Click Thru %` DESC')


#######################################
## Created by Elizaveta Chaplygina
#######################################
## 1.23.19 at Hult
#######################################
#######################################
###

library(sqldf)
OUR_BEST_CAMPAGN <- sqldf('SELECT Keyword, `Publisher Name`, `Total Volume of Bookings`, `Net Revenue`, ROA
                          FROM air_df
                                WHERE Keyword LIKE "air%france"
                                AND `Publisher Name` LIKE "%Yahoo%" 
                                ORDER BY `Net Revenue` DESC')

air_df$ROA <- air_df$Amount/air_df$`Total Cost`


###############################
#### GROUPING by ticket volumes
##############################
for (i in 1:nrow(air_df)){
  
if(air_df$`Total Volume of Bookings`[i] > 0 & air_df$`Total Volume of Bookings`[i]  <= 10 ) {air_df$volume_group[i]<- 1}
else if (air_df$`Total Volume of Bookings`[i] > 10 & air_df$`Total Volume of Bookings`[i] <= 50) {air_df$volume_group[i] <-2 }
else if (air_df$`Total Volume of Bookings`[i] > 50 & air_df$`Total Volume of Bookings`[i]<= 100) {air_df$volume_group[i] <-3 }
else if (air_df$`Total Volume of Bookings`[i] > 100) {air_df$volume_group[i] <-4 }
}

# GROUPING 
OUR_BEST_CAMPAGN <- sqldf('SELECT Keyword, ROA, volume_group
                          FROM air_df
                          WHERE Keyword LIKE "air france" OR Keyword LIKE "[air france]" OR Keyword LIKE "airfrance" OR Keyword LIKE "[airfrance]"
                          
                          ORDER BY volume_group, ROA')


# ROA CREATION 
ROA_by_volume_group <- sqldf('SELECT Keyword, SUM(Amount) AS newAmount, SUM(`Total Cost`) AS newCost,volume_group
                     FROM air_df
                           GROUP BY volume_group
                           ORDER BY volume_group DESC')
ROA_by_volume_group$ROA <- ROA_by_volume_group$newAmount/ROA_by_volume_group$newCost # this is ROA generating line
View(ROA_by_volume_group)


# Which key word leads to the volume of sales 10-50 bookings?
keywords_group2 <- sqldf('SELECT Keyword, `Publisher Name`
                     FROM air_df
                         WHERE volume_group = 2
                         ORDER BY `Publisher Name`')

# Best Campaign by net_revenue: Air France Branded
# Best Campaign by "money-making word": Air France Branded (top 10 campaign)
# Best Campaign by conversion impressions to clicks: Geo Targeted NY, Geo Targeted Miami ,  Air France Brand & French Destinations 


# Campaigns that have 0 bookings but the highest impressions
library("sqldf")
Best_impressions <- sqldf('SELECT Keyword, Campaign, Impressions, `Total Cost`, `Publisher Name`
                      FROM air_df
                      WHERE `Total Volume of Bookings` = 0                          
                      ORDER BY Impressions DESC')


##########################
#########################
# METRICS PER PUBLISHER FOR BRANDED AND NOT BRANDED
# ROA, Volume, Total cost
# + same metrics divided by Branded and not Branded words
#########################
###########################
metrics_by_publisher <- sqldf('SELECT `Publisher Name`, SUM(Amount) AS Total_Amount, SUM(`Total Cost`) AS Total_Cost,
                              SUM(`Total Volume of Bookings`) AS Volume, SUM(Impressions) AS Impressions
                              FROM air_df
                             GROUP BY `Publisher Name`
                             ORDER BY `Publisher Name` DESC')
metrics_by_publisher$ROA <- metrics_by_publisher$Total_Amount/metrics_by_publisher$Total_Cost # this is ROA generating line
View(metrics_by_publisher)

metrics_by_brand <- sqldf('SELECT `Publisher Name`, SUM(Amount) AS Total_Amount_Brand, SUM(`Total Cost`) AS Total_Cost_Brand,
                              SUM(`Total Volume of Bookings`) AS Volume_Brand, SUM(Impressions) AS Impressions_Brand
                              FROM air_df
                              WHERE Keyword LIKE "air france" OR Keyword LIKE "[air france]" OR Keyword LIKE "airfrance" OR Keyword LIKE "[airfrance]"
                              GROUP BY `Publisher Name`
                              ORDER BY `Publisher Name` DESC')
metrics_by_brand$ROA_Brand <- metrics_by_brand$Total_Amount_Brand/metrics_by_brand$Total_Cost_Brand # this is ROA generating line
View(metrics_by_brand)

metrics_by_no_brand <- sqldf('SELECT `Publisher Name`, SUM(Amount) AS Total_Amount_No_Brand, SUM(`Total Cost`) AS Total_Cost_No_Brand,
                              SUM(`Total Volume of Bookings`) AS Volume_No_Brand, SUM(Impressions) AS Impressions_No_Brand
                          FROM air_df
                          WHERE Keyword NOT LIKE "air france" AND Keyword NOT LIKE "[air france]" AND Keyword NOT LIKE "airfrance" AND Keyword NOT LIKE "[airfrance]"
                          GROUP BY `Publisher Name`
                          ORDER BY `Publisher Name` DESC')
metrics_by_no_brand$ROA_No_Brand <- metrics_by_no_brand$Total_Amount_No_Brand/metrics_by_no_brand$Total_Cost_No_Brand # this is ROA generating line
View(metrics_by_no_brand)

Metrics <- metrics_by_publisher
Metrics[2:6] <- NULL
Metrics$Total_Amount <- metrics_by_publisher$Total_Amount
Metrics$Total_Amount_Brand <- metrics_by_brand$Total_Amount_Brand
Metrics$Total_Amount_No_Brand <- metrics_by_no_brand$Total_Amount_No_Brand

Metrics$Total_Cost <- metrics_by_publisher$Total_Cost
Metrics$Total_Cost_Brand <- metrics_by_brand$Total_Cost_Brand
Metrics$Total_Cost_No_Brand <- metrics_by_no_brand$Total_Cost_No_Brand

Metrics$Volume <- metrics_by_publisher$Volume
Metrics$Volume_Brand <- metrics_by_brand$Volume_Brand
Metrics$Volume_No_Brand <- metrics_by_no_brand$Volume_No_Brand

Metrics$ROA <- metrics_by_publisher$ROA
Metrics$ROA_Brand <- metrics_by_brand$ROA_Brand
Metrics$ROA_No_Brand <- metrics_by_no_brand$ROA_No_Brand


##########
# The final data frame with the metrics
##########
View(Metrics)
install.packages('xlsx')
library(xlsx)
write.csv(Metrics, "Desktop/R/Metrics.csv")


#######################################
#######################################
# BAR PLOT
#######################################
#######################################

# Create data
data<-data.frame(name=c("Yahoo - US","MSN - Global","MSN - US","Google Global","Overture - Global", "Google - US", "Overture - US" ) ,  
                value=c(19, 12, 11, 7.7, 6.7, 4.9, 2.5))
data$name <- factor(data$name, levels = data$name[order(data$value)])
 # changed order of factor levels

# Barplot

library(ggplot2)
ggplot(data, aes(x=name, y=value)) + geom_bar(stat = "identity", fill = "darkblue",alpha=0.8)+
  theme_minimal() + ylab("ROA")+ggtitle("Return on Advertizing") 


#Air France Branded
#Air France Brand & French Destinations
#Air France Global Campaign
#Unassigned

# Air france branded Campaign metrics
metrics_by_AFB <- sqldf('SELECT Campaign, Keyword, `Publisher Name`, Amount/`Total Cost` AS ROA,
                              `Total Volume of Bookings` AS Volume,  `Total Cost` as costs, Clicks, `Net Revenue`
                             FROM air_df
                              WHERE Campaign = "Air France Branded"
                             ORDER BY ROA DESC, `Total Volume of Bookings` DESC')

View(metrics_by_AFB)

#Revenue 
sum(metrics_by_AFB$`Net Revenue`)
sum(air_df$`Net Revenue`)
#%
sum(metrics_by_AFB$`Net Revenue`)/sum(air_df$`Net Revenue`)
sum()
f <- air_df$ROA[2:nrow(air_df)]
# ROA
mean(f)
mean(metrics_by_AFB$ROA)
# Volume
sum(metrics_by_AFB$Volume)
sum(air_df$`Total Volume of Bookings`)

# %
sum(metrics_by_AFB$Volume)/sum(air_df$`Total Volume of Bookings`)


#############################################################
# Word cloud for the dramatic effect during the presentation
#############################################################

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer")

install.packages("wordcloud")
library("wordcloud")


word_cloud = wordcloud(air_df$Keyword,air_df$Clicks, scale= c(100,20),min.freq = 10, max.words =Inf, random.order=TRUE, random.color = TRUE)
wordcloud(words = air_df$Keyword, freq = air_df$Clicks, min.freq = 10,
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=black.colors(12))

