read.csv("C:/Users/asus/Desktop/dbms project/data_set.csv")->games

View(games)

# before starting we must install some pakages, namely:
#"dsplyr","ISLR","ggplot2"
library(dplyr)
library(dplyr)
library(ISLR)
library(ggplot2)
library(caTools)



# these results are only relevent for the games that were in the top 419 ranks

# shows games published in which years have the highest sales along with a count of games that year
data <-tbl_df(games)
data %>%
  group_by(Year) %>%
  summarize(Global_Sales = sum(Global_Sales,na.rm = TRUE),
            count = n()) %>% 
  arrange(desc (Global_Sales))
#visualization
ggplot(games,aes(x=Global_Sales,fill=Year))+geom_histogram(alpha=0.4)
ggplot(games,aes(x=Global_Sales,y=Year))+geom_smooth(method = 'lm',se=F,span=10)


#shows which publisher has been most successful along with the count of games that publisher has made
data <-tbl_df(games)
data %>%
  group_by(Publisher) %>%
  summarize(Global_Sales = sum(Global_Sales,na.rm = TRUE),
            count = n()) %>% 
  arrange(desc (Global_Sales))
#visualization
ggplot(games,aes(x=Global_Sales,y=Publisher))+geom_smooth(method = 'lm',se=F,span=10)
ggplot(games,aes(x=Global_Sales,fill=Publisher))+geom_histogram(alpha=0.4)

#shows which platform has the most sales along with count of the games played on that platform
data <-tbl_df(games)
data %>%
  group_by(Platform) %>%
  summarize(Global_Sales = sum(Global_Sales,na.rm = TRUE),
            count = n()) %>% 
  arrange(desc (Global_Sales))
#visualization
ggplot(games,aes(x=Global_Sales,y=Platform))+geom_smooth(method = 'lm',se=F,span=10)
ggplot(games,aes(x=Global_Sales,fill=Platform))+geom_histogram(alpha=0.4)

#shows which genre of games have the highest sales along with count of games in that genre
data <-tbl_df(games)
data %>%
  group_by(Genre) %>%
  summarize(Global_Sales = sum(Global_Sales,na.rm = TRUE),
            count = n()) %>% 
  arrange(desc (Global_Sales))
#visualization
ggplot(games,aes(x=Global_Sales,fill=Genre))+geom_histogram(alpha=0.4)
ggplot(games,aes(x=Global_Sales,y=Genre))+geom_smooth(method = 'lm',se=F,span=10)



#prediction

#now we will split the dataset into training and testing sets
#splitRatio 0.7 indicates 70% dataset would we traing set and 30% would be testing set
sample.split(games$Rank,SplitRatio = 0.70) -> split_index
train<-subset(games,split_index==T)
test<-subset(games,split_index==F)
nrow(train)
nrow(test)

#now we build a regression model (mod1) 
#of Global sales of games with sales in Japan,North America and Europe
lm(Global_Sales~JP_Sales+NA_Sales+EU_Sales,data=train)->mod1
View(mod1)

#we deploy our testing set to be predicted by our regression model
predict(mod1,test)->result
cbind(actual=test$Global_Sales,predicted=result)->compare_result

View(compare_result)

as.data.frame(compare_result)->compare_result

#we substract the predicted value from the actual values to get error
compare_result$actual-compare_result$predicted->error
cbind(compare_result,error)->compare_result
View(compare_result)

#we calculate the RMS(root mean sqaure) value of error
sqrt(mean(compare_result$error^2))->rmsel
rmsel

#To see the summary of results of our prediction
summary(mod1)


