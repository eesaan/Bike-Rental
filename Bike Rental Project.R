# Removing all the prior stored objects:
rm(list=ls())

# Setting the working directory:
setwd("W:/DataScience/edWisor/Project")

# Importing Libraries:
  
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)

# Loading the data:
df = read.csv("day.csv", sep=",", header = TRUE)


# Checking the dataframe head:
head(df)


# **** DATA PRE PROCESSING **** #

# Checking the strucutre & datatype information of data:
str(df)


# *Missing Value Analysis*

data.frame(apply(df,2,function(x){sum(is.na(x))}))

# No missing value in the data. All records are complete.

# Dropping the unnecessary columns:
  
df = select(df,-c(instant,dteday,casual,registered))

# List of categorical & numerical columns:

cat_var = c("season","yr","mnth","holiday","weekday","workingday","weathersit")

num_var = c("temp","atemp","hum","windspeed","cnt")

# Converting variables in the proper datatype:
df =  df %>% mutate_at(.vars = cat_var,.funs = as.factor)
str(df)



#*Visualisations for exploring relationships b/w the variables*
options(scipen=999)

# Barplot of Mean Count v/s Season:

ggplot(data=df, aes(x= season, y=cnt,fill=season))+ stat_summary(fun.y = mean, geom = "bar")
+ ggtitle("Mean Count v/s Season") + 
  scale_fill_hue('Season', breaks = levels(df$season),labels=c('spring', 'summer', 'fall', 'winter')) + 
  labs(y="Mean Count")


# Barplot of Mean Count v/s Weather Situation:

ggplot(data=df, aes(x= weathersit, y=cnt, fill= weathersit)) + stat_summary(fun.y = mean, geom = "bar") + 
  ggtitle("Mean Count v/s Weather Situation") + 
  scale_fill_hue('weathersit', breaks = levels(df$weathersit),labels=c("Clear","Cloudy","Rain"))  + 
  labs(y="Mean Count")



# Barplot of Mean Count v/s Holiday:

ggplot(data=df, aes(x= holiday, y=cnt, fill=holiday)) + stat_summary(fun.y = mean, geom = "bar") + 
  ggtitle("Mean Count v/s Holiday") + 
  scale_fill_hue('Holiday Status', breaks = levels(df$holiday),labels=c("No Holiday", "Holiday"))  + 
  labs(y="Mean Count")

# Barplot of Mean Count v/s Weekday:

ggplot(data=df, aes(x= weekday, y=cnt, fill=weekday)) + 
  stat_summary(fun.y = mean, geom = "bar") + ggtitle("Mean Count v/s Day of the Week") + 
  scale_fill_hue("Day of the Week", breaks = levels(df$weekday),labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")) +
  labs(y="Mean Count")

# Barplot of Mean Count v/s Working Day:

ggplot(data=df, aes(x= workingday, y=cnt, fill=workingday)) + stat_summary(fun.y = mean, geom = "bar") + 
  ggtitle("Mean Count v/s Working Day") + 
  scale_fill_hue('Day Type', breaks = levels(df$workingday),labels=c("Not Working","Working")) + 
  labs(y="Mean Count")


# Barplot of Mean Count v/s Month:

ggplot(data=df, aes(x= mnth, y=cnt,fill=mnth)) + stat_summary(fun.y = mean, geom = "bar") + 
  ggtitle("Mean Count v/s Month") + 
  scale_fill_hue('Months', breaks = levels(df$mnth),labels=c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec"))+ labs(y="Mean Count")

# Barplot of Mean Count v/s Year:

ggplot(data=df, aes(x= yr, y=cnt, fill=yr)) + stat_summary(fun.y = mean, geom = "bar") + 
  ggtitle("Mean Count v/s Year")+scale_fill_hue('Year', breaks = levels(df$yr),labels=c("2011","2012"))  + 
  labs(x="Year",y="Mean Count")

# Distribution of Target variable, "cnt":

ggplot(data= df, aes(x= cnt)) + geom_histogram(bins = 30,colour="orange") + ggtitle("Total Rental Count Distribution")
+ labs(y="")

# Scatterplots of Numerical Variables:
  

s1 = ggplot(data=df, aes(x= temp, y=cnt)) + geom_point(shape = 21, colour = "black", fill = "white", size= 2, stroke= 0.5) + ggtitle("Rental Count v/s Temperature") + labs(x="Normalised Temperature",y="Count")

s2 = ggplot(data=df, aes(x= windspeed, y=cnt)) + geom_point(shape = 21, colour = "black", fill = "white", size= 2, stroke= 0.5)+ ggtitle("Rental Count v/s Windspeed") + labs(x="Normalised Windspeed",y="Count")

s3 = ggplot(data=df, aes(x= hum, y=cnt)) + geom_point(shape = 21, colour = "black", fill = "white", size= 2, stroke= 0.5)+ ggtitle("Rental Count v/s Humidity") + labs(x="Normalised Humidity",y="Count")

s4 = ggplot(data=df, aes(x= atemp, y=cnt)) + geom_point(shape = 21, colour = "black", fill = "white", size= 2, stroke= 0.5) + ggtitle("Rental Count v/s Felt Temperature") + labs(x="Normalised Felt Temperature",y="Count")

grid.arrange(s1, s2, s3, s4, nrow = 2,ncol=2)


#*Outlier Analysis*
  
#Boxplot of numerical variables:


box1 = ggplot(data = df, aes(y = temp)) + geom_boxplot() + theme_classic() + ggtitle("Temp.")
box2 = ggplot(data = df, aes(y = atemp)) + geom_boxplot() + theme_classic() + ggtitle("Felt Temp.")
box3 = ggplot(data = df, aes(y = hum)) + geom_boxplot() + theme_classic() + ggtitle("Humidity")
box4 = ggplot(data = df, aes(y = windspeed)) + geom_boxplot() + theme_classic() + ggtitle("Windspeed")
box5 = ggplot(data = df, aes(y = cnt)) + geom_boxplot() +theme_classic() + ggtitle("Count")

grid.arrange(box1,box2,box3,box4,box5, nrow=1)


# Removing Outliers using boxplot method:

for(i in num_var)
{
  print(i)
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df = df[which(!df[,i] %in% val),]
} 
  
# *Feature Selection*
  
# Generating  a correlation matrix:

corr_mat = cor(select(df, num_var))
ggcorrplot(corr_mat,title = "Correlation Heatmap",type = "upper",ggtheme = theme_classic(),lab=TRUE)

  
  
# Dropping the column "atemp" from the dataset:

df = select(df,-c("atemp"))
  
  
# ANOVA Test on "weekday" variable:

anov_week = aov(cnt ~ weekday, data = df)
summary(anov_week)
  
# ANOVA Test on "workingday" variable:

anov_work = aov(cnt ~ workingday, data = df)
summary(anov_work)
  
  
# Dropping the "weekday" & "workingday" variables because, the p-value is very high, F-statistic is low:

df = select(df, -c("weekday","workingday"))
  
  
# *** MODEL DEVELOPMENT ***

  
# Splitting the dataset into training & testing set:

library(caTools)
set.seed(123)
split = sample.split(df$cnt, SplitRatio = 0.25)
df_train = subset(df, split== TRUE)
df_test = subset(df, split== FALSE)
  
  
# Loading Error Metrics package:
library(Metrics)
  
  
  
# Linear Regression Model:
    
# Fitting the model to the training set:

lin_reg = lm(formula= cnt ~., data= df_train)
  
# Predciting the values for the test set:

pred_lm = predict(lin_reg, newdata = df_test)
  
# Mean Absolute Error:
  
cat("Linear Regression\n",paste("Mean Absolute Error:", mae(actual = df_test$cnt, predicted = pred_lm)))
  
  
# Decision Tree Regression Model:
library(rpart)
  
# Fitting the model to the training set:

d_tree = rpart(formula= cnt ~., data= df_train)
  
# Predicting the values for the test set:

pred_dt = predict(d_tree, df_test)
  
# Mean Absolute Error:

cat("Decision Tree Regression\n",paste("Mean Absolute Error:", mae(actual = df_test$cnt, predicted = pred_dt)))
  
  
# Random Forest Regression Model:
    
library(randomForest)
# Elbow method to find the optimal value of "ntree":
  
error= vector()
k= seq(10,300,by=10)
for(i in k)
  {
    rf_elbow = randomForest(formula= cnt ~., data= df_train, ntree= i)
    pred_elbow = predict(rf_elbow, newdata =  df_test)
    err = mae(actual= df_test$cnt, predicted = pred_elbow)
    error = c(error,err)
  }
  plot(x= k,y= error,"b")
  
  
# Getting the Optimum value for ntree parameter:
  for (i in 1:length(k))
  {
    if(error[i]==min(error))
    {
      opt = k[i]
    }
  }   
  
# Fitting the model to the training set:
  
rf = randomForest(formula= cnt~., data= df_train, ntree = opt)
  
# Predicting the values for the test set:
  
pred_rf = predict(rf, df_test)
  
# Mean Absolute Error:

cat("Random Forest Regression\n",paste("Mean Absolute Error:", mae(actual = df_test$cnt, predicted = pred_rf)))
  
  
  