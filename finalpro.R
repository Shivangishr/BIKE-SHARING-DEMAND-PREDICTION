data<-read.csv("BIKE_FINAL.csv")
hist(data$count)
y <- (data$count)^2
y <- log(y)
hist(y)
boxplot(data$count)
summary(data$count)
cnt<-data$count[data$count<=647]
boxplot(cnt)
m<-mean(cnt)
h<-ceiling(m)
data$count<-ifelse(data$count>647,h,data$count)
boxplot(data$count)
summary(data$count)
cnt<-data$count[data$count<=560]
boxplot(jitter(cnt))
boxplot.stats(jitter(cnt))
data$count<-ifelse(data$count>570,h,data$count)
boxplot(jitter(data$count),horizontal = TRUE)
hist(data$count)
data$count<-log(data$count)
data$count<-ceiling(data$count)
hist(data$count)
for(i in 15:16)
{
  max<-max(data[,i])
  min<-min(data[,i])
  for(j in 1:nrow(data))
  {
    data[j,i]<-(data[j,i]-min)/(max-min)
  }
}
library(caTools)
set.seed(123)
split=sample.split(data$count,SplitRatio=0.8)
train = subset(data,split==TRUE)
test = subset(data,split==FALSE)
library(caret)
library(e1071)
train<-data
train$rain<-NULL
test$rain<-NULL
classifier<-svm(formula=count~temp+atemp+season1+season2+season3+season4+not.holiday+holiday+not.working+working+clear+mist+snow+wind+humd+time11+time12+time13+time14+time15+time16+time17,data=train,type='nu-regression',kernal='radial basis',gamma = 0.0391)
data1<-read.csv("ttest4.csv")
for(i in 1:4)
{
  max<-max(data1[,i])
  min<-min(data1[,i])
  for(j in 1:nrow(data1))
  {
    data1[j,i]<-(data1[j,i]-min)/(max-min)
  }
}

y_pred=predict(classifier,newdata=test[-23])
p<-exp(y_pred)
p<-ceiling(p)
data1$count<-cbind(data1,p)
#p1<-exp(p)-------------                
p<-ceiling(y_pred)
RMSE(p,test[,23])
hist(p)
write.csv(data1,"FINAL.csv")
