# 28/12/2020
# STMO - Final Project
# Sata KAMARA

rm(list=ls())

################################################################################
################################################################################
# PRE-PROCESSING: DATA CLEANSING
################################################################################
################################################################################

# Read the file

library(readxl)

data <- read_excel("NAMQ_10_LP_ULC.xlsx", sheet="Sheet 1")

# Convert the dataset

data = as.data.frame(data)
summary(data)

# Visualize the dataframe

library(visdat)

vis_dat(data)

# Clean the data

even_indexes <- seq(2,49,2)
indexes <- append(1,even_indexes)
df <- data[,indexes]

library(tidyr)

df <- df %>% drop_na()

vis_dat(df)

################################################################################
################################################################################
# ANALYZE OF THE DATASET
################################################################################
################################################################################

# Prepare the Covid dataset

covid_year <- df[22:25]
covid_year <- append(df[1],covid_year)

# Labor productivity per country per quarter

barplot(covid_year$`2019-Q4`,names.arg=covid_year$TIME,col='yellow',
        main="Fourth quarter of 2019",cex.names=0.8,las=2)
barplot(covid_year$`2020-Q1`,names.arg=covid_year$TIME,col='red',
        main="First quarter of 2020",cex.names=0.8,las=2)
barplot(covid_year$`2020-Q2`,names.arg=covid_year$TIME,col='blue',
        main="Second quarter of 2020",cex.names=0.8,las=2)
barplot(covid_year$`2020-Q3`,names.arg=covid_year$TIME,col='green',
        main="Third quarter of 2020",cex.names=0.8,las=2)

# Histograms of the labor productivity

hist(covid_year$`2019-Q4`)
hist(covid_year$`2020-Q1`)
hist(covid_year$`2020-Q2`)
hist(covid_year$`2020-Q3`)

# Histogram of the whole period

library(data.table)

avg_covid <- (covid_year$`2019-Q4`+covid_year$`2020-Q1`+
                covid_year$`2020-Q2`+covid_year$`2020-Q3`)/4
avg_covid <- as.list(avg_covid)
covid_year <- c(covid_year,transpose(avg_covid))
names(covid_year) <- c("Country","2019-Q4","2020-Q1","2020-Q2",
                       "2020-Q3","Period")
hist(covid_year$Period)

summary(covid_year$Period)
var(covid_year$Period)
quantile(covid_year$Period,0.80)
Mod<-function(x) #mode
  unique(x)[which.max(tabulate(match(x,unique(x))))]
Mod(covid_year$Period)

# Boxplots of each quarter

boxplot(covid_year$`2019-Q4`,covid_year$`2020-Q1`,covid_year$`2020-Q2`,
        covid_year$`2020-Q3`,main="Comparison of the last quarters of 2019/2020",
        names=c("Q4-2019","Q1-2020","Q2-2020","Q3-2020"),las=2,
        col=c('yellow','red','green','blue'))

# Stem-and-leaf diagrams

stem(covid_year$`2019-Q4`)
stem(covid_year$`2020-Q1`)
stem(covid_year$`2020-Q2`)
stem(covid_year$`2020-Q3`)

#Empirical cumulative distribution of the period

plot(ecdf(covid_year$Period),main=
       "Empirical cumulative distribution of the period")

################################################################################
################################################################################
# HYPOTHESIS TESTS FOR DISTRIBUTION
################################################################################
################################################################################

################################################################################
# STUDY CASE: SPAIN
################################################################################

#Extract the data for Spain

spain <- df[11,2:25]
spain <- transpose(spain)
spain <- spain$V1

# General information

plot(spain,main="Plot of the different labor productivity of Spain")

summary(spain)

hist(spain,main="Histogram of the Spanish labor productivity between 2015 and now")

#Distribution of the variable spain: t-Student

library(dgof)

n_spain <- (2*var(spain))/(var(spain)-1) #var(student)=n/(n-2)
test_spain <- rt(n_spain,n_spain)
ks.test(spain,test_spain)

################################################################################
# STUDY CASE: BULGARIA
################################################################################

#Extract the data for Bulgaria

bulgaria <- df[4,2:25]
bulgaria <- transpose(bulgaria)
bulgaria <- bulgaria$V1

# General information

plot(bulgaria,main="Plot of the different labor productivity of Bulgarian")

summary(bulgaria)

hist(bulgaria,main="Histogram of the Bulgarian labor productivity between 2015 and now")

#Distribution of the variable bulgaria

n_bulgaria <- length(bulgaria)
test_bulgaria <- rpois(n_bulgaria,mean(bulgaria))
ks.test(bulgaria,test_bulgaria)

################################################################################
# STUDY CASE: CROATIA
################################################################################

#Extract the data for Croatia

croatia <- df[13,2:25]
croatia <- transpose(croatia)
croatia <- croatia$V1

# General information

plot(croatia,main="Plot of the different labor productivity of Croatia")

summary(croatia)

hist(croatia,main="Histogram of the Croatian labor productivity between 2015 and now")

#Distribution of the variable croatia

test_croatia <- rpois(n=length(croatia),lambda=mean(croatia))
ks.test(croatia,test_croatia)

################################################################################
# STUDY CASE: ITALY
################################################################################

#Extract the data for Italy

italy <- df[14,2:25]
italy <- transpose(italy)
italy <- italy$V1

# General information

plot(italy,main="Plot of the different labor productivity of Italy")

summary(italy)

hist(italy,main="Histogram of the Italian labor productivity between 2015 and now")

#Distribution of the variable italy

n_italy <- (2*var(italy))/(var(italy)-1) #var(student)=n/(n-2)
test_italy <- rt(n_italy,n_italy)
ks.test(italy,test_italy)

################################################################################
# STUDY CASE: NETHERLANDS
################################################################################

#Extract the data for Netherlands

netherlands <- df[21,2:25]
netherlands <- transpose(netherlands)
netherlands <- netherlands$V1

# General information

plot(netherlands,main="Plot of the different labor productivity of Netherlands")

summary(netherlands)

hist(netherlands,main="Histogram of the Dutch labor productivity between 2015 and now")

#Distribution of the variable netherlands

n_netherlands <- length(netherlands)
test_netherlands <- runif(n_netherlands)
ks.test(netherlands,test_netherlands)

################################################################################
# STUDY CASE: FINLAND
################################################################################

#Extract the data for Finland

finland <- df[28,2:25]
finland <- transpose(finland)
finland <- finland$V1

# General information

plot(finland,main="Plot of the different labor productivity of Finland")

summary(finland)

hist(finland,main="Histogram of the Finnish labor productivity between 2015 and now")

#Distribution of the variable finland

n_finland = length(finland)
test_finland <- rgeom(n_finland,1/mean(finland))
ks.test(finland,test_finland)
test_finland <- rpois(n_finland,mean(finland))
ks.test(finland,test_finland)

################################################################################
# STUDY CASE: SWITZERLAND
################################################################################

#Extract the data for Switzerland

switzerland <- df[33,2:25]
switzerland <- transpose(switzerland)
switzerland <- switzerland$V1

# General information

plot(switzerland,main="Plot of the different labor productivity of Switzerland")

summary(switzerland)

hist(switzerland,main="Histogram of the Swiss labor productivity between 2015 and now")

#Distribution of the variable switzerland

n_switzerland <- length(switzerland)
test_switzerland <- rpois(n_switzerland,mean(switzerland))
ks.test(switzerland,test_switzerland)

################################################################################
# STUDY CASE: EU
################################################################################

#Extract the data for EU

eu <- df[1,2:25]
eu <- transpose(eu)
eu <- eu$V1

# General information

plot(eu,main="Plot of the different labor productivity of EU")

summary(eu)

hist(eu,main="Histogram of the labor productivity of EU between 2015 and now")

#Distribution of the variable eu

n_eu <- length(eu)
test_eu <- rpois(n_eu,mean(eu))
ks.test(eu,test_eu)

################################################################################
################################################################################
# ESTIMATION OF MOMENTS AND CONFIDENCE INTERVALS
################################################################################
################################################################################

################################################################################
# STUDY CASE: SWITZERLAND
################################################################################

# Estimator of the mean, the variance and the standard deviation

n=10000
vector_mean<-rep(0,n)
vector_var<-rep(0,n)
i<-0
while(i<=n){
  resample_swiss<-sample(switzerland,replace=TRUE)
  vector_mean[i]<-mean(resample_swiss)
  vector_var[i]<-var(resample_swiss)
  i<-i+1
}
mean(vector_mean)
mean(vector_var)
sqrt(mean(vector_var))

# Method 1: normality of vector_mean and vector_var

# Confidence interval of the mean

mean(vector_mean)-2.262*sqrt(var(vector_mean)/length(vector_mean))
mean(vector_mean)+2.262*sqrt(var(vector_mean)/length(vector_mean))

# Confidence interval of the variance and the standard deviation

low<-mean(vector_var)-2.262*sqrt(var(vector_var)/length(vector_var))
up<-mean(vector_var)+2.262*sqrt(var(vector_var)/length(vector_var))
low
up
sqrt(low)
sqrt(up)

# Method 2: Bootstrapping

resample_boot <- sample(switzerland,replace=TRUE)

library(boot)

boot_mean <- function(switzerland, resample_boot){
  mean(switzerland[resample_boot])
}
boot_sd<-function(switzerland, resample_boot){
  sd(switzerland[resample_boot])
}

mean_results <- boot(switzerland,boot_mean,R=2000)
sd_results<-boot(switzerland,boot_sd,R=2000)

boot.ci(boot.out=mean_results,type="all")
boot.ci(boot.out=sd_results,type="all")

################################################################################
################################################################################
# REGRESSION MODELLING
################################################################################
################################################################################

################################################################################
# STUDY CASE: SWITZERLAND
################################################################################

# Creation of the dataframe with the quarters

Q1 <- c(switzerland[2],switzerland[6],switzerland[10],switzerland[14],
        switzerland[18])
Q2 <- c(switzerland[3],switzerland[7],switzerland[11],switzerland[15],
        switzerland[19])
Q3 <- c(switzerland[4],switzerland[8],switzerland[12],switzerland[16],
        switzerland[20])
Q4 <- c(switzerland[5],switzerland[9],switzerland[13],switzerland[17],
        switzerland[21])

df_swiss <- data.frame(Q1,Q2,Q3,Q4)

library(corrplot)

cor_swiss <- cor(df_swiss)
corrplot(cor_swiss)

# Construction of the linear model

lm_swiss.fit=lm(Q4[1:4]~Q1[1:4]+Q2[1:4]+Q3[1:4],data=df_swiss)

summary(lm_swiss.fit)

plot(x=0.4216*Q1[1:4]-0.3265*Q2[1:4]+1.0244*Q3[1:4]-8.9627,xlab='Number of year',
     ylab='Labor productivity',
     main='Comparison of linear model and actual fourth quarter',
     col='red',pch=8)
points(x=Q4,col='blue',axes=F,pch=19,add=T)

# Prediction interval

model_swiss=lm(Q4~Q1+Q2+Q3,data=df_swiss)
new.data = data.frame(Q1=switzerland[22],Q2=switzerland[23],Q3=switzerland[24])
predict(model_swiss,newdata=new.data,interval='prediction')

# Prediction of the value of Q4-2020

pred_swiss <- 0.4216*switzerland[22]-0.3265*switzerland[23]+
  1.0244*switzerland[24]-8.9627
pred_swiss #106.6638

# Computation of the errors

RSS_swiss <- c(crossprod(lm_swiss.fit$residuals))
RSS_swiss
MSE_swiss <- RSS_swiss/length(lm_swiss.fit$residuals)
MSE_swiss
RMSE_swiss <- sqrt(MSE_swiss)
RMSE_swiss

################################################################################
# STUDY CASE: EUROPE
################################################################################

Q_1 <- c(df[3:33,3],df[3:33,7],df[3:33,11],df[3:33,15],df[3:33,19])
Q_2 <- c(df[3:33,4],df[3:33,8],df[3:33,12],df[3:33,16],df[3:33,20])
Q_3 <- c(df[3:33,5],df[3:33,9],df[3:33,13],df[3:33,17],df[3:33,21])
Q_4 <- c(df[3:33,6],df[3:33,10],df[3:33,14],df[3:33,18],df[3:33,22])

df_europe <- data.frame(Q_1,Q_2,Q_3,Q_4)

cor_europe <- cor(df_europe)
corrplot(cor_europe)

# Construction of the linear model

0.8*155 # Taking 80% of the values for training
lm_europe.fit=lm(Q_4[1:124]~Q_1[1:124]+Q_2[1:124]+Q_3[1:124])

summary(lm_europe.fit)

plot(x=-0.30049*Q_1+0.80748*Q_2+0.65412*Q_3-14.11516,xlab='Number of quarter',
     ylab='Labor productivity',
     main='Comparison of linear model and actual fourth quarter',
     col='red',pch=8)
points(x=Q_4,col='blue',pch=19)

# Prediction interval

model_europe=lm(Q_4~Q_1+Q_2+Q_3,data=df_europe)
new.data2 = data.frame(Q_1=df[33,23],Q_2=df[33,24],Q_3=df[33,25])
predict(model_europe,newdata=new.data2,interval='prediction')

# Prediction of the value of Q4-2020

pred_europe <- -0.30049*df[33,23]+0.80748*df[33,24]+0.65412*df[33,25]-14.11516
pred_europe #101.1693

# Computation of the errors

RSS_eu <- c(crossprod(lm_europe.fit$residuals))
RSS_eu
MSE_eu <- RSS_eu/length(lm_europe.fit$residuals)
MSE_eu
RMSE_eu <- sqrt(MSE_eu)
RMSE_eu

################################################################################
################################################################################
# CLASSIFICATION MODELLING
################################################################################
################################################################################

# Computation of the average 

avg_europe <- (df_europe$Q_1+df_europe$Q_2+df_europe$Q_3+df_europe$Q_4)/4
avg_europe <- as.list(avg_europe)
df_europe <- c(df_europe,transpose(avg_europe))
names(df_europe) <- c("Q_1","Q_2","Q_3","Q_4","AVG")

summary(df_europe$AVG) #Median = 105.08

# Two classes of level of labor productivity

library(dplyr)

df_europe <- data.frame(df_europe) %>% mutate(LP=ifelse(
  df_europe$AVG<= mean(df_europe$AVG),"Low","High"))

# Split train and test sets

train <- df_europe[1:124,]
test <- df_europe[125:155,]

# Logistic regression for classification

glm.fit=glm(train$AVG~train$Q_1+train$Q_2+train$Q_3+train$Q_4)

summary(glm.fit)
summary(glm.fit)$coef

# Prediction

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]

glm.pred=rep("Low",156)
glm.pred[glm.probs>mean(df_europe$AVG)]="High"
glm.pred[glm.probs<=mean(df_europe$AVG)]="Low"

# Confusion matrix

table(glm.pred[1:155],df_europe$LP)

# Computation of the errors

RSS_class <- c(crossprod(glm.fit$residuals))
RSS_class
MSE_class <- RSS_class/length(glm.fit$residuals)
MSE_class
RMSE_class <- sqrt(MSE_class)
RMSE_class

