#***********************Set the directory path*****************************************#
setwd("C:\\Data Analytics Classes\\Statistics\\Stats project\\MultiRegression Model\\Final\\Final")
getwd()

#*************************import the files in r**************************************************#
crime_data = read.csv('Crimes\\crime_data.csv', header = TRUE)
edu_data = read.csv('Education\\edu_data.csv',header=TRUE)
pop_data = read.csv('Population\\pop_data1.csv', header= TRUE)
pov_data = read.csv("Poverty\\pov_data1.csv", header = TRUE)

View(crime_data)
View(edu_data)
View(pop_data)
View(pov_data)
unique(crime_data$GEO)
unique(pop_data$CITIES)
unique(edu_data$CITIES)
unique(pov_data$GEO)

#------------Subsetting the dataframe based on the year and countries---------------------

years <- c(2008)
countries <- c('Belgium','Norge','Bulgaria',
                'Sofia','Plovdiv','Varna','Burgas',
               'Pleven','Ruse','Vidin','Stara Zagora','Praha','Berlin',
               'Bremen','Tallinn','Madrid','Barcelona','Sevilla','Zaragoza','Murcia','Valladolid','Toledo','Badajoz',
               'Italy','Latvia','Riga','Greece','Sweden','Hamburg',
               #'Poland',
               'Lithuania','Hungary','Groningen','Austria','Wien','Salzburg','Slovenia','Slovakia',
               'Finland','Stockholm','Oslo')

#---------Finding countries in Poverty data which are common with the other three datasets in (countries vector)

pov_ct <- unique(pov_data$GEO)
pov_ct[! pov_ct %in% countries]
countries[!countries %in% pov_ct]
unique(pov_data[!unique(pov_data$GEO) %in% countries,]$GEO)
str(pov_data$Value)
pov_data$Value <- as.numeric(pov_data$Value)

res <- c()
for(ct in countries){
  #pov_data$GEO == ct
    tmp <- pov_data[pov_data$GEO != ct,]
    if(NROW(tmp) > 0){
      res <- c(res,paste(ct,sum(tmp$Value), sep="--"))
    }
}
res

#---------------------Merge data into one data frame-----------------------------------------
country_vec <- c()
year_vec <- c()
cr_rate <- c()
count = 1

for(yr in years){
  for(cnt in countries){
    tmp <- crime_data[crime_data$GEO==cnt & crime_data$TIME==yr & crime_data$Value!=':',]
    tmp$Value<-as.numeric(gsub(',','',tmp$Value))
    cr_rate <- c(cr_rate,mean(tmp$Value))
    country_vec[count] <- cnt
    year_vec[count] <- yr
    count= count+1
  }
}
NROW(cr_rate)
final_df <- data.frame(cbind(year_vec,country_vec,cr_rate))
final_df

edu_cities <- c()#finding the common records in the education city data and taking mean of the values
for(yr in years){
  for(cnt in countries){
    tmp <- edu_data[edu_data$CITIES==cnt & edu_data$TIME==yr & edu_data$Value!=':',]
    tmp$Value<-as.numeric(gsub(',','',tmp$Value))
    edu_cities<-c(edu_cities,mean(tmp$Value))
  }
}
NROW(edu_cities)
final_df <- cbind(final_df,edu_cities)
final_df

pop_cities <- c()
for(yr in years){
  for(cnt in countries){
    tmp<- pop_data[pop_data$CITIES==cnt &pop_data$TIME==yr & pop_data$Value != ':',]
    tmp$Value<- as.numeric(gsub(',','',tmp$Value))
    pop_cities<-c(pop_cities,mean(tmp$Value))
  }
}
NROW(pop_cities)
final_df <- cbind(final_df,pop_cities)
final_df

pov_ct <- c()
for(yr in years){
  for(cnt in countries){
    tmp<- pov_data[pov_data$GEO == cnt &pov_data$TIME == yr,]
    tmp$Value<-as.numeric(gsub(',','',tmp$Value))
    pov_ct<-c(pov_ct,mean(tmp$Value))
  }
}
NROW(pov_ct)
final_df<-cbind(final_df,pov_ct)
final_df$cr_rate<-as.numeric(paste(cr_rate))

str(final_df)
head(final_df)

#----------------------------------Copying the final df in a csv file--------------------------
write.csv(final_df,"C:\\Data Analytics Classes\\Statistics\\Stats project\\MultiRegression Model\\Final\\Final\\Final_data.csv", row.names=FALSE)

#----------------------------------removing scientific notation--------------------------------
options(scipen=999)

#--------------------------------Corelation Matrix (Pearson Test)------------------------------#
library(PerformanceAnalytics)
library(dplyr)
tmp_df <- data.frame(select(final_df,cr_rate,pov_ct,edu_cities,pop_cities))
str(tmp_df)
tmp_df
final_df
tmp_df
par(mfrow=c(1,1))
attach(tmp_df)
#-----------------Checking for the outliers in each vector-------------------
boxplot(pov_ct)
outvar <- boxplot(pov_ct)$out
outvar


plot(edu_cities,pov_ct)
abline(pov_ct,edu_cities)
plot(cr_rate,pov_ct)
abline(pov_ct,cr_rate)

head(tmp_df)
chart.Correlation(tmp_df,histogram = TRUE)

#************************************Applying log transformation******************************
tmp_df$cr_rate <- log(tmp_df$cr_rate)
tmp_df$edu_cities <- log(tmp_df$edu_cities)
tmp_df$pov_ct<-log(tmp_df$pov_ct)
tmp_df$pop_cities <- log(tmp_df$pop_cities)
chart.Correlation(tmp_df,histogram = TRUE)


#*****************************Apply Linear model on the data frame*****************************
lmfit1 = lm(cr_rate~edu_cities+pov_ct+pop_cities+I(pop_cities^2)+pop_cities:pov_ct, data = tmp_df)
summary(lmfit1)


#*****************************Interaction and subset selection*********************************
lmfit2 = lm(cr_rate~edu_cities*pop_cities*pov_ct*I(pop_cities^2),data=tmp_df)
summary(lmfit2)


lmfit3 = step(lmfit2)
summary(lmfit3)

lmfit4 = lm(formula = cr_rate ~ I(pop_cities^2), data = tmp_df)
summary(lmfit4)


#******************************Model diagnosis************************************************
par(mfrow=c(1,1))
plot(lmfit3)

