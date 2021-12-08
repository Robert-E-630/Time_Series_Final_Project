################################################################################
###Final Project - Forecasting Chicago Crime Rates
################################################################################
# 1.EDA 
  #- For Whole City - Avg Crime Rate by
      #- Week Day
      #Hour
      #Month
      #Year
  # Same Analysis for Bronzeville and West Loop

#2. Single Season Auto Arima
  #Hourly, Weekday, Month
  #agg to Daily Level and fine Seasnality at 7 * 12 = 84 Frequency

#3. Multiple Season Auto Arima

#4. Add Covariates
  # CTA Ridership
  # Temperature by area
  # if modeling single district, have crime rate at neighborhour districts
  # Graffilte Role

#5. Granger Causality

#6 LSTMs and DeepAR


################################################################################
###Load Libraries and Data Sets
################################################################################

dattaPath <- "C:/Users/Rober/Desktop/msca_31006_time_series/Final_Project"

df_crime <- read.csv((file.path(dattaPath, 'Crimes_2001_to_Present.csv')))
df_crime_2 <- df_crime[,c("Date","Primary.Type")]

#remove old data series
rm(df_crime)


################################################################################
###EDA for entire CITY
################################################################################

library(ggplot2)
library(plotly)
library(dplyr)

library(viridisLite)
library(viridis)

Plot1 <- df_crime_2 %>%
  group_by(Primary.Type) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x = count, y = reorder(Primary.Type, count), text = paste("Crime: ", Primary.Type, "<br>",
                                                                       "Happened: ", count, " times"))) +
  geom_col(aes(fill = Primary.Type), show.legend = F) + 
  scale_fill_viridis(option = "D", discrete = T, direction = 1, begin = 0.9, end = 0.3) +
  labs(title = "Top 10 Crime Categories in Chicago", x = "Number of Crime", y = "Crime") +
  theme_gray()


ggplotly(Plot1, tooltip = "text")

################################################################################
###EDA for Seasonality
################################################################################

library(TSA)
library(xts)
library(forecast)

#R - How to calculate average of a variable by hour in R
#https://stackoverflow.com/questions/24645628/how-to-calculate-average-of-a-variable-by-hour-in-r

#at Each time stamp / row, a single crime occurred.
df_crime_2$Ct<-1
df_crime_3<-df_crime_2[,c("Date","Ct")]



min(df_crime_3$Date)
#"01/01/2001 01:00:00 AM"

max(df_crime_3$Date)
#"12/31/2020 12:55:00 AM"


#R - date sequence by hour
#https://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r/10887924

hourly_daily_dates <- seq(as.POSIXct("2001-01-01 00:00:00"), 
                          as.POSIXct("2020-12-31 23:59:59"), by="hour")

min(hourly_daily_dates)
#"2001-01-01 CST"
#"2001-01-01 00:00:00 CST"

max(hourly_daily_dates)
#"2020-12-31 23:00:00 CST"


#doesnt like 2:00pm

df_crime_3[index(df_crime_3)==2625402,]
df_crime_3[index(df_crime_3)==111,]
df_crime_3[index(df_crime_3)==7027268,]
df_crime_3[index(df_crime_3)==441482,]
df_crime_3[index(df_crime_3)==4304126,]
df_crime_3[index(df_crime_3)==4304127,]
df_crime_3[index(df_crime_3)==4304128,]

########################################################################################
## Final Hourly Data Frame is df_hourly_NAs
#xts hourly wrapper
##https://stackoverflow.com/questions/16019187/why-is-there-no-apply-hourly-in-r-with-xts-zoo
#Merge with hourly sequence needed in case there exist some hours with no Crime
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
#One issue is what happens at transitions to and from DST
#https://rdrr.io/r/base/as.POSIXlt.html
########################################################################################



hourly_imputed_sum <- function(dff) {
  
  
  dff$xct_Date <- as.POSIXct(dff$Date, format = "%m/%d/%Y %I:%M:%OS %p")
  dff_2<-dff[!is.na(dff$xct_Date),]
  df_crime_xts<-xts(dff_2$Ct, order.by = dff_2$xct_Date)
  hourly_daily_dates <- seq(as.POSIXct("2001-01-01 00:00:00"), 
                            as.POSIXct("2020-12-31 23:59:59"), by="hour")
  
  
  xts2 <- xts(x=rep(NaN, times=length(hourly_daily_dates)), order.by=hourly_daily_dates)
  xts2 <- xts(x=rep(NaN, times=length(hourly_daily_dates)), order.by=hourly_daily_dates)
  
  df_merged_hourly_daily<-merge(xts2,df_crime_xts)
  df_merged_hourly_daily$xts2<-NULL

  #hourly wrapper
  df_hourly_NAs <- period.apply(df_merged_hourly_daily, endpoints(df_merged_hourly_daily, "hours"),
                                sum, na.rm=TRUE)

  
  df_hourly_fin<-na.approx(df_hourly_NAs)
  return(df_hourly_fin)
}




dfff_testtt<-df_crime_2[,c("Date","Ct")]
dfff_testtt

a <-hourly_imputed_sum(dfff_testtt)

head(a)

plot(a)

plot.xts(a, main='All Chicago - Hourly Crime', 
         screens = factor(1, 1), auto.legend = TRUE)

#Plot March 2005
plot.xts(a['2005-03'])

#Plot March 3rd 2005
plot.xts(a['2005-03-03'])


########################################################################
## Looking at Hourly Data between Dec 2019 to Jan 2020
## Appear to be Anomalies during New Years
########################################################################

#Plot December 2019 through Jan 2020
plot.xts(a['2019-12/2020-01'])


#Plot December 20th 2019 through Jan 5th 2020
plot.xts(a['2019-12-20/2020-01-05'], 
         main='All Chicago - Hourly Crime', 
         screens = factor(1, 1), auto.legend = TRUE)


#############################################################################
### Aggregated Crime Rate to Day level shows a Decreasing Trend
#############################################################################
head(a)

a_daily <-apply.daily(a,sum)
plot.xts(a_daily, main='All Chicago - Daily Crime', 
         screens = factor(1, 1), auto.legend = TRUE)
head(a_daily)

write.csv(x=a_daily, file="myFileName")


#Save daily crime rate xts object to csv file
df_daily<-as.data.frame(a_daily)

df_daily$Dtss<-rownames(df_daily)
head(df_daily)

write.csv(df_daily,'chicago_daily_crime.csv', row.names = FALSE)

#############################################################################
### Aggregated Crime Rate to Monthly level shows a Decreasing Trend
#############################################################################

a_monthly <-apply.monthly(a,sum)
plot.xts(a_monthly, main='All Chicago - Monthly Crime', 
         screens = factor(1, 1), auto.legend = TRUE)
head(a_monthly)

#############################################################################
### Aggregated Crime Rate to Yearly level shows a Decreasing Trend
#############################################################################

a_yearly <-apply.yearly(a,sum)
plot.xts(a_yearly, main='All Chicago - Yearly Crime', 
         screens = factor(1, 1), auto.legend = TRUE)
head(a_yearly)


########################################################################
## EDA Bar Plots - For Whole City - Avg Crime Rate by Month
########################################################################

#https://stackoverflow.com/questions/62481050/average-of-months-data-jan-dec-in-xts-objects
#to find average for each month, first aggregate to month level, then use below


avg_monthly <- do.call(rbind,lapply(split(a_monthly, base::months(index(a_monthly))), mean, na.rm=TRUE))
df_avg_monthly <- as.data.frame(avg_monthly[match(month.name, rownames(avg_monthly)),] )

df_avg_monthly$monthss <- rownames(df_avg_monthly)
rownames(df_avg_monthly) <- 1:nrow(df_avg_monthly)
colnames(df_avg_monthly) <- c("Avg_Ct", "monthss")

head(df_avg_monthly)

mts<-df_avg_monthly$monthss


#Stark Monthly Seasonality - Higher in June / August / September
#Lower in Winter Months.
#Up tick in January possibly due to New Years Eve

cccc<-ggplot(df_avg_monthly, aes(x=factor(monthss, levels = mts), y=Avg_Ct)) + 
  geom_col(fill = "lightblue", colour = "black", width=1) +
  geom_text(aes(label = sprintf("%0.0f", round(Avg_Ct, digits = 2))), vjust=1.5, colour= "white", size=3)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) + 
  xlab("Month")

show(cccc)


########################################################################
## EDA Bar Plots - For Whole City - Avg Crime Rate by Weekday
########################################################################

#https://stackoverflow.com/questions/62481050/average-of-months-data-jan-dec-in-xts-objects
#to find average for each month, first aggregate to month level, then use below


avg_weekday <- do.call(rbind, lapply(split(a_daily, base::weekdays(index(a_daily))), mean, na.rm=TRUE))
dff_avg_weekday<-as.data.frame(avg_weekday)


dff_avg_weekday$wds <- rownames(dff_avg_weekday)

rownames(dff_avg_weekday) <- 1:nrow(dff_avg_weekday)
colnames(dff_avg_weekday) <- c('Count', 'Day')

head(dff_avg_weekday)

#https://stackoverflow.com/questions/16193549/how-can-i-create-a-vector-containing-the-days-of-the-week/16193697
days.of.week <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))



bbbb<-ggplot(dff_avg_weekday, aes(x=factor(Day, levels = days.of.week), y=Count)) + 
  geom_col(fill = "lightblue", colour = "black", width=1) +
  #geom_text(aes(label = sprintf("%0.0f", round(Day, digits = 2))), vjust=1.5, colour= "white", size=3)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  xlab("Day of the Week")

show(bbbb)


#Week Day Average does not show signs of Reporting Lags on Mondays. 
#Some Seasonality, but not as great as the Monthly nor 

bbbb<-ggplot(dff_avg_weekday, aes(x=factor(Day, levels = days.of.week), y=Count)) +
  geom_col(fill = "lightblue", colour = "black", width=1) +
  geom_text(aes(label = sprintf("%0.0f", round(dff_avg_weekday$Count, digits = 2))), vjust=1.5, colour= "white", size=3)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  xlab("Day of the Week")

show(bbbb)



########################################################################
## EDA Bar Plots - For Whole City - Avg Crime Rate by Hour
########################################################################
  #convert xts to data frame
  # Create new Column in DF that's the Time Stamp found in row names
  # 

df_aa <- as.data.frame(a)
df_aa$hr_time <-rownames(df_aa)
df_aa$hhour <- format(as.POSIXct(df_aa$hr_time,format="%Y-%m-%d %H:%M:%S"),"%H")

df_bb <- df_aa[,c("hhour","df_crime_xts")]
rownames(df_bb) <- 1:nrow(df_bb)
avg_hourly <- aggregate(df_bb[,2], list(df_bb$hhour), mean)
colnames(avg_hourly) <- c('hr','ct')



#https://stackoverflow.com/questions/29334075/why-is-the-plot-generated-from-ggplot-not-showing-up
aaa<-ggplot(avg_hourly, aes(x=factor(hr), y=ct)) + 
  geom_col(fill = "lightblue", colour = "black", width=1) +
  #geom_text(aes(label = ct), vjust=1.5, colour= "white")+
  xlab("Hour of the Day")

show(aaa)


########################################################################
## Auto Arima Forecasting at Hourly Level of granualarity
## Without Seasonality pretty poor performance
########################################################################



plot(a)
acf(a)
ggtsdisplay(a)

#Plot Oct 2021 through Nov 2021
plot.xts(a['2021-10/2021-11'],main='All Chicago - Hourly Crime', 
         screens = factor(1, 1), auto.legend = TRUE)

#Plot Nov 2021
plot.xts(a['2021-11'],main='All Chicago - Hourly Crime', 
         screens = factor(1, 1), auto.legend = TRUE)

#Plot last 10 days of Nov 2021
plot.xts(a['2021-11-17/2021-11-27'],main='All Chicago - Hourly Crime', 
         screens = factor(1, 1), auto.legend = TRUE)

#Create a Data Frame for 2021

a_2021_11<-a['2021-11']

plot(a_2021_11)

length(a_2021_11)

train_mod <- auto.arima(a_2021_11, stepwise = FALSE,
                        approximation = FALSE, allowdrift = TRUE,
                        allowmean = TRUE)
summary(train_mod)

plot(forecast(train_mod, h=12))





########################################################################
## Single Season - Auto Arima Forecasting - Hourly - Nov 2021
########################################################################


#Converting xts object to TS object in order to explicitly assign frequency

head(a_2021_11)

ts_train_df <- ts(coredata(a_2021_11), frequency=24)

Hourly_model<-auto.arima(ts_train_df, D=1, stepwise = FALSE, approximation = FALSE)
summary(Hourly_model)

# Series: ts_train_df
# ARIMA(2,0,0)(0,1,1)[24]
#
# Coefficients:
#   ar1     ar2     sma1
# 0.1562  0.1049  -0.7841
# s.e.  0.0399  0.0399   0.0318
#
# sigma^2 estimated as 39.28:  log likelihood=-2043.9
# AIC=4095.81   AICc=4095.87   BIC=4113.56
#
# Training set error measures:
#   ME     RMSE      MAE      MPE     MAPE      MASE        ACF1
# Training set -0.4647598 6.135617 4.734344 -11.4187 26.43497 0.7624233 -0.01736903

plot(forecast(Hourly_model, h=48))

#allow mean and drift

Hourly_model_2<-auto.arima(ts_train_df, D=1, stepwise = FALSE,
                           approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)

summary(Hourly_model_2)

# ARIMA(2,0,0)(0,1,1)[24]
#
# Coefficients:
#   ar1     ar2     sma1
# 0.1562  0.1049  -0.7841
# s.e.  0.0399  0.0399   0.0318
#
# sigma^2 estimated as 39.28:  log likelihood=-2043.9
# AIC=4095.81   AICc=4095.87   BIC=4113.56
#
# Training set error measures:
#   ME     RMSE      MAE      MPE     MAPE      MASE        ACF1
# Training set -0.4647598 6.135617 4.734344 -11.4187 26.43497 0.7624233 -0.01736903

########################################################################
## Single Season - Auto Arima Forecasting - Daily Sum - Annual Seaonality
########################################################################

#https://stackoverflow.com/questions/18220034/auto-arima-not-parallelizing
#https://stats.stackexchange.com/questions/179144/r-parallelising-auto-arima

# a_daily_2021 <-apply.daily(a['2021'],sum)
# plot.xts(a_daily_2021, main='All Chicago - Daily Crime', 
#          screens = factor(1, 1), auto.legend = TRUE)

head(a_daily)

a_daily <-apply.daily(a,sum)
plot.xts(a_daily, main='All Chicago - Daily Crime',
         screens = factor(1, 1), auto.legend = TRUE)

ts_train_df_2 <- ts(coredata(a_daily), frequency=365)

daily_model_annual<-auto.arima(ts_train_df_2, D=1, stepwise = FALSE, 
                           approximation = FALSE, parallel=TRUE, num.cores=8, allowdrift = TRUE, allowmean = TRUE)

summary(daily_model_annual)


# ARIMA(4,1,1)(0,1,0)[365] 
# 
# Coefficients:
#   ar1     ar2     ar3     ar4      ma1
# 0.2055  0.0526  0.0909  0.0747  -0.9801
# s.e.  0.0123  0.0123  0.0123  0.0122   0.0035
# 
# sigma^2 estimated as 11265:  log likelihood=-44230.34
# AIC=88472.68   AICc=88472.7   BIC=88514.03
# 
# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set -0.03272973 103.5259 73.79026 -0.5757504 8.125349 0.7898774 0.002954293

plot(forecast(daily_model_annual, h=100))

############################################################################################
## November 2021 - Single Season - Auto Arima Forecasting - Daily Sum - Annual seasonality
############################################################################################


train_xts<-a_daily[index(a_daily) <"2021-11-01"]
test_xts<-a_daily[index(a_daily) >="2021-11-01"]

ts_train_Nov <- ts(coredata(train_xts), frequency=365)
ts_test_Nov <- ts(coredata(test_xts), frequency=365)


daily_nov_mode<-auto.arima(ts_train_Nov, D=1, stepwise = FALSE, 
                               approximation = FALSE, parallel=TRUE, num.cores=8, allowdrift = TRUE, allowmean = TRUE)

summary(daily_nov_mode)

# Series: ts_train_Nov 
# ARIMA(4,1,1)(0,1,0)[365] 
# 
# Coefficients:
#   ar1     ar2     ar3     ar4      ma1
# 0.2057  0.0521  0.0908  0.0747  -0.9800
# s.e.  0.0123  0.0123  0.0123  0.0122   0.0035
# 
# sigma^2 estimated as 11294:  log likelihood=-44075.53
# AIC=88163.06   AICc=88163.07   BIC=88204.39
# 
# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set 0.008904043 103.6517 73.88451 -0.5687305 8.121778 0.7894068 0.003001251


##########################################################################################
#Residual Diagnostics
##########################################################################################


res <- residuals(daily_nov_mode)
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from ARIMA(4,1,1)(0,1,0)[365]")


gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

checkresiduals(daily_nov_mode)

##########################################################################################
#Forecasting November 2021
##########################################################################################
plot(forecast(daily_nov_mode, h=27))

#Create xts object with predicted November Dates

new_dates <- seq(as.Date(end(train_xts)), by="day", length.out=27)

#27 - length(new_dates)
pred_xts <- xts(forecast(daily_nov_mode, h=27)$mean, new_dates)





## Create Data Frames from Predicted and Test xts objects
df_nov_test_xts<-as.data.frame(coredata(test_xts))
df_nov_pred_xts<-as.data.frame(coredata(pred_xts))

names(df_nov_pred_xts)[names(df_nov_pred_xts) == "V1"] <- "Pred_Ct"
names(df_nov_test_xts)[names(df_nov_test_xts) == "df_crime_xts"] <- "Test_Ct"


df_nov_pred_xts$idx<-rownames(df_nov_pred_xts)
df_nov_test_xts$idx<-rownames(df_nov_test_xts)






# bbbb<-ggplot(dff_avg_weekday, aes(x=factor(Day, levels = days.of.week), y=Count)) + 
#   geom_col(fill = "lightblue", colour = "black", width=1) +
#   geom_text(aes(label = sprintf("%0.0f", round(Day, digits = 2))), vjust=1.5, colour= "white", size=3)+
#   theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
#   xlab("Day of the Week")
# 
# show(bbbb)


#melting into long form
#https://stackoverflow.com/questions/9531904/plot-multiple-columns-on-the-same-graph-in-r
library(ggplot2)
library(reshape2)
df_mergged <- merge(df_nov_pred_xts,df_nov_test_xts)
df_mergged_2 <- melt(df_mergged, id.vars="idx")

ggplot(df_mergged_2, aes(idx,value, col=variable)) + 
  geom_point() + 
  stat_smooth() 

df_mergged_2

xyz<-ggplot(df_mergged_2, aes(idx,value, colour=variable, group = variable)) + 
  geom_line()
show(xyz)

################################################################################
### Find RMSE for hold out test data set - November 2021 
################################################################################

library(Metrics)
#rmse(test_xts,pred_xts) - doesn't work

rmse(coredata(test_xts),coredata(pred_xts))


########################################################################
## Single Season - Auto Arima Forecasting - Daily Sum - weekly seasonality
########################################################################
