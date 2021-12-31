######################## ELECTRICITY PRICES #######################

# Name:   Florian Miedema
# Email:  florianmiedema@hotmail.com  

###################################################################

dev.off(dev.list()["RStudioGD"])
rm(list = ls())  ## libs
cat("\014")

# Installing Packages 
packages <- c("rstudioapi", "dplyr", "ISLR", "readr", "readxl", "keras", "tensorflow", 
              "zoo", "stargazer", "xtable", "tidyverse", "boot", "caret", "leaps",
              "rattle.data", "nnet", "glmnet", "ggplot2", "tree", "ipred", "randomForest",
              "Metrics", "ROCR", "gbm", "pls", "skimr", "mlbench", "tidyr", "ranger", 
              "lubridate", "data.table", "PerformanceAnalytics", "eeptools", "lsr", 
              "scales", "rsample", "doBy", "matrixStats", "fasstr")

if(length(setdiff(packages, rownames(installed.packages()))) > 0) { 
  install.packages(setdiff(packages, rownames (installed.packages())))
} 

# Loading Packages 
lapply(packages, library, character.only = TRUE)

# Setting up the Working Directory
filepath <- rstudioapi::getSourceEditorContext() $path
dirpath <- dirname(rstudioapi::getSourceEditorContext() $path)
setwd(dirpath) 

# Loading the Data Set
data = readRDS("2021_Electricity_data.RDS")

summary(data)

# Setting Up the Dates 
data$year = year(data$date)
data$month = month(data$date)
data$day = day(data$date)
data$hour = hour(data$date)
data$Date = as.Date(data$date)
data$weekday = weekdays(data$date)

##################### QUESTION 1 #########################

#### QUESTION 1.1 ####

# Computing the Mean Dutch Price Per Hour
mean_prices = data %>%
  group_by(hour) %>%
  mutate(mean_price = mean(dutch_power))

mean_pricess = mean_prices[1:24, ] %>%
  select(mean_price)

# Computing the Dutch Mean Hourly Price in the Weekends 
weekend_file = data %>%
  filter(weekday == "zaterdag" | weekday == "zondag")

weekend_file = weekend_file %>%
  group_by(hour) %>%
  mutate(mean_price_weekend = mean(dutch_power))

weekend_file = weekend_file[1:24, ] %>%
  select(mean_price_weekend)

# Merging the Mean Hourly Price to the Mean Prices Per Hour
mean_pricess = inner_join(mean_pricess, weekend_file, by = "hour")

mean_pricess = mean_pricess %>%
  rename("Average Price Per Hour" = mean_price, "Average Hourly Price in the Weekend" = mean_price_weekend) %>%
  select(hour, "Average Price Per Hour", "Average Hourly Price in the Weekend") %>%
  gather(key = "variable", value = "value", -hour)

# Plotting the Results 
ggplot(mean_pricess, aes(x = hour, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("Average Hourly Price of Dutch Electricity (in Euros)") +
  xlab('Hours') +
  ylab('Average Hourly Price (???)') +
  theme(legend.position = "right")

#### QUESTION 1.2 ####
# Computing the Volatility of Dutch Price Per Hour
sd_prices = data %>%
  group_by(hour) %>%
  mutate(sd_price = sd(dutch_power))

# Taking the Hourly Volatility and the Hour Variable Seperate
sd_pricess = sd_prices[1:24, ] %>%
  select(sd_price)

# Plotting the Results
ggplot(sd_pricess, aes(x = hour, y = sd_price), color = "red") + 
  geom_line(aes(color = "red")) +
  ggtitle("Dutch Price Volatility Per Hour") +
  xlab('Hours') +
  ylab('Average Price Volatility')

# Removing Unnecessary Data
rm(list = ls()[grep("^mean", ls())])
rm(list = ls()[grep("^sd", ls())])
rm(list = ls()[grep("^weekend", ls())])

#### QUESTION 1.3 ####
# Removing the 29th of February
data = data %>%
  filter(Date <= "2020-02-28" | Date >= "2020-03-01")

# Giving Each Day of the Year a Number
data = add_date_variables(data, dates = Date, water_year_start = 1)

# Adjusting the Number After 29th of February
data = data %>%
  mutate(DayofYear = ifelse(Date >= "2020-03-01", DayofYear - 1, DayofYear))

# Dropping Unnecessary Variables
data = data %>%
  select(-CalendarYear, -Month, -MonthName, -WaterYear)

# Computing the Daily Average 
daily_average_file = data %>%
  group_by(year, month, day) %>%
  mutate(mean_daily = mean(dutch_power))

# Computing the Average Daily Price Over the Year
average_file = daily_average_file %>%
  group_by(DayofYear) %>%
  mutate(mean_per_day = mean(mean_daily))

# Taking the Average Daily Prices Per Year in a Seperate File
daily_mean_price = average_file %>%
  select(DayofYear, mean_per_day)

# Keeping the Unique Values
daily_mean_price = unique(daily_mean_price)

# Plotting the Results
ggplot(daily_mean_price, aes(x = DayofYear, y = mean_per_day)) +
  geom_line(aes(color = "red")) +
  ggtitle("Average Price Per Day (in Euros)") +
  xlab('Days') +
  ylab('Average Price (???)') +
  theme(legend.position = "none")

# Removing Unnecessary Data
rm(list = ls()[grep("^average", ls())])
rm(list = ls()[grep("^daily", ls())])

#### QUESTION 1.4 ####
# Separating the April-September File
data_april_september = data %>%
  filter(month == 4 | month == 5 | month == 6 | month == 7 | month == 8 | month == 9)

# Computing the Mean Solar Generation Per Hour
data_april_september = data_april_september %>%
  group_by(hour) %>%
  mutate(mean_solar_hour = mean(solar))

# Separating the Important Variables
data_april_september = data_april_september %>%
  select(hour, mean_solar_hour)

# Keeping the Unique Values
data_april_september = unique(data_april_september)

# Separating the October-March File
data_october_march = data %>%
  filter(month == 10 | month == 11 | month == 12 | month == 1 | month == 2 | month == 3)

# Computing the Mean Solar Generation Per Hour
data_october_march = data_october_march %>%
  group_by(hour) %>%
  mutate(mean_solar_hour = mean(solar))

# Separating the Important Variables
data_october_march = data_october_march %>%
  select(hour, mean_solar_hour)

# Keeping the Unique Values
data_october_march = unique(data_october_march)

# Merging the Two Files Together
merged_data = inner_join(data_april_september, data_october_march, by = "hour")

# Renaming the Variables
merged_data = merged_data %>%
  rename(april_september = "mean_solar_hour.x", october_march = "mean_solar_hour.y")

# Plotting the Results 
merged_data = merged_data %>%
  rename("Average Hourly Solar Generation April-September" = april_september,
         "Average Hourly Solar Generation October-March" = october_march) %>%
  select(hour, "Average Hourly Solar Generation April-September", "Average Hourly Solar Generation October-March") %>%
  gather(key = "variable", value = "value", -hour)

ggplot(merged_data, aes(x = hour, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("Average Hourly Solar Generation") +
  xlab('Hours') +
  ylab('Solar Generation') +
  theme(legend.position = "right")

# Removing Unnecessary Data
rm(list = ls()[grep("^merged", ls())])
rm(data_april_september)
rm(data_october_march)

#### QUESTION 1.5 ####
# Computing the Daily Average of Solar
daily_average_file = data %>%
  group_by(year, month, day) %>%
  mutate(mean_daily = mean(solar))

# Computing the Average Daily Price Over the Year
average_file = daily_average_file %>%
  group_by(DayofYear) %>%
  mutate(mean_per_day = mean(mean_daily))

# Taking the Average Daily Prices Per Year in a Separate File
daily_mean_solar = average_file %>%
  select(DayofYear, mean_per_day)

# Keeping the Unique Values
daily_mean_solar = unique(daily_mean_solar)

# Computing the Daily Average of Off-Shore Wind
daily_average_file = data %>%
  group_by(year, month, day) %>%
  mutate(mean_daily = mean(wind_off_shore))

# Computing the Average Daily Price Over the Year
average_file = daily_average_file %>%
  group_by(DayofYear) %>%
  mutate(mean_per_day = mean(mean_daily))

# Taking the Average Daily Prices Per Year in a Separate File
daily_mean_wind = average_file %>%
  select(DayofYear, mean_per_day)

# Keeping the Unique Values
daily_mean_wind = unique(daily_mean_wind)

# Merging the Two Files
merged_data = inner_join(daily_mean_solar, daily_mean_wind, by = "DayofYear")

merged_data = merged_data %>%
  rename("Daily Mean Solar Power" = mean_per_day.x, "Daily Mean Off-Shore Wind" = mean_per_day.y)

# Plotting the Results 
merged_data = merged_data %>%
  select(DayofYear, "Daily Mean Solar Power", "Daily Mean Off-Shore Wind") %>%
  gather(key = "variable", value = "value", -DayofYear)

ggplot(merged_data, aes(x = DayofYear, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("Average Daily Solar and Off-Shore Wind Generation") +
  xlab('Days') +
  ylab('Generation') +
  theme(legend.position = "right")

# Removing Unnecessary Data
rm(list = ls()[grep("^daily", ls())])
rm(list = ls()[grep("^merged", ls())])
rm(average_file)

##################### QUESTION 2 #########################
#### Question 2.1
# Taking a Subset of Dutch Power
data2 = data %>%
  select(Date, dutch_power)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("dutch_power", row_number()))

merged_file_dutch = spread(data= data3, key = id, value = dutch_power)

merged_file_dutch = merged_file_dutch %>%
  select(dutch_power1, dutch_power2, dutch_power3, dutch_power4,
         dutch_power5, dutch_power6, dutch_power7, dutch_power8, dutch_power9,
         dutch_power10, dutch_power11, dutch_power12, dutch_power13, dutch_power14,
         dutch_power15, dutch_power16, dutch_power17, dutch_power18, dutch_power19,
         dutch_power20, dutch_power21, dutch_power22, dutch_power23, dutch_power24)

# Taking a Subset of German Power
data2 = data %>%
  select(Date, german_power)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("german_power", row_number()))

merged_file_german = spread(data= data3, key = id, value = german_power)

merged_file_german = merged_file_german %>%
  select(german_power1, german_power2, german_power3, german_power4,
         german_power5, german_power6, german_power7, german_power8, german_power9,
         german_power10, german_power11, german_power12, german_power13, german_power14,
         german_power15, german_power16, german_power17, german_power18, german_power19,
         german_power20, german_power21, german_power22, german_power23, german_power24)

# Taking a Subset of Belgium Power
data2 = data %>%
  select(Date, belgium_power)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("belgium_power", row_number()))

merged_file_belgium = spread(data= data3, key = id, value = belgium_power)

merged_file_belgium = merged_file_belgium %>%
  select(belgium_power1, belgium_power2, belgium_power3, belgium_power4,
         belgium_power5, belgium_power6, belgium_power7, belgium_power8, belgium_power9,
         belgium_power10, belgium_power11, belgium_power12, belgium_power13, belgium_power14,
         belgium_power15, belgium_power16, belgium_power17, belgium_power18, belgium_power19,
         belgium_power20, belgium_power21, belgium_power22, belgium_power23, belgium_power24)

# Taking a Subset of Norway Power
data2 = data %>%
  select(Date, norway_power)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("norway_power", row_number()))

merged_file_norway = spread(data= data3, key = id, value = norway_power)

merged_file_norway = merged_file_norway %>%
  select(norway_power1, norway_power2, norway_power3, norway_power4,
         norway_power5, norway_power6, norway_power7, norway_power8, norway_power9,
         norway_power10, norway_power11, norway_power12, norway_power13, norway_power14,
         norway_power15, norway_power16, norway_power17, norway_power18, norway_power19,
         norway_power20, norway_power21, norway_power22, norway_power23, norway_power24)

# Taking a Subset of Dutch Load
data2 = data %>%
  select(Date, dutch_load)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("dutch_load", row_number()))

merged_file_dutch_load = spread(data= data3, key = id, value = dutch_load)

merged_file_dutch_load = merged_file_dutch_load %>%
  select(dutch_load1, dutch_load2, dutch_load3, dutch_load4,
         dutch_load5, dutch_load6, dutch_load7, dutch_load8, dutch_load9,
         dutch_load10, dutch_load11, dutch_load12, dutch_load13, dutch_load14,
         dutch_load15, dutch_load16, dutch_load17, dutch_load18, dutch_load19,
         dutch_load20, dutch_load21, dutch_load22, dutch_load23, dutch_load24)

# Taking a Subset of Dutch Generation Forecast
data2 = data %>%
  select(Date, dutch_generation_forecast)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("dutch_generation_forecast", row_number()))

merged_file_dutch_generation_forecast = spread(data= data3, key = id, value = dutch_generation_forecast)

merged_file_dutch_generation_forecast = merged_file_dutch_generation_forecast %>%
  select(dutch_generation_forecast1, dutch_generation_forecast2, dutch_generation_forecast3, dutch_generation_forecast4,
         dutch_generation_forecast5, dutch_generation_forecast6, dutch_generation_forecast7, dutch_generation_forecast8, dutch_generation_forecast9,
         dutch_generation_forecast10, dutch_generation_forecast11, dutch_generation_forecast12, dutch_generation_forecast13, dutch_generation_forecast14,
         dutch_generation_forecast15, dutch_generation_forecast16, dutch_generation_forecast17, dutch_generation_forecast18, dutch_generation_forecast19,
         dutch_generation_forecast20, dutch_generation_forecast21, dutch_generation_forecast22, dutch_generation_forecast23, dutch_generation_forecast24)

# Taking a Subset of Solar
data2 = data %>%
  select(Date, solar)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("solar", row_number()))

merged_file_solar = spread(data= data3, key = id, value = solar)

merged_file_solar = merged_file_solar %>%
  select(solar1, solar2, solar3, solar4,
         solar5, solar6, solar7, solar8, solar9,
         solar10, solar11, solar12, solar13, solar14,
         solar15, solar16, solar17, solar18, solar19,
         solar20, solar21, solar22, solar23, solar24)

# Taking a Subset of Off-Shore Wind
data2 = data %>%
  select(Date, wind_off_shore)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("wind_off_shore", row_number()))

merged_file_wind_off_shore = spread(data= data3, key = id, value = wind_off_shore)

merged_file_wind_off_shore = merged_file_wind_off_shore %>%
  select(wind_off_shore1, wind_off_shore2, wind_off_shore3, wind_off_shore4,
         wind_off_shore5, wind_off_shore6, wind_off_shore7, wind_off_shore8, wind_off_shore9,
         wind_off_shore10, wind_off_shore11, wind_off_shore12, wind_off_shore13, wind_off_shore14,
         wind_off_shore15, wind_off_shore16, wind_off_shore17, wind_off_shore18, wind_off_shore19,
         wind_off_shore20, wind_off_shore21, wind_off_shore22, wind_off_shore23, wind_off_shore24)

# Taking a Subset of On-Shore Wind
data2 = data %>%
  select(Date, wind_on_shore)

# Computing the New Data Frame
data3 = data2 %>%
  group_by(Date) %>%
  mutate(id = paste0("wind_on_shore", row_number()))

merged_file_wind_on_shore = spread(data= data3, key = id, value = wind_on_shore)

merged_file_wind_on_shore = merged_file_wind_on_shore %>%
  select(wind_on_shore1, wind_on_shore2, wind_on_shore3, wind_on_shore4,
         wind_on_shore5, wind_on_shore6, wind_on_shore7, wind_on_shore8, wind_on_shore9,
         wind_on_shore10, wind_on_shore11, wind_on_shore12, wind_on_shore13, wind_on_shore14,
         wind_on_shore15, wind_on_shore16, wind_on_shore17, wind_on_shore18, wind_on_shore19,
         wind_on_shore20, wind_on_shore21, wind_on_shore22, wind_on_shore23, wind_on_shore24)

# Merging All the Files Together
merged_data = inner_join(merged_file_dutch, merged_file_german, by = "Date")
merged_data = inner_join(merged_data, merged_file_belgium, by = "Date")
merged_data = inner_join(merged_data, merged_file_norway, by = "Date")
merged_data = inner_join(merged_data, merged_file_dutch_load, by = "Date")
merged_data = inner_join(merged_data, merged_file_dutch_generation_forecast, by = "Date")
merged_data = inner_join(merged_data, merged_file_solar, by = "Date")
merged_data = inner_join(merged_data, merged_file_wind_off_shore, by = "Date")
merged_data = inner_join(merged_data, merged_file_wind_on_shore, by = "Date")

# Taking a Subset of the Complete Data Set, to Show for Q2.1
show_data = merged_data %>%
  select(Date, dutch_power1, dutch_power24, german_power1, german_power24, belgium_power1, 
         belgium_power24, norway_power1, norway_power24, dutch_load1, dutch_load24, 
         dutch_generation_forecast1, dutch_generation_forecast24, solar1, solar24, 
         wind_off_shore1, wind_off_shore24, wind_on_shore1, wind_on_shore24,)

# Removing Unnecessary Data
rm(list = ls()[grep("^daily", ls())])
rm(list = ls()[grep("^merged_file", ls())])
rm(data2)
rm(data3)
rm(show_data)

#### Question 2.2
# Splitting the Outcome (Dutch Power) 
y = merged_data %>%
  select(Date, dutch_power1, dutch_power2, dutch_power3, dutch_power4,
         dutch_power5, dutch_power6, dutch_power7, dutch_power8, dutch_power9,
         dutch_power10, dutch_power11, dutch_power12, dutch_power13, dutch_power14,
         dutch_power15, dutch_power16, dutch_power17, dutch_power18, dutch_power19,
         dutch_power20, dutch_power21, dutch_power22, dutch_power23, dutch_power24)

# Dropping the First Observation of the Outcome
y = y[2:1095,]

# Splitting the Features
x = merged_data %>%
  select(-dutch_power1, -dutch_power2, -dutch_power3, -dutch_power4,
         -dutch_power5, -dutch_power6, -dutch_power7, -dutch_power8, -dutch_power9,
         -dutch_power10, -dutch_power11, -dutch_power12, -dutch_power13, -dutch_power14,
         -dutch_power15, -dutch_power16, -dutch_power17, -dutch_power18, -dutch_power19,
         -dutch_power20, -dutch_power21, -dutch_power22, -dutch_power23, -dutch_power24)

# Lagging All the Features Except the Date Variable
x[,2:193] = lapply(x[,2:193], lag)

# Dropping the Missing Values in the First Row
x = x[2:1095,]

#### Question 2.3
# Identifying the Features With More Than 10% Zero's
lapply(x, function(x){ length(which(x==0))/length(x)})

# Dropping the Features With More Zero's Than the 10% Cutoff Point
x = x %>%
  select(-solar1, -solar2, -solar3, -solar4, -solar5, -solar6, -solar7, -solar8,
    -solar18, -solar19, - solar20, -solar21, -solar22, -solar23, -solar24)

# Splitting X Into a Training Sample and a Test Sample 
x_train = x %>%
  filter(Date <= "2020-06-30")

x_test = x %>%
  filter(Date > "2020-06-30")

# Splitting Y Into a Training Sample and a Test Sample
y_train = y %>%
  filter(Date <= "2020-06-30")

y_test = y %>%
  filter(Date > "2020-06-30")

y_partial_test = y_test

# Temporarily Removing the Date Variable
x_train = x_train %>%
  ungroup() %>%
  select(-Date)

# Computing the Mean and Standard Deviation of the Training Sample
mean_x = apply(x_train, 2, mean)
std_x = apply(x_train, 2, sd)

# Normalizing the Training Sample
x_train = scale(x_train, center = mean_x, scale = std_x)

x_train = x_train %>%
  as_tibble()

x_train$Date = y_train$Date

x_train = x_train %>%
  select(Date, everything())

# Normalizing the Test Sample (with the Mean and SD from the Training Sample)
x_test = x_test %>%
  ungroup() %>%
  select(-Date)

x_test = scale(x_test, center = mean_x, scale = std_x)

x_test = x_test %>%
  as_tibble()

x_test$Date = y_test$Date

x_test = x_test %>%
  select(Date, everything())

x_partial_test = x_test %>%
  select(Date, everything())

# Removing Unnecessary Data
rm(mean_x)
rm(std_x)

##################### QUESTION 3 #########################
#### Question 3.1
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power1, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power1 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power1, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm = caret::RMSE(pred, y_test$dutch_power1)      # 8.6992

# Creating a Data Frame For the Output
df = matrix(1:2, nrow = 1, ncol = 2)
colnames(df) = c("In-Sample R-Squared", "Out-of-Sample RMSE")
df[,1] = r2_lm
df[,2] = rmse_lm

# Exporting the Output
stargazer(df, type = "html", title= "Linear Model", out="Q3.1.doc")

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)
rm(df)
rm(x)
rm(y)

#### Question 3.2
# Dividing the Training Data Into Training Sample and Validation Sample

# Splitting X Into a Training Sample and a Validation Sample 
x_partial_train = x_train %>%
  filter(Date <= "2020-01-01")

x_val = x_train %>%
  filter(Date > "2020-01-01")

# Splitting Y Into a Training Sample and a Validation Sample
y_partial_train = y_train %>%
  filter(Date <= "2020-01-01")

y_val = y_train %>%
  filter(Date > "2020-01-01")

######## LASSO #######

# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power1[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power1[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power1[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso = sqrt(mean(error_lasso^2))     # 9.1727

# Finding the Optimal Lambda
grid[opt]                                  # 0.5885

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

######## PLS ##########

# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power1 = y_partial_train$dutch_power1
x_partial_test$dutch_power1 = y_partial_test$dutch_power1

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power1 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

plot(pls.fit)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls = caret::R2(pred_pls_IS, x_partial_train$dutch_power1)   # 0.6858

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power1 - pred_pls
rmse_pls = sqrt(mean(error_pls^2))                              # 7.0415

# Plotting the Important Variables 
plot(varImp(pls.fit), 10, main = "Variable Importance")

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

######## Random Forest ##########

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power1 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
  }

print(model)

# Computing the In-Sample R-Squared
r2_rf = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power1 - pred_rf
rmse_rf = sqrt(mean(error_rf^2))

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power1)

x_partial_test = x_partial_test %>%
  select(-dutch_power1)

# Creating a Matrix For the Results
df = matrix(1:6, nrow = 3, ncol = 2)
colnames(df) = c("In-Sample R-Squared", "Out-of-Sample RMSE")
rownames(df) = c("Lasso", "PLS","Random Forest")

df[1,1] = r2_lasso
df[1,2] = rmse_lasso

df[2,1] = r2_pls
df[2,2] = rmse_pls

df[3,1] = r2_rf
df[3,2] = rmse_rf

# Exporting the Output
stargazer(df, type = "html", title= "R-Squared & RMSE", out="Q3.2.doc")

# Removing Unnecessary Data
rm(list = ls()[grep("^r2", ls())])
rm(list = ls()[grep("^rmse", ls())])
rm(model)
rm(df)
rm(grid)
rm(i)
rm(mtry)
rm(nodesize)
rm(sampsize)

##################### QUESTION 4 #########################
#### Question 4.1
# Computing the In-Sample R-Squared and the Out-of-Sample RMSE per Model
# Model 1
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power1, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power1 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm1 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power1, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm1 = caret::RMSE(pred, y_test$dutch_power1)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 2
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power2, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power2 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm2 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power2, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm2 = caret::RMSE(pred, y_test$dutch_power2)      

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 3
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power3, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power3 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm3 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power3, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm3 = caret::RMSE(pred, y_test$dutch_power3)       

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 4
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power4, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power4 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm4 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power4, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm4 = caret::RMSE(pred, y_test$dutch_power4)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 5
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power5, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power5 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm5 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power5, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm5 = caret::RMSE(pred, y_test$dutch_power5)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 6
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power6, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power6 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm6 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power6, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm6 = caret::RMSE(pred, y_test$dutch_power6)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 7
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power7, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power7 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm7 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power7, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm7 = caret::RMSE(pred, y_test$dutch_power7)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 8
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power8, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power8 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm8 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power8, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm8 = caret::RMSE(pred, y_test$dutch_power8)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 9
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power9, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power9 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm9 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power9, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm9 = caret::RMSE(pred, y_test$dutch_power9)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 10
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power10, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power10 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm10 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power10, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm10 = caret::RMSE(pred, y_test$dutch_power10)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 11
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power11, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power11 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm11 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power11, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm11 = caret::RMSE(pred, y_test$dutch_power11)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 12
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power12, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power12 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm12 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power12, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm12 = caret::RMSE(pred, y_test$dutch_power12)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 13
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power13, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power13 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm13 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power13, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm13 = caret::RMSE(pred, y_test$dutch_power13)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 14
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power14, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power14 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm14 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power14, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm14 = caret::RMSE(pred, y_test$dutch_power14)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 15
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power15, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power15 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm15 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power15, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm15 = caret::RMSE(pred, y_test$dutch_power15)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 16
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power16, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power16 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm16 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power16, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm16 = caret::RMSE(pred, y_test$dutch_power16)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 17
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power17, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power17 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm17 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power17, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm17 = caret::RMSE(pred, y_test$dutch_power17)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 18
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power18, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power18 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm18 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power18, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm18 = caret::RMSE(pred, y_test$dutch_power18)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 19
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power19, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power19 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm19 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power19, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm19 = caret::RMSE(pred, y_test$dutch_power19)

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 20
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power20, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power20 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm20 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power20, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm20 = caret::RMSE(pred, y_test$dutch_power2)      

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 21
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power21, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power21 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm21 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power21, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm21 = caret::RMSE(pred, y_test$dutch_power21)      

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 22
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power22, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power22 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm22 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power22, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm22 = caret::RMSE(pred, y_test$dutch_power22)      

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 23
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power23, dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power24 = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power23 ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm23 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power23, dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm23 = caret::RMSE(pred, y_test$dutch_power23)      

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Model 24
# Taking an Temporary File
temp = y_train %>%
  ungroup() %>%
  select(dutch_power24)

# Lagging the 24:00 Price 
temp$dutch_power_lagged = lag(temp$dutch_power24)

# Running the Linear Regression 
model = lm(dutch_power_lagged ~ dutch_power24, data = temp)

# In-Sample R-Squared
r2_lm24 = summary(model)$r.squared

# Out-of-Sample 
temp = y_test %>%
  ungroup() %>%
  select(dutch_power24)

# Computing the Fitted Values
pred = predict(model, temp)

# Out-of-Sample RMSE
rmse_lm24 = caret::RMSE(pred, y_test$dutch_power24)      

# Removing Unnecessary Data
rm(temp)
rm(pred)
rm(model)

# Creating a Matrix For the Results
df = matrix(1:48, nrow = 24, ncol = 2)
colnames(df) = c("In-Sample R-Squared", "Out-of-Sample RMSE")
rownames(df) = c("Model 1", "Model 2","Model 3", "Model 4", "Model 5",
                 "Model 6", "Model 7", "Model 8", "Model 9", "Model 10",
                 "Model 11", "Model 12", "Model 13", "Model 14", "Model 15",
                 "Model 16", "Model 17", "Model 18", "Model 19", "Model 20",
                 "Model 21", "Model 22", "Model 23", "Model 24") 

# Putting the Values in the Data Frame
df[1,1] = r2_lm1
df[1,2] = rmse_lm1

df[2,1] = r2_lm2
df[2,2] = rmse_lm2

df[3,1] = r2_lm3
df[3,2] = rmse_lm3

df[4,1] = r2_lm4
df[4,2] = rmse_lm4

df[5,1] = r2_lm5
df[5,2] = rmse_lm5

df[6,1] = r2_lm6
df[6,2] = rmse_lm6

df[7,1] = r2_lm7
df[7,2] = rmse_lm7

df[8,1] = r2_lm8
df[8,2] = rmse_lm8

df[9,1] = r2_lm9
df[9,2] = rmse_lm9

df[10,1] = r2_lm10
df[10,2] = rmse_lm10

df[11,1] = r2_lm11
df[11,2] = rmse_lm11

df[12,1] = r2_lm12
df[12,2] = rmse_lm12

df[13,1] = r2_lm13
df[13,2] = rmse_lm13

df[14,1] = r2_lm14
df[14,2] = rmse_lm14

df[15,1] = r2_lm15
df[15,2] = rmse_lm15

df[16,1] = r2_lm16
df[16,2] = rmse_lm16

df[17,1] = r2_lm17
df[17,2] = rmse_lm17

df[18,1] = r2_lm18
df[18,2] = rmse_lm18

df[19,1] = r2_lm19
df[19,2] = rmse_lm19

df[20,1] = r2_lm20
df[20,2] = rmse_lm20

df[21,1] = r2_lm21
df[21,2] = rmse_lm21

df[22,1] = r2_lm22
df[22,2] = rmse_lm22

df[23,1] = r2_lm23
df[23,2] = rmse_lm23

df[24,1] = r2_lm24
df[24,2] = rmse_lm24

# Creating a Graph for the In-Sample R-Squared
df_r2 = df[,1] %>%
  as_tibble()

df_r2 = df_r2 %>%
  mutate(value = value * 100)

df_r2$value = round(df_r2$value, digits = 2)

colnames(df_r2) = "R_Squared"

df_r2$Model = 1:24

ggplot(data = df_r2, aes(x = Model, y = R_Squared)) +
  geom_line(aes(color = "red")) +
  ggtitle("In-Sample R-Squared (in Percentages)") +
  xlab('Models') +
  ylab('R-Squared (%)') +
  theme(legend.position = "none")

# Creating a Graph for the Out-of-Sample RMSE
df_rmse = df[,2] %>%
  as_tibble()

colnames(df_rmse) = "RMSE"

df_rmse$Model = 1:24

ggplot(data = df_rmse, aes(x = Model, y = RMSE)) +
  geom_line(aes(color = "red")) +
  ggtitle("Out-of-Sample RMSE") +
  xlab('Models') +
  ylab('RMSE') +
  theme(legend.position = "none")

# Removing Unnecessary Data
rm(list = ls()[grep("^r2", ls())])
rm(list = ls()[grep("^rmse", ls())])

#### Question 4.2
#### LASSO ####
# Creating an Empty Matrix (for question 5.2 already)
tmp.lasso = matrix(1:2, nrow = 10, ncol = 24)
colnames(tmp.lasso) = c("lasso1", "lasso2", "lasso3", "lasso4", "lasso5", 
                        "lasso6", "lasso7", "lasso8", "lasso9", "lasso10", 
                        "lasso11", "lasso12", "lasso13", "lasso14", "lasso15", 
                        "lasso16", "lasso17", "lasso18", "lasso19", "lasso20", 
                        "lasso21", "lasso22", "lasso23", "lasso24")

# Running the 24 Lasso Models
# Model 1
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power1[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power1[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power1[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso1 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso1 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,1] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 2
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power2[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power2[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power2[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso2 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso2 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,2] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 3
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power3[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power3[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power3[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso3 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso3 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,3] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 4
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power4[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power4[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power4[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso4 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso4 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,4] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 5
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power5[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power5[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power5[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso5 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso5 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,5] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 6
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power6[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power6[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power6[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso6 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso6 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,6] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 7
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power7[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power7[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power7[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso7 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso7 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,7] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 8
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power8[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power8[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power8[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso8 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso8 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,8] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 9
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power9[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power9[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power9[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso9 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso9 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,9] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 10
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power10[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power10[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power10[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso10 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso10 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,10] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 11
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power11[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power11[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power11[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso11 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso11 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,11] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 12
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power12[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power12[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power12[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso12 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso12 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,12] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 13
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power13[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power13[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power13[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso13 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso13 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,13] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 14
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power14[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power14[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power14[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso14 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso14 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,14] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 15
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power15[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power15[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power15[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso15 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso15 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,15] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 16
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power16[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power16[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power16[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso16 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso16 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,16] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 17
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power17[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power17[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power17[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso17 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso17 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,17] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 18
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power18[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power18[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power18[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso18 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso18 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,18] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 19
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power19[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power19[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power19[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso19 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso19 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,19] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 20
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power20[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power20[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power20[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso20 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso20 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,20] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 21
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power21[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power21[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power21[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso21 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso21 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,21] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 22
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power22[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power22[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power22[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso22 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso22 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,22] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 23
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power23[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power23[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power23[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso23 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso23 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,23] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)

# Model 24
# Setting the Rolling Window
n    = 73                             
h    = 18                             
k    = 18                              

# Making the Grid for the Tuning Parameter Lambda
grid = 10^seq(10, -10, length.out = 1000)  

# Creating Empty Vectors
lambda = actual = pred = 100             
model  = list()

# Running the Loop
for (t in seq(from = 1, to = 10, by = 1)) { 
  if(t %% 25==0) cat(paste0("iteration: ", t, "\n"))
  
  # Predictor                                              
  x_train2 = model.matrix(Date ~ ., data = x_partial_train)[(1+(n*(t-1))):(n*t), ]      
  x_test2 = model.matrix(Date ~ ., data = x_val)[(1+(h*(t-1))):(h*t), ]
  x_oos2  = model.matrix(Date ~ ., data = x_partial_test)[(1+(k*(t-1))):(k*t), ]         
  
  # Target
  y_train2 = y_partial_train$dutch_power24[(1+(n*(t-1))):(n*t)]
  y_test2 = y_val$dutch_power24[(1+(h*(t-1))):(h*t)]
  y_oos2  = y_partial_test$dutch_power24[(1+(k*(t-1))):(k*t)]
  
  # Fitting the LASSO 
  lasso.mod = glmnet(x_train2, y_train2, alpha = 1, lambda = grid, thresh = 10^-12)
  
  # Selecting the Tuning Parameter
  lasso.pre = predict(lasso.mod, s = grid, newx = x_test2)
  SE  = (lasso.pre - y_test2)^2
  mse = colMeans(SE)
  opt = which.min(mse)
  
  # Predict Returns  
  lasso.oos = predict(lasso.mod, s = grid[opt], newx = x_oos2)
  
  # Saving the Output
  lambda[t]  = grid[opt]
  model[[t]] = coef(lasso.mod, s = grid[opt])
  actual[t]  = y_oos2[1]
  pred[t]    = lasso.oos[1]
}

# Computing the In-Sample R-Squared
r2_lasso24 = max(lasso.mod$dev.ratio)        # 0.8875

# Computing the Out-of-Sample RMSE
error_lasso = actual - pred
rmse_lasso24 = sqrt(mean(error_lasso^2))     # 9.1727

tmp.lasso[,24] = pred

# Removing Unnecessary Data
rm(list = ls()[grep("^lasso", ls())])
rm(model)
rm(x_oos2)
rm(x_test2)
rm(x_train2)
rm(actual)
rm(error_lasso)
rm(grid)
rm(h)
rm(k)
rm(lambda)
rm(mse)
rm(n)
rm(opt)
rm(pred)
rm(t)
rm(y_oos2)
rm(y_test2)
rm(y_train2)
rm(SE)


#### PLS ####
# Creating an Empty Matrix (for question 5.2 already)
tmp.pls = matrix(1:2, nrow = 184, ncol = 24)
colnames(tmp.pls) = c("pls1", "pls2", "pls3", "pls4", "pls5", "pls6", "pls7",
                  "pls8", "pls9", "pls10", "pls11", "pls12", "pls13", "pls14",
                  "pls15", "pls16", "pls17", "pls18", "pls19", "pls20", "pls21",
                  "pls22", "pls23", "pls24")

# Running the 24 PLS Models
# Model 1
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power1 = y_partial_train$dutch_power1
x_partial_test$dutch_power1 = y_partial_test$dutch_power1

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power1 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls1 = caret::R2(pred_pls_IS, x_partial_train$dutch_power1)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power1 - pred_pls
rmse_pls1 = sqrt(mean(error_pls^2))                              

tmp.pls[,1] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power1)

x_partial_test = x_partial_test %>%
  select(-dutch_power1)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 2
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power2 = y_partial_train$dutch_power2
x_partial_test$dutch_power2 = y_partial_test$dutch_power2

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power2 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls2 = caret::R2(pred_pls_IS, x_partial_train$dutch_power2)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power2 - pred_pls
rmse_pls2 = sqrt(mean(error_pls^2))                              

tmp.pls[,2] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power2)

x_partial_test = x_partial_test %>%
  select(-dutch_power2)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 3
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power3 = y_partial_train$dutch_power3
x_partial_test$dutch_power3 = y_partial_test$dutch_power3

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power3 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls3 = caret::R2(pred_pls_IS, x_partial_train$dutch_power3)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power3 - pred_pls
rmse_pls3 = sqrt(mean(error_pls^2))                              

tmp.pls[,3] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power3)

x_partial_test = x_partial_test %>%
  select(-dutch_power3)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 4
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power4 = y_partial_train$dutch_power4
x_partial_test$dutch_power4 = y_partial_test$dutch_power4

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power4 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls4 = caret::R2(pred_pls_IS, x_partial_train$dutch_power4)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power4 - pred_pls
rmse_pls4 = sqrt(mean(error_pls^2))                              

tmp.pls[,4] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power4)

x_partial_test = x_partial_test %>%
  select(-dutch_power4)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 5
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power5 = y_partial_train$dutch_power5
x_partial_test$dutch_power5 = y_partial_test$dutch_power5

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power5 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls5 = caret::R2(pred_pls_IS, x_partial_train$dutch_power5)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power5 - pred_pls
rmse_pls5 = sqrt(mean(error_pls^2))                              

tmp.pls[,5] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power5)

x_partial_test = x_partial_test %>%
  select(-dutch_power5)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 6
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power6 = y_partial_train$dutch_power6
x_partial_test$dutch_power6 = y_partial_test$dutch_power6

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power6 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls6 = caret::R2(pred_pls_IS, x_partial_train$dutch_power6)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power6 - pred_pls
rmse_pls6 = sqrt(mean(error_pls^2))                              

tmp.pls[,6] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power6)

x_partial_test = x_partial_test %>%
  select(-dutch_power6)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 7
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power7 = y_partial_train$dutch_power7
x_partial_test$dutch_power7 = y_partial_test$dutch_power7

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power7 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls7 = caret::R2(pred_pls_IS, x_partial_train$dutch_power7)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power7 - pred_pls
rmse_pls7 = sqrt(mean(error_pls^2))                              

tmp.pls[,7] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power7)

x_partial_test = x_partial_test %>%
  select(-dutch_power7)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 8
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power8 = y_partial_train$dutch_power8
x_partial_test$dutch_power8 = y_partial_test$dutch_power8

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power8 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls8 = caret::R2(pred_pls_IS, x_partial_train$dutch_power8)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power8 - pred_pls
rmse_pls8 = sqrt(mean(error_pls^2))                              

tmp.pls[,8] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power8)

x_partial_test = x_partial_test %>%
  select(-dutch_power8)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 9
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power9 = y_partial_train$dutch_power9
x_partial_test$dutch_power9 = y_partial_test$dutch_power9

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power9 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls9 = caret::R2(pred_pls_IS, x_partial_train$dutch_power9)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power9 - pred_pls
rmse_pls9 = sqrt(mean(error_pls^2))                              

tmp.pls[,9] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power9)

x_partial_test = x_partial_test %>%
  select(-dutch_power9)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 10
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power10 = y_partial_train$dutch_power10
x_partial_test$dutch_power10 = y_partial_test$dutch_power10

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power10 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls10 = caret::R2(pred_pls_IS, x_partial_train$dutch_power10)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power10 - pred_pls
rmse_pls10 = sqrt(mean(error_pls^2))                              

tmp.pls[,10] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power10)

x_partial_test = x_partial_test %>%
  select(-dutch_power10)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 11
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power11 = y_partial_train$dutch_power11
x_partial_test$dutch_power11 = y_partial_test$dutch_power11

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power11 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls11 = caret::R2(pred_pls_IS, x_partial_train$dutch_power11)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power11 - pred_pls
rmse_pls11 = sqrt(mean(error_pls^2))                              

tmp.pls[,11] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power11)

x_partial_test = x_partial_test %>%
  select(-dutch_power11)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 12
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power12 = y_partial_train$dutch_power12
x_partial_test$dutch_power12 = y_partial_test$dutch_power12

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power12 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls12 = caret::R2(pred_pls_IS, x_partial_train$dutch_power12)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power12 - pred_pls
rmse_pls12 = sqrt(mean(error_pls^2))                              

tmp.pls[,12] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power12)

x_partial_test = x_partial_test %>%
  select(-dutch_power12)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 13
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power13 = y_partial_train$dutch_power13
x_partial_test$dutch_power13 = y_partial_test$dutch_power13

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power13 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls13 = caret::R2(pred_pls_IS, x_partial_train$dutch_power13)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power13 - pred_pls
rmse_pls13 = sqrt(mean(error_pls^2))                              

tmp.pls[,13] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power13)

x_partial_test = x_partial_test %>%
  select(-dutch_power13)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 14
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power14 = y_partial_train$dutch_power14
x_partial_test$dutch_power14 = y_partial_test$dutch_power14

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power14 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls14 = caret::R2(pred_pls_IS, x_partial_train$dutch_power14)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power14 - pred_pls
rmse_pls14 = sqrt(mean(error_pls^2))                              

tmp.pls[,14] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power14)

x_partial_test = x_partial_test %>%
  select(-dutch_power14)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 15
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power15 = y_partial_train$dutch_power15
x_partial_test$dutch_power15 = y_partial_test$dutch_power15

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power15 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls15 = caret::R2(pred_pls_IS, x_partial_train$dutch_power15)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power15 - pred_pls
rmse_pls15 = sqrt(mean(error_pls^2))                              

tmp.pls[,15] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power15)

x_partial_test = x_partial_test %>%
  select(-dutch_power15)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 16
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power16 = y_partial_train$dutch_power16
x_partial_test$dutch_power16 = y_partial_test$dutch_power16

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power16 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls16 = caret::R2(pred_pls_IS, x_partial_train$dutch_power16)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power16 - pred_pls
rmse_pls16 = sqrt(mean(error_pls^2))                              

tmp.pls[,16] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power16)

x_partial_test = x_partial_test %>%
  select(-dutch_power16)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 17
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power17 = y_partial_train$dutch_power17
x_partial_test$dutch_power17 = y_partial_test$dutch_power17

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power17 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls17 = caret::R2(pred_pls_IS, x_partial_train$dutch_power17)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power17 - pred_pls
rmse_pls17 = sqrt(mean(error_pls^2))                              

tmp.pls[,17] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power17)

x_partial_test = x_partial_test %>%
  select(-dutch_power17)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 18
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power18 = y_partial_train$dutch_power18
x_partial_test$dutch_power18 = y_partial_test$dutch_power18

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power18 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls18 = caret::R2(pred_pls_IS, x_partial_train$dutch_power18)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power18 - pred_pls
rmse_pls18 = sqrt(mean(error_pls^2))                              

tmp.pls[,18] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power18)

x_partial_test = x_partial_test %>%
  select(-dutch_power18)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 19
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power19 = y_partial_train$dutch_power19
x_partial_test$dutch_power19 = y_partial_test$dutch_power19

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power19 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls19 = caret::R2(pred_pls_IS, x_partial_train$dutch_power19)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power19 - pred_pls
rmse_pls19 = sqrt(mean(error_pls^2))                              

tmp.pls[,19] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power19)

x_partial_test = x_partial_test %>%
  select(-dutch_power19)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 20
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power20 = y_partial_train$dutch_power20
x_partial_test$dutch_power20 = y_partial_test$dutch_power20

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power20 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls20 = caret::R2(pred_pls_IS, x_partial_train$dutch_power20)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power20 - pred_pls
rmse_pls20 = sqrt(mean(error_pls^2))                              

tmp.pls[,20] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power20)

x_partial_test = x_partial_test %>%
  select(-dutch_power20)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 21
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power21 = y_partial_train$dutch_power21
x_partial_test$dutch_power21 = y_partial_test$dutch_power21

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power21 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls21 = caret::R2(pred_pls_IS, x_partial_train$dutch_power21)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power21 - pred_pls
rmse_pls21 = sqrt(mean(error_pls^2))                              

tmp.pls[,21] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power21)

x_partial_test = x_partial_test %>%
  select(-dutch_power21)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 22
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power22 = y_partial_train$dutch_power22
x_partial_test$dutch_power22 = y_partial_test$dutch_power22

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power22 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls22 = caret::R2(pred_pls_IS, x_partial_train$dutch_power22)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power22 - pred_pls
rmse_pls22 = sqrt(mean(error_pls^2))                              

tmp.pls[,22] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power22)

x_partial_test = x_partial_test %>%
  select(-dutch_power22)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 23
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power23 = y_partial_train$dutch_power23
x_partial_test$dutch_power23 = y_partial_test$dutch_power23

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power23 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls23 = caret::R2(pred_pls_IS, x_partial_train$dutch_power23)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power23 - pred_pls
rmse_pls23 = sqrt(mean(error_pls^2))                              

tmp.pls[,23] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power23)

x_partial_test = x_partial_test %>%
  select(-dutch_power23)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)

# Model 24
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power24 = y_partial_train$dutch_power24
x_partial_test$dutch_power24 = y_partial_test$dutch_power24

# Setting the Train Control
x = nrow(x_partial_train)
H = 10
IW = x - 10*H

trCnt = trainControl(
  method = "timeslice", 
  initialWindow = IW,
  horizon = H,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Fitting the PLS
pls.fit = train(
  dutch_power24 ~ .,
  data = x_partial_train,
  method = "pls",
  tuneLength = 20,
  trControl = trCnt 
)

# Computing the In-Sample R-Squared
pred_pls_IS =  pls.fit %>% predict(x_partial_train)
r2_pls24 = caret::R2(pred_pls_IS, x_partial_train$dutch_power24)   

# Computing the Out-of-Sample RMSE 
pred_pls = predict(pls.fit, x_partial_test)
error_pls = x_partial_test$dutch_power24 - pred_pls
rmse_pls24 = sqrt(mean(error_pls^2))                              

tmp.pls[,24] = pred_pls

# Removing the Temporarily Variables 
x_partial_train = x_partial_train %>%
  select(-dutch_power24)

x_partial_test = x_partial_test %>%
  select(-dutch_power24)

# Removing Unnecessary Data
rm(H)
rm(IW)
rm(error_pls)
rm(pred_pls)
rm(pls.fit)
rm(trCnt)
rm(x)
rm(pred_pls_IS)


#### Random Forest ####
tmp.rf = matrix(1:2, nrow = 184, ncol = 24)
colnames(tmp.rf) = c("rf1", "rf2", "rf3", "rf4", "rf5", "rf6", "rf7", "rf8", 
                      "rf9", "rf10", "rf11", "rf12", "rf13", "rf14", "rf15", "rf16", 
                      "rf17", "rf18", "rf19", "rf20", "rf21", "rf22", "rf23", "rf24")

# Running the 24 Random Forest Models
# Model 1
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power1 = y_partial_train$dutch_power1
x_partial_test$dutch_power1 = y_partial_test$dutch_power1

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power1 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf1 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power1 - pred_rf
rmse_rf1 = sqrt(mean(error_rf^2))

tmp.rf[,1] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power1)

x_partial_test = x_partial_test %>%
  select(-dutch_power1)

# Model 2
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power2 = y_partial_train$dutch_power2
x_partial_test$dutch_power2 = y_partial_test$dutch_power2

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power2 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf2 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power2 - pred_rf
rmse_rf2 = sqrt(mean(error_rf^2))

tmp.rf[,2] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power2)

x_partial_test = x_partial_test %>%
  select(-dutch_power2)

# Model 3
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power3 = y_partial_train$dutch_power3
x_partial_test$dutch_power3 = y_partial_test$dutch_power3

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power3 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf3 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power3 - pred_rf
rmse_rf3 = sqrt(mean(error_rf^2))

tmp.rf[,3] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power3)

x_partial_test = x_partial_test %>%
  select(-dutch_power3)

# Model 4
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power4 = y_partial_train$dutch_power4
x_partial_test$dutch_power4 = y_partial_test$dutch_power4

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power4 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf4 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power4 - pred_rf
rmse_rf4 = sqrt(mean(error_rf^2))

tmp.rf[,4] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power4)

x_partial_test = x_partial_test %>%
  select(-dutch_power4)

# Model 5
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power5 = y_partial_train$dutch_power5
x_partial_test$dutch_power5 = y_partial_test$dutch_power5

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power5 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf5 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power5 - pred_rf
rmse_rf5 = sqrt(mean(error_rf^2))

tmp.rf[,5] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power5)

x_partial_test = x_partial_test %>%
  select(-dutch_power5)

# Model 6
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power6 = y_partial_train$dutch_power6
x_partial_test$dutch_power6 = y_partial_test$dutch_power6

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power6 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf6 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power6 - pred_rf
rmse_rf6 = sqrt(mean(error_rf^2))

tmp.rf[,6] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power6)

x_partial_test = x_partial_test %>%
  select(-dutch_power6)

# Model 7
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power7 = y_partial_train$dutch_power7
x_partial_test$dutch_power7 = y_partial_test$dutch_power7

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power7 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf7 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power7 - pred_rf
rmse_rf7 = sqrt(mean(error_rf^2))

tmp.rf[,7] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power7)

x_partial_test = x_partial_test %>%
  select(-dutch_power7)

# Model 8
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power8 = y_partial_train$dutch_power8
x_partial_test$dutch_power8 = y_partial_test$dutch_power8

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power8 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf8 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power8 - pred_rf
rmse_rf8 = sqrt(mean(error_rf^2))

tmp.rf[,8] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power8)

x_partial_test = x_partial_test %>%
  select(-dutch_power8)

# Model 9
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power9 = y_partial_train$dutch_power9
x_partial_test$dutch_power9 = y_partial_test$dutch_power9

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power9 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf9 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power9 - pred_rf
rmse_rf9 = sqrt(mean(error_rf^2))

tmp.rf[,9] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power9)

x_partial_test = x_partial_test %>%
  select(-dutch_power9)

# Model 10
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power10 = y_partial_train$dutch_power10
x_partial_test$dutch_power10 = y_partial_test$dutch_power10

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power10 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf10 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power10 - pred_rf
rmse_rf10 = sqrt(mean(error_rf^2))

tmp.rf[,10] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power10)

x_partial_test = x_partial_test %>%
  select(-dutch_power10)

# Model 11
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power11 = y_partial_train$dutch_power11
x_partial_test$dutch_power11 = y_partial_test$dutch_power11

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power11 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf11 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power11 - pred_rf
rmse_rf11 = sqrt(mean(error_rf^2))

tmp.rf[,11] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power11)

x_partial_test = x_partial_test %>%
  select(-dutch_power11)

# Model 12
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power12 = y_partial_train$dutch_power12
x_partial_test$dutch_power12 = y_partial_test$dutch_power12

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power12 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf12 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power12 - pred_rf
rmse_rf12 = sqrt(mean(error_rf^2))

tmp.rf[,12] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power12)

x_partial_test = x_partial_test %>%
  select(-dutch_power12)

# Model 13
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power13 = y_partial_train$dutch_power13
x_partial_test$dutch_power13 = y_partial_test$dutch_power13

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power13 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf13 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power13 - pred_rf
rmse_rf13 = sqrt(mean(error_rf^2))

tmp.rf[,13] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power13)

x_partial_test = x_partial_test %>%
  select(-dutch_power13)

# Model 14
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power14 = y_partial_train$dutch_power14
x_partial_test$dutch_power14 = y_partial_test$dutch_power14

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power14 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf14 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power14 - pred_rf
rmse_rf14 = sqrt(mean(error_rf^2))

tmp.rf[,14] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power14)

x_partial_test = x_partial_test %>%
  select(-dutch_power14)

# Model 15
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power15 = y_partial_train$dutch_power15
x_partial_test$dutch_power15 = y_partial_test$dutch_power15

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power15 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf15 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power15 - pred_rf
rmse_rf15 = sqrt(mean(error_rf^2))

tmp.rf[,15] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power15)

x_partial_test = x_partial_test %>%
  select(-dutch_power15)

# Model 16
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power16 = y_partial_train$dutch_power16
x_partial_test$dutch_power16 = y_partial_test$dutch_power16

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power16 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf16 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power16 - pred_rf
rmse_rf16 = sqrt(mean(error_rf^2))

tmp.rf[,16] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power16)

x_partial_test = x_partial_test %>%
  select(-dutch_power16)

# Model 17
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power17 = y_partial_train$dutch_power17
x_partial_test$dutch_power17 = y_partial_test$dutch_power17

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power17 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf17 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power17 - pred_rf
rmse_rf17 = sqrt(mean(error_rf^2))

tmp.rf[,17] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power17)

x_partial_test = x_partial_test %>%
  select(-dutch_power17)

# Model 18
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power18 = y_partial_train$dutch_power18
x_partial_test$dutch_power18 = y_partial_test$dutch_power18

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power18 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf18 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power18 - pred_rf
rmse_rf18 = sqrt(mean(error_rf^2))

tmp.rf[,18] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power18)

x_partial_test = x_partial_test %>%
  select(-dutch_power18)

# Model 19
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power19 = y_partial_train$dutch_power19
x_partial_test$dutch_power19 = y_partial_test$dutch_power19

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power19 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf19 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power19 - pred_rf
rmse_rf19 = sqrt(mean(error_rf^2))

tmp.rf[,19] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power19)

x_partial_test = x_partial_test %>%
  select(-dutch_power19)

# Model 20
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power20 = y_partial_train$dutch_power20
x_partial_test$dutch_power20 = y_partial_test$dutch_power20

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power20 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf20 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power20 - pred_rf
rmse_rf20 = sqrt(mean(error_rf^2))

tmp.rf[,20] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power20)

x_partial_test = x_partial_test %>%
  select(-dutch_power20)

# Model 21
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power21 = y_partial_train$dutch_power21
x_partial_test$dutch_power21 = y_partial_test$dutch_power21

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power21 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf21 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power21 - pred_rf
rmse_rf21 = sqrt(mean(error_rf^2))

tmp.rf[,21] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power21)

x_partial_test = x_partial_test %>%
  select(-dutch_power21)

# Model 22
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power22 = y_partial_train$dutch_power22
x_partial_test$dutch_power22 = y_partial_test$dutch_power22

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power22 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf22 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power22 - pred_rf
rmse_rf22 = sqrt(mean(error_rf^2))

tmp.rf[,22] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power22)

x_partial_test = x_partial_test %>%
  select(-dutch_power22)

# Model 23
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power23 = y_partial_train$dutch_power23
x_partial_test$dutch_power23 = y_partial_test$dutch_power23

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power23 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf23 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power23 - pred_rf
rmse_rf23 = sqrt(mean(error_rf^2))

tmp.rf[,23] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power23)

x_partial_test = x_partial_test %>%
  select(-dutch_power23)

# Model 24
# Temporarily Including the Target Variable in the Training Data
x_partial_train$dutch_power24 = y_partial_train$dutch_power24
x_partial_test$dutch_power24 = y_partial_test$dutch_power24

mtry = c(floor(sqrt(ncol(x_partial_train))), 2*(floor(sqrt(ncol(x_partial_train)))))
nodesize = c(10, 20)
sampsize = c(floor(nrow(x_partial_train) * 0.7), floor(nrow(x_partial_train) * 0.85))

grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

for (i in 1:nrow(grid)) {
  model = randomForest(formula  = dutch_power24 ~ .,
                       data 		= x_partial_train,
                       mtry		  = grid$mtry[i],
                       nodesize = grid$nodesize[i],
                       sampsize	= grid$sampsize[i])
}

# Computing the In-Sample R-Squared
r2_rf24 = max(model$rsq)

# Computing the Out-of_Sample RMSE
pred_rf = predict(model, x_partial_test)
error_rf = x_partial_test$dutch_power24 - pred_rf
rmse_rf24 = sqrt(mean(error_rf^2))

tmp.rf[,24] = pred_rf

# Removing the Temporary Target Variable
x_partial_train = x_partial_train %>%
  select(-dutch_power24)

x_partial_test = x_partial_test %>%
  select(-dutch_power24)

# Computing the R-Squared File
data_r2 = matrix(1:120, nrow = 24, ncol = 5)

data_r2 = data_r2 %>%
  as_tibble()

data_r2 = data_r2 %>%
  rename(lm = V1,
         lasso = V2,
         pls = V3,
         rf = V4,
         model = V5)

data_r2 = data_r2 %>%
  as.data.frame()

data_r2$lm = df_r2$R_Squared

# Placing the Values in the Data Frame
data_r2[1,2] = r2_lasso1
data_r2[2,2] = r2_lasso2
data_r2[3,2] = r2_lasso3
data_r2[4,2] = r2_lasso4
data_r2[5,2] = r2_lasso5
data_r2[6,2] = r2_lasso6
data_r2[7,2] = r2_lasso7
data_r2[8,2] = r2_lasso8
data_r2[9,2] = r2_lasso9
data_r2[10,2] = r2_lasso10
data_r2[11,2] = r2_lasso11
data_r2[12,2] = r2_lasso12
data_r2[13,2] = r2_lasso13
data_r2[14,2] = r2_lasso14
data_r2[15,2] = r2_lasso15
data_r2[16,2] = r2_lasso16
data_r2[17,2] = r2_lasso17
data_r2[18,2] = r2_lasso18
data_r2[19,2] = r2_lasso19
data_r2[20,2] = r2_lasso20
data_r2[21,2] = r2_lasso21
data_r2[22,2] = r2_lasso22
data_r2[23,2] = r2_lasso23
data_r2[24,2] = r2_lasso24

data_r2[1,3] = r2_pls1
data_r2[2,3] = r2_pls2
data_r2[3,3] = r2_pls3
data_r2[4,3] = r2_pls4
data_r2[5,3] = r2_pls5
data_r2[6,3] = r2_pls6
data_r2[7,3] = r2_pls7
data_r2[8,3] = r2_pls8
data_r2[9,3] = r2_pls9
data_r2[10,3] = r2_pls10
data_r2[11,3] = r2_pls11
data_r2[12,3] = r2_pls12
data_r2[13,3] = r2_pls13
data_r2[14,3] = r2_pls14
data_r2[15,3] = r2_pls15
data_r2[16,3] = r2_pls16
data_r2[17,3] = r2_pls17
data_r2[18,3] = r2_pls18
data_r2[19,3] = r2_pls19
data_r2[20,3] = r2_pls20
data_r2[21,3] = r2_pls21
data_r2[22,3] = r2_pls22
data_r2[23,3] = r2_pls23
data_r2[24,3] = r2_pls24

data_r2[1,4] = r2_rf1
data_r2[2,4] = r2_rf2
data_r2[3,4] = r2_rf3
data_r2[4,4] = r2_rf4
data_r2[5,4] = r2_rf5
data_r2[6,4] = r2_rf6
data_r2[7,4] = r2_rf7
data_r2[8,4] = r2_rf8
data_r2[9,4] = r2_rf9
data_r2[10,4] = r2_rf10
data_r2[11,4] = r2_rf11
data_r2[12,4] = r2_rf12
data_r2[13,4] = r2_rf13
data_r2[14,4] = r2_rf14
data_r2[15,4] = r2_rf15
data_r2[16,4] = r2_rf16
data_r2[17,4] = r2_rf17
data_r2[18,4] = r2_rf18
data_r2[19,4] = r2_rf19
data_r2[20,4] = r2_rf20
data_r2[21,4] = r2_rf21
data_r2[22,4] = r2_rf22
data_r2[23,4] = r2_rf23
data_r2[24,4] = r2_rf24

data_r2$model = 1:24

data_r2 = data_r2 %>%
  mutate(lasso = lasso * 100)

data_r2 = data_r2 %>%
  mutate(pls = pls * 100)

data_r2 = data_r2 %>%
  mutate(rf = rf * 100)

data_r2$lasso = round(data_r2$lasso, digits = 2)
data_r2$pls = round(data_r2$pls, digits = 2)
data_r2$rf = round(data_r2$rf, digits = 2)

# Plotting the Results for the R-Squared
temp = data_r2 %>%
  rename("Linear Model" = lm,
         "LASSO" = lasso,
         "Partial Least Squares" = pls,
         "Random Forest" = rf) %>%
  gather(key = "variable", value = "value", -model)

ggplot(temp, aes(x = model, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("In-Sample R-Squared Per Model (In Percentages)") +
  xlab('Model') +
  ylab('R-Squared (%)') +
  theme(legend.position = "right")

# Computing the RMSE File
data_rmse = matrix(1:120, nrow = 24, ncol = 5)

data_rmse = data_rmse %>%
  as_tibble()

data_rmse = data_rmse %>%
  rename(lm = V1,
         lasso = V2,
         pls = V3,
         rf = V4,
         model = V5)

data_rmse = data_rmse %>%
  as.data.frame()

data_rmse$lm = df_rmse$RMSE

# Placing the Values in the Data Frame
data_rmse[1,2] = rmse_lasso1
data_rmse[2,2] = rmse_lasso2
data_rmse[3,2] = rmse_lasso3
data_rmse[4,2] = rmse_lasso4
data_rmse[5,2] = rmse_lasso5
data_rmse[6,2] = rmse_lasso6
data_rmse[7,2] = rmse_lasso7
data_rmse[8,2] = rmse_lasso8
data_rmse[9,2] = rmse_lasso9
data_rmse[10,2] = rmse_lasso10
data_rmse[11,2] = rmse_lasso11
data_rmse[12,2] = rmse_lasso12
data_rmse[13,2] = rmse_lasso13
data_rmse[14,2] = rmse_lasso14
data_rmse[15,2] = rmse_lasso15
data_rmse[16,2] = rmse_lasso16
data_rmse[17,2] = rmse_lasso17
data_rmse[18,2] = rmse_lasso18
data_rmse[19,2] = rmse_lasso19
data_rmse[20,2] = rmse_lasso20
data_rmse[21,2] = rmse_lasso21
data_rmse[22,2] = rmse_lasso22
data_rmse[23,2] = rmse_lasso23
data_rmse[24,2] = rmse_lasso24

data_rmse[1,3] = rmse_pls1
data_rmse[2,3] = rmse_pls2
data_rmse[3,3] = rmse_pls3
data_rmse[4,3] = rmse_pls4
data_rmse[5,3] = rmse_pls5
data_rmse[6,3] = rmse_pls6
data_rmse[7,3] = rmse_pls7
data_rmse[8,3] = rmse_pls8
data_rmse[9,3] = rmse_pls9
data_rmse[10,3] = rmse_pls10
data_rmse[11,3] = rmse_pls11
data_rmse[12,3] = rmse_pls12
data_rmse[13,3] = rmse_pls13
data_rmse[14,3] = rmse_pls14
data_rmse[15,3] = rmse_pls15
data_rmse[16,3] = rmse_pls16
data_rmse[17,3] = rmse_pls17
data_rmse[18,3] = rmse_pls18
data_rmse[19,3] = rmse_pls19
data_rmse[20,3] = rmse_pls20
data_rmse[21,3] = rmse_pls21
data_rmse[22,3] = rmse_pls22
data_rmse[23,3] = rmse_pls23
data_rmse[24,3] = rmse_pls24

data_rmse[1,4] = rmse_rf1
data_rmse[2,4] = rmse_rf2
data_rmse[3,4] = rmse_rf3
data_rmse[4,4] = rmse_rf4
data_rmse[5,4] = rmse_rf5
data_rmse[6,4] = rmse_rf6
data_rmse[7,4] = rmse_rf7
data_rmse[8,4] = rmse_rf8
data_rmse[9,4] = rmse_rf9
data_rmse[10,4] = rmse_rf10
data_rmse[11,4] = rmse_rf11
data_rmse[12,4] = rmse_rf12
data_rmse[13,4] = rmse_rf13
data_rmse[14,4] = rmse_rf14
data_rmse[15,4] = rmse_rf15
data_rmse[16,4] = rmse_rf16
data_rmse[17,4] = rmse_rf17
data_rmse[18,4] = rmse_rf18
data_rmse[19,4] = rmse_rf19
data_rmse[20,4] = rmse_rf20
data_rmse[21,4] = rmse_rf21
data_rmse[22,4] = rmse_rf22
data_rmse[23,4] = rmse_rf23
data_rmse[24,4] = rmse_rf24


data_rmse$model = 1:24

# Plotting the Results for the RMSE
test = data_rmse %>%
  rename("Linear Model" = lm,
         "LASSO" = lasso,
         "Partial Least Squares" = pls,
         "Random Forest" = rf) %>%
  gather(key = "variable", value = "value", -model)

ggplot(test, aes(x = model, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("Out-of-Sample RMSE Per Model") +
  xlab('Model') +
  ylab('RMSE') +
  theme(legend.position = "right")

# Removing Unnecessary Data
rm(list = ls()[grep("^r2", ls())])
rm(list = ls()[grep("^rmse", ls())])
rm(data_r2)
rm(data_rmse)
rm(df)
rm(df_r2)
rm(df_rmse)
rm(grid)
rm(model)
rm(temp)
rm(test)
rm(error_rf)
rm(i)
rm(mtry)
rm(nodesize)
rm(pred_rf)
rm(sampsize)

##################### QUESTION 5 #########################
#### Question 5.1
temp = y_train %>%
  ungroup() %>%
  select(-Date)

mean_hourly_price = colMeans(temp)
which.max(mean_hourly_price)          # max mean price: 20:00-21:00
which.min(mean_hourly_price)          # min mean price: 05:00-06:00

profit = -y_test$dutch_power5 + y_test$dutch_power20

tmp = matrix(1:2, nrow = 2, ncol = 1)
rownames(tmp) = c("Cumulative Profit (EUR)", "Daily Volatility")
colnames(tmp) = c("Linear Model")

tmp[1,1] = round(sum(profit), digits = 2)
tmp[2,1] = sd(profit)

stargazer(tmp, type = "html", out = "Q5.1.doc")

#### Question 5.2
# Lasso
mean_hourly_price = colMeans(tmp.lasso)
which.max(mean_hourly_price)          # max mean price: 21:00-22:00
which.min(mean_hourly_price)          # min mean price: 04:00-05:00

profit = -y_test$dutch_power4 + y_test$dutch_power21

rownames(tmp) = c("Cumulative Profit (EUR)", "Daily Volatility")
colnames(tmp) = c("Lasso Model")

tmp[1,1] = round(sum(profit), digits = 2)
tmp[2,1] = sd(profit)

stargazer(tmp, type = "html", out = "Q5.2.lasso.doc")

# PLS
mean_hourly_price = colMeans(tmp.pls)
which.max(mean_hourly_price)          # max mean price: 20:00-21:00
which.min(mean_hourly_price)          # min mean price: 06:00-07:00

profit = -y_test$dutch_power6 + y_test$dutch_power20

rownames(tmp) = c("Cumulative Profit (EUR)", "Daily Volatility")
colnames(tmp) = c("PLS Model")

tmp[1,1] = round(sum(profit), digits = 2)
tmp[2,1] = sd(profit)

stargazer(tmp, type = "html", out = "Q5.2.pls.doc")

# Random Forest
mean_hourly_price = colMeans(tmp.rf)
which.max(mean_hourly_price)          # max mean price: 20:00-21:00
which.min(mean_hourly_price)          # min mean price: 04:00-05:00

profit = -y_test$dutch_power4 + y_test$dutch_power20

rownames(tmp) = c("Cumulative Profit (EUR)", "Daily Volatility")
colnames(tmp) = c("Random Forest Model")

tmp[1,1] = round(sum(profit), digits = 2)
tmp[2,1] = sd(profit)

stargazer(tmp, type = "html", out = "Q5.2.rf.doc")

#### Question 5.3
mean_hourly_price = colMeans(tmp.rf)

profit = -y_test$dutch_power5 + y_test$dutch_power10 -y_test$dutch_power16 + y_test$dutch_power20

rownames(tmp) = c("Cumulative Profit (EUR)", "Daily Volatility")
colnames(tmp) = c("Own Trading Strategy")

tmp[1,1] = round(sum(profit), digits = 2)
tmp[2,1] = sd(profit)

stargazer(tmp, type = "html", out = "Q5.3.doc")

# Removing Unnecessary Data
rm(list = ls()[grep("^tmp", ls())])
rm(temp)
rm(profit)
rm(mean_hourly_price)
