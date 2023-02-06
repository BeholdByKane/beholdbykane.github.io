## setting up the environment
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("skimr")
install.packages("janitor")

library(tidyverse)
library(lubridate)
library(here)
library(skimr)
library(janitor)

## importing datasets and setting up frames

daily_activity <- read.csv("Bellabeat Project/dailyActivity_merged.csv")
daily_sleep <- read.csv("Bellabeat Project/sleepDay_merged.csv")
weight_log <- read.csv("Bellabeat Project/weightLogInfo_merged.csv")

## checking datasets

head(daily_activity)
colnames(daily_activity)

head(daily_sleep)
colnames(daily_sleep)

head(weight_log)
colnames(weight_log)

##checking for duplicated items in the datasets

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(weight_log))

dailySleep_cleaned <- unique(daily_sleep)
sum(duplicated(dailySleep_cleaned))

## changing "ActivityDate" and "SleepDay" to "Date" for ease of merging later and 
## making all 3 sets have matching Date formats

dailyActivity_cleaned <- daily_activity %>%
  rename(Date = ActivityDate) %>%
  mutate(Date = mdy(Date))
dailySleep_cleaned <- dailySleep_cleaned %>%
  rename(Date = SleepDay) %>%
  mutate(Date = mdy_hms(Date))

weightLog_cleaned <- weight_log %>%
  mutate(Date = mdy_hms(Date))

str(dailyActivity_cleaned)
str(dailySleep_cleaned)
str(weightLog_cleaned)

## checking for missing values/inconsistent entries

sum(is.na(dailyActivity_cleaned))
sum(is.na(dailySleep_cleaned))
sum(is.na(weightLog_cleaned))

## there are 65 values missing in weightLog_cleaned

colSums(is.na(weightLog_cleaned))
nrow(weightLog_cleaned)

## all missing values are from the 'Fat' column. There also 67 rows available in 'weightLog_cleaned'
## meaning that only 2 values are in 'Fat'. Because this is a third party dataset, it can not be determined
## what thhese values were supposed to be. Because of this and the column not being needed for my analysis,
## 'Fat' will be removed.

weightLog_cleaned <- weightLog_cleaned[, -5]

## confirming removal of 'Fat'

colnames(weightLog_cleaned)
colSums(is.na(weightLog_cleaned))

## there is more user data logged in dailyActivity than both dailySleep and weightLog

n_distinct(dailyActivity_cleaned$Id)
n_distinct(dailySleep_cleaned$Id)
n_distinct(weightLog_cleaned$Id)

## there are a few of columns with zero values in dailyActivity, which can not be possible  
## given the data logged. In order to eliminate any potential bias, rows with these values will be removed

dailyActivity_cleaned  <- dailyActivity_cleaned %>%
  filter(TotalSteps > 0, Calories > 0, TotalDistance  > 0)

## confirm changes

sum(dailyActivity_cleaned$TotalSteps <= 0)
sum(dailyActivity_cleaned$Calories <= 0)
sum(dailyActivity_cleaned$TotalDistance <= 0)

## there is no need to make changes to weightLog and dailySleep because there are no zero values

##lastly, check the number of rows after cleaning the  data

nrow(dailyActivity_cleaned)
nrow(dailySleep_cleaned)
nrow(weightLog_cleaned)

## now to summarize the data

dailyActivity_cleaned %>%
  select(TotalSteps, TotalDistance, Calories, SedentaryMinutes) %>%
  summary()
dailySleep_cleaned %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
weightLog_cleaned %>%
  select(BMI, WeightKg) %>%
  summary()

##Findings

Average Steps: 8329
Average Distance: 5.986km
Average Calories Burned: 2362/day
Average Sednetary Minutes: 955.2 (about 16 hours)
Average Sleep Time: 419.2 minutes (about 7 hours)
Average Time in Bed: 458.2 minutes
Average BMI:25.19
Average Weight: 72.04kg

Notable Outlier: One user only reported one instance of sleeping for an hour but logged 31 days of activity.

Findings are subject to bias because the gender of participants was not specified 
  and we do not have enough data.

##I need to merge dataframes to discover more trends through visualizations.
  
final_dailySleep <- merge(dailyActivity_cleaned, dailySleep_cleaned, by= c("Id", "Date"))
final_dailyWeight <- merge(dailyActivity_cleaned, weightLog_cleaned, by= c("Id", "Date"))

## Total Steps vs Calories burned

ggplot(data = final_dailySleep, aes(x=TotalSteps, y=Calories)) +
  geom_point(aes(color = Calories))  +
  geom_smooth(method = "loess") +
labs(title="Total Steps vs Calories Burned",
    x = "Total Steps Taken",
    y = "Calories Burned")

##Positive Relationship. More calories are burned at higher steps taken.

##Sedentary Minutes vs Calories Burned

ggplot(data = final_dailySleep, aes(x = SedentaryMinutes, y = Calories)) +
  geom_point(aes(color = Calories)) +
  geom_smooth(method = "loess") +
labs(title = "Sedentary Minutes vs Calories Burned",
    x = "Sedentary Minutes",
    y = "Calories Burned")

## No positive or negative trend in the data, 
## but general conclusion is more time spent idle = less calories burned

## Steps vs BMI

ggplot(data = final_dailyWeight, aes(x = TotalSteps, y = BMI)) +
  geom_point(aes(color = BMI)) +
  geom_smooth() +
labs(title = "Total Steps vs Body Mass Index (BMI)",
     x = "Total Steps",
     y = "BMI")

##less steps = higher BMI. the average BMI of or entrants is 25.19, which is considered overweight according
##to CDC.gov

## Sedentary Time vs BMI

ggplot(data = final_dailyWeight, aes(x = SedentaryMinutes, y = BMI)) +
  geom_point(aes(color = BMI)) +
  geom_smooth() +
  labs(title = "Sedentary Minutes vs Body Mass Index (BMI)",
       x = "Sedentary Minutes",
       y = "BMI")

## BMI stays roughly the same thoughout entrants with a few outliers.
##Sedentary time can have an effect on BMI, but activity is more likely to affect it.

## Sedentary vs Time spent Sleeping

ggplot(data = final_dailySleep, aes(x = TotalMinutesAsleep/60 , y = SedentaryMinutes/60)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Sedentary Time vs Time Spent Sleeping",
       x = "Sedentary Time",
       y = "Time Spent Sleeping")

##negative correlation here, but can not conclude that sedentary time is the cause. there are a number of
##reasons affected one's sleep like insomnnia, level of exhaustion, etc.

##Conclusions

##BMI and Weight: The average BMI of the entrants was 25.19. According to the Center for Disease Control
##this is considered to be overweight. According to our data, spending less time active and more of it doing
##sedentary activities can lead to higher BMI ratings. However there are other factors that contribute to
##BMI. This along with the limitations of our data (only 67 entries in the weight log) delivered biased results.

##Sleep: The average amount of time spent sleeping is about 7 hours, which is good! But without considering
##health conditions which could affect one's sleep and the age group of our entrants not being specified,
##the data is biased.

##Recomendations

##Incentives for reaching goals to drive engagement: Set daily steps goals and weekly activity goals. Remind ALL users, not just obese or overweight ones.
##Reward users with points which can be redeemed for gift cards, wellness products and accessories, or leisurely activities such as spa visits.
##Sedentary lifestyles can be the result of one's job, so you can also add reminders to get up and take short walking breaks.

##Sleep is just as important as physical activity in monitoring one's health. As such, we can advise users on sleep schedules to keep them on track with 
##their goals and allow them to add personalized alarms to reach said goals.

##Adding a weight log can also help drive interaction with the Bellabeat app. Tracking one's weight is another important factor in monitoring their health.
##and can encourage them to continue their journey with Bellabeat.

##Marketing Strategy

##Incentives drive interaction with the app, but be careful not to alienate or offend your users. As an example, do not advise an overweight or obese person
##to be more active in order to lose weight as it is insensitive and can drive them away from the Bellabeat experience. Increased interaction via the reward system
##can help collect user data, but to be more effective we can also introduce surveys once a month. The surveys are opt-in, and all users would be informed on how their
##privacy would be secured and how their data would be used to improve the Bellabeat experience.