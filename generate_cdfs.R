# ****************************************
# The doctor will see you now - Personalised Waiting Times
# 
# Forecasting Waitlist - Fitting Error Distribution
# This will nuance the predictions produced
# 
# Emma Tarmey
# 14/08/2022
# ****************************************


# ----- Preamble -----

library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(stringr)
library(reshape2)
library(MLmetrics)
library(actuar)
library(GoFKernel)

rm(list = ls())
setwd("...")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)



# ----- Load data from Excel Sheets -----

ts_foot_ankle_priority_1_enter_by_week  <- read.csv("foot_ankle_priority_1_enter_by_week_timeseries.csv")
ts_foot_ankle_priority_2_enter_by_week  <- read.csv("foot_ankle_priority_2_enter_by_week_timeseries.csv")
ts_foot_ankle_priority_all_exit_by_week <- read.csv("foot_ankle_priority_all_exit_by_week_timeseries.csv")

ts_hand_priority_1_enter_by_week  <- read.csv("hand_priority_1_enter_by_week_timeseries.csv")
ts_hand_priority_2_enter_by_week  <- read.csv("hand_priority_2_enter_by_week_timeseries.csv")
ts_hand_priority_all_exit_by_week <- read.csv("hand_priority_all_exit_by_week_timeseries.csv")

ts_hip_priority_1_enter_by_week  <- read.csv("hip_priority_1_enter_by_week_timeseries.csv")
ts_hip_priority_2_enter_by_week  <- read.csv("hip_priority_2_enter_by_week_timeseries.csv")
ts_hip_priority_all_exit_by_week <- read.csv("hip_priority_all_exit_by_week_timeseries.csv")

ts_hip_knee_priority_1_enter_by_week  <- read.csv("hip_knee_priority_1_enter_by_week_timeseries.csv")
ts_hip_knee_priority_2_enter_by_week  <- read.csv("hip_knee_priority_2_enter_by_week_timeseries.csv")
ts_hip_knee_priority_all_exit_by_week <- read.csv("hip_knee_priority_all_exit_by_week_timeseries.csv")

ts_knee_priority_1_enter_by_week  <- read.csv("knee_priority_1_enter_by_week_timeseries.csv")
ts_knee_priority_2_enter_by_week  <- read.csv("knee_priority_2_enter_by_week_timeseries.csv")
ts_knee_priority_all_exit_by_week <- read.csv("knee_priority_all_exit_by_week_timeseries.csv")

ts_shoulder_priority_1_enter_by_week  <- read.csv("shoulder_priority_1_enter_by_week_timeseries.csv")
ts_shoulder_priority_2_enter_by_week  <- read.csv("shoulder_priority_2_enter_by_week_timeseries.csv")
ts_shoulder_priority_all_exit_by_week <- read.csv("shoulder_priority_all_exit_by_week_timeseries.csv")



# ----- Make Time-series Object -----

# set window across all data-sets here
# note: incompletes begin at 01/06/2020
window_start = c(2018, 5)
window_end   = c(2021, 5)

test_start = c(2021, 6)
test_end   = c(2022, 6)



# FOOT & ANKLE

# convert time/date codes
ts_foot_ankle_priority_1_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_foot_ankle_priority_1_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_foot_ankle_priority_1_enter_by_week.timeseries    <- ts(ts_foot_ankle_priority_1_enter_by_week$frequency,
                                                           start     = c(2016, 9),
                                                           end       = c(2022, 5),
                                                           frequency = 52)

# window to subset timeseries
ts_foot_ankle_priority_1_enter_by_week.testseries <- window(ts_foot_ankle_priority_1_enter_by_week.timeseries,
                                                            start = test_start,
                                                            end   = test_end)
ts_foot_ankle_priority_1_enter_by_week.timeseries <- window(ts_foot_ankle_priority_1_enter_by_week.timeseries,
                                                            start = window_start,
                                                            end   = window_end)
plot(ts_foot_ankle_priority_1_enter_by_week.timeseries, main = "Foot & Ankle Priority 1 Queue Enter Data")



# convert time/date codes
ts_foot_ankle_priority_2_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_foot_ankle_priority_2_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_foot_ankle_priority_2_enter_by_week.timeseries    <- ts(ts_foot_ankle_priority_2_enter_by_week$frequency,
                                                           start     = c(2016, 9),
                                                           end       = c(2022, 5),
                                                           frequency = 52)

# window to subset timeseries
ts_foot_ankle_priority_2_enter_by_week.timeseries <- window(ts_foot_ankle_priority_2_enter_by_week.timeseries,
                                                            start = window_start,
                                                            end   = window_end)
plot(ts_foot_ankle_priority_2_enter_by_week.timeseries, main = "Foot & Ankle Priority 2 Queue Enter Data")




# convert time/date codes
ts_foot_ankle_priority_all_exit_by_week$week_starting <- as.Date( as.numeric(word(ts_foot_ankle_priority_all_exit_by_week$range, 1)), origin = "1900-01-01" )
ts_foot_ankle_priority_all_exit_by_week.timeseries    <- ts(ts_foot_ankle_priority_all_exit_by_week$frequency,
                                                            start     = c(2016, 9),
                                                            end       = c(2022, 5),
                                                            frequency = 52)

# window to subset timeseries
ts_foot_ankle_priority_all_exit_by_week.timeseries <- window(ts_foot_ankle_priority_all_exit_by_week.timeseries,
                                                             start = window_start,
                                                             end   = window_end)
plot(ts_foot_ankle_priority_all_exit_by_week.timeseries, main = "Foot & Ankle Priority All Queue Exit Data")



# HAND

# convert time/date codes
ts_hand_priority_1_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_hand_priority_1_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_hand_priority_1_enter_by_week.timeseries    <- ts(ts_hand_priority_1_enter_by_week$frequency,
                                                     start     = c(2016, 9),
                                                     end       = c(2022, 5),
                                                     frequency = 52)

# window to subset timeseries
ts_hand_priority_1_enter_by_week.testseries <- window(ts_hand_priority_1_enter_by_week.timeseries,
                                                      start = test_start,
                                                      end   = test_end)
ts_hand_priority_1_enter_by_week.timeseries <- window(ts_hand_priority_1_enter_by_week.timeseries,
                                                      start = window_start,
                                                      end   = window_end)
plot(ts_hand_priority_1_enter_by_week.timeseries, main = "Hand Priority 1 Queue Enter Data")



# convert time/date codes
ts_hand_priority_2_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_hand_priority_2_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_hand_priority_2_enter_by_week.timeseries    <- ts(ts_hand_priority_2_enter_by_week$frequency,
                                                     start     = c(2016, 9),
                                                     end       = c(2022, 5),
                                                     frequency = 52)

# window to subset timeseries
ts_hand_priority_2_enter_by_week.timeseries <- window(ts_hand_priority_2_enter_by_week.timeseries,
                                                      start = window_start,
                                                      end   = window_end)
plot(ts_hand_priority_2_enter_by_week.timeseries, main = "Hand Priority 2 Queue Enter Data")




# convert time/date codes
ts_hand_priority_all_exit_by_week$week_starting <- as.Date( as.numeric(word(ts_hand_priority_all_exit_by_week$range, 1)), origin = "1900-01-01" )
ts_hand_priority_all_exit_by_week.timeseries    <- ts(ts_hand_priority_all_exit_by_week$frequency,
                                                      start     = c(2016, 9),
                                                      end       = c(2022, 5),
                                                      frequency = 52)

# window to subset timeseries
ts_hand_priority_all_exit_by_week.timeseries <- window(ts_hand_priority_all_exit_by_week.timeseries,
                                                       start = window_start,
                                                       end   = window_end)
plot(ts_hand_priority_all_exit_by_week.timeseries, main = "Hand Priority All Queue Exit Data")


# HIP

# convert time/date codes
ts_hip_priority_1_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_hip_priority_1_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_hip_priority_1_enter_by_week.timeseries    <- ts(ts_hip_priority_1_enter_by_week$frequency,
                                                    start     = c(2016, 9),
                                                    end       = c(2022, 5),
                                                    frequency = 52)

# window to subset timeseries
ts_hip_priority_1_enter_by_week.testseries <- window(ts_hip_priority_1_enter_by_week.timeseries,
                                                     start = test_start,
                                                     end   = test_end)
ts_hip_priority_1_enter_by_week.timeseries <- window(ts_hip_priority_1_enter_by_week.timeseries,
                                                     start = window_start,
                                                     end   = window_end)
plot(ts_hip_priority_1_enter_by_week.timeseries, main = "Hip Priority 1 Queue Enter Data")



# convert time/date codes
ts_hip_priority_2_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_hip_priority_2_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_hip_priority_2_enter_by_week.timeseries    <- ts(ts_hip_priority_2_enter_by_week$frequency,
                                                    start     = c(2016, 9),
                                                    end       = c(2022, 5),
                                                    frequency = 52)

# window to subset timeseries
ts_hip_priority_2_enter_by_week.timeseries <- window(ts_hip_priority_2_enter_by_week.timeseries,
                                                     start = window_start,
                                                     end   = window_end)
plot(ts_hip_priority_2_enter_by_week.timeseries, main = "Hip Priority 2 Queue Enter Data")




# convert time/date codes
ts_hip_priority_all_exit_by_week$week_starting <- as.Date( as.numeric(word(ts_hip_priority_all_exit_by_week$range, 1)), origin = "1900-01-01" )
ts_hip_priority_all_exit_by_week.timeseries    <- ts(ts_hip_priority_all_exit_by_week$frequency,
                                                     start     = c(2016, 9),
                                                     end       = c(2022, 5),
                                                     frequency = 52)

# window to subset timeseries
ts_hip_priority_all_exit_by_week.timeseries <- window(ts_hip_priority_all_exit_by_week.timeseries,
                                                      start = window_start,
                                                      end   = window_end)
plot(ts_hip_priority_all_exit_by_week.timeseries, main = "Hip Priority All Queue Exit Data")



# HIP & KNEE

# convert time/date codes
ts_hip_knee_priority_1_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_hip_knee_priority_1_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_hip_knee_priority_1_enter_by_week.timeseries    <- ts(ts_hip_knee_priority_1_enter_by_week$frequency,
                                                         start     = c(2016, 9),
                                                         end       = c(2022, 5),
                                                         frequency = 52)

# window to subset timeseries
ts_hip_knee_priority_1_enter_by_week.testseries <- window(ts_hip_knee_priority_1_enter_by_week.timeseries,
                                                          start = test_start,
                                                          end   = test_end)
ts_hip_knee_priority_1_enter_by_week.timeseries <- window(ts_hip_knee_priority_1_enter_by_week.timeseries,
                                                          start = window_start,
                                                          end   = window_end)
plot(ts_hip_knee_priority_1_enter_by_week.timeseries, main = "Hip & Knee Priority 1 Queue Enter Data")



# convert time/date codes
ts_hip_knee_priority_2_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_hip_knee_priority_2_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_hip_knee_priority_2_enter_by_week.timeseries    <- ts(ts_hip_knee_priority_2_enter_by_week$frequency,
                                                         start     = c(2016, 9),
                                                         end       = c(2022, 5),
                                                         frequency = 52)

# window to subset timeseries
ts_hip_knee_priority_2_enter_by_week.timeseries <- window(ts_hip_knee_priority_2_enter_by_week.timeseries,
                                                          start = window_start,
                                                          end   = window_end)
plot(ts_hip_knee_priority_2_enter_by_week.timeseries, main = "Hip & Knee Priority 2 Queue Enter Data")




# convert time/date codes
ts_hip_knee_priority_all_exit_by_week$week_starting <- as.Date( as.numeric(word(ts_hip_knee_priority_all_exit_by_week$range, 1)), origin = "1900-01-01" )
ts_hip_knee_priority_all_exit_by_week.timeseries    <- ts(ts_hip_knee_priority_all_exit_by_week$frequency,
                                                          start     = c(2016, 9),
                                                          end       = c(2022, 5),
                                                          frequency = 52)

# window to subset timeseries
ts_hip_knee_priority_all_exit_by_week.timeseries <- window(ts_hip_knee_priority_all_exit_by_week.timeseries,
                                                           start = window_start,
                                                           end   = window_end)
plot(ts_hip_knee_priority_all_exit_by_week.timeseries, main = "Hip & Knee Priority All Queue Exit Data")



# KNEE

# convert time/date codes
ts_knee_priority_1_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_knee_priority_1_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_knee_priority_1_enter_by_week.timeseries    <- ts(ts_knee_priority_1_enter_by_week$frequency,
                                                     start     = c(2016, 9),
                                                     end       = c(2022, 5),
                                                     frequency = 52)

# window to subset timeseries
ts_knee_priority_1_enter_by_week.testseries <- window(ts_knee_priority_1_enter_by_week.timeseries,
                                                      start = test_start,
                                                      end   = test_end)
ts_knee_priority_1_enter_by_week.timeseries <- window(ts_knee_priority_1_enter_by_week.timeseries,
                                                      start = window_start,
                                                      end   = window_end)
plot(ts_knee_priority_1_enter_by_week.timeseries, main = "Knee Priority 1 Queue Enter Data")



# convert time/date codes
ts_knee_priority_2_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_knee_priority_2_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_knee_priority_2_enter_by_week.timeseries    <- ts(ts_knee_priority_2_enter_by_week$frequency,
                                                     start     = c(2016, 9),
                                                     end       = c(2022, 5),
                                                     frequency = 52)

# window to subset timeseries
ts_knee_priority_2_enter_by_week.timeseries <- window(ts_knee_priority_2_enter_by_week.timeseries,
                                                      start = window_start,
                                                      end   = window_end)
plot(ts_knee_priority_2_enter_by_week.timeseries, main = "Knee Priority 2 Queue Enter Data")




# convert time/date codes
ts_knee_priority_all_exit_by_week$week_starting <- as.Date( as.numeric(word(ts_knee_priority_all_exit_by_week$range, 1)), origin = "1900-01-01" )
ts_knee_priority_all_exit_by_week.timeseries    <- ts(ts_knee_priority_all_exit_by_week$frequency,
                                                      start     = c(2016, 9),
                                                      end       = c(2022, 5),
                                                      frequency = 52)

# window to subset timeseries
ts_knee_priority_all_exit_by_week.timeseries <- window(ts_knee_priority_all_exit_by_week.timeseries,
                                                       start = window_start,
                                                       end   = window_end)
plot(ts_knee_priority_all_exit_by_week.timeseries, main = "Knee Priority All Queue Exit Data")



# SHOULDER

# convert time/date codes
ts_shoulder_priority_1_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_shoulder_priority_1_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_shoulder_priority_1_enter_by_week.timeseries    <- ts(ts_shoulder_priority_1_enter_by_week$frequency,
                                                         start     = c(2016, 9),
                                                         end       = c(2022, 5),
                                                         frequency = 52)

# window to subset timeseries
ts_shoulder_priority_1_enter_by_week.testseries <- window(ts_shoulder_priority_1_enter_by_week.timeseries,
                                                          start = test_start,
                                                          end   = test_end)
ts_shoulder_priority_1_enter_by_week.timeseries <- window(ts_shoulder_priority_1_enter_by_week.timeseries,
                                                          start = window_start,
                                                          end   = window_end)
plot(ts_shoulder_priority_1_enter_by_week.timeseries, main = "Shoulder Priority 1 Queue Enter Data")



# convert time/date codes
ts_shoulder_priority_2_enter_by_week$week_starting <- as.Date( as.numeric(word(ts_shoulder_priority_2_enter_by_week$range, 1)), origin = "1900-01-01" )
ts_shoulder_priority_2_enter_by_week.timeseries    <- ts(ts_shoulder_priority_2_enter_by_week$frequency,
                                                         start     = c(2016, 9),
                                                         end       = c(2022, 5),
                                                         frequency = 52)

# window to subset timeseries
ts_shoulder_priority_2_enter_by_week.timeseries <- window(ts_shoulder_priority_2_enter_by_week.timeseries,
                                                          start = window_start,
                                                          end   = window_end)
plot(ts_shoulder_priority_2_enter_by_week.timeseries, main = "Shoulder Priority 2 Queue Enter Data")




# convert time/date codes
ts_shoulder_priority_all_exit_by_week$week_starting <- as.Date( as.numeric(word(ts_shoulder_priority_all_exit_by_week$range, 1)), origin = "1900-01-01" )
ts_shoulder_priority_all_exit_by_week.timeseries    <- ts(ts_shoulder_priority_all_exit_by_week$frequency,
                                                          start     = c(2016, 9),
                                                          end       = c(2022, 5),
                                                          frequency = 52)

# window to subset timeseries
ts_shoulder_priority_all_exit_by_week.timeseries <- window(ts_shoulder_priority_all_exit_by_week.timeseries,
                                                           start = window_start,
                                                           end   = window_end)
plot(ts_shoulder_priority_all_exit_by_week.timeseries, main = "Shoulder Priority All Queue Exit Data")






# ----- Forecasting -----


# FOOT & ANKLE

#  PRIORITY 1 ENTRY

# Build TBATS Model
ts_foot_ankle_priority_1_enter_by_week.tbats <- tbats(ts_foot_ankle_priority_1_enter_by_week.timeseries)
summary(ts_foot_ankle_priority_1_enter_by_week.tbats)

# Predict TBATS Values
ts_foot_ankle_priority_1_enter_by_week.tbats.forecast <- forecast(ts_foot_ankle_priority_1_enter_by_week.tbats, h = 12)
plot(ts_foot_ankle_priority_1_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Foot & Ankle Priority 1 Queue Entry TBATS Model Prediction")


# PRIORITY 2 ENTRY

# Build TBATS Model
ts_foot_ankle_priority_2_enter_by_week.tbats <- tbats(ts_foot_ankle_priority_2_enter_by_week.timeseries)
summary(ts_foot_ankle_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_foot_ankle_priority_2_enter_by_week.tbats.forecast <- forecast(ts_foot_ankle_priority_2_enter_by_week.tbats, h = 12)
plot(ts_foot_ankle_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Foot & Ankle Priority 2 Queue Entry TBATS Model Prediction")


# PRIORITY ALL EXIT

# Build TBATS Model
ts_foot_ankle_priority_all_exit_by_week.tbats <- tbats(ts_foot_ankle_priority_all_exit_by_week.timeseries)
summary(ts_foot_ankle_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_foot_ankle_priority_all_exit_by_week.tbats.forecast <- forecast(ts_foot_ankle_priority_all_exit_by_week.tbats, h = 12)
plot(ts_foot_ankle_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Foot & Ankle Priority All Queue Exit TBATS Model Prediction")



# HAND

#  PRIORITY 1 ENTRY

# Build TBATS Model
ts_hand_priority_1_enter_by_week.tbats <- tbats(ts_hand_priority_1_enter_by_week.timeseries)
summary(ts_hand_priority_1_enter_by_week.tbats)

# Predict TBATS Values
ts_hand_priority_1_enter_by_week.tbats.forecast <- forecast(ts_hand_priority_1_enter_by_week.tbats, h = 12)
plot(ts_hand_priority_1_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hand Priority 1 Queue Entry TBATS Model Prediction")


# PRIORITY 2 ENTRY

# Build TBATS Model
ts_hand_priority_2_enter_by_week.tbats <- tbats(ts_hand_priority_2_enter_by_week.timeseries)
summary(ts_hand_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_hand_priority_2_enter_by_week.tbats.forecast <- forecast(ts_hand_priority_2_enter_by_week.tbats, h = 12)
plot(ts_hand_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hand Priority 2 Queue Entry TBATS Model Prediction")


# PRIORITY ALL EXIT

# Build TBATS Model
ts_hand_priority_all_exit_by_week.tbats <- tbats(ts_hand_priority_all_exit_by_week.timeseries)
summary(ts_hand_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_hand_priority_all_exit_by_week.tbats.forecast <- forecast(ts_hand_priority_all_exit_by_week.tbats, h = 12)
plot(ts_hand_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hand Priority All Queue Exit TBATS Model Prediction")



# HIP

#  PRIORITY 1 ENTRY

# Build TBATS Model
ts_hip_priority_1_enter_by_week.tbats <- tbats(ts_hip_priority_1_enter_by_week.timeseries)
summary(ts_hip_priority_1_enter_by_week.tbats)

# Predict TBATS Values
ts_hip_priority_1_enter_by_week.tbats.forecast <- forecast(ts_hip_priority_1_enter_by_week.tbats, h = 12)
plot(ts_hip_priority_1_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hip Priority 1 Queue Entry TBATS Model Prediction")


# PRIORITY 2 ENTRY

# Build TBATS Model
ts_hip_priority_2_enter_by_week.tbats <- tbats(ts_hip_priority_2_enter_by_week.timeseries)
summary(ts_hip_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_hip_priority_2_enter_by_week.tbats.forecast <- forecast(ts_hip_priority_2_enter_by_week.tbats, h = 12)
plot(ts_hip_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hip Priority 2 Queue Entry TBATS Model Prediction")


# PRIORITY ALL EXIT

# Build TBATS Model
ts_hip_priority_all_exit_by_week.tbats <- tbats(ts_hip_priority_all_exit_by_week.timeseries)
summary(ts_hip_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_hip_priority_all_exit_by_week.tbats.forecast <- forecast(ts_hip_priority_all_exit_by_week.tbats, h = 12)
plot(ts_hip_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hip Priority All Queue Exit TBATS Model Prediction")



# HIP & KNEE

#  PRIORITY 1 ENTRY

# Build TBATS Model
ts_hip_knee_priority_1_enter_by_week.tbats <- tbats(ts_hip_knee_priority_1_enter_by_week.timeseries)
summary(ts_hip_knee_priority_1_enter_by_week.tbats)

# Predict TBATS Values
ts_hip_knee_priority_1_enter_by_week.tbats.forecast <- forecast(ts_hip_knee_priority_1_enter_by_week.tbats, h = 12)
plot(ts_hip_knee_priority_1_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hip & Knee Priority 1 Queue Entry TBATS Model Prediction")


# PRIORITY 2 ENTRY

# Build TBATS Model
ts_hip_knee_priority_2_enter_by_week.tbats <- tbats(ts_hip_knee_priority_2_enter_by_week.timeseries)
summary(ts_hip_knee_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_hip_knee_priority_2_enter_by_week.tbats.forecast <- forecast(ts_hip_knee_priority_2_enter_by_week.tbats, h = 12)
plot(ts_hip_knee_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hip & Knee Priority 2 Queue Entry TBATS Model Prediction")


# PRIORITY ALL EXIT

# Build TBATS Model
ts_hip_knee_priority_all_exit_by_week.tbats <- tbats(ts_hip_knee_priority_all_exit_by_week.timeseries)
summary(ts_hip_knee_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_hip_knee_priority_all_exit_by_week.tbats.forecast <- forecast(ts_hip_knee_priority_all_exit_by_week.tbats, h = 12)
plot(ts_hip_knee_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hip & Knee Priority All Queue Exit TBATS Model Prediction")



# KNEE

#  PRIORITY 1 ENTRY

# Build TBATS Model
ts_knee_priority_1_enter_by_week.tbats <- tbats(ts_knee_priority_1_enter_by_week.timeseries)
summary(ts_knee_priority_1_enter_by_week.tbats)

# Predict TBATS Values
ts_knee_priority_1_enter_by_week.tbats.forecast <- forecast(ts_knee_priority_1_enter_by_week.tbats, h = 12)
plot(ts_knee_priority_1_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Knee Priority 1 Queue Entry TBATS Model Prediction")


# PRIORITY 2 ENTRY

# Build TBATS Model
ts_knee_priority_2_enter_by_week.tbats <- tbats(ts_knee_priority_2_enter_by_week.timeseries)
summary(ts_knee_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_knee_priority_2_enter_by_week.tbats.forecast <- forecast(ts_knee_priority_2_enter_by_week.tbats, h = 12)
plot(ts_knee_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Knee Priority 2 Queue Entry TBATS Model Prediction")


# PRIORITY ALL EXIT

# Build TBATS Model
ts_knee_priority_all_exit_by_week.tbats <- tbats(ts_knee_priority_all_exit_by_week.timeseries)
summary(ts_knee_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_knee_priority_all_exit_by_week.tbats.forecast <- forecast(ts_knee_priority_all_exit_by_week.tbats, h = 12)
plot(ts_knee_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Knee Priority All Queue Exit TBATS Model Prediction")



# SHOULDER

#  PRIORITY 1 ENTRY

# Build TBATS Model
ts_shoulder_priority_1_enter_by_week.tbats <- tbats(ts_shoulder_priority_1_enter_by_week.timeseries)
summary(ts_shoulder_priority_1_enter_by_week.tbats)

# Predict TBATS Values
ts_shoulder_priority_1_enter_by_week.tbats.forecast <- forecast(ts_shoulder_priority_1_enter_by_week.tbats, h = 12)
plot(ts_shoulder_priority_1_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Shoulder Priority 1 Queue Entry TBATS Model Prediction")


# PRIORITY 2 ENTRY

# Build TBATS Model
ts_shoulder_priority_2_enter_by_week.tbats <- tbats(ts_shoulder_priority_2_enter_by_week.timeseries)
summary(ts_shoulder_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_shoulder_priority_2_enter_by_week.tbats.forecast <- forecast(ts_shoulder_priority_2_enter_by_week.tbats, h = 12)
plot(ts_shoulder_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Shoulder Priority 2 Queue Entry TBATS Model Prediction")


# PRIORITY ALL EXIT

# Build TBATS Model
ts_shoulder_priority_all_exit_by_week.tbats <- tbats(ts_shoulder_priority_all_exit_by_week.timeseries)
summary(ts_shoulder_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_shoulder_priority_all_exit_by_week.tbats.forecast <- forecast(ts_shoulder_priority_all_exit_by_week.tbats, h = 12)
plot(ts_shoulder_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Shoulder Priority All Queue Exit TBATS Model Prediction")




# ----- Extracting Appropriate Error Values from Forecasts -----


# FOOT & ANKLE

# PRIORITY 1 ENTER

# syntax sugar for readability
fc <- ts_foot_ankle_priority_1_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.footankle.p1.enter.mean <- fc$mean
fc.footankle.p1.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.footankle.p1.enter.res  <- residuals(fc)
fc.footankle.p1.enter.mean
fc.footankle.p1.enter.sd
fc.footankle.p1.enter.res


# PRIORITY 2 ENTER

# syntax sugar for readability
fc <- ts_foot_ankle_priority_2_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.footankle.p2.enter.mean <- fc$mean
fc.footankle.p2.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.footankle.p2.enter.res  <- residuals(fc)
fc.footankle.p2.enter.mean
fc.footankle.p2.enter.sd
fc.footankle.p2.enter.res


# PRIORITY ALL EXIT

# syntax sugar for readability
fc <- ts_foot_ankle_priority_all_exit_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.footankle.pALL.exit.mean <- fc$mean
fc.footankle.pALL.exit.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.footankle.pALL.exit.res  <- residuals(fc)
fc.footankle.pALL.exit.mean
fc.footankle.pALL.exit.sd
fc.footankle.pALL.exit.res


# HAND

# PRIORITY 1 ENTER

# syntax sugar for readability
fc <- ts_hand_priority_1_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hand.p1.enter.mean <- fc$mean
fc.hand.p1.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hand.p1.enter.res  <- residuals(fc)
fc.hand.p1.enter.mean
fc.hand.p1.enter.sd
fc.hand.p1.enter.res


# PRIORITY 2 ENTER

# syntax sugar for readability
fc <- ts_hand_priority_2_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hand.p2.enter.mean <- fc$mean
fc.hand.p2.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hand.p2.enter.res  <- residuals(fc)
fc.hand.p2.enter.mean
fc.hand.p2.enter.sd
fc.hand.p2.enter.res


# PRIORITY ALL EXIT

# syntax sugar for readability
fc <- ts_hand_priority_all_exit_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hand.pALL.exit.mean <- fc$mean
fc.hand.pALL.exit.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hand.pALL.exit.res  <- residuals(fc)
fc.hand.pALL.exit.mean
fc.hand.pALL.exit.sd
fc.hand.pALL.exit.res


# HIP

# PRIORITY 1 ENTER

# syntax sugar for readability
fc <- ts_hip_priority_1_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hip.p1.enter.mean <- fc$mean
fc.hip.p1.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hip.p1.enter.res  <- residuals(fc)
fc.hip.p1.enter.mean
fc.hip.p1.enter.sd
fc.hip.p1.enter.res


# PRIORITY 2 ENTER

# syntax sugar for readability
fc <- ts_hip_priority_2_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hip.p2.enter.mean <- fc$mean
fc.hip.p2.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hip.p2.enter.res  <- residuals(fc)
fc.hip.p2.enter.mean
fc.hip.p2.enter.sd
fc.hip.p2.enter.res


# PRIORITY ALL EXIT

# syntax sugar for readability
fc <- ts_hip_priority_all_exit_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hip.pALL.exit.mean <- fc$mean
fc.hip.pALL.exit.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hip.pALL.exit.res  <- residuals(fc)
fc.hip.pALL.exit.mean
fc.hip.pALL.exit.sd
fc.hip.pALL.exit.res


# HIP & KNEE

# PRIORITY 1 ENTER

# syntax sugar for readability
fc <- ts_hip_knee_priority_1_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hipknee.p1.enter.mean <- fc$mean
fc.hipknee.p1.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hipknee.p1.enter.res  <- residuals(fc)
fc.hipknee.p1.enter.mean
fc.hipknee.p1.enter.sd
fc.hipknee.p1.enter.res


# PRIORITY 2 ENTER

# syntax sugar for readability
fc <- ts_hip_knee_priority_2_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hipknee.p2.enter.mean <- fc$mean
fc.hipknee.p2.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hipknee.p2.enter.res  <- residuals(fc)
fc.hipknee.p2.enter.mean
fc.hipknee.p2.enter.sd
fc.hipknee.p2.enter.res


# PRIORITY ALL EXIT

# syntax sugar for readability
fc <- ts_hip_knee_priority_all_exit_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.hipknee.pALL.exit.mean <- fc$mean
fc.hipknee.pALL.exit.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.hipknee.pALL.exit.res  <- residuals(fc)
fc.hipknee.pALL.exit.mean
fc.hipknee.pALL.exit.sd
fc.hipknee.pALL.exit.res



# KNEE

# PRIORITY 1 ENTER

# syntax sugar for readability
fc <- ts_knee_priority_1_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.knee.p1.enter.mean <- fc$mean
fc.knee.p1.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.knee.p1.enter.res  <- residuals(fc)
fc.knee.p1.enter.mean
fc.knee.p1.enter.sd
fc.knee.p1.enter.res


# PRIORITY 2 ENTER

# syntax sugar for readability
fc <- ts_knee_priority_2_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.knee.p2.enter.mean <- fc$mean
fc.knee.p2.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.knee.p2.enter.res  <- residuals(fc)
fc.knee.p2.enter.mean
fc.knee.p2.enter.sd
fc.knee.p2.enter.res


# PRIORITY ALL EXIT

# syntax sugar for readability
fc <- ts_knee_priority_all_exit_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.knee.pALL.exit.mean <- fc$mean
fc.knee.pALL.exit.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.knee.pALL.exit.res  <- residuals(fc)
fc.knee.pALL.exit.mean
fc.knee.pALL.exit.sd
fc.knee.pALL.exit.res



# SHOULDER

# PRIORITY 1 ENTER

# syntax sugar for readability
fc <- ts_shoulder_priority_1_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.shoulder.p1.enter.mean <- fc$mean
fc.shoulder.p1.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.shoulder.p1.enter.res  <- residuals(fc)
fc.shoulder.p1.enter.mean
fc.shoulder.p1.enter.sd
fc.shoulder.p1.enter.res


# PRIORITY 2 ENTER

# syntax sugar for readability
fc <- ts_shoulder_priority_2_enter_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.shoulder.p2.enter.mean <- fc$mean
fc.shoulder.p2.enter.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.shoulder.p2.enter.res  <- residuals(fc)
fc.shoulder.p2.enter.mean
fc.shoulder.p2.enter.sd
fc.shoulder.p2.enter.res


# PRIORITY ALL EXIT

# syntax sugar for readability
fc <- ts_shoulder_priority_all_exit_by_week.tbats.forecast
fc$mean
fc$upper[,2] # 95% confidence interval
fc$lower[,2] # 95% confidence interval
residuals(fc)

# extract distribution information
fc.shoulder.pALL.exit.mean <- fc$mean
fc.shoulder.pALL.exit.sd   <- (fc$upper[,2] - fc$lower[,2]) / (2 * qnorm(.5 + fc$level[1] / 200))
fc.shoulder.pALL.exit.res  <- residuals(fc)
fc.shoulder.pALL.exit.mean
fc.shoulder.pALL.exit.sd
fc.shoulder.pALL.exit.res




# ----- Fitting Discretised, Truncated, Re-Normalised Predictive Distribution -----

# FOOT & ANKLE

# PRIORITY 1 ENTER

mean(fc.footankle.p1.enter.res)
sd(fc.footankle.p1.enter.res)
max(fc.footankle.p1.enter.res)
min(fc.footankle.p1.enter.res)

density( fc.footankle.p1.enter.res )
ecdf( fc.footankle.p1.enter.res )

fc.footankle.p1.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.footankle.p1.enter.res )
  return( cdf(x) )
}

fc.footankle.p1.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.footankle.p1.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}


plot(fc.footankle.p1.enter.res,
     main = "Foot & Ankle Priority 1 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.footankle.p1.enter.res ),
     main = "Foot & Ankle Priority 1 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.footankle.p1.enter.res ),
     main = "Foot & Ankle Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.footankle.p1.enter.res.cdf,
     -10,
     10,
     main = "Foot & Ankle Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.footankle.p1.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "Foot & Ankle Priority 1 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY 2 ENTER

mean(fc.footankle.p2.enter.res)
sd(fc.footankle.p2.enter.res)
max(fc.footankle.p2.enter.res)
min(fc.footankle.p2.enter.res)

density( fc.footankle.p2.enter.res )
ecdf( fc.footankle.p2.enter.res )

fc.footankle.p2.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.footankle.p2.enter.res )
  return( cdf(x) )
}

fc.footankle.p2.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.footankle.p2.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.footankle.p2.enter.res,
     main = "Foot & Ankle Priority 2 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.footankle.p2.enter.res ),
     main = "Foot & Ankle Priority 2 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.footankle.p2.enter.res ),
     main = "Foot & Ankle Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.footankle.p2.enter.res.cdf,
     -10,
     10,
     main = "Foot & Ankle Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.footankle.p2.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "Foot & Ankle Priority 2 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY ALL EXIT

mean(fc.footankle.pALL.exit.res)
sd(fc.footankle.pALL.exit.res)
max(fc.footankle.pALL.exit.res)
min(fc.footankle.pALL.exit.res)

density( fc.footankle.pALL.exit.res )
ecdf( fc.footankle.pALL.exit.res )

fc.footankle.pALL.exit.res.cdf <- function(x) {
  cdf <- ecdf( fc.footankle.pALL.exit.res )
  return( cdf(x) )
}

fc.footankle.pALL.exit.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.footankle.pALL.exit.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.footankle.pALL.exit.res,
     main = "Foot & Ankle Priority ALL Exit Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.footankle.pALL.exit.res ),
     main = "Foot & Ankle Priority ALL Exit Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.footankle.pALL.exit.res ),
     main = "Foot & Ankle Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.footankle.pALL.exit.res.cdf,
     -10,
     10,
     main = "Foot & Ankle Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.footankle.pALL.exit.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "Foot & Ankle Priority ALL Exit Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")


# Hand

# PRIORITY 1 ENTER

mean(fc.hand.p1.enter.res)
sd(fc.hand.p1.enter.res)
max(fc.hand.p1.enter.res)
min(fc.hand.p1.enter.res)

density( fc.hand.p1.enter.res )
ecdf( fc.hand.p1.enter.res )

fc.hand.p1.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.hand.p1.enter.res )
  return( cdf(x) )
}

fc.hand.p1.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hand.p1.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}


plot(fc.hand.p1.enter.res,
     main = "Hand Priority 1 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hand.p1.enter.res ),
     main = "Hand Priority 1 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hand.p1.enter.res ),
     main = "Hand Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hand.p1.enter.res.cdf,
     -10,
     10,
     main = "Hand Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hand.p1.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "Hand Priority 1 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY 2 ENTER

mean(fc.hand.p2.enter.res)
sd(fc.hand.p2.enter.res)
max(fc.hand.p2.enter.res)
min(fc.hand.p2.enter.res)

density( fc.hand.p2.enter.res )
ecdf( fc.hand.p2.enter.res )

fc.hand.p2.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.hand.p2.enter.res )
  return( cdf(x) )
}

fc.hand.p2.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hand.p2.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.hand.p2.enter.res,
     main = "Hand Priority 2 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hand.p2.enter.res ),
     main = "Hand Priority 2 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hand.p2.enter.res ),
     main = "Hand Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hand.p2.enter.res.cdf,
     -10,
     10,
     main = "Hand Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hand.p2.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "Hand Priority 2 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY ALL EXIT

mean(fc.hand.pALL.exit.res)
sd(fc.hand.pALL.exit.res)
max(fc.hand.pALL.exit.res)
min(fc.hand.pALL.exit.res)

density( fc.hand.pALL.exit.res )
ecdf( fc.hand.pALL.exit.res )

fc.hand.pALL.exit.res.cdf <- function(x) {
  cdf <- ecdf( fc.hand.pALL.exit.res )
  return( cdf(x) )
}

fc.hand.pALL.exit.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hand.pALL.exit.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.hand.pALL.exit.res,
     main = "Hand Priority ALL Exit Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hand.pALL.exit.res ),
     main = "Hand Priority ALL Exit Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hand.pALL.exit.res ),
     main = "Hand Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hand.pALL.exit.res.cdf,
     -10,
     10,
     main = "Hand Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hand.pALL.exit.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "Hand Priority ALL Exit Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")


# hip

# PRIORITY 1 ENTER

mean(fc.hip.p1.enter.res)
sd(fc.hip.p1.enter.res)
max(fc.hip.p1.enter.res)
min(fc.hip.p1.enter.res)

density( fc.hip.p1.enter.res )
ecdf( fc.hip.p1.enter.res )

fc.hip.p1.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.hip.p1.enter.res )
  return( cdf(x) )
}

fc.hip.p1.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hip.p1.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}


plot(fc.hip.p1.enter.res,
     main = "hip Priority 1 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hip.p1.enter.res ),
     main = "hip Priority 1 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hip.p1.enter.res ),
     main = "hip Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hip.p1.enter.res.cdf,
     -10,
     10,
     main = "hip Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hip.p1.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "hip Priority 1 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY 2 ENTER

mean(fc.hip.p2.enter.res)
sd(fc.hip.p2.enter.res)
max(fc.hip.p2.enter.res)
min(fc.hip.p2.enter.res)

density( fc.hip.p2.enter.res )
ecdf( fc.hip.p2.enter.res )

fc.hip.p2.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.hip.p2.enter.res )
  return( cdf(x) )
}

fc.hip.p2.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hip.p2.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.hip.p2.enter.res,
     main = "hip Priority 2 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hip.p2.enter.res ),
     main = "hip Priority 2 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hip.p2.enter.res ),
     main = "hip Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hip.p2.enter.res.cdf,
     -10,
     10,
     main = "hip Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hip.p2.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "hip Priority 2 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY ALL EXIT

mean(fc.hip.pALL.exit.res)
sd(fc.hip.pALL.exit.res)
max(fc.hip.pALL.exit.res)
min(fc.hip.pALL.exit.res)

density( fc.hip.pALL.exit.res )
ecdf( fc.hip.pALL.exit.res )

fc.hip.pALL.exit.res.cdf <- function(x) {
  cdf <- ecdf( fc.hip.pALL.exit.res )
  return( cdf(x) )
}

fc.hip.pALL.exit.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hip.pALL.exit.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.hip.pALL.exit.res,
     main = "hip Priority ALL Exit Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hip.pALL.exit.res ),
     main = "hip Priority ALL Exit Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hip.pALL.exit.res ),
     main = "hip Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hip.pALL.exit.res.cdf,
     -10,
     10,
     main = "hip Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hip.pALL.exit.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "hip Priority ALL Exit Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")


# hipknee

# PRIORITY 1 ENTER

mean(fc.hipknee.p1.enter.res)
sd(fc.hipknee.p1.enter.res)
max(fc.hipknee.p1.enter.res)
min(fc.hipknee.p1.enter.res)

density( fc.hipknee.p1.enter.res )
ecdf( fc.hipknee.p1.enter.res )

fc.hipknee.p1.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.hipknee.p1.enter.res )
  return( cdf(x) )
}

fc.hipknee.p1.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hipknee.p1.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}


plot(fc.hipknee.p1.enter.res,
     main = "hipknee Priority 1 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hipknee.p1.enter.res ),
     main = "hipknee Priority 1 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hipknee.p1.enter.res ),
     main = "hipknee Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hipknee.p1.enter.res.cdf,
     -10,
     10,
     main = "hipknee Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hipknee.p1.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "hipknee Priority 1 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY 2 ENTER

mean(fc.hipknee.p2.enter.res)
sd(fc.hipknee.p2.enter.res)
max(fc.hipknee.p2.enter.res)
min(fc.hipknee.p2.enter.res)

density( fc.hipknee.p2.enter.res )
ecdf( fc.hipknee.p2.enter.res )

fc.hipknee.p2.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.hipknee.p2.enter.res )
  return( cdf(x) )
}

fc.hipknee.p2.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hipknee.p2.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.hipknee.p2.enter.res,
     main = "hipknee Priority 2 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hipknee.p2.enter.res ),
     main = "hipknee Priority 2 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hipknee.p2.enter.res ),
     main = "hipknee Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hipknee.p2.enter.res.cdf,
     -10,
     10,
     main = "hipknee Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hipknee.p2.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "hipknee Priority 2 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY ALL EXIT

mean(fc.hipknee.pALL.exit.res)
sd(fc.hipknee.pALL.exit.res)
max(fc.hipknee.pALL.exit.res)
min(fc.hipknee.pALL.exit.res)

density( fc.hipknee.pALL.exit.res )
ecdf( fc.hipknee.pALL.exit.res )

fc.hipknee.pALL.exit.res.cdf <- function(x) {
  cdf <- ecdf( fc.hipknee.pALL.exit.res )
  return( cdf(x) )
}

fc.hipknee.pALL.exit.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.hipknee.pALL.exit.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.hipknee.pALL.exit.res,
     main = "hipknee Priority ALL Exit Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.hipknee.pALL.exit.res ),
     main = "hipknee Priority ALL Exit Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.hipknee.pALL.exit.res ),
     main = "hipknee Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.hipknee.pALL.exit.res.cdf,
     -10,
     10,
     main = "hipknee Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.hipknee.pALL.exit.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "hipknee Priority ALL Exit Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")


# knee

# PRIORITY 1 ENTER

mean(fc.knee.p1.enter.res)
sd(fc.knee.p1.enter.res)
max(fc.knee.p1.enter.res)
min(fc.knee.p1.enter.res)

density( fc.knee.p1.enter.res )
ecdf( fc.knee.p1.enter.res )

fc.knee.p1.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.knee.p1.enter.res )
  return( cdf(x) )
}

fc.knee.p1.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.knee.p1.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}


plot(fc.knee.p1.enter.res,
     main = "knee Priority 1 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.knee.p1.enter.res ),
     main = "knee Priority 1 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.knee.p1.enter.res ),
     main = "knee Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.knee.p1.enter.res.cdf,
     -10,
     10,
     main = "knee Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.knee.p1.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "knee Priority 1 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY 2 ENTER

mean(fc.knee.p2.enter.res)
sd(fc.knee.p2.enter.res)
max(fc.knee.p2.enter.res)
min(fc.knee.p2.enter.res)

density( fc.knee.p2.enter.res )
ecdf( fc.knee.p2.enter.res )

fc.knee.p2.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.knee.p2.enter.res )
  return( cdf(x) )
}

fc.knee.p2.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.knee.p2.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.knee.p2.enter.res,
     main = "knee Priority 2 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.knee.p2.enter.res ),
     main = "knee Priority 2 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.knee.p2.enter.res ),
     main = "knee Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.knee.p2.enter.res.cdf,
     -10,
     10,
     main = "knee Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.knee.p2.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "knee Priority 2 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY ALL EXIT

mean(fc.knee.pALL.exit.res)
sd(fc.knee.pALL.exit.res)
max(fc.knee.pALL.exit.res)
min(fc.knee.pALL.exit.res)

density( fc.knee.pALL.exit.res )
ecdf( fc.knee.pALL.exit.res )

fc.knee.pALL.exit.res.cdf <- function(x) {
  cdf <- ecdf( fc.knee.pALL.exit.res )
  return( cdf(x) )
}

fc.knee.pALL.exit.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.knee.pALL.exit.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.knee.pALL.exit.res,
     main = "knee Priority ALL Exit Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.knee.pALL.exit.res ),
     main = "knee Priority ALL Exit Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.knee.pALL.exit.res ),
     main = "knee Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.knee.pALL.exit.res.cdf,
     -10,
     10,
     main = "knee Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.knee.pALL.exit.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "knee Priority ALL Exit Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")


# shoulder

# PRIORITY 1 ENTER

mean(fc.shoulder.p1.enter.res)
sd(fc.shoulder.p1.enter.res)
max(fc.shoulder.p1.enter.res)
min(fc.shoulder.p1.enter.res)

density( fc.shoulder.p1.enter.res )
ecdf( fc.shoulder.p1.enter.res )

fc.shoulder.p1.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.shoulder.p1.enter.res )
  return( cdf(x) )
}

fc.shoulder.p1.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.shoulder.p1.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}


plot(fc.shoulder.p1.enter.res,
     main = "shoulder Priority 1 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.shoulder.p1.enter.res ),
     main = "shoulder Priority 1 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.shoulder.p1.enter.res ),
     main = "shoulder Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.shoulder.p1.enter.res.cdf,
     -10,
     10,
     main = "shoulder Priority 1 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.shoulder.p1.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "shoulder Priority 1 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY 2 ENTER

mean(fc.shoulder.p2.enter.res)
sd(fc.shoulder.p2.enter.res)
max(fc.shoulder.p2.enter.res)
min(fc.shoulder.p2.enter.res)

density( fc.shoulder.p2.enter.res )
ecdf( fc.shoulder.p2.enter.res )

fc.shoulder.p2.enter.res.cdf <- function(x) {
  cdf <- ecdf( fc.shoulder.p2.enter.res )
  return( cdf(x) )
}

fc.shoulder.p2.enter.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.shoulder.p2.enter.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.shoulder.p2.enter.res,
     main = "shoulder Priority 2 Entry Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.shoulder.p2.enter.res ),
     main = "shoulder Priority 2 Entry Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.shoulder.p2.enter.res ),
     main = "shoulder Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.shoulder.p2.enter.res.cdf,
     -10,
     10,
     main = "shoulder Priority 2 Entry Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.shoulder.p2.enter.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "shoulder Priority 2 Entry Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")




# PRIORITY ALL EXIT

mean(fc.shoulder.pALL.exit.res)
sd(fc.shoulder.pALL.exit.res)
max(fc.shoulder.pALL.exit.res)
min(fc.shoulder.pALL.exit.res)

density( fc.shoulder.pALL.exit.res )
ecdf( fc.shoulder.pALL.exit.res )

fc.shoulder.pALL.exit.res.cdf <- function(x) {
  cdf <- ecdf( fc.shoulder.pALL.exit.res )
  return( cdf(x) )
}

fc.shoulder.pALL.exit.res.inversecdf <- function(x) {
  try.seq <- seq(from = -10, to = 10, by = 1)
  
  for (t in try.seq) {
    cdf.value <- fc.shoulder.pALL.exit.res.cdf(t)
    if (cdf.value >= x) {
      return (t)
    }
  }
  
  return (max(try.seq))
}

plot(fc.shoulder.pALL.exit.res,
     main = "shoulder Priority ALL Exit Forecast Residual Series",
     xlab = "Time",
     ylab = "Residual")

plot(density( fc.shoulder.pALL.exit.res ),
     main = "shoulder Priority ALL Exit Forecast Residual Distribution",
     xlab = "Residual",
     ylab = "Probability")

plot(ecdf( fc.shoulder.pALL.exit.res ),
     main = "shoulder Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

plot(fc.shoulder.pALL.exit.res.cdf,
     -10,
     10,
     main = "shoulder Priority ALL Exit Forecast Residual CDF",
     xlab = "Residual",
     ylab = "CDF")

barplot(fc.shoulder.pALL.exit.res.cdf( seq(from = -10, to = 10, by = 1) ),
        main = "shoulder Priority ALL Exit Forecast Residual Discretised CDF",
        names.arg = seq(from = -10, to = 10, by = 1),
        xlab = "Residual",
        ylab = "CDF")





# ----- Sampling from DTRN Predictive Distribution -----

# generate.p1.enter.CDF.sample <- function() {
#   p      <- runif(1, min = 0, max = 1)
#   sample <- fc.footankle.p1.enter.res.inversecdf(p)
#   return (sample)
# }
# 
# generate.p2.enter.CDF.sample <- function() {
#   p      <- runif(1, min = 0, max = 1)
#   sample <- fc.footankle.p2.enter.res.inversecdf(p)
#   return (sample)
# }
# 
# generate.pALL.exit.CDF.sample <- function() {
#   p      <- runif(1, min = 0, max = 1)
#   sample <- fc.footankle.pALL.exit.res.inversecdf(p)
#   return (sample)
# }
# 
# 
# generate.CDF.sample <- function(priority = NULL) {
#   if (priority == 1) {
#     return ( generate.p1.enter.CDF.sample() )
#   }
#   
#   else if (priority == 2) {
#     return ( generate.p2.enter.CDF.sample() )
#   }
#   
#   else {
#     return ( generate.pALL.exit.CDF.sample() )
#   }
# }



# ----- Translate CDF Functions into Lookup Tables -----


p.values <- seq(from = 0, to = 1, by = 0.001)


# Foot & Ankle Priority 1 Entry
fc.footankle.p1.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.footankle.p1.enter.res.inversecdf.value  <- fc.footankle.p1.enter.res.inversecdf(p)
  fc.footankle.p1.enter.res.inversecdf.values <- c(fc.footankle.p1.enter.res.inversecdf.values, fc.footankle.p1.enter.res.inversecdf.value)
}
fc.footankle.p1.enter.res.inversecdf.lookup <- data.frame(p.values, fc.footankle.p1.enter.res.inversecdf.values)

# Foot & Ankle Priority 2 Entry
fc.footankle.p2.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.footankle.p2.enter.res.inversecdf.value  <- fc.footankle.p2.enter.res.inversecdf(p)
  fc.footankle.p2.enter.res.inversecdf.values <- c(fc.footankle.p2.enter.res.inversecdf.values, fc.footankle.p2.enter.res.inversecdf.value)
}
fc.footankle.p2.enter.res.inversecdf.lookup <- data.frame(p.values, fc.footankle.p2.enter.res.inversecdf.values)

# Foot & Ankle Priority ALL Exit
fc.footankle.pALL.exit.res.inversecdf.values <- c()
for (p in p.values) {
  fc.footankle.pALL.exit.res.inversecdf.value  <- fc.footankle.pALL.exit.res.inversecdf(p)
  fc.footankle.pALL.exit.res.inversecdf.values <- c(fc.footankle.pALL.exit.res.inversecdf.values, fc.footankle.pALL.exit.res.inversecdf.value)
}
fc.footankle.pALL.exit.res.inversecdf.lookup <- data.frame(p.values, fc.footankle.pALL.exit.res.inversecdf.values)



# Hand Priority 1 Entry
fc.hand.p1.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hand.p1.enter.res.inversecdf.value  <- fc.hand.p1.enter.res.inversecdf(p)
  fc.hand.p1.enter.res.inversecdf.values <- c(fc.hand.p1.enter.res.inversecdf.values, fc.hand.p1.enter.res.inversecdf.value)
}
fc.hand.p1.enter.res.inversecdf.lookup <- data.frame(p.values, fc.hand.p1.enter.res.inversecdf.values)

# Hand Priority 2 Entry
fc.hand.p2.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hand.p2.enter.res.inversecdf.value  <- fc.hand.p2.enter.res.inversecdf(p)
  fc.hand.p2.enter.res.inversecdf.values <- c(fc.hand.p2.enter.res.inversecdf.values, fc.hand.p2.enter.res.inversecdf.value)
}
fc.hand.p2.enter.res.inversecdf.lookup <- data.frame(p.values, fc.hand.p2.enter.res.inversecdf.values)

# Hand Priority ALL Exit
fc.hand.pALL.exit.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hand.pALL.exit.res.inversecdf.value  <- fc.hand.pALL.exit.res.inversecdf(p)
  fc.hand.pALL.exit.res.inversecdf.values <- c(fc.hand.pALL.exit.res.inversecdf.values, fc.hand.pALL.exit.res.inversecdf.value)
}
fc.hand.pALL.exit.res.inversecdf.lookup <- data.frame(p.values, fc.hand.pALL.exit.res.inversecdf.values)



# hip Priority 1 Entry
fc.hip.p1.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hip.p1.enter.res.inversecdf.value  <- fc.hip.p1.enter.res.inversecdf(p)
  fc.hip.p1.enter.res.inversecdf.values <- c(fc.hip.p1.enter.res.inversecdf.values, fc.hip.p1.enter.res.inversecdf.value)
}
fc.hip.p1.enter.res.inversecdf.lookup <- data.frame(p.values, fc.hip.p1.enter.res.inversecdf.values)

# hip Priority 2 Entry
fc.hip.p2.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hip.p2.enter.res.inversecdf.value  <- fc.hip.p2.enter.res.inversecdf(p)
  fc.hip.p2.enter.res.inversecdf.values <- c(fc.hip.p2.enter.res.inversecdf.values, fc.hip.p2.enter.res.inversecdf.value)
}
fc.hip.p2.enter.res.inversecdf.lookup <- data.frame(p.values, fc.hip.p2.enter.res.inversecdf.values)

# hip Priority ALL Exit
fc.hip.pALL.exit.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hip.pALL.exit.res.inversecdf.value  <- fc.hip.pALL.exit.res.inversecdf(p)
  fc.hip.pALL.exit.res.inversecdf.values <- c(fc.hip.pALL.exit.res.inversecdf.values, fc.hip.pALL.exit.res.inversecdf.value)
}
fc.hip.pALL.exit.res.inversecdf.lookup <- data.frame(p.values, fc.hip.pALL.exit.res.inversecdf.values)



# hipknee Priority 1 Entry
fc.hipknee.p1.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hipknee.p1.enter.res.inversecdf.value  <- fc.hipknee.p1.enter.res.inversecdf(p)
  fc.hipknee.p1.enter.res.inversecdf.values <- c(fc.hipknee.p1.enter.res.inversecdf.values, fc.hipknee.p1.enter.res.inversecdf.value)
}
fc.hipknee.p1.enter.res.inversecdf.lookup <- data.frame(p.values, fc.hipknee.p1.enter.res.inversecdf.values)

# hipknee Priority 2 Entry
fc.hipknee.p2.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hipknee.p2.enter.res.inversecdf.value  <- fc.hipknee.p2.enter.res.inversecdf(p)
  fc.hipknee.p2.enter.res.inversecdf.values <- c(fc.hipknee.p2.enter.res.inversecdf.values, fc.hipknee.p2.enter.res.inversecdf.value)
}
fc.hipknee.p2.enter.res.inversecdf.lookup <- data.frame(p.values, fc.hipknee.p2.enter.res.inversecdf.values)

# hipknee Priority ALL Exit
fc.hipknee.pALL.exit.res.inversecdf.values <- c()
for (p in p.values) {
  fc.hipknee.pALL.exit.res.inversecdf.value  <- fc.hipknee.pALL.exit.res.inversecdf(p)
  fc.hipknee.pALL.exit.res.inversecdf.values <- c(fc.hipknee.pALL.exit.res.inversecdf.values, fc.hipknee.pALL.exit.res.inversecdf.value)
}
fc.hipknee.pALL.exit.res.inversecdf.lookup <- data.frame(p.values, fc.hipknee.pALL.exit.res.inversecdf.values)



# knee Priority 1 Entry
fc.knee.p1.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.knee.p1.enter.res.inversecdf.value  <- fc.knee.p1.enter.res.inversecdf(p)
  fc.knee.p1.enter.res.inversecdf.values <- c(fc.knee.p1.enter.res.inversecdf.values, fc.knee.p1.enter.res.inversecdf.value)
}
fc.knee.p1.enter.res.inversecdf.lookup <- data.frame(p.values, fc.knee.p1.enter.res.inversecdf.values)

# knee Priority 2 Entry
fc.knee.p2.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.knee.p2.enter.res.inversecdf.value  <- fc.knee.p2.enter.res.inversecdf(p)
  fc.knee.p2.enter.res.inversecdf.values <- c(fc.knee.p2.enter.res.inversecdf.values, fc.knee.p2.enter.res.inversecdf.value)
}
fc.knee.p2.enter.res.inversecdf.lookup <- data.frame(p.values, fc.knee.p2.enter.res.inversecdf.values)

# knee Priority ALL Exit
fc.knee.pALL.exit.res.inversecdf.values <- c()
for (p in p.values) {
  fc.knee.pALL.exit.res.inversecdf.value  <- fc.knee.pALL.exit.res.inversecdf(p)
  fc.knee.pALL.exit.res.inversecdf.values <- c(fc.knee.pALL.exit.res.inversecdf.values, fc.knee.pALL.exit.res.inversecdf.value)
}
fc.knee.pALL.exit.res.inversecdf.lookup <- data.frame(p.values, fc.knee.pALL.exit.res.inversecdf.values)



# shoulder Priority 1 Entry
fc.shoulder.p1.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.shoulder.p1.enter.res.inversecdf.value  <- fc.shoulder.p1.enter.res.inversecdf(p)
  fc.shoulder.p1.enter.res.inversecdf.values <- c(fc.shoulder.p1.enter.res.inversecdf.values, fc.shoulder.p1.enter.res.inversecdf.value)
}
fc.shoulder.p1.enter.res.inversecdf.lookup <- data.frame(p.values, fc.shoulder.p1.enter.res.inversecdf.values)

# shoulder Priority 2 Entry
fc.shoulder.p2.enter.res.inversecdf.values <- c()
for (p in p.values) {
  fc.shoulder.p2.enter.res.inversecdf.value  <- fc.shoulder.p2.enter.res.inversecdf(p)
  fc.shoulder.p2.enter.res.inversecdf.values <- c(fc.shoulder.p2.enter.res.inversecdf.values, fc.shoulder.p2.enter.res.inversecdf.value)
}
fc.shoulder.p2.enter.res.inversecdf.lookup <- data.frame(p.values, fc.shoulder.p2.enter.res.inversecdf.values)

# shoulder Priority ALL Exit
fc.shoulder.pALL.exit.res.inversecdf.values <- c()
for (p in p.values) {
  fc.shoulder.pALL.exit.res.inversecdf.value  <- fc.shoulder.pALL.exit.res.inversecdf(p)
  fc.shoulder.pALL.exit.res.inversecdf.values <- c(fc.shoulder.pALL.exit.res.inversecdf.values, fc.shoulder.pALL.exit.res.inversecdf.value)
}
fc.shoulder.pALL.exit.res.inversecdf.lookup <- data.frame(p.values, fc.shoulder.pALL.exit.res.inversecdf.values)





# ----- Write CDF functions to CSV files -----

setwd("...")

write.csv(fc.footankle.p1.enter.res.inversecdf.lookup,  file = "foot_ankle_priority_1_residual_cdf.csv")
write.csv(fc.footankle.p2.enter.res.inversecdf.lookup,  file = "foot_ankle_priority_2_residual_cdf.csv")
write.csv(fc.footankle.pALL.exit.res.inversecdf.lookup, file = "foot_ankle_priority_ALL_residual_cdf.csv")

write.csv(fc.hand.p1.enter.res.inversecdf.lookup,  file = "hand_priority_1_residual_cdf.csv")
write.csv(fc.hand.p2.enter.res.inversecdf.lookup,  file = "hand_priority_2_residual_cdf.csv")
write.csv(fc.hand.pALL.exit.res.inversecdf.lookup, file = "hand_priority_ALL_residual_cdf.csv")

write.csv(fc.hip.p1.enter.res.inversecdf.lookup,  file = "hip_priority_1_residual_cdf.csv")
write.csv(fc.hip.p2.enter.res.inversecdf.lookup,  file = "hip_priority_2_residual_cdf.csv")
write.csv(fc.hip.pALL.exit.res.inversecdf.lookup, file = "hip_priority_ALL_residual_cdf.csv")

write.csv(fc.hipknee.p1.enter.res.inversecdf.lookup,  file = "hip_knee_priority_1_residual_cdf.csv")
write.csv(fc.hipknee.p2.enter.res.inversecdf.lookup,  file = "hip_knee_priority_2_residual_cdf.csv")
write.csv(fc.hipknee.pALL.exit.res.inversecdf.lookup, file = "hip_knee_priority_ALL_residual_cdf.csv")

write.csv(fc.knee.p1.enter.res.inversecdf.lookup,  file = "knee_priority_1_residual_cdf.csv")
write.csv(fc.knee.p2.enter.res.inversecdf.lookup,  file = "knee_priority_2_residual_cdf.csv")
write.csv(fc.knee.pALL.exit.res.inversecdf.lookup, file = "knee_priority_ALL_residual_cdf.csv")

write.csv(fc.shoulder.p1.enter.res.inversecdf.lookup,  file = "shoulder_priority_1_residual_cdf.csv")
write.csv(fc.shoulder.p2.enter.res.inversecdf.lookup,  file = "shoulder_priority_2_residual_cdf.csv")
write.csv(fc.shoulder.pALL.exit.res.inversecdf.lookup, file = "shoulder_priority_ALL_residual_cdf.csv")




# ----- Post-amble -----

# reset warning level
options(warn = oldw)

message("complete <3")
