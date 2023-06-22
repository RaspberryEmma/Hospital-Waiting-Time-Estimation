# ****************************************
# The doctor will see you now - Personalised Waiting Times
#
# Predictive Tool for Estimating Queue Clear Times
#
# Emma Tarmey
# 09/08/2022
# ****************************************


# ----- Preamble -----
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(forecast)

rm(list = ls())
setwd("...")


# ----- Load data from Excel Sheets -----


# backlog queue data

setwd("...")
incompletes_processed_data <- read.csv("incompletes_processed.csv")
clock_stops_processed_data <- read.csv("clock_stops_processed.csv")


# error cdf data

setwd("...")

cdf_foot_ankle_priority_2_lookup   <- read.csv("foot_ankle_priority_2_residual_cdf.csv")
cdf_foot_ankle_priority_ALL_lookup <- read.csv("foot_ankle_priority_ALL_residual_cdf.csv")

cdf_hand_priority_2_lookup   <- read.csv("hand_priority_2_residual_cdf.csv")
cdf_hand_priority_ALL_lookup <- read.csv("hand_priority_ALL_residual_cdf.csv")

cdf_hip_priority_2_lookup   <- read.csv("hip_priority_2_residual_cdf.csv")
cdf_hip_priority_ALL_lookup <- read.csv("hip_priority_ALL_residual_cdf.csv")

cdf_hip_knee_priority_2_lookup   <- read.csv("hip_knee_priority_2_residual_cdf.csv")
cdf_hip_knee_priority_ALL_lookup <- read.csv("hip_knee_priority_ALL_residual_cdf.csv")

cdf_knee_priority_2_lookup   <- read.csv("knee_priority_2_residual_cdf.csv")
cdf_knee_priority_ALL_lookup <- read.csv("knee_priority_ALL_residual_cdf.csv")

cdf_shoulder_priority_2_lookup   <- read.csv("shoulder_priority_2_residual_cdf.csv")
cdf_shoulder_priority_ALL_lookup <- read.csv("shoulder_priority_ALL_residual_cdf.csv")



# timeseries data

setwd("...")

ts_foot_ankle_priority_2_enter_by_week   <- read.csv("foot_ankle_priority_2_enter_by_week_timeseries.csv")
ts_foot_ankle_priority_all_exit_by_week  <- read.csv("foot_ankle_priority_all_exit_by_week_timeseries.csv")

ts_hand_priority_2_enter_by_week   <- read.csv("hand_priority_2_enter_by_week_timeseries.csv")
ts_hand_priority_all_exit_by_week  <- read.csv("hand_priority_all_exit_by_week_timeseries.csv")

ts_hip_priority_2_enter_by_week   <- read.csv("hip_priority_2_enter_by_week_timeseries.csv")
ts_hip_priority_all_exit_by_week  <- read.csv("hip_priority_all_exit_by_week_timeseries.csv")

ts_hip_knee_priority_2_enter_by_week   <- read.csv("hip_knee_priority_2_enter_by_week_timeseries.csv")
ts_hip_knee_priority_all_exit_by_week  <- read.csv("hip_knee_priority_all_exit_by_week_timeseries.csv")

ts_knee_priority_2_enter_by_week   <- read.csv("knee_priority_2_enter_by_week_timeseries.csv")
ts_knee_priority_all_exit_by_week  <- read.csv("knee_priority_all_exit_by_week_timeseries.csv")

ts_shoulder_priority_2_enter_by_week   <- read.csv("shoulder_priority_2_enter_by_week_timeseries.csv")
ts_shoulder_priority_all_exit_by_week  <- read.csv("shoulder_priority_all_exit_by_week_timeseries.csv")



# ----- Find correctly corresponding forecasts -----


# set window across all data-sets here
# note: incompletes snapshot taken at 25/06/2022
window_start        <- c(2018, 5)
window_end          <- c(2021, 10)
weeks_to_skip       <- (4 * 8) # skip to present day
predictions_to_make <- (52 * 2)


# foot_ankle

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

# hand

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

# hip

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

# hip_knee

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

# knee

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

# shoulder

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




# ALL FORECAST MODELS

# Build TBATS Model
ts_foot_ankle_priority_2_enter_by_week.tbats <- tbats(ts_foot_ankle_priority_2_enter_by_week.timeseries)
summary(ts_foot_ankle_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_foot_ankle_priority_2_enter_by_week.tbats.forecast <- forecast(ts_foot_ankle_priority_2_enter_by_week.tbats, h = predictions_to_make)
plot(ts_foot_ankle_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Foot & Ankle Priority 2 Queue Entry TBATS Model Prediction")

# Build TBATS Model
ts_foot_ankle_priority_all_exit_by_week.tbats <- tbats(ts_foot_ankle_priority_all_exit_by_week.timeseries)
summary(ts_foot_ankle_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_foot_ankle_priority_all_exit_by_week.tbats.forecast <- forecast(ts_foot_ankle_priority_all_exit_by_week.tbats, h = predictions_to_make)
plot(ts_foot_ankle_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Foot & Ankle Priority All Queue Exit TBATS Model Prediction")


# Build TBATS Model
ts_hand_priority_2_enter_by_week.tbats <- tbats(ts_hand_priority_2_enter_by_week.timeseries)
summary(ts_hand_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_hand_priority_2_enter_by_week.tbats.forecast <- forecast(ts_hand_priority_2_enter_by_week.tbats, h = predictions_to_make)
plot(ts_hand_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hand Priority 2 Queue Entry TBATS Model Prediction")

# Build TBATS Model
ts_hand_priority_all_exit_by_week.tbats <- tbats(ts_hand_priority_all_exit_by_week.timeseries)
summary(ts_hand_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_hand_priority_all_exit_by_week.tbats.forecast <- forecast(ts_hand_priority_all_exit_by_week.tbats, h = predictions_to_make)
plot(ts_hand_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "Hand Priority All Queue Exit TBATS Model Prediction")


# Build TBATS Model
ts_hip_priority_2_enter_by_week.tbats <- tbats(ts_hip_priority_2_enter_by_week.timeseries)
summary(ts_hip_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_hip_priority_2_enter_by_week.tbats.forecast <- forecast(ts_hip_priority_2_enter_by_week.tbats, h = predictions_to_make)
plot(ts_hip_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "hip Priority 2 Queue Entry TBATS Model Prediction")

# Build TBATS Model
ts_hip_priority_all_exit_by_week.tbats <- tbats(ts_hip_priority_all_exit_by_week.timeseries)
summary(ts_hip_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_hip_priority_all_exit_by_week.tbats.forecast <- forecast(ts_hip_priority_all_exit_by_week.tbats, h = predictions_to_make)
plot(ts_hip_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "hip Priority All Queue Exit TBATS Model Prediction")


# Build TBATS Model
ts_hip_knee_priority_2_enter_by_week.tbats <- tbats(ts_hip_knee_priority_2_enter_by_week.timeseries)
summary(ts_hip_knee_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_hip_knee_priority_2_enter_by_week.tbats.forecast <- forecast(ts_hip_knee_priority_2_enter_by_week.tbats, h = predictions_to_make)
plot(ts_hip_knee_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "hip_knee Priority 2 Queue Entry TBATS Model Prediction")

# Build TBATS Model
ts_hip_knee_priority_all_exit_by_week.tbats <- tbats(ts_hip_knee_priority_all_exit_by_week.timeseries)
summary(ts_hip_knee_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_hip_knee_priority_all_exit_by_week.tbats.forecast <- forecast(ts_hip_knee_priority_all_exit_by_week.tbats, h = predictions_to_make)
plot(ts_hip_knee_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "hip_knee Priority All Queue Exit TBATS Model Prediction")


# Build TBATS Model
ts_knee_priority_2_enter_by_week.tbats <- tbats(ts_knee_priority_2_enter_by_week.timeseries)
summary(ts_knee_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_knee_priority_2_enter_by_week.tbats.forecast <- forecast(ts_knee_priority_2_enter_by_week.tbats, h = predictions_to_make)
plot(ts_knee_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "knee Priority 2 Queue Entry TBATS Model Prediction")

# Build TBATS Model
ts_knee_priority_all_exit_by_week.tbats <- tbats(ts_knee_priority_all_exit_by_week.timeseries)
summary(ts_knee_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_knee_priority_all_exit_by_week.tbats.forecast <- forecast(ts_knee_priority_all_exit_by_week.tbats, h = predictions_to_make)
plot(ts_knee_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "knee Priority All Queue Exit TBATS Model Prediction")


# Build TBATS Model
ts_shoulder_priority_2_enter_by_week.tbats <- tbats(ts_shoulder_priority_2_enter_by_week.timeseries)
summary(ts_shoulder_priority_2_enter_by_week.tbats)

# Predict TBATS Values
ts_shoulder_priority_2_enter_by_week.tbats.forecast <- forecast(ts_shoulder_priority_2_enter_by_week.tbats, h = predictions_to_make)
plot(ts_shoulder_priority_2_enter_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "shoulder Priority 2 Queue Entry TBATS Model Prediction")

# Build TBATS Model
ts_shoulder_priority_all_exit_by_week.tbats <- tbats(ts_shoulder_priority_all_exit_by_week.timeseries)
summary(ts_shoulder_priority_all_exit_by_week.tbats)

# Predict TBATS Values
ts_shoulder_priority_all_exit_by_week.tbats.forecast <- forecast(ts_shoulder_priority_all_exit_by_week.tbats, h = predictions_to_make)
plot(ts_shoulder_priority_all_exit_by_week.tbats.forecast, xlab = "Date", ylab = "Queue Entry", main = "shoulder Priority All Queue Exit TBATS Model Prediction")




# interpret priority data
incompletes_processed_data$Priority <- as.numeric( word(incompletes_processed_data$Priority, 1) )
incompletes_processed_data <- ( incompletes_processed_data %>% drop_na(c("Priority")) )
incompletes_processed_data <- ( incompletes_processed_data %>% drop_na(c("ServiceArea")) )

# isolate speciality
speciality <- "Foot & Ankle"

queue_subset <- (incompletes_processed_data %>% filter(ServiceArea == speciality))

p <- ggplot(incompletes_processed_data, aes(x = factor(ServiceArea) )) +
     geom_bar(aes(fill = factor(Priority)), position = position_stack(reverse = T), width = 0.5) +
     labs(title = "Incompletes Queueing System", x = "ServiceArea", y = "Number of Backlogged Referrals", fill = "Priority")
p

q <- ggplot(queue_subset, aes(x = factor(ServiceArea) )) +
     geom_bar(aes(fill = factor(Priority)), position = position_stack(reverse = T), width = 0.2) +
     labs(title = "Incompletes Queueing System", x = "ServiceArea", y = "Number of Backlogged Referrals", fill = "Priority")
q

queue_data <- ggplot_build(q)[["data"]][[1]][["count"]]
queue_data

if (length(queue_data) == 0) {
  queue_data <- c(0, 0, 0)
}

if (length(queue_data) == 1) {
  queue_data <- c(queue_data, 0, 0)
}

if (length(queue_data) == 2) {
    queue_data <- c(queue_data, 0)
}


# ----- Produce Queue System Summary -----


sample.CDF <- function(CDF = NULL) {
  
  # standardise CDF columns
  colnames(CDF) <- c("Index", "p.values", "CDF.values")
  CDF           <- (CDF %>% select("p.values", "CDF.values"))
  
  # set-up sampling variables
  p       <- runif(1, min = 0, max = 1)
  sample  <- 0
  
  # find first corresponding match to random number p in CDF look-up table
  for (i in (1:length(CDF$p.values))) {
    if (CDF$p.values[i] < p) {
      # do nothing, continue iterating
    }
    else{
      # accept first sample, stop iterating
      sample <- CDF$CDF.values[i]
      break
    }
  }
  
  return (sample)
}


queue_clear_time <- function(queue_data = NULL,
                             priority   = NULL,
                             enter_priority_2.forecast,
                             enter_priority_2.cdf,
                             exit_priority_all.forecast,
                             exit_priority_all.cdf) {
  
  # Variables used in simulation
  queue_clear_weeks <- 0
  priority_1_queue  <- queue_data[1]
  priority_2_queue  <- queue_data[2] + queue_data[3] # priority 3 treated as noise but current state still accounted for
  
  enter_priority_2.cdf.value  <- 0
  exit_priority_all.cdf.value <- 0
  
  # If patient is priority 2 (emergency) or 3 (2 week wait),
  # then we may ignore the current backlog of priority 1 (routine) patients
  # We treat priority 3 as noise with respect to the forecasts due to rarity (many zeroes)
  # Though, as such patients exist, we may still conclude a very short RTT time
  
  if (priority == 1) {
    
    # Clear time for all Priority 1 and Priority 2 backlog
    while ( (priority_1_queue > 0) || (priority_2_queue > 0) ) {
      
      # sample residual values from pre-computed CDFs
      enter_priority_2.cdf.value  <- sample.CDF( enter_priority_2.cdf  )
      exit_priority_all.cdf.value <- sample.CDF( exit_priority_all.cdf )
      
      priority_2_queue  <- ( priority_2_queue + (enter_priority_2.forecast[ queue_clear_weeks + 1 ] + enter_priority_2.cdf.value ) - (exit_priority_all.forecast[ queue_clear_weeks + 1 ] + exit_priority_all.cdf.value) )
      priority_1_queue  <- ( priority_1_queue )
      
      if (priority_2_queue < 0) {
        priority_1_queue <- (priority_1_queue + priority_2_queue)
        priority_2_queue <- 0
      }
      
      if (priority_1_queue < 0) {
        priority_1_queue <- 0
      }
      
      # Graph Current State of Queue
      queue_clear_weeks <- queue_clear_weeks + 1
      barplot_data      <- c(priority_1_queue, priority_2_queue, 0)
    }
    
  }
  else if (priority == 2) {
    
    # Clear time for all Priority 2 backlog
    while ( priority_2_queue > 0 ) {
      
      # sample residual values from pre-computed CDFs
      enter_priority_2.cdf.value  <- sample.CDF( enter_priority_2.cdf  )
      exit_priority_all.cdf.value <- sample.CDF( exit_priority_all.cdf )
      
      priority_2_queue  <- ( priority_2_queue - (exit_priority_all.forecast[ queue_clear_weeks + 1 ] + exit_priority_all.cdf.value) )
      
      # Graph Current State of Queue
      queue_clear_weeks <- queue_clear_weeks + 1
      barplot_data      <- c(priority_1_queue, priority_2_queue, 0)
    }
  }
  else {
    # Very few patients at this priority level exist, hence cannot really forecast
    # Additional statistical context provided elsewhere
    queue_clear_weeks <- NULL
  }
  
  
  return (queue_clear_weeks)
}


# Graph Initial State of Queue
barplot_data      <- queue_data
barplot(barplot_data,
        main      = paste("Current State of Queueing System for Service Area", speciality, "\nWeek = ", 0),
        names.arg = c("Priority 1: \nRoutine", "Priority 2: \nUrgent", "Priority 3: \nTwo-Week Wait"),
        xlab      = "Patient Priority",
        ylab      = "Number of Backlogged Referrals in Queue")

queue_clear_time(queue_data = queue_data,
                 priority   = 1,
                 enter_priority_2.forecast   = as.integer( ts_foot_ankle_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                 enter_priority_2.cdf        = cdf_foot_ankle_priority_2_lookup,
                 exit_priority_all.forecast  = as.integer( ts_foot_ankle_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                 exit_priority_all.cdf       = cdf_foot_ankle_priority_ALL_lookup)

queue_clear_time(queue_data = queue_data,
                 priority   = 2,
                 enter_priority_2.forecast   = as.integer( ts_foot_ankle_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                 enter_priority_2.cdf        = cdf_foot_ankle_priority_2_lookup,
                 exit_priority_all.forecast  = as.integer( ts_foot_ankle_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                 exit_priority_all.cdf       = cdf_foot_ankle_priority_ALL_lookup)



# ----- Simulate Queue Clear n times to produce clear-time distribution -----

simulate_queue <- function(n            = NULL,
                           priority     = NULL,
                           service_area = NULL,
                           enter_priority_2.forecast   = NULL,
                           enter_priority_2.cdf        = NULL,
                           exit_priority_all.forecast  = NULL,
                           exit_priority_all.cdf       = NULL,
                           incompletes_processed_data  = NULL) {
  
  # setup variables to hold results
  clear_times  <- c()
  current_time <- 0
  
  # isolate incompletes data to records concerning our service area
  queue_subset <- incompletes_processed_data %>% filter(ServiceArea == service_area)
  
  # generate initial queue data using ggplot
  q <- ggplot(queue_subset, aes(x = factor(ServiceArea) )) +
    geom_bar(aes(fill = factor(Priority)), position = position_stack(reverse = T), width = 0.2) +
    labs(title = "Incompletes Queueing System", x = "ServiceArea", y = "Number of Backlogged Referrals", fill = "Priority")
  queue_data <- ggplot_build(q)[["data"]][[1]][["count"]]
  
  # adjust queue system summary data for uniform size across cases
  if (length(queue_data) == 0) {
    queue_data <- c(0, 0, 0)
  }
  
  if (length(queue_data) == 1) {
    queue_data <- c(queue_data, 0, 0)
  }
  
  if (length(queue_data) == 2) {
    queue_data <- c(queue_data, 0)
  }
  
  # generate all n queue clear time results
  for (i in (1:n)) {
    # generate new estimate
    current_time <- queue_clear_time(queue_data = queue_data,
                                     priority   = priority,
                                     enter_priority_2.forecast   = enter_priority_2.forecast,
                                     enter_priority_2.cdf        = enter_priority_2.cdf,
                                     exit_priority_all.forecast  = exit_priority_all.forecast,
                                     exit_priority_all.cdf       = exit_priority_all.cdf)
    
    # add new estimate to vector
    clear_times <- c(clear_times, current_time)
  }
  
  return(clear_times)
}



# ----- Generate Results from Repeat Simulation (N=100) -----

foot_ankle_p1_N100_final_results <- simulate_queue(n            = 100,
                                           priority     = 1,
                                           service_area = "Foot & Ankle",
                                           enter_priority_2.forecast   = as.integer( ts_foot_ankle_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                           enter_priority_2.cdf        = cdf_foot_ankle_priority_2_lookup,
                                           exit_priority_all.forecast  = as.integer( ts_foot_ankle_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                           exit_priority_all.cdf       = cdf_foot_ankle_priority_ALL_lookup,
                                           incompletes_processed_data  = incompletes_processed_data)

hand_p1_N100_final_results <- simulate_queue(      n            = 100,
                                           priority     = 1,
                                           service_area = "Hand",
                                           enter_priority_2.forecast   = as.integer( ts_hand_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                           enter_priority_2.cdf        = cdf_hand_priority_2_lookup,
                                           exit_priority_all.forecast  = as.integer( ts_hand_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                           exit_priority_all.cdf       = cdf_hand_priority_ALL_lookup,
                                           incompletes_processed_data  = incompletes_processed_data)

hip_p1_N100_final_results <- simulate_queue(       n            = 100,
                                           priority     = 1,
                                           service_area = "Hip",
                                           enter_priority_2.forecast   = as.integer( ts_hip_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                           enter_priority_2.cdf        = cdf_hip_priority_2_lookup,
                                           exit_priority_all.forecast  = as.integer( ts_hip_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                           exit_priority_all.cdf       = cdf_hip_priority_ALL_lookup,
                                           incompletes_processed_data  = incompletes_processed_data)

hip_knee_p1_N100_final_results <- simulate_queue(  n            = 100,
                                           priority     = 1,
                                           service_area = "Hip & Knee",
                                           enter_priority_2.forecast   = as.integer( ts_hip_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                           enter_priority_2.cdf        = cdf_hip_knee_priority_2_lookup,
                                           exit_priority_all.forecast  = as.integer( ts_hip_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                           exit_priority_all.cdf       = cdf_hip_knee_priority_ALL_lookup,
                                           incompletes_processed_data  = incompletes_processed_data)

knee_p1_N100_final_results <- simulate_queue(       n            = 100,
                                            priority     = 1,
                                            service_area = "Knee",
                                            enter_priority_2.forecast   = as.integer( ts_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                            enter_priority_2.cdf        = cdf_knee_priority_2_lookup,
                                            exit_priority_all.forecast  = as.integer( ts_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                            exit_priority_all.cdf       = cdf_knee_priority_ALL_lookup,
                                            incompletes_processed_data  = incompletes_processed_data)

shoulder_p1_N100_final_results <- simulate_queue(   n            = 100,
                                            priority     = 1,
                                            service_area = "Shoulder",
                                            enter_priority_2.forecast   = as.integer( ts_shoulder_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                            enter_priority_2.cdf        = cdf_shoulder_priority_2_lookup,
                                            exit_priority_all.forecast  = as.integer( ts_shoulder_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                            exit_priority_all.cdf       = cdf_shoulder_priority_ALL_lookup,
                                            incompletes_processed_data  = incompletes_processed_data)

foot_ankle_p2_N100_final_results <- simulate_queue(n            = 100,
                                                   priority     = 2,
                                                   service_area = "Foot & Ankle",
                                                   enter_priority_2.forecast   = as.integer( ts_foot_ankle_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                   enter_priority_2.cdf        = cdf_foot_ankle_priority_2_lookup,
                                                   exit_priority_all.forecast  = as.integer( ts_foot_ankle_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                   exit_priority_all.cdf       = cdf_foot_ankle_priority_ALL_lookup,
                                                   incompletes_processed_data  = incompletes_processed_data)

hand_p2_N100_final_results <- simulate_queue(      n            = 100,
                                                   priority     = 2,
                                                   service_area = "Hand",
                                                   enter_priority_2.forecast   = as.integer( ts_hand_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                   enter_priority_2.cdf        = cdf_hand_priority_2_lookup,
                                                   exit_priority_all.forecast  = as.integer( ts_hand_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                   exit_priority_all.cdf       = cdf_hand_priority_ALL_lookup,
                                                   incompletes_processed_data  = incompletes_processed_data)

hip_p2_N100_final_results <- simulate_queue(       n            = 100,
                                                   priority     = 2,
                                                   service_area = "Hip",
                                                   enter_priority_2.forecast   = as.integer( ts_hip_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                   enter_priority_2.cdf        = cdf_hip_priority_2_lookup,
                                                   exit_priority_all.forecast  = as.integer( ts_hip_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                   exit_priority_all.cdf       = cdf_hip_priority_ALL_lookup,
                                                   incompletes_processed_data  = incompletes_processed_data)

hip_knee_p2_N100_final_results <- simulate_queue(  n            = 100,
                                                   priority     = 2,
                                                   service_area = "Hip & Knee",
                                                   enter_priority_2.forecast   = as.integer( ts_hip_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                   enter_priority_2.cdf        = cdf_hip_knee_priority_2_lookup,
                                                   exit_priority_all.forecast  = as.integer( ts_hip_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                   exit_priority_all.cdf       = cdf_hip_knee_priority_ALL_lookup,
                                                   incompletes_processed_data  = incompletes_processed_data)

knee_p2_N100_final_results <- simulate_queue(       n            = 100,
                                                    priority     = 2,
                                                    service_area = "Knee",
                                                    enter_priority_2.forecast   = as.integer( ts_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_knee_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_knee_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

shoulder_p2_N100_final_results <- simulate_queue(   n            = 100,
                                                    priority     = 2,
                                                    service_area = "Shoulder",
                                                    enter_priority_2.forecast   = as.integer( ts_shoulder_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_shoulder_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_shoulder_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_shoulder_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)



# ----- Save Results to CSV (N=100) -----

setwd("...")

write.csv(foot_ankle_p1_N100_final_results,
          file = "foot_ankle_p1_N100_final_results.csv",
          row.names = TRUE)

write.csv(hand_p1_N100_final_results,
          file = "hand_p1_N100_final_results.csv",
          row.names = TRUE)

write.csv(hip_p1_N100_final_results,
          file = "hip_p1_N100_final_results.csv",
          row.names = TRUE)

write.csv(hip_knee_p1_N100_final_results,
          file = "hip_knee_p1_N100_final_results.csv",
          row.names = TRUE)

write.csv(knee_p1_N100_final_results,
          file = "knee_p1_N100_final_results.csv",
          row.names = TRUE)

write.csv(shoulder_p1_N100_final_results,
          file = "shoulder_p1_N100_final_results.csv",
          row.names = TRUE)

write.csv(foot_ankle_p2_N100_final_results,
          file = "foot_ankle_p2_N100_final_results.csv",
          row.names = TRUE)

write.csv(hand_p2_N100_final_results,
          file = "hand_p2_N100_final_results.csv",
          row.names = TRUE)

write.csv(hip_p2_N100_final_results,
          file = "hip_p2_N100_final_results.csv",
          row.names = TRUE)

write.csv(hip_knee_p2_N100_final_results,
          file = "hip_knee_p2_N100_final_results.csv",
          row.names = TRUE)

write.csv(knee_p2_N100_final_results,
          file = "knee_p2_N100_final_results.csv",
          row.names = TRUE)

write.csv(shoulder_p2_N100_final_results,
          file = "shoulder_p2_N100_final_results.csv",
          row.names = TRUE)


# ----- Generate Results from Repeat Simulation (N=1000) -----

foot_ankle_p1_N1000_final_results <- simulate_queue(n            = 1000,
                                                    priority     = 1,
                                                    service_area = "Foot & Ankle",
                                                    enter_priority_2.forecast   = as.integer( ts_foot_ankle_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_foot_ankle_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_foot_ankle_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_foot_ankle_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

hand_p1_N1000_final_results <- simulate_queue(      n            = 1000,
                                                    priority     = 1,
                                                    service_area = "Hand",
                                                    enter_priority_2.forecast   = as.integer( ts_hand_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_hand_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_hand_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_hand_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

hip_p1_N1000_final_results <- simulate_queue(       n            = 1000,
                                                    priority     = 1,
                                                    service_area = "Hip",
                                                    enter_priority_2.forecast   = as.integer( ts_hip_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_hip_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_hip_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_hip_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

hip_knee_p1_N1000_final_results <- simulate_queue(  n            = 1000,
                                                    priority     = 1,
                                                    service_area = "Hip & Knee",
                                                    enter_priority_2.forecast   = as.integer( ts_hip_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_hip_knee_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_hip_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_hip_knee_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

knee_p1_N1000_final_results <- simulate_queue(       n            = 1000,
                                                     priority     = 1,
                                                     service_area = "Knee",
                                                     enter_priority_2.forecast   = as.integer( ts_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                     enter_priority_2.cdf        = cdf_knee_priority_2_lookup,
                                                     exit_priority_all.forecast  = as.integer( ts_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                     exit_priority_all.cdf       = cdf_knee_priority_ALL_lookup,
                                                     incompletes_processed_data  = incompletes_processed_data)

shoulder_p1_N1000_final_results <- simulate_queue(   n            = 1000,
                                                     priority     = 1,
                                                     service_area = "Shoulder",
                                                     enter_priority_2.forecast   = as.integer( ts_shoulder_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                     enter_priority_2.cdf        = cdf_shoulder_priority_2_lookup,
                                                     exit_priority_all.forecast  = as.integer( ts_shoulder_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                     exit_priority_all.cdf       = cdf_shoulder_priority_ALL_lookup,
                                                     incompletes_processed_data  = incompletes_processed_data)

foot_ankle_p2_N1000_final_results <- simulate_queue(n            = 1000,
                                                    priority     = 2,
                                                    service_area = "Foot & Ankle",
                                                    enter_priority_2.forecast   = as.integer( ts_foot_ankle_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_foot_ankle_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_foot_ankle_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_foot_ankle_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

hand_p2_N1000_final_results <- simulate_queue(      n            = 1000,
                                                    priority     = 2,
                                                    service_area = "Hand",
                                                    enter_priority_2.forecast   = as.integer( ts_hand_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_hand_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_hand_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_hand_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

hip_p2_N1000_final_results <- simulate_queue(       n            = 1000,
                                                    priority     = 2,
                                                    service_area = "Hip",
                                                    enter_priority_2.forecast   = as.integer( ts_hip_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_hip_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_hip_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_hip_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

hip_knee_p2_N1000_final_results <- simulate_queue(  n            = 1000,
                                                    priority     = 2,
                                                    service_area = "Hip & Knee",
                                                    enter_priority_2.forecast   = as.integer( ts_hip_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                    enter_priority_2.cdf        = cdf_hip_knee_priority_2_lookup,
                                                    exit_priority_all.forecast  = as.integer( ts_hip_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                    exit_priority_all.cdf       = cdf_hip_knee_priority_ALL_lookup,
                                                    incompletes_processed_data  = incompletes_processed_data)

knee_p2_N1000_final_results <- simulate_queue(       n            = 1000,
                                                     priority     = 2,
                                                     service_area = "Knee",
                                                     enter_priority_2.forecast   = as.integer( ts_knee_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                     enter_priority_2.cdf        = cdf_knee_priority_2_lookup,
                                                     exit_priority_all.forecast  = as.integer( ts_knee_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                     exit_priority_all.cdf       = cdf_knee_priority_ALL_lookup,
                                                     incompletes_processed_data  = incompletes_processed_data)

shoulder_p2_N1000_final_results <- simulate_queue(   n            = 1000,
                                                     priority     = 2,
                                                     service_area = "Shoulder",
                                                     enter_priority_2.forecast   = as.integer( ts_shoulder_priority_2_enter_by_week.tbats.forecast$mean   )[-c(1:weeks_to_skip)],
                                                     enter_priority_2.cdf        = cdf_shoulder_priority_2_lookup,
                                                     exit_priority_all.forecast  = as.integer( ts_shoulder_priority_all_exit_by_week.tbats.forecast$mean  )[-c(1:weeks_to_skip)],
                                                     exit_priority_all.cdf       = cdf_shoulder_priority_ALL_lookup,
                                                     incompletes_processed_data  = incompletes_processed_data)


  
# ----- Save Results to CSV (N=1000) -----

setwd("...")

write.csv(foot_ankle_p1_N1000_final_results,
          file = "foot_ankle_p1_N1000_final_results.csv",
          row.names = TRUE)

write.csv(hand_p1_N1000_final_results,
          file = "hand_p1_N1000_final_results.csv",
          row.names = TRUE)

write.csv(hip_p1_N1000_final_results,
          file = "hip_p1_N1000_final_results.csv",
          row.names = TRUE)

write.csv(hip_knee_p1_N1000_final_results,
          file = "hip_knee_p1_N1000_final_results.csv",
          row.names = TRUE)

write.csv(knee_p1_N1000_final_results,
          file = "knee_p1_N1000_final_results.csv",
          row.names = TRUE)

write.csv(shoulder_p1_N1000_final_results,
          file = "shoulder_p1_N1000_final_results.csv",
          row.names = TRUE)

write.csv(foot_ankle_p2_N1000_final_results,
          file = "foot_ankle_p2_N1000_final_results.csv",
          row.names = TRUE)

write.csv(hand_p2_N1000_final_results,
          file = "hand_p2_N1000_final_results.csv",
          row.names = TRUE)

write.csv(hip_p2_N1000_final_results,
          file = "hip_p2_N1000_final_results.csv",
          row.names = TRUE)

write.csv(hip_knee_p2_N1000_final_results,
          file = "hip_knee_p2_N1000_final_results.csv",
          row.names = TRUE)

write.csv(knee_p2_N1000_final_results,
          file = "knee_p2_N1000_final_results.csv",
          row.names = TRUE)

write.csv(shoulder_p2_N1000_final_results,
          file = "shoulder_p2_N1000_final_results.csv",
          row.names = TRUE)


