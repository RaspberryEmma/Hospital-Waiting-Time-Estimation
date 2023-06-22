# ****************************************
# The doctor will see you now - Personalised Waiting Times
#
# Final re-factor of all existing data manipulation work: Putting all data preparation into one file
#
# This file will perform the following actions in this order:
#     (1) Take as input the clock stops data, incompletes queue data and appropriate lookup tables
#     (2) Filter all anomalies from / generally clean the clock stops data and incompletes queue data
#     (3) Filter to exclusively 110 Trauma and Orthopaedics cases (i.e: excluding 108 Spinal)
#     (4) Filter to exclusively Inpatient cases (i.e: no outpatient clockstops or day case incompletes)
#     (5) Assign to every case in both key datasets a Speciality / Service Area based on staff and OPCS codes
#     (6) Generate time series of incoming / outgoing cases, separated by Speciality and Priority
#     (7) Save all such time series to CSV files, forecasting is then conducted elsewhere
#
# Emma Tarmey
# 18/08/2022
# ****************************************


# ----- Preamble -----
library(dplyr)
library(stringr)

rm(list = ls())
setwd("...")


# ----- Load data from Excel Sheets -----
clock_stops_data      <- read.csv("Clock_stops_TandO_2018_Anon.csv")
incompletes_data      <- read.csv("20220626_Incompletes_anon.csv")
#procedure_lookup_data <- read.csv("Procedure_decode.csv")
staff_data            <- read.csv("staff_names_and_codes.csv")

# trim white-space from data
clock_stops_data      <- (clock_stops_data %>% mutate(across(where(is.character), str_trim)))
incompletes_data      <- (incompletes_data %>% mutate(across(where(is.character), str_trim)))
#procedure_lookup_data <- (procedure_lookup_data %>% mutate(across(where(is.character), str_trim)))
staff_data            <- (staff_data %>% mutate(across(where(is.character), str_trim)))

# clean-up staff lookup data
colnames(staff_data) <- c("Code", "Title", "Firstname", "Surname", "ServiceArea")
staff_data$Code      <- gsub('[^[:alnum:] ]', '', staff_data$Code)
staff_data$Code      <- gsub('?', '', staff_data$Code)
staff_data_lookup    <- staff_data %>% select("Code", "ServiceArea")


# ----- Filter all Anomalies -----


# ----- Filter to exclusively Trauma and Orthopaedics Cases -----
clock_stops_TO_data <- clock_stops_data %>% filter(activity_treatment_function_code == 110)
incompletes_TO_data <- incompletes_data %>% filter(Specialty.code == 110)


# ----- Filter to exclusively Inpatient Cases -----
clock_stops_TO_inpatient_data <- clock_stops_TO_data %>% filter(waiting_list_type == "IRTT")
incompletes_TO_inpatient_data <- incompletes_TO_data %>% filter(AWL.intended.mgmt == "1 - INPATIENT")


# ----- Assign each case a Service Area -----
clock_stops_TO_inpatient_with_service_data <- merge(clock_stops_TO_inpatient_data,
                                                    staff_data_lookup,
                                                    by.x  = "consultant_code",
                                                    by.y  = "Code",
                                                    all.x = TRUE)
incompletes_TO_inpatient_with_service_data <- merge(incompletes_TO_inpatient_data,
                                                    staff_data_lookup,
                                                    by.x  = "Consultant.code",
                                                    by.y  = "Code",
                                                    all.x = TRUE)


# ----- Generate Historical Time Series for each Priority and Service Area -----

# convert dates to numeric values
clock_stops_TO_inpatient_with_service_data$rtt_startdate <- as.numeric(
                                                            as.Date(clock_stops_TO_inpatient_with_service_data$rtt_startdate,
                                                                       format = "%Y-%m-%d",
                                                                       origin = "1900-01-01"))
clock_stops_TO_inpatient_with_service_data$rtt_enddate   <- as.numeric(
                                                            as.Date(clock_stops_TO_inpatient_with_service_data$rtt_enddate,
                                                                       format = "%Y-%m-%d",
                                                                       origin = "1900-01-01"))

# subset data by service area
foot_ankle_data <- (clock_stops_TO_inpatient_with_service_data %>% filter(ServiceArea == "Foot & Ankle")) %>% select(rtt_startdate, rtt_days_wait, rtt_weeks_wait, rtt_enddate, priority_type_code)
hand_data       <- (clock_stops_TO_inpatient_with_service_data %>% filter(ServiceArea == "Hand"))         %>% select(rtt_startdate, rtt_days_wait, rtt_weeks_wait, rtt_enddate, priority_type_code)
hip_data        <- (clock_stops_TO_inpatient_with_service_data %>% filter(ServiceArea == "Hip"))          %>% select(rtt_startdate, rtt_days_wait, rtt_weeks_wait, rtt_enddate, priority_type_code)
hip_knee_data   <- (clock_stops_TO_inpatient_with_service_data %>% filter(ServiceArea == "Hip & Knee"))   %>% select(rtt_startdate, rtt_days_wait, rtt_weeks_wait, rtt_enddate, priority_type_code)
knee_data       <- (clock_stops_TO_inpatient_with_service_data %>% filter(ServiceArea == "Knee"))         %>% select(rtt_startdate, rtt_days_wait, rtt_weeks_wait, rtt_enddate, priority_type_code)
shoulder_data   <- (clock_stops_TO_inpatient_with_service_data %>% filter(ServiceArea == "Shoulder"))     %>% select(rtt_startdate, rtt_days_wait, rtt_weeks_wait, rtt_enddate, priority_type_code)


# subset data by priority level
foot_ankle_priority_1_data <- foot_ankle_data %>% filter(priority_type_code == 1)
foot_ankle_priority_2_data <- foot_ankle_data %>% filter(priority_type_code == 2)
foot_ankle_priority_3_data <- foot_ankle_data %>% filter(priority_type_code == 3)

hand_priority_1_data       <- hand_data %>% filter(priority_type_code == 1)
hand_priority_2_data       <- hand_data %>% filter(priority_type_code == 2)
hand_priority_3_data       <- hand_data %>% filter(priority_type_code == 3)

hip_priority_1_data        <- hip_data %>% filter(priority_type_code == 1)
hip_priority_2_data        <- hip_data %>% filter(priority_type_code == 2)
hip_priority_3_data        <- hip_data %>% filter(priority_type_code == 3)

hip_knee_priority_1_data   <- hip_knee_data %>% filter(priority_type_code == 1)
hip_knee_priority_2_data   <- hip_knee_data %>% filter(priority_type_code == 2)
hip_knee_priority_3_data   <- hip_knee_data %>% filter(priority_type_code == 3)

knee_priority_1_data       <- knee_data %>% filter(priority_type_code == 1)
knee_priority_2_data       <- knee_data %>% filter(priority_type_code == 2)
knee_priority_3_data       <- knee_data %>% filter(priority_type_code == 3)

shoulder_priority_1_data   <- shoulder_data %>% filter(priority_type_code == 1)
shoulder_priority_2_data   <- shoulder_data %>% filter(priority_type_code == 2)
shoulder_priority_3_data   <- shoulder_data %>% filter(priority_type_code == 3)


# isolate frequencies of each occurence to form time-series

# foot ankle priority 1 enter queue
bins   <- seq(min(foot_ankle_priority_1_data$rtt_startdate), max(foot_ankle_priority_1_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(foot_ankle_priority_1_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
foot_ankle_enter_priority_1_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# foot ankle priority 2 enter queue
bins   <- seq(min(foot_ankle_priority_2_data$rtt_startdate), max(foot_ankle_priority_2_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(foot_ankle_priority_2_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
foot_ankle_enter_priority_2_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# foot ankle priority 3 enter queue
bins   <- seq(min(foot_ankle_priority_3_data$rtt_startdate), max(foot_ankle_priority_3_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(foot_ankle_priority_3_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
foot_ankle_enter_priority_3_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# hand priority 1 enter queue
bins   <- seq(min(hand_priority_1_data$rtt_startdate), max(hand_priority_1_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hand_priority_1_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hand_enter_priority_1_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# hand priority 2 enter queue
bins   <- seq(min(hand_priority_2_data$rtt_startdate), max(hand_priority_2_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hand_priority_2_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hand_enter_priority_2_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# hand priority 3 enter queue
bins   <- seq(min(hand_priority_3_data$rtt_startdate), max(hand_priority_3_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hand_priority_3_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hand_enter_priority_3_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# hip priority 1 enter queue
bins   <- seq(min(hip_priority_1_data$rtt_startdate), max(hip_priority_1_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hip_priority_1_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_enter_priority_1_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# hip priority 2 enter queue
bins   <- seq(min(hip_priority_2_data$rtt_startdate), max(hip_priority_2_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hip_priority_2_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_enter_priority_2_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# hip priority 3 enter queue
# NOTE: In this particular data-set, there exist no priority 3 hip referrals
#       Consequently, a time-series cannot be generated.
#bins   <- seq(min(hip_priority_3_data$rtt_startdate), max(hip_priority_3_data$rtt_startdate)+7, by=7)
#ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
#freq   <- hist(hip_priority_3_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_enter_priority_3_timeseries <- data.frame(range = c(), frequency = c())



# hip knee priority 1 enter queue
bins   <- seq(min(hip_knee_priority_1_data$rtt_startdate), max(hip_knee_priority_1_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hip_knee_priority_1_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_knee_enter_priority_1_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# hip knee priority 2 enter queue
bins   <- seq(min(hip_knee_priority_2_data$rtt_startdate), max(hip_knee_priority_2_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hip_knee_priority_2_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_knee_enter_priority_2_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# hip knee priority 3 enter queue
bins   <- seq(min(hip_knee_priority_3_data$rtt_startdate), max(hip_knee_priority_3_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hip_knee_priority_3_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_knee_enter_priority_3_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# knee priority 1 enter queue
bins   <- seq(min(knee_priority_1_data$rtt_startdate), max(knee_priority_1_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(knee_priority_1_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
knee_enter_priority_1_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# knee priority 2 enter queue
bins   <- seq(min(knee_priority_2_data$rtt_startdate), max(knee_priority_2_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(knee_priority_2_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
knee_enter_priority_2_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# knee priority 3 enter queue
bins   <- seq(min(knee_priority_3_data$rtt_startdate), max(knee_priority_3_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(knee_priority_3_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
knee_enter_priority_3_timeseries <- data.frame(range = ranges, frequency = freq$counts)



# shoulder priority 1 enter queue
bins   <- seq(min(shoulder_priority_1_data$rtt_startdate), max(shoulder_priority_1_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(shoulder_priority_1_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
shoulder_enter_priority_1_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# shoulder priority 2 enter queue
bins   <- seq(min(shoulder_priority_2_data$rtt_startdate), max(shoulder_priority_2_data$rtt_startdate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(shoulder_priority_2_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
shoulder_enter_priority_2_timeseries <- data.frame(range = ranges, frequency = freq$counts)

# shoulder priority 3 enter queue
# NOTE: In this particular data-set, there exist no priority 3 shoulder referrals
#       Consequently, a time-series cannot be generated.
#bins   <- seq(min(shoulder_priority_3_data$rtt_startdate), max(shoulder_priority_3_data$rtt_startdate)+7, by=7)
#ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
#freq   <- hist(shoulder_priority_3_data$rtt_startdate, breaks=bins, include.lowest=TRUE, plot=TRUE)
shoulder_enter_priority_3_timeseries <- data.frame(range = c(), frequency = c())



# foot ankle exit queue
bins   <- seq(min(foot_ankle_data$rtt_enddate), max(foot_ankle_data$rtt_enddate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(foot_ankle_data$rtt_enddate, breaks=bins, include.lowest=TRUE, plot=TRUE)
foot_ankle_exit_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# hand exit queue
bins   <- seq(min(hand_data$rtt_enddate), max(hand_data$rtt_enddate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hand_data$rtt_enddate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hand_exit_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# hip exit queue
bins   <- seq(min(hip_data$rtt_enddate), max(hip_data$rtt_enddate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hip_data$rtt_enddate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_exit_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# hip knee exit queue
bins   <- seq(min(hip_knee_data$rtt_enddate), max(hip_knee_data$rtt_enddate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(hip_knee_data$rtt_enddate, breaks=bins, include.lowest=TRUE, plot=TRUE)
hip_knee_exit_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# knee exit queue
bins   <- seq(min(knee_data$rtt_enddate), max(knee_data$rtt_enddate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(knee_data$rtt_enddate, breaks=bins, include.lowest=TRUE, plot=TRUE)
knee_exit_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# shoulder exit queue
bins   <- seq(min(shoulder_data$rtt_enddate), max(shoulder_data$rtt_enddate)+7, by=7)
ranges <- paste(head(bins,-1), bins[-1], sep=" - ")
freq   <- hist(shoulder_data$rtt_enddate, breaks=bins, include.lowest=TRUE, plot=TRUE)
shoulder_exit_timeseries <- data.frame(range = ranges, frequency = freq$counts)


# ----- Write Resultant Time-Series to CSV ----

write.csv(clock_stops_TO_inpatient_with_service_data, file = "clock_stops_processed.csv", row.names = TRUE)
write.csv(incompletes_TO_inpatient_with_service_data, file = "incompletes_processed.csv", row.names = TRUE)

setwd("C:/Users/Tarmi/Desktop/University Notes/Semester 3/CORMSIS Project/Data Analysis/ProcessedData/TimeSeries")

write.csv(foot_ankle_enter_priority_1_timeseries, file = "foot_ankle_priority_1_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(foot_ankle_enter_priority_2_timeseries, file = "foot_ankle_priority_2_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(foot_ankle_enter_priority_3_timeseries, file = "foot_ankle_priority_3_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(foot_ankle_exit_timeseries,             file = "foot_ankle_priority_all_exit_by_week_timeseries.csv", row.names = TRUE)

write.csv(hand_enter_priority_1_timeseries, file = "hand_priority_1_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hand_enter_priority_2_timeseries, file = "hand_priority_2_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hand_enter_priority_3_timeseries, file = "hand_priority_3_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hand_exit_timeseries,             file = "hand_priority_all_exit_by_week_timeseries.csv", row.names = TRUE)

write.csv(hip_enter_priority_1_timeseries, file = "hip_priority_1_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hip_enter_priority_2_timeseries, file = "hip_priority_2_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hip_enter_priority_3_timeseries, file = "hip_priority_3_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hip_exit_timeseries,             file = "hip_priority_all_exit_by_week_timeseries.csv", row.names = TRUE)

write.csv(hip_knee_enter_priority_1_timeseries, file = "hip_knee_priority_1_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hip_knee_enter_priority_2_timeseries, file = "hip_knee_priority_2_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hip_knee_enter_priority_3_timeseries, file = "hip_knee_priority_3_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(hip_knee_exit_timeseries,             file = "hip_knee_priority_all_exit_by_week_timeseries.csv", row.names = TRUE)

write.csv(knee_enter_priority_1_timeseries, file = "knee_priority_1_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(knee_enter_priority_2_timeseries, file = "knee_priority_2_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(knee_enter_priority_3_timeseries, file = "knee_priority_3_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(knee_exit_timeseries,             file = "knee_priority_all_exit_by_week_timeseries.csv", row.names = TRUE)

write.csv(shoulder_enter_priority_1_timeseries, file = "shoulder_priority_1_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(shoulder_enter_priority_2_timeseries, file = "shoulder_priority_2_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(shoulder_enter_priority_3_timeseries, file = "shoulder_priority_3_enter_by_week_timeseries.csv",  row.names = TRUE)
write.csv(shoulder_exit_timeseries,             file = "shoulder_priority_all_exit_by_week_timeseries.csv", row.names = TRUE)

