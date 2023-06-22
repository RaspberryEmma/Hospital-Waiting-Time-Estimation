# ****************************************
# The doctor will see you now - Personalised Waiting Times
#
# Presenting Results from Predictive Tool Simulation
#
# Emma Tarmey
# 05/09/2022
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


# results of simulation (N = 100)
setwd("...")
foot_ankle_p1_N100_final_results <- read.csv("foot_ankle_p1_N100_final_results.csv")$x
hand_p1_N100_final_results       <- read.csv("hand_p1_N100_final_results.csv")$x
hip_p1_N100_final_results        <- read.csv("hip_p1_N100_final_results.csv")$x
hip_knee_p1_N100_final_results   <- read.csv("hip_knee_p1_N100_final_results.csv")$x
knee_p1_N100_final_results       <- read.csv("knee_p1_N100_final_results.csv")$x
shoulder_p1_N100_final_results   <- read.csv("shoulder_p1_N100_final_results.csv")$x

p1_N100_final_results <- data.frame(foot_ankle_p1_N100_final_results,
                                    hand_p1_N100_final_results,
                                    hip_knee_p1_N100_final_results,
                                    hip_knee_p1_N100_final_results,
                                    knee_p1_N100_final_results,
                                    shoulder_p1_N100_final_results)

foot_ankle_p2_N100_final_results <- read.csv("foot_ankle_p2_N100_final_results.csv")$x
hand_p2_N100_final_results       <- read.csv("hand_p2_N100_final_results.csv")$x
hip_p2_N100_final_results        <- read.csv("hip_p2_N100_final_results.csv")$x
hip_knee_p2_N100_final_results   <- read.csv("hip_knee_p2_N100_final_results.csv")$x
knee_p2_N100_final_results       <- read.csv("knee_p2_N100_final_results.csv")$x
shoulder_p2_N100_final_results   <- read.csv("shoulder_p2_N100_final_results.csv")$x

p2_N100_final_results <- data.frame(foot_ankle_p2_N100_final_results,
                                    hand_p2_N100_final_results,
                                    hip_knee_p2_N100_final_results,
                                    hip_knee_p2_N100_final_results,
                                    knee_p2_N100_final_results,
                                    shoulder_p2_N100_final_results)


# results of simulation (N = 1000)
setwd("...")
foot_ankle_p1_N1000_final_results <- read.csv("foot_ankle_p1_N1000_final_results.csv")$x
hand_p1_N1000_final_results       <- read.csv("hand_p1_N1000_final_results.csv")$x
hip_p1_N1000_final_results        <- read.csv("hip_p1_N1000_final_results.csv")$x
hip_knee_p1_N1000_final_results   <- read.csv("hip_knee_p1_N1000_final_results.csv")$x
knee_p1_N1000_final_results       <- read.csv("knee_p1_N1000_final_results.csv")$x
shoulder_p1_N1000_final_results   <- read.csv("shoulder_p1_N1000_final_results.csv")$x

p1_N1000_final_results <- data.frame(foot_ankle_p1_N1000_final_results,
                                     hand_p1_N1000_final_results,
                                     hip_knee_p1_N1000_final_results,
                                     hip_knee_p1_N1000_final_results,
                                     knee_p1_N1000_final_results,
                                     shoulder_p1_N1000_final_results)

foot_ankle_p2_N1000_final_results <- read.csv("foot_ankle_p2_N1000_final_results.csv")$x
hand_p2_N1000_final_results       <- read.csv("hand_p2_N1000_final_results.csv")$x
hip_p2_N1000_final_results        <- read.csv("hip_p2_N1000_final_results.csv")$x
hip_knee_p2_N1000_final_results   <- read.csv("hip_knee_p2_N1000_final_results.csv")$x
knee_p2_N1000_final_results       <- read.csv("knee_p2_N1000_final_results.csv")$x
shoulder_p2_N1000_final_results   <- read.csv("shoulder_p2_N1000_final_results.csv")$x

p2_N1000_final_results <- data.frame(foot_ankle_p2_N1000_final_results,
                                     hand_p2_N1000_final_results,
                                     hip_knee_p2_N1000_final_results,
                                     hip_knee_p2_N1000_final_results,
                                     knee_p2_N1000_final_results,
                                     shoulder_p2_N1000_final_results)


# ----- Summaries of Results -----

summary( foot_ankle_p1_N100_final_results)
summary( hand_p1_N100_final_results)
summary( hip_p1_N100_final_results)
summary( hip_knee_p1_N100_final_results)
summary( knee_p1_N100_final_results)
summary( shoulder_p1_N100_final_results)

summary( foot_ankle_p2_N100_final_results)
summary( hand_p2_N100_final_results)
summary( hip_p2_N100_final_results)
summary( hip_knee_p2_N100_final_results)
summary( knee_p2_N100_final_results)
summary( shoulder_p2_N100_final_results)

summary( foot_ankle_p1_N1000_final_results)
summary( hand_p1_N1000_final_results)
summary( hip_p1_N1000_final_results)
summary( hip_knee_p1_N1000_final_results)
summary( knee_p1_N1000_final_results)
summary( shoulder_p1_N1000_final_results)

summary( foot_ankle_p2_N1000_final_results)
summary( hand_p2_N1000_final_results)
summary( hip_p2_N1000_final_results)
summary( hip_knee_p2_N1000_final_results)
summary( knee_p2_N1000_final_results)
summary( shoulder_p2_N1000_final_results)




# ----- Graphs of Results -----

colors <- c("Foot & Ankle Queue Clear Time" = "red",
            "Hand Queue Clear Time"         = "blue",
            "Hip Queue Clear Time"          = "green",
            "Hip & Knee Queue Clear Time"   = "yellow",
            "Knee Queue Clear Time"         = "purple",
            "Shoulder Queue Clear Time"     = "black")
ggplot() + 
    geom_density(data = p1_N100_final_results,
                 aes(x = foot_ankle_p1_N100_final_results, color="Foot & Ankle Queue Clear Time")) + 
    geom_vline(data = p1_N100_final_results,
               aes(xintercept = mean(foot_ankle_p1_N100_final_results)),
               color    = "red",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N100_final_results,
                 aes(x = hand_p1_N100_final_results,       color="Hand Queue Clear Time")) +
    geom_vline(data = p1_N100_final_results,
               aes(xintercept = mean(hand_p1_N100_final_results)),
               color    = "blue",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N100_final_results,
                 aes(x = hip_p1_N100_final_results,        color="Hip Queue Clear Time")) +
    geom_vline(data = p1_N100_final_results,
               aes(xintercept = mean(hip_p1_N100_final_results)),
               color    = "green",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N100_final_results,
                 aes(x = hip_knee_p1_N100_final_results,   color="Hip & Knee Queue Clear Time")) +
    geom_vline(data = p1_N100_final_results,
               aes(xintercept = mean(hip_knee_p1_N100_final_results)),
               color    = "yellow",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N100_final_results,
                 aes(x = knee_p1_N100_final_results,       color="Knee Queue Clear Time")) +
    geom_vline(data = p1_N100_final_results,
               aes(xintercept = mean(knee_p1_N100_final_results)),
               color    = "purple",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N100_final_results,
                 aes(x = shoulder_p1_N100_final_results,   color="Shoulder Queue Clear Time")) +
    geom_vline(data = p1_N100_final_results,
               aes(xintercept = mean(shoulder_p1_N100_final_results)),
               color    = "black",
               linetype = "dashed",
               size     = 0.5) +
    ggtitle( paste( "Referral to Treatment Estimated Queue Clear Time Distributions", "Priority 1 Routine", "(N = 100)" ) ) +
    labs(x = "Overall Waiting Times (weeks)", y = "Density", color = "Legend") +
    scale_color_manual(values = colors)

ggplot() + 
    geom_density(data = p2_N100_final_results,
                 aes(x = foot_ankle_p2_N100_final_results, color="Foot & Ankle Queue Clear Time")) + 
    geom_vline(data = p2_N100_final_results,
               aes(xintercept = mean(foot_ankle_p2_N100_final_results)),
               color    = "red",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N100_final_results,
                 aes(x = hand_p2_N100_final_results,       color="Hand Queue Clear Time")) +
    geom_vline(data = p2_N100_final_results,
               aes(xintercept = mean(hand_p2_N100_final_results)),
               color    = "blue",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N100_final_results,
                 aes(x = hip_p2_N100_final_results,        color="Hip Queue Clear Time")) +
    geom_vline(data = p2_N100_final_results,
               aes(xintercept = mean(hip_p2_N100_final_results)),
               color    = "green",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N100_final_results,
                 aes(x = hip_knee_p2_N100_final_results,   color="Hip & Knee Queue Clear Time")) +
    geom_vline(data = p2_N100_final_results,
               aes(xintercept = mean(hip_knee_p2_N100_final_results)),
               color    = "yellow",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N100_final_results,
                 aes(x = knee_p2_N100_final_results,       color="Knee Queue Clear Time")) +
    geom_vline(data = p2_N100_final_results,
               aes(xintercept = mean(knee_p2_N100_final_results)),
               color    = "purple",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N100_final_results,
                 aes(x = shoulder_p2_N100_final_results,   color="Shoulder Queue Clear Time")) +
    geom_vline(data = p2_N100_final_results,
               aes(xintercept = mean(shoulder_p2_N100_final_results)),
               color    = "black",
               linetype = "dashed",
               size     = 0.5) +
    ggtitle( paste( "Referral to Treatment Estimated Queue Clear Time Distributions", "Priority 2 Urgent", "(N = 100)" ) ) +
    labs(x = "Overall Waiting Times (weeks)", y = "Density", color = "Legend") +
    scale_color_manual(values = colors)

ggplot() + 
    geom_density(data = p1_N1000_final_results,
                 aes(x = foot_ankle_p1_N1000_final_results, color="Foot & Ankle Queue Clear Time")) + 
    geom_vline(data = p1_N1000_final_results,
               aes(xintercept = mean(foot_ankle_p1_N1000_final_results)),
               color    = "red",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N1000_final_results,
                 aes(x = hand_p1_N1000_final_results,       color="Hand Queue Clear Time")) +
    geom_vline(data = p1_N1000_final_results,
               aes(xintercept = mean(hand_p1_N1000_final_results)),
               color    = "blue",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N1000_final_results,
                 aes(x = hip_p1_N1000_final_results,        color="Hip Queue Clear Time")) +
    geom_vline(data = p1_N1000_final_results,
               aes(xintercept = mean(hip_p1_N1000_final_results)),
               color    = "green",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N1000_final_results,
                 aes(x = hip_knee_p1_N1000_final_results,   color="Hip & Knee Queue Clear Time")) +
    geom_vline(data = p1_N1000_final_results,
               aes(xintercept = mean(hip_knee_p1_N1000_final_results)),
               color    = "yellow",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N1000_final_results,
                 aes(x = knee_p1_N1000_final_results,       color="Knee Queue Clear Time")) +
    geom_vline(data = p1_N1000_final_results,
               aes(xintercept = mean(knee_p1_N1000_final_results)),
               color    = "purple",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p1_N1000_final_results,
                 aes(x = shoulder_p1_N1000_final_results,   color="Shoulder Queue Clear Time")) +
    geom_vline(data = p1_N1000_final_results,
               aes(xintercept = mean(shoulder_p1_N1000_final_results)),
               color    = "black",
               linetype = "dashed",
               size     = 0.5) +
    ggtitle( paste( "Referral to Treatment Estimated Queue Clear Time Distributions", "Priority 1 Routine", "(N = 1000)" ) ) +
    labs(x = "Overall Waiting Times (weeks)", y = "Density", color = "Legend") +
    scale_color_manual(values = colors)

ggplot() + 
    geom_density(data = p2_N1000_final_results,
                 aes(x = foot_ankle_p2_N1000_final_results, color="Foot & Ankle Queue Clear Time")) + 
    geom_vline(data = p2_N1000_final_results,
               aes(xintercept = mean(foot_ankle_p2_N1000_final_results)),
               color    = "red",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N1000_final_results,
                 aes(x = hand_p2_N1000_final_results,       color="Hand Queue Clear Time")) +
    geom_vline(data = p2_N1000_final_results,
               aes(xintercept = mean(hand_p2_N1000_final_results)),
               color    = "blue",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N1000_final_results,
                 aes(x = hip_p2_N1000_final_results,        color="Hip Queue Clear Time")) +
    geom_vline(data = p2_N1000_final_results,
               aes(xintercept = mean(hip_p2_N1000_final_results)),
               color    = "green",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N1000_final_results,
                 aes(x = hip_knee_p2_N1000_final_results,   color="Hip & Knee Queue Clear Time")) +
    geom_vline(data = p2_N1000_final_results,
               aes(xintercept = mean(hip_knee_p2_N1000_final_results)),
               color    = "yellow",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N1000_final_results,
                 aes(x = knee_p2_N1000_final_results,       color="Knee Queue Clear Time")) +
    geom_vline(data = p2_N1000_final_results,
               aes(xintercept = mean(knee_p2_N1000_final_results)),
               color    = "purple",
               linetype = "dashed",
               size     = 0.5) +
    geom_density(data = p2_N1000_final_results,
                 aes(x = shoulder_p2_N1000_final_results,   color="Shoulder Queue Clear Time")) +
    geom_vline(data = p2_N1000_final_results,
               aes(xintercept = mean(shoulder_p2_N1000_final_results)),
               color    = "black",
               linetype = "dashed",
               size     = 0.5) +
    ggtitle( paste( "Referral to Treatment Estimated Queue Clear Time Distributions", "Priority 2 Urgent", "(N = 1000)" ) ) +
    labs(x = "Overall Waiting Times (weeks)", y = "Density", color = "Legend") +
    scale_color_manual(values = colors)




