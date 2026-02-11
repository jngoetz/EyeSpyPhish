library(mgcv)
library(DHARMa)
library(mgcViz)
library(plyr)
library(dplyr)
library(gratia)


setwd("I:/Cyber")
df <- read.csv("aggregate_data.csv")  %>%
  mutate(subject = as.factor(RECORDING_SESSION_LABEL))


model_linear <- gam(legitpresence ~ 
                      overall_duration_mean + 
                      mean_total_time + 
                      overall_count_mean + 
                      overall_amplitude_mean + 
                      mean_total_distance, 
                    data = df,
                    family = binomial(link = "logit"))
summary(model_linear)


model_nonlinear_select <- gam(legitpresence ~ 
                         s(overall_duration_mean, k = 10) + 
                         s(mean_total_time, k = 10) + 
                         s(overall_count_mean, k = 10) + 
                         s(overall_amplitude_mean, k = 10) + 
                         s(mean_total_distance, k = 10),
             data = df,
             family = binomial(link = "logit"),
             method = "REML",
             select = TRUE)
summary(model_nonlinear_select)

gam.check(model_nonlinear_select) # checks if k is high enough


model_nonlinear <- gam(legitpresence ~ 
                         s(overall_duration_mean, k = 10) + 
                         s(mean_total_time, k = 10) + 
                         s(overall_count_mean, k = 10) + 
                         s(overall_amplitude_mean, k = 10) + 
                         s(mean_total_distance, k = 10),
                       data = df,
                       family = binomial(link = "logit"),
                       method = "REML")
summary(model_nonlinear)

simulationOutput <- simulateResiduals(fittedModel = model_nonlinear, plot = T) # checks assumptions for GAM
draw(model_nonlinear, scales = "free")


AIC(model_linear, model_nonlinear)
