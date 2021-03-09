
# Clean-up environment
rm(list = ls())

# Load libraries
library(dplyr)
library(broom)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(data.table)
library(lfe)
library(readxl)


# Import Data -------------------------------------------------------------
df <- read_excel("619702-XLS-ENG.xlsx", sheet = 3)


# CREATE USEFUL COLUMNS ---------------------------------------------------
df <- df %>% mutate(
  time = strftime(period_start, "%H:%M:%S", usetz = FALSE),
  tot_trips = trips_pool + trips_express_pool,
  cost_per_trip = total_driver_payout/tot_trips,
  single_matches = total_matches - total_double_matches,
  unmatched_trips = tot_trips - total_matches
)

# Get the summary by different matching types of trip
summary(df %>% select(unmatched_trips, single_matches, total_double_matches, tot_trips))

# Calculate the matching rate for each type
df <- df %>% mutate(
  match_rate = total_matches/tot_trips,
  single_match_rate = single_matches/tot_trips,
  double_match_rate = total_double_matches/tot_trips,
  no_match_rate = unmatched_trips/tot_trips
)

# Create a summary of the average value for differnt wait times: overall, 2-min, 5-min
# Calculate for overall
summ <- df %>% summarize(wait_time = 'all',
                         tot_trips = mean(tot_trips),
                         tot_trips = mean(tot_trips), 
                         ep_trips = mean(trips_express_pool), 
                         p_trips = mean(trips_pool), 
                         cancellations = mean(rider_cancellations), 
                         cost_per_trip = mean(cost_per_trip), 
                         match_rate = mean(match_rate), 
                         double_match_rate = mean(double_match_rate))
# calculate for 2-min and 5-min, then merge vertically with the 'overall'
summ <- rbind(summ, df %>% 
                group_by(wait_time) %>% 
                summarize(
                  tot_trips = mean(tot_trips),
                  tot_trips = mean(tot_trips), 
                  ep_trips = mean(trips_express_pool), 
                  p_trips = mean(trips_pool), 
                  cancellations = mean(rider_cancellations), 
                  cost_per_trip = mean(cost_per_trip), 
                  match_rate = mean(match_rate), 
                  double_match_rate = mean(double_match_rate)
                  
                ))
#format
stargazer(summ, type = 'text', summary = FALSE, flip = TRUE, colnames = FALSE)



# PLOTS ---------------------------------------------------------------
# plot for total trips
p_tot <- ggplot(df, aes(x = tot_trips, fill = wait_time)) +
  geom_density(alpha = 0.5) +
  theme(legend.position = c(.8,.8)) +
  ggtitle("Total Ride-Sharing Trips") + 
  labs(fill = "Wait Time")
# plot for pool
p_pool <- ggplot(df, aes(x = trips_pool, fill = wait_time)) +
  geom_density(alpha = 0.5) +
  theme(legend.position = c(.8,.8)) +
  ggtitle("Total Ride-Sharing Trips") + 
  labs(fill = "Wait Time")
# plot for express pool
p_exp_pool <- ggplot(df, aes(x = trips_express_pool, fill = wait_time)) +
  geom_density(alpha = 0.5) +
  theme(legend.position = c(.8,.8)) +
  ggtitle("Total Ride-Sharing Trips") + 
  labs(fill = "Wait Time")
# plot for cancellation
p_cancel <- ggplot(df, aes(x = rider_cancellations, fill = wait_time)) +
  geom_density(alpha = 0.5) +
  theme(legend.position = c(.8,.8)) +
  ggtitle("Total Ride-Sharing Trips") + 
  labs(fill = "Wait Time")

# create a list for the plots
plotlist = list(p_tot, p_pool, p_exp_pool, p_cancel)
all_p <- grid.arrange(grobs = plotlist, ncol = 2)


# T-Test ------------------------------------------------------------------
# Turn df into a data table
df <- data.table(df)

t1 <- t.test(df[wait_time == '5 mins']$tot_trips, df[wait_time == '2 mins']$tot_trips)
t2 <- t.test(df[wait_time=="5 mins"]$trips_pool,df[wait_time=="2 mins"]$trips_pool)
t3 <- t.test(df[wait_time=="5 mins"]$trips_express_pool,df[wait_time=="2 mins"]$trips_express)
t4 <- t.test(df[wait_time=="5 mins"]$rider_cancellations,df[wait_time=="2 mins"]$rider_cancellations)
t5 <- t.test(df[wait_time=="5 mins"]$cost_per_trip,df[wait_time=="2 mins"]$cost_per_trip)
t6 <- t.test(df[wait_time=="5 mins"]$match_rate,df[wait_time=="2 mins"]$match_rate)
t7 <- t.test(df[wait_time=="5 mins"]$double_match_rate,df[wait_time=="2 mins"]$double_match_rate)

# get the t-test for rush hour
t8 <- t.test(df[wait_time == '5 mins' & commute == TRUE]$tot_trips, df[wait_time == '2 mins' & commute == TRUE]$tot_trips)
t9 <- t.test(df[wait_time=="5 mins" & commute == TRUE]$match_rate,df[wait_time=="2 mins" & commute == TRUE]$match_rate)
t10 <- t.test(df[wait_time=="5 mins" & commute == TRUE]$cost_per_trip,df[wait_time=="2 mins" & commute == TRUE]$cost_per_trip)
# get the t-test for Non-rush hour
t11 <- t.test(df[wait_time == '5 mins' & commute == FALSE]$tot_trips, df[wait_time == '2 mins' & commute == FALSE]$tot_trips)
t12 <- t.test(df[wait_time=="5 mins" & commute == FALSE]$match_rate,df[wait_time=="2 mins" & commute == FALSE]$match_rate)
t13 <- t.test(df[wait_time=="5 mins" & commute == FALSE]$cost_per_trip,df[wait_time=="2 mins" & commute == FALSE]$cost_per_trip)

save.t.test <- capture.output(t1, t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)
writeLines(save.t.test, con = file("t.test.output.txt"))

# REGRESSION --------------------------------------------------------------
# Regressions: base results - LOGS
r1 <- felm(log(tot_trips) ~ treat, data = df)
r2 <- felm(log(trips_pool) ~ treat, data = df)
r3 <- felm(log(trips_express_pool) ~ treat, data = df)
r4 <- felm(log(rider_cancellations) ~ treat, data = df)
r5 <- felm(log(cost_per_trip) ~ treat, data = df)
r6 <- felm(match_rate ~ treat, data = df)
r7 <- felm(double_match_rate ~ treat, data = df)
stargazer(r1,r2,r3,r4,r5,r6,r7,type = "text",title = "Baseline Results - Trips/Cancellations/Costs in Logs",
          dep.var.labels = c('Tot Trips', 'POOL Trips', 'Express POOL Trips', 'Cancels', 'Cost/Trip', 'Match Rate', 'Double Match Rate'),
          out = "base_results_logs.txt")

# Regressions: base results - LEVELS
r1 <- felm(tot_trips ~ treat, data = df)
r2 <- felm(trips_pool ~ treat, data = df)
r3 <- felm(trips_express_pool ~ treat, data = df)
r4 <- felm(rider_cancellations ~ treat, data = df)
r5 <- felm(cost_per_trip ~ treat, data = df)
r6 <- felm(match_rate ~ treat, data = df)
r7 <- felm(double_match_rate ~ treat, data = df)
stargazer(r1,r2,r3,r4,r5,r6,r7,type = "text",title = "Baseline Results - Trips/Cancellations/Costs in Levels",
          dep.var.labels = c('Tot Trips', 'POOL Trips', 'Express POOL Trips', 'Cancels', 'Cost/Trip', 'Match Rate', 'Double Match Rate'),
          out = "base_results_levels.txt")

p <- ggplot(df, aes(y = tot_trips, x = treat)) + geom_point() + geom_smooth(method='lm')
p



# Regressions: results for commuting hours - LOGS
r1 <- felm(log(tot_trips) ~ treat, data = df[commute=="TRUE"])
r2 <- felm(log(trips_pool) ~ treat, data = df[commute=="TRUE"])
r3 <- felm(log(trips_express_pool) ~ treat, data = df[commute=="TRUE"])
r4 <- felm(log(rider_cancellations) ~ treat, data = df[commute=="TRUE"])
r5 <- felm(log(cost_per_trip) ~ treat, data = df[commute=="TRUE"])
r6 <- felm(match_rate ~ treat, data = df[commute=="TRUE"])
r7 <- felm(double_match_rate ~ treat, data = df[commute=="TRUE"])
stargazer(r1,r2,r3,r4,r5,r6,r7,type = "text",title = "Commuting Hours - Trips/Cancellations/Costs in Logs",
          dep.var.labels = c('Tot Trips', 'POOL Trips', 'Express POOL Trips', 'Cancels', 'Cost/Trip', 'Match Rate', 'Double Match Rate'),
          out = "commute_results_logs.txt")

# Regressions: results for commuting hours - LEVELS
r1 <- felm(tot_trips ~ treat, data = df[commute=="TRUE"])
r2 <- felm(trips_pool ~ treat, data = df[commute=="TRUE"])
r3 <- felm(trips_express_pool ~ treat, data = df[commute=="TRUE"])
r4 <- felm(rider_cancellations ~ treat, data = df[commute=="TRUE"])
r5 <- felm(cost_per_trip ~ treat, data = df[commute=="TRUE"])
r6 <- felm(match_rate ~ treat, data = df[commute=="TRUE"])
r7 <- felm(double_match_rate ~ treat, data = df[commute=="TRUE"])
stargazer(r1,r2,r3,r4,r5,r6,r7,type = "text",title = "Commuting Hours - Trips/Cancellations/Costs in Levels",
          dep.var.labels = c('Tot Trips', 'POOL Trips', 'Express POOL Trips', 'Cancels', 'Cost/Trip', 'Match Rate', 'Double Match Rate'),
          out = "commute_results_levels.txt")

# Regressions: results for non-commuting hours - LOGS
r1 <- felm(log(tot_trips) ~ treat, data = df[commute!="TRUE"])
r2 <- felm(log(trips_pool) ~ treat, data = df[commute!="TRUE"])
r3 <- felm(log(trips_express_pool) ~ treat, data = df[commute!="TRUE"])
r4 <- felm(log(rider_cancellations) ~ treat, data = df[commute!="TRUE"])
r5 <- felm(log(cost_per_trip) ~ treat, data = df[commute!="TRUE"])
r6 <- felm(match_rate ~ treat, data = df[commute!="TRUE"])
r7 <- felm(double_match_rate ~ treat, data = df[commute!="TRUE"])
stargazer(r1,r2,r3,r4,r5,r6,r7,type = "text",title = "Non Commuting Hours - Trips/Cancellations/Costs in Logs",
          dep.var.labels = c('Tot Trips', 'POOL Trips', 'Express POOL Trips', 'Cancels', 'Cost/Trip', 'Match Rate', 'Double Match Rate'),
          out = "noncommute_results_logs.txt")

# Regressions: results for non-commuting hours - LEVELS
r1 <- felm(tot_trips ~ treat, data = df[commute!="TRUE"])
r2 <- felm(trips_pool ~ treat, data = df[commute!="TRUE"])
r3 <- felm(trips_express_pool ~ treat, data = df[commute!="TRUE"])
r4 <- felm(rider_cancellations ~ treat, data = df[commute!="TRUE"])
r5 <- felm(cost_per_trip ~ treat, data = df[commute!="TRUE"])
r6 <- felm(match_rate ~ treat, data = df[commute!="TRUE"])
r7 <- felm(double_match_rate ~ treat, data = df[commute!="TRUE"])
stargazer(r1,r2,r3,r4,r5,r6,r7,type = "text",title = "Non Commuting Hours - Trips/Cancellations/Costs in Levels",
          dep.var.labels = c('Tot Trips', 'POOL Trips', 'Express POOL Trips', 'Cancels', 'Cost/Trip', 'Match Rate', 'Double Match Rate'),
          out = "noncommute_results_levels.txt")

# Clearing the plots
dev.off()

# Clearing the console (Ctrl+L)
cat("\014")

# Clearing the R environment
rm(list = ls())