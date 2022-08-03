library(tidyverse)
library(tidytuesdayR)
library(tidymodels)

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 44)
ultra_rankings <- tuesdata$ultra_rankings
race <- tuesdata$race

glimpse(ultra_rankings)
glimpse(race)

#initial plot of age and time to get idea for shape of data
my.theme <- theme(
  plot.title = element_text(color = "black", size = 22, face = "bold"),
  axis.title = element_text(color = "black", size = 18),
  axis.text = element_text(color = "black", size = 16), 
  legend.text = element_text(color = "black", size = 12))

p1 <- ultra_rankings %>%
  ggplot(aes(age, time_in_seconds)) +
  geom_jitter(aes(color = race_year_id), alpha = 0.7, show.legend = FALSE) +
  labs(title = "Ultramarathon finishing time by Age", x = "Age", y = "Time (seconds)") +
  #theme(legend.position = "none") +
  theme_classic() +
  my.theme
p1

#initial observations
#change race_year_id to factor
#change 0 to NA in age (why so many 0s?)

#inner join
joined <- ultra_rankings %>%
  inner_join(race, by = "race_year_id")

#changing race_year_id, rank, gender, nationality to factor
joined <- joined %>%
  mutate(race_year_id = as.factor(race_year_id),
         rank = as.factor(rank),
         gender = as.factor(gender),
         nationality = as.factor(nationality))
glimpse(joined)


#leadville 100 distance is 0 - want it to be 100 (looked up distance to
#double check)
#first create new df with just leadville so can manipulate only those distances
leadville <- joined %>%
  filter(race_year_id == "25331")

#remove leadville stats with 0 distance from joined df
joined <- joined %>%
  filter(race_year_id != "25331")

#add in 100 mile distance in km to subsetted df
leadville$distance <-replace(leadville$distance, leadville$distance == 0, 160.9)

#row bind the two df's back together with updated leadville distance
joined <- joined %>%
  rbind(leadville)

#making 0's into na's so can get rid of all together
#if 0 value, not worth figuring out why - easier to just get rid of
#will still maintain dataset > 100000 rows
joined$age <- na_if(joined$age, 0)
joined$distance <- na_if(joined$distance, 0)
colSums(is.na(joined))

#drop all na's as throwing off some calculations and plots - can skip this step
#if want to keep them
joined <- joined %>%
  drop_na()
colSums(is.na(joined))

#according to original repo, age column is age at 2021, regardless of when race
#occurred (https://github.com/BjnNowak/UltraTrailRunning/blob/main/cleaning_script.R)
#must calculate age at race column
joined <- joined %>%
  mutate(Year = lubridate::year(date)) %>%
  mutate(AgeAtRace = age - (2021 - Year))

#distribution plot of ages - good plot to include
p2 <- joined %>%
  ggplot(aes(AgeAtRace)) +
  geom_histogram(aes(fill = gender)) +
  labs(title = "Frequency distribution of finisher ages", x = "Age",
       y = "Number of finishers") +
  theme_classic() +
  my.theme
p2
#filter out ages older than 75

p3 <- joined %>%
  filter(AgeAtRace < 75) %>%
  ggplot(aes(AgeAtRace, time_in_seconds)) +
  geom_jitter(aes(alpha = 0.7), show.legend = FALSE) +
  labs(title = "Ultramarathon finishing time by age", x = "Age",
       y = "Time (seconds)") +
  theme_classic() +
  my.theme
p3
#no clear pattern with age and time apparent

#get count of entries per race
joined %>%
  count(event, sort = TRUE)
#majority of events are under 2000 participants


#create speed column (kmph/mph) to standardize and be able to compare between
#courses and get average speed per course
#create mile column to have mileage if want to
joined <- joined %>%
  rename(distance_km = distance) %>%
  mutate(time_in_min = time_in_seconds/60,
         distance_mi = distance_km/1.609) %>%
  mutate(time_in_hrs = time_in_min/60) %>%
  mutate(mph = distance_mi/time_in_hrs)

#good plot to include - age and speed by gender
joined %>%
  filter(AgeAtRace < 75, mph < 20) %>%
  ggplot(aes(AgeAtRace, mph)) +
  geom_jitter(aes(color = gender), alpha = 0.5)

#good plot to include
joined %>%
  filter(mph < 20) %>%
  ggplot(aes(time_in_hrs, mph)) +
  geom_jitter(alpha = 0.3)
#speed decreases exponentially as time increases
#should data be transformed to be normal dist?

#filter out ages > 80 and speed > 20, time in min > 150
summary(joined)
joined <- joined %>% 
  filter(AgeAtRace < 80,
         time_in_min > 150,
         mph < 20)
summary(joined)

#distribution of speed
p4 <- joined %>% 
  ggplot(aes(mph)) +
  geom_histogram(bins = 25) +
  labs(title = "Speed distribution of ultramarathon finishers", x = "Speed (mph)",
       y = "Number of finishers") +
  theme_classic() +
  my.theme
p4
#log normal distribution - should use log when making model

#get average course speed, broken down by gender
event_avg <- joined %>%
  group_by(event) %>%
  summarise(course_speed = mean(mph)) %>% 
  ungroup()

#convert event to factor for plotting
event_avg <- event_avg %>%
  mutate(event = as.factor(event))
glimpse(event_avg)

#plot of speed per course showing top 10 races (n= 20)
p5 <- event_avg %>%
  top_n(20, course_speed) %>%
  ggplot(aes(fct_reorder(event, course_speed), course_speed)) +
  geom_col() +
  coord_flip() +
  labs(title = "Course speed of top 20 ultramarathon courses",
       y = "Course Speed",
       x = "Event name") +
  theme_classic() +
  my.theme
p5


#seeing if there are runners that have run multiple races
joined %>%
  count(runner, sort = TRUE)

#runner speed, arranged descending
joined %>%
  group_by(runner) %>%
  summarise(runner_speed = mean(mph)) %>%
  filter(runner_speed < 10) %>%
  arrange(desc(runner_speed))

#want to see runner speed per course
joined %>%
  group_by(event, runner) %>%
  summarise(runner_speed = mean(mph))

#mean speed by gender by age
p6 <- joined %>%
  filter(mph < 10, AgeAtRace < 75) %>%
  ggplot(aes(AgeAtRace, mph)) +
  geom_smooth(aes(color = gender)) +
  labs(title = "Mean speed by age and gender",
       x = "Age (years)",
       y = "Speed (mph)") +
  theme_classic() +
  my.theme
p6
#men speed declines faster than women
#women faster through middle age and more consistent as get older
#men speed increases after 60?
summary(joined)

#get percentage of men vs. women finishers by year total and broken down
#by event

#also compare mean time (speed) by year for men vs. women
#create new df with all this info?

#df of percentage of men vs. women finishers by year
gender_ratio <- joined %>%
  group_by(Year, gender) %>%
  summarise(count = n()) %>%
  mutate(per = count/sum(count) * 100) %>%
  mutate(per.round = round(per, 1)) %>%
  ungroup()
gender_ratio

#convert year to factor for plot - should check that factor for whole
#joined dataset
gender_ratio <- gender_ratio %>%
  mutate(Year = as.factor(Year))
glimpse(gender_ratio)

#make plot of above dataset
p7 <- gender_ratio %>% 
  ggplot(aes(Year, per, fill = gender, label = per.round)) +
  geom_col(aes(fill = gender)) +
  coord_flip() +
  geom_text(size = 4.5, position = position_stack(vjust = 0.3)) +
  labs(title = "Gender percentage of race finishers by year",
       y = "Percentage",
       x = "Year") +
  theme_classic() +
  my.theme
p7

#figure out which machine learning model is best - likely regression
#random forest regression?
#want to predict runner speed from various factors (age, gender,
#course length, elevation gain())

#creating model - linear regression and random forest regression
#first create modelling data set with only variables included in model
joined_mod <- joined %>%
  select(mph, AgeAtRace, gender, distance_mi, elevation_gain)

#split data into training and testing by gender
#large class imbalance within gender, so ensuring that each dataset has roughly
#equivalent proportion of men and women
set.seed(123)
race_split <- joined_mod %>%
  initial_split(prop = 0.8,
                strata = gender)

race_train <- training(race_split)
race_test <- testing(race_split)


#linear regression model specification
lm_mod <- linear_reg() %>%
  set_engine("lm")

#random forest model specification
rf_mod <- rand_forest() %>% 
  set_mode("regression") %>% 
  set_engine("randomForest")


#fitting linear regression and random forest models to data without bootstrapping
fit_lm <- lm_mod %>% 
  fit(log(mph) ~ .,
      data = race_train)
fit_lm

fit_rf <- rf_mod %>%
  fit(log(mph) ~ .,
      data = race_train)
fit_rf

#create 25 bootstrap resamples
race_boot <- bootstraps(race_train)

#fitting lm and rf models to data with bootstrap resampling
lm_res <- lm_mod %>% 
  fit_resamples(
    log(mph) ~ .,
    resamples = race_boot,
    control = control_resamples(save_pred = TRUE)
  )

#random forest takes super long time to run
rf_res <- rf_mod %>% 
  fit_resamples(
    log(mph) ~ .,
    resamples = race_boot,
    control = control_resamples(save_pred = TRUE)
  )

glimpse(lm_res)

#evaluating performance with metrics
collect_metrics(lm_res)
collect_metrics(rf_res)

#evaluating model performance on training data without bootstraps
results <- race_train %>% 
  mutate(mph = log(mph)) %>% 
  bind_cols(predict(fit_lm, race_train) %>% 
              rename(.pred_lm = .pred)) %>% 
  bind_cols(predict(fit_rf, race_train) %>% 
              rename(.pred_rf = .pred))

metrics(results, truth = mph, estimate = .pred_lm)
metrics(results, truth = mph, estimate = .pred_rf)

#evaluating model performance on testing data without bootstraps
results.b <- race_test %>% 
  mutate(mph = log(mph)) %>% 
  bind_cols(predict(fit_lm, race_test) %>% 
              rename(.pred_lm = .pred)) %>% 
  bind_cols(predict(fit_rf, race_test) %>% 
              rename(.pred_rf = .pred))

metrics(results.b, truth = mph, estimate = .pred_lm)
metrics(results.b, truth = mph, estimate = .pred_rf)

#evaluating fit for both models with bootstraps
results2 <-  bind_rows(lm_res %>%
                        collect_predictions() %>%
                        mutate(model = "Linear model"),
                      rf_res %>%
                        collect_predictions() %>%
                        mutate(model = "Random forest"))

p8 <- results2 %>%
  ggplot(aes(`log(mph)`, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(aes(color = id), size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(~ model) +
  labs(title = "Comparison of bootstrapped models",
       x = "Actual values",
       y = "Predicted values") +
  theme_classic() +
  my.theme
p8

#DON'T INCLUDE after this - not necessary to do bootstrapping on testing data
#bootstrapping, fitting, performance on testing dataset
#create 25 bootstrap resamples
race_boot2 <- bootstraps(race_test)

#fitting lm and rf models to data with bootstrap resampling
lm_res2 <- lm_mod %>% 
  fit_resamples(
    log(mph) ~ .,
    resamples = race_boot2,
    control = control_resamples(save_pred = TRUE)
  )

#random forest takes super long time to run
rf_res2 <- rf_mod %>% 
  fit_resamples(
    log(mph) ~ .,
    resamples = race_boot2,
    control = control_resamples(save_pred = TRUE)
  )

glimpse(lm_res)

#evaluating performance with metrics
collect_metrics(lm_res2)
collect_metrics(rf_res2)


#evaluating fit for both models with bootstraps
results4 <-  bind_rows(lm_res2 %>%
                         collect_predictions() %>%
                         mutate(model = "lm"),
                       rf_res2 %>%
                         collect_predictions() %>%
                         mutate(model = "rf"))

results4 %>%
  ggplot(aes(`log(mph)`, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(aes(color = id), size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(~ model)

#performance is better with bootstrapped rf instead of lm, even though plot
#doesn't really show that
#training data performed better than testing data, but only marginally

#READ maybe don't want to use testing data when resampling, as resampling
#serves purpose of simulating a new dataset and shows how the model may perform
#re-read tidymodels tutorial resampling ending for more info
