################################################################################
# Austin Edmunds                                                               #
# 2/28/2023                                                                    #
# Cherry Tree Blossom Analysis Code                                            #
# STAT 490                                                                     #
################################################################################

# First, we need to install a couple of packages prior to doing any coding or 
# analyzation. We will use these to modify our data and plot it easier and also
# load in historical temperatures to use in our prediction model.

install.packages("tidyverse")

library(tidyverse)

install.packages("rnoaa")

library(rnoaa)

# Then we need to load in our actual data from the 3 locations that have 
# historical bloom dates. 

cherry <- read.csv("~/GMU/Current Classes/STAT 490/Cherry_tree_project/Cherry Tree Predictions/data/kyoto.csv") %>%
    bind_rows(read.csv("~/GMU/Current Classes/STAT 490/Cherry_tree_project/Cherry Tree Predictions/data/washingtondc.csv")) %>%
    bind_rows(read.csv("~/GMU/Current Classes/STAT 490/Cherry_tree_project/Cherry Tree Predictions/data/liestal.csv"))

# Then we can look at our data to see the different variables attached and for 
# what years we have data.

View(cherry)

# Plotting the data at each location to compare bloom dates

cherry %>% 
    filter(year >= 1900) %>%
    ggplot(aes(x = year, y = bloom_doy)) +
    geom_line(color = "blue", linetype = "dashed") +
    geom_point(size = 2, color = "blue") +
    scale_x_continuous(breaks = seq(1900, 2020, by = 20)) +
    facet_grid(cols = vars(str_to_title(location))) +
    labs(x = "Year",
         y = "Peak bloom date (Days Since Jan. 1st)",
         title = "Time Series Plot of Peak Bloom Date by Location
         ") +
    theme(plot.title = element_text(hjust = 0.5))

# Now we are going to create functions in order to find the temperatures from
# previous years using the "rnoaa" packahe available in R. One will be to find
# the min temperature and one will find the max temperature, and they also find
# each temperature by season. 

get_max_temperature <- function (stationid) {
    ghcnd_search(stationid = stationid, var = c("tmax"),
                 date_min = "1980-01-01", date_max = "2023-01-31")[[1]] %>%
        mutate(year = as.integer(format(date, "%Y")),
               month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
               season = cut(month, breaks = c(0, 2, 5, 8, 11),
                            include.lowest = TRUE,
                            labels = c("Winter", "Spring", "Summer", "Fall")),
               year = if_else(month == 0, year + 1L, year)) %>%
        group_by(year, season) %>%
        summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}


get_min_temperature <- function (stationid) {
    ghcnd_search(stationid = stationid, var = c("tmin"),
                 date_min = "1980-01-01", date_max = "2023-01-31")[[1]] %>%
        mutate(year = as.integer(format(date, "%Y")),
               month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
               season = cut(month, breaks = c(0, 2, 5, 8, 11),
                            include.lowest = TRUE,
                            labels = c("Winter", "Spring", "Summer", "Fall")),
               year = if_else(month == 0, year + 1L, year)) %>%
        group_by(year, season) %>%
        summarize(tmin_avg = mean(tmin, na.rm = TRUE))
}

# Using the created functions to pull min and max temperature averages for 
# every year since 1980 for all 4 locations by season. 

historic_max_temperatures <-
    tibble(location = "washingtondc", get_max_temperature("USC00186350")) %>%
    bind_rows(tibble(location = "liestal", get_max_temperature("GME00127786"))) %>%
    bind_rows(tibble(location = "kyoto", get_max_temperature("JA000047759"))) %>%
    bind_rows(tibble(location = "vancouver", get_max_temperature("CA001108395")))

historic_min_temperatures <-
    tibble(location = "washingtondc", get_min_temperature("USC00186350")) %>%
    bind_rows(tibble(location = "liestal", get_min_temperature("GME00127786"))) %>%
    bind_rows(tibble(location = "kyoto", get_min_temperature("JA000047759"))) %>%
    bind_rows(tibble(location = "vancouver", get_min_temperature("CA001108395")))

# Now we join the two datasets together and create a new variable that is the 
# midpoint temperature for each year based on season. We are going to only look
# at the temperatures in the Winter and Spring season, since these two are the 
# main indicators in the bloom date due to chilling degree days and growing
# degree days. 

forecast_temp <-
    historic_max_temperatures %>%
    left_join(historic_min_temperatures) %>%
    mutate(temp_avg = (tmax_avg/10 + tmin_avg/10) / 2) %>%
    select(-c(tmax_avg, tmin_avg)) %>%
    filter(season %in% c("Winter", "Spring"))

# Now we can plot the temperature by year to check our work. We will see that
# the temperature is increasing with time, which is what we want to see since 
# that is what is happening. 

forecast_temp %>%
    filter(season == "Spring") %>%
    ggplot() +
    aes(year, temp_avg) +
    geom_point() +
    facet_wrap(~location) +
    geom_smooth(method = "lm", color = "red", fullrange=TRUE) +
    xlim(1980, 2030) +
    labs(x = "Year",
         y = "Average Temperature (In Celsius)",
         title = "Time Series Plot of Average Temperature by Location for Spring
         Seasons") +
    theme(plot.title = element_text(hjust = 0.5))

forecast_temp %>%
    filter(season == "Winter") %>%
    ggplot() +
    aes(year, temp_avg) +
    geom_point() +
    facet_wrap(~location) +
    geom_smooth(method = "lm", color = "red", fullrange=TRUE) +
    xlim(1980, 2030) +
    labs(x = "Year",
         y = "Average Temperature (In Celsius)",
         title = "Time Series Plot of Average Temperature by Location for Spring
         Seasons") +
    theme(plot.title = element_text(hjust = 0.5))

# Here we take the midpoint temperature from the spring and winter seasons to
# create a final variable "temp_avg" which is the average temperature of the 
# spring and winter midpoint temperatures. We also get rid of all the duplicate
# values because it duplicated each temperature. 

forecast_temp <-
    forecast_temp %>%
    group_by(year, location) %>%
    mutate(temp_avg = mean(temp_avg)) %>%
    distinct(temp_avg)

# Now we will perform linear regression in order to predict the next 10 years'
# average temperatures, since that will be how we will make our prediction 
# model for the bloom date for the next 10 years.

temp_kyoto <- lm(temp_avg ~ year, data = forecast_temp, subset = (location == "kyoto"))

temp_liestal <- lm(temp_avg ~ year, data = forecast_temp, subset = (location == "liestal"))

temp_vancouver <- lm(temp_avg ~ year, data = forecast_temp, subset = (location == "vancouver"))

temp_washingtondc <- lm(temp_avg ~ year, data = forecast_temp, subset = (location == "washingtondc"))

# Now that we have all of the models for our temperatures, we can use them to 
# predict the temperatures that we need for our bloom date prediction model.

#KYOTO

temp_predict_kyoto <- data.frame(year = 2023:2032,
                                        location = "kyoto",
                                        temp_avg = NA)

predict(temp_kyoto, newdata = temp_predict_kyoto)

temp_predict_kyoto[3] = c(10.23231, 10.24859, 10.26488, 10.28117, 10.29746,
                          10.31374, 10.33003, 10.34632, 10.36261, 10.37889)

#LIESTAL

temp_predict_liestal <- data.frame(year = 2023:2032,
                                        location = "liestal",
                                        temp_avg = NA)

predict(temp_liestal, newdata = temp_predict_liestal)

temp_predict_liestal[3] = c(7.426667, 7.453363, 7.480060, 7.506756, 7.533452,
                            7.560148, 7.586844, 7.613540, 7.640236, 7.666932)

#VANCOUVER

temp_predict_vancouver <- data.frame(year = 2023:2032,
                                        location = "vancouver",
                                        temp_avg = NA)

predict(temp_vancouver, newdata = temp_predict_vancouver)

temp_predict_vancouver[3] = c(6.548913, 6.543382, 6.537851, 6.532320, 6.526790,
                              6.521259, 6.515728, 6.510197, 6.504666, 6.499135)

#WASHINGTON DC

temp_predict_washingtondc <- data.frame(year = 2023:2032,
                                        location = "washingtondc",
                                        temp_avg = NA)

predict(temp_washingtondc, newdata = temp_predict_washingtondc)

temp_predict_washingtondc[3] = c(9.124042, 9.172560, 9.221079, 9.269597,
                                 9.318116, 9.366634, 9.415153, 9.463672,
                                 9.512190, 9.560709)

# Now to combine all of the temperatures into one common date frame with our
# other temperatures that were given in our rnoaa package. 

updated_forecast_temp <-
    forecast_temp %>%
    bind_rows(temp_predict_kyoto) %>%
    bind_rows(temp_predict_liestal) %>%
    bind_rows(temp_predict_vancouver) %>%
    bind_rows(temp_predict_washingtondc) %>%
    arrange(location)

# And then we can join our previous "cherry" dataframe that contains the 
# bloom date so that we can now perform our prediction on the data. 
    
updated_cherry <-
    left_join(updated_forecast_temp, cherry, by = join_by(year, location), keep = FALSE)

# Deleting one value that was duplicated incorrectly. 

updated_cherry <- updated_cherry[-44,]

# Here we need to remove the data with no information, which would be the data
# on vancouver and any data past this year. 

cherry <-
    updated_cherry %>%
    filter(location != "vancouver") %>%
    filter(year < 2024)

# Here we fit our linear regression and we are using the temp_avg variable that
# we created earlier as well as the year and then interacting on the location
# that the trees are located at. 

lm_fit1 <- lm(bloom_doy ~ temp_avg + year * location, data = cherry)

# Now we can find our predictions from our model for the bloom date. 

predictions <- expand_grid(location = unique(cherry$location),
                           year = 1980:2032) %>%
    left_join(updated_cherry) %>%
    bind_cols(predicted_doy = predict(lm_fit1, newdata = .))


# Function that converts the day of peak bloom to the actual date for the
# specific year. 

doy_to_date <- function (year, doy) {
    strptime(paste(year, doy, sep = '-'), '%Y-%j') %>% # create date object
        strftime('%Y-%m-%d') # translate back to date string in ISO 8601 format
}

# Using our new date function to alter our previous predictions. 

predictions <-
    predictions %>% 
    filter(year > 2022) %>% 
    mutate(predicted_doy = round(predicted_doy),
           predicted_date = doy_to_date(year, predicted_doy))

# Here we use just the temperature and previous data to predict the bloom date
# for all sites, which is what we will use to predict for vancouver since there
# are no previous records for vancouver to go off of. 

lm_fit2 <- lm(bloom_doy ~ temp_avg + year, data = cherry)

predictions_vancouver <- tibble(location = 'vancouver', year = 2023:2032) %>% 
    left_join(updated_cherry) %>%
    bind_cols(predicted_doy = round(predict(lm_fit2, newdata = .)))

# These are the predictions for vancouver based on all locations and the
# temperatures. 

predictions_vancouver

# Removing an incorrect duplicate value. 

predictions_vancouver <- predictions_vancouver[-1,]

# Combining the two predictions into one final data frame for submission. 

predictions <- predictions %>%
    bind_rows(predictions_vancouver) %>%
    select(year, location, predicted_doy)

# Altering the data so it is ready for submission

submission_predictions <- predictions %>% 
    filter(year > 2022) %>% 
    pivot_wider(names_from = 'location', values_from = 'predicted_doy') %>% 
    select(year, kyoto, liestal, washingtondc, vancouver)

# Writing the data to the correct file for submission

write.csv(submission_predictions, file = "cherry_tree_predictions.csv",
          row.names = FALSE)
