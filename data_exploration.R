### Self-quant
### Author: Kurt Peters, IXIS Digital (kurt@ixisdigital.com)
### Wed Dec 18 21:18:20 2019 


# LIBRARIES ####
library(tidyverse); library(reshape2)
# library(googlesheets)
library(googledrive); library(googlesheets4)
library(IXIS.Auto)
library(lubridate)

options(freezer = "./RDS")

# HRV
# CNS Tap
# Grip Strength (via Squegg Log)
# Daily Log (Google Form)
# Training Log (Google Form)
# Google Fit? (biking)
# External factors
#   temperature, UV index, hours of daylight
#   GCal meeting volume



# GOOGLE FORMS LOGS ####
# Note: This service account token was created following instructions here:
#       https://gargle.r-lib.org/articles/get-api-credentials.html#service-account-token
drive_auth(path = "~/self-quant-da1720ca6caa.json")
#sheets_auth(path = "~/self-quant-da1720ca6caa.json")
gs4_auth()

# !!NOTE!!
# Will need to share my service acct email address with each sheet explicitly:
# sq-take2@self-quant.iam.gserviceaccount.com
# GDC project admin: https://console.developers.google.com/iam-admin/serviceaccounts?project=self-quant

sh.get <- gs4_get("https://docs.google.com/spreadsheets/d/1iJ8pJQFfBsVb5a7U4VFyPfnUSBzlmRy6fbZ6iZwQwW8/edit#gid=1089961427")
glog.daily.0 <- range_read(ss = sh.get$spreadsheet_url, sheet = 1, col_types = "c")

sh.get <- sheets_get("https://docs.google.com/spreadsheets/d/1vIc_I-d6BzPLthQw3HxnS_WloWHjCqvCwaqlo-w0yUM/edit#gid=593645552")
glog.training.0 <- sheets_read(ss = sh.get$spreadsheet_url, sheet = 1, col_types = "c")
ggplot(aes(x = as.Date(Timestamp, format = "%m/%d/%Y"), y = `Overall Workout Ratings [Energy Level]`), data = glog.training.0) + geom_bar(stat = "identity")

sh.get <- sheets_get("https://docs.google.com/spreadsheets/d/1KoCYJ-i336L6s3irAFtyyFUyYph6mSafrNkoRCCASMA/edit#gid=1478806256")
glog.squegg.0 <- sheets_read(ss = sh.get$spreadsheet_url, sheet = 1, col_types = "c")
glog.squegg.0 %>% 
  melt(id.var = "Timestamp") %>% 
  ggplot(aes(x = as.Date(Timestamp, "%m/%d/%Y"), y = value, color = variable, group = variable)) +
  geom_line()

# For forms with a date field, I enter the date only if the log isn't on the same day
# So overwrite the date if it's entered
glog.daily.1 <- 
  glog.daily.0 %>% 
  mutate(
    dim_date = as.Date(Timestamp, format = "%m/%d/%Y"),
    dim_manualDate = as.Date(Date, format = "%m/%d/%Y"),
    dim_date = as.Date(ifelse(!is.na(dim_manualDate), paste(dim_manualDate), paste(dim_date)))
  ) %>% 
  rename(dim_dlTimestamp = Timestamp, dim_dlNotes = Notes) %>% 
  select(-Date, -dim_manualDate)
names(glog.daily.1) <- make.names(names(glog.daily.1))

glog.training.1 <- 
  glog.training.0 %>% 
  mutate(
    dim_date = as.Date(Timestamp, format = "%m/%d/%Y"),
    dim_manualDate = as.Date(Date, format = "%m/%d/%Y"),
    dim_date = as.Date(ifelse(!is.na(dim_manualDate), paste(dim_manualDate), paste(dim_date)))
  ) %>% 
  rename(dim_tlTimestamp = Timestamp, dim_tlNotes = Notes) %>% 
  select(-Date, -dim_manualDate)
names(glog.training.1) <- make.names(names(glog.training.1))

glog.squegg.1 <- 
  glog.squegg.0 %>% 
  mutate(
    dim_date = as.Date(Timestamp, format = "%m/%d/%Y")    
  ) %>% 
  select(dim_date, dynamometerRH = `Right Hand`, dynamometerLH = `Left Hand`)

# Join daily logs by date
glog.all.1 <- 
  list(
    glog.daily.1,
    glog.training.1,
    glog.squegg.1
  ) %>% 
  reduce(full_join, by = "dim_date")





# SCALE####
# Export: directly from app by viewing trends, selecting all-time, and saving to CSV
# Note: %BF seems to correlate perfectly with weight, so not informative as a separate metric
scale.0 <- read_csv("./data/RENPHO-Kurt-data.csv")
scale.1 <- 
  scale.0 %>% 
  mutate(
    dim_date = as_date(`Time of Measurement`, format = "%b %e, %Y", tz = Sys.timezone(location = TRUE)),
    weight = as.numeric(gsub("lb", "", Weight)),
    ffbw = as.numeric(gsub("lb", "", `Fat-free Body Weight`)),
  )

cor(scale.1$weight, scale.1$ffbw) #essentially identical

scale.1 %>% 
  ggplot(aes(x = dim_date, y = weight)) +
  geom_line() +
  geom_smooth(span = .25) +
  scale_x_date(date_breaks = "1 month", minor_breaks = "1 week") +
  scale_y_continuous(minor_breaks = 1:200) +
  theme_bw()


ts.weight <- ts(scale.1$weight, frequency = 90)
plot(decompose(ts.weight))
plot(stl(ts.weight, s.window = "periodic"))


# Frequency: The units are the reciprocal of cycles per unit time
# > Thinking... spectral analysis is useful for analyzing cyclic patterns,
#   i.e., repeating but not periodic (seasonal) patterns in the ts variance
#   So HF indicates processes that cause short-term cycles
#   Whereas LF indicates processes that cause longer-term cycles
ts(scale.1$weight, frequency = 7) %>% 
  spec.pgram(detrend = TRUE, spans = c(3, 5))


# library(prophet)
# fit.prophet <- 
#   scale.1 %>% 
#   select(ds = dim_date, y = weight) %>% 
#   mutate(y = log(y)) %>% 
#   prophet()
# future <- make_future_dataframe(fit.prophet, periods = 90)
# fcast <- predict(fit.prophet, future)
# plot(fit.prophet, fcast)



# library(forecast)
# plot(forecast(auto.arima(ts.weight), h = 90))



# CNS Tap Test ####
# Export from app, download CSV from email to data/ folder
tap.0 <- read_csv("./data/CNS_tap_test/CNSTapTest.csv")
tap.1 <- 
  tap.0 %>% 
  mutate(
    dim_date = as.Date(Date, format = "%m/%d/%y")
  ) %>% 
  select(dim_date, tapLeft = Left, tapRight = Right)



# POLAR HR MONITOR ####
# Export: Via Polar website -- choose download data; an email will be sent when download is available
#         which is a zip of JSON files

# List files in polar_export folder that start with "training-session"
polar_files <- list.files("./data/polar_export/", pattern = "training-session.*json")

# Loop over files to compute key stats from each ride:
# Read file in, filter to exercises$sport == "CYCLING"
polar.0 <- tibble()
for (i in polar_files) {
  message(i)
  polar.temp <- jsonlite::fromJSON("./data/polar_export/" %//% i)
  if (polar.temp$exercises$sport %notin% c("CYCLING")) next
  # date; startTime; stopTime; duration; hrMax; hrAvg; speedMax; speedAvg; beatsPerMileMax; beatsPerMileAvg  
  polar.0 <- 
    bind_rows(
      polar.0,
      tibble(
        dim_date = as.Date(polar.temp$startTime, format = "%Y-%m-%d"),
        startTime = as.POSIXct(polar.temp$stopTime, format = "%Y-%m-%dT%H:%M:%OS"),
        stopTime = as.POSIXct(polar.temp$startTime, format = "%Y-%m-%dT%H:%M:%OS"),
        duration = as.numeric(difftime(startTime, stopTime, units = "hours")),
        distance = polar.temp$distance / 1609.3, #convert meters to miles
        hrMax = polar.temp$maximumHeartRate,
        hrAvg = polar.temp$averageHeartRate,
        speedMax = polar.temp$exercises$speed$max * 0.62,
        speedAvg = polar.temp$exercises$speed$avg * 0.62,
        # For max HR and speed, compute (bpm*60) / mph = beats per mile at max power output        
        beatsPerMileMax = (hrMax * 60) / speedMax,
        # For avg HR and speed, compute (bpm*60) / mph = beats per mile at avg power output        
        beatsPerMileAvg = (hrAvg * 60) / speedAvg
      )
    )
}

# Impute mean distance if missing (i.e., GPS failed to record)
polar.1 <- 
  polar.0 %>% 
  mutate(
    distance = ifelse(distance == 0, NA_real_, distance),
    distance = distance %na% mean(distance, na.rm = TRUE),
    speedAvg = ifelse(speedAvg == 0, NA_real_, speedAvg),    
    speedAvg = speedAvg %na% (distance / duration),
    speedMax = ifelse(speedMax == 0, NA_real_, speedMax),        
    beatsPerMileAvg = ifelse(is.infinite(beatsPerMileAvg), NA_real_, beatsPerMileAvg),
    beatsPerMileAvg = beatsPerMileAvg %na% ((hrAvg * 60) / speedAvg),
    beatsPerMileMax = ifelse(is.infinite(beatsPerMileMax), NA_real_, beatsPerMileMax)
  )

# Efficiency avg and max by session
polar.1 %>% 
  select(startTime, beatsPerMileAvg, beatsPerMileMax) %>% 
  melt(id.var = "startTime") %>% 
  ggplot(aes(x = startTime, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  geom_line() +
  facet_grid(variable ~ .)

# Efficiency distributions
polar.1 %>% 
  select(startTime, beatsPerMileAvg, beatsPerMileMax) %>% 
  melt(id.var = "startTime") %>% 
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 5) +
  facet_grid(variable ~ .)

# Plot avg and max speed by ride (over time)
polar.1 %>% 
  select(startTime, speedAvg, speedMax) %>% 
  melt(id.var = "startTime") %>% 
  ggplot(aes(x = startTime, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  geom_line() +
  facet_grid(variable ~ .)

#TODO: summary table (mean, CV, 7d mean, 7d CV, min, max, etc.) by type (cycling, sprint)


#TODO: Process sprint ("RUNNING") data from Polar Beat
#      Could probably just fold into the above and add a dimension
#      to polar.0 to distinguish "CYCLING" vs. "RUNNING"




# ELITE HRV ####
# Recordings (via Polar H10) exported from app

# https://en.wikipedia.org/wiki/Heart_rate_variability
# SDNN, the standard deviation of NN intervals. Often calculated over a 24-hour period. SDANN, the standard deviation of the average NN intervals calculated over short periods, usually 5 minutes. SDNN is therefore a measure of changes in heart rate due to cycles longer than 5 minutes. SDNN reflects all the cyclic components responsible for variability in the period of recording, therefore it represents total variability.
# RMSSD ("root mean square of successive differences"), the square root of the mean of the squares of the successive differences between adjacent NNs.[36]
# SDSD ("standard deviation of successive differences"), the standard deviation of the successive differences between adjacent NNs.[36]
# NN50, the number of pairs of successive NNs that differ by more than 50 ms.
# pNN50, the proportion of NN50 divided by total number of NNs.
# NN20, the number of pairs of successive NNs that differ by more than 20 ms.[37]
# pNN20, the proportion of NN20 divided by total number of NNs.
# EBC ("estimated breath cycle"), the range (max-min) within a moving window of a given time duration within the study period. The windows can move in a self-overlapping way or be strictly distinct (sequential) windows. EBC is often provided in data acquisition scenarios where HRV feedback in real time is a primary goal. EBC derived from PPG over 10-second and 16-second sequential and overlapping windows has been shown to correlate highly with SDNN.[38]

# List files in polar_export folder that start with "training-session"
hrv_files <- list.files("./data/EliteHRV_export/Kurt_Peters")

# Loop over files to compute key stats from each ride:
# Read file in, filter to exercises$sport == "CYCLING"
hrv.0 <- tibble()
for (i in hrv_files) {
  message(i)
  hrv.temp <- read_table("./data/EliteHRV_export/Kurt_Peters/" %//% i, col_names = "RR")
  hrv.temp$index <- 1:nrow(hrv.temp)
  hrv.0 <- 
    bind_rows(
      hrv.0,
      tibble(
        dim_date = as.Date(i),
        dim_dateTime = as.POSIXct(i, format = "%Y-%m-%d %H-%M-%OS"),
        index = hrv.temp$index,
        RR = hrv.temp$RR
      )
    )
}

# HR preprocessing
hrv.1 <- 
  hrv.0 %>% 
  # > Filter down to morning (pre-noon) readyness readings  
  filter(hour(dim_dateTime) < 12) %>% 
  # > Exclude RR outliers (by reading)
  group_by(dim_date) %>% 
  mutate(
    ptile = percent_rank(RR)
  ) %>% 
  filter(ptile > .03 & ptile < .97) %>% 
  ungroup()

hist(hrv.1$RR)

hrv.1 %>% 
  ggplot(aes(x = index, y = RR, color = dim_date)) +
  geom_line(alpha = .1) +
  theme_bw()

hrv.summary <- 
  hrv.1 %>% 
  group_by(dim_date) %>% 
  mutate(
    RRlag1 = lead(RR, 1)
  ) %>% 
  summarize(
    meanHR = 1 / ((mean(RR, na.rm = TRUE) / 1000) / 60),
    SDRR = sd(RR, na.rm = TRUE),
    # RMSSD: the square root of the mean of the squares of the successive differences between adjacent NNs    
    RMSSD = sqrt(mean(diff(RR, differences = 1)^2,  na.rm = TRUE)),
    # Tighter lag-1 correlation would indicate lower variability    
    r = cor(RR, RRlag1, use = "pairwise.complete.obs")    
  )

hrv.summary %>% 
  filter(dim_date > as.Date("2019-12-01")) %>% 
  melt(id.var = "dim_date") %>% 
  ggplot(aes(x = dim_date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = .5) +
  #geom_bar(stat = "identity") +
  facet_grid(variable ~ ., scales = "free_y") +
  theme_bw()

# Lag1 correlation is inversely correlated with HRV, as expected -- but not at all perfectly
hrv.summary %>% 
  #filter(dim_date > as.Date("2019-12-01")) %>% 
  ggplot(aes(x = RMSSD, y = r)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "lm") +
  theme_bw()



# CRONOMETER ####
# Export nutrition summary CSV from website

cron.0 <- read_csv("./data/cronometer_export/dailySummary_LTD_2020-01-04.csv")

cron.1 <- 
  cron.0 %>% 
  mutate(
    pctCaloriesFat = `Fat (g)` / `Energy (kcal)`,
    pctCaloriesCHO = `Carbs (g)` / `Energy (kcal)`,
    pctCaloriesProtein = `Protein (g)` / `Energy (kcal)`,
    ratioFatToCHO = `Fat (g)` / `Carbs (g)`
  ) %>% 
  rename(
    dim_date = Date
  )

cron.1 %>% 
  select(`Energy (kcal)`, `Carbs (g)`, `Protein (g)`, `Fat (g)`, starts_with("pct"), starts_with("ratio")) %>% 
  GGally::ggpairs()



# Melt and split out nutrient and unit dimensions
cron.long <- 
  cron.0 %>% 
  #filter(Completed) %>% 
  melt(id.var = "Date", variable.name = "nutrient", value.name = "amount") %>% 
  separate(nutrient, into = c("nutrient", "units"), sep = " \\(", remove = TRUE, extra = "merge", fill = "right") %>% 
  mutate(
    units = gsub("\\)", "", units)
  ) %>% 
  rename(dim_date = Date)
  
cron.long %>% 
  filter(nutrient %in% c("Carbs", "Fat", "Protein")) %>% 
  ggplot(aes(x = dim_date, y = amount, fill = nutrient)) +
  geom_bar(stat = "identity")
  geom_area()


# WEATHER ####
library(darksky) 
# Default units are Imperial
# API key set in .Renviron
# options(DARKSKY_API_KEY = "ba05ff23901df877c463958713a74230")
# darksky_api_key()

BTV_LAT <- 44.513876
BTV_LON <- -73.264549
weather <- get_current_forecast(btv_lat, btv_lon) # today + 7d forecast
weather$daily$cloudCover

#print(sprintf("You have used %s API calls.", weather$`x-forecast-api-calls`))

# One-time pull: get all historical data from start of recordings (scale) through yesterday
# Will save this and pull whatever is needed in future
date_range <- seq.Date(from = min(scale.1$dim_date), to = Sys.Date() - 1, by = "1 day")
weather.ltd.0 <- 
  date_range %>% 
  map(~get_forecast_for(BTV_LAT, BTV_LON, .x))
names(weather.ltd.0) <- paste(date_range)
freeze(weather.ltd.0)  
revive(weather.ltd.0)  

weather.ltd.1 <- tibble()
weather.ltd.1 <-
  lapply(weather.ltd.0, function(x) x[[2]]) %>% 
  bind_rows(weather.ltd.1)

weather.ltd.1 %>% 
  select(
    time, temperatureHigh, temperatureLow, humidity, pressure, windSpeed, windGust,
    cloudCover, uvIndex, visibility, precipProbability
  ) %>% 
  #filter(year(time) == 2018) %>% 
  melt(id.var = "time") %>% 
  ggplot(aes(x = time, y = value, group = variable)) +
  geom_line() +
  geom_smooth(span = .25, se = FALSE) +
  scale_x_datetime(date_breaks = "1 month", minor_breaks = "1 week") +
  facet_grid(variable ~ ., scales = "free_y") +
  theme_bw()


# df: data.frame limited to specific timeseries columns for cross-correlating
ccfMatrix <- function(df, lag.max = 30) {
  
  # Lay out plot grid
  ncomb <- choose(length(names(df)), 2)
  rows <- ceiling(sqrt(ncomb))
  par(mfrow = c(rows, rows))
  
  # Identify all combinations of variables in df
  comb.mat <- combn(names(df), 2)
  for (i in 1:ncol(comb.mat)) {
    ccf(df[[comb.mat[1, i]]], df[[comb.mat[2, i]]], lag.max = lag.max, main = comb.mat[1, i] %&% " vs. " %&% comb.mat[2, i])
  }
  
  # Reset plot pane
  par(mfrow = c(1,1))
  
}

weather.ltd.1 %>% 
  select(
    temperatureHigh, humidity, pressure, windSpeed, cloudCover, precipProbability
  ) %>% 
  na.omit() %>% 
  ccfMatrix()  

weather.ltd.1 %>% 
  select(
    temperatureHigh, humidity, pressure, windSpeed, cloudCover, precipProbability
  ) %>% 
  na.omit() %>% 
  GGally::ggpairs()


weather.ltd.0 %>% 
  map_df("daily") %>% 
  ggplot(aes(x = time, y = cloudCover)) +
  geom_line()

weather.ltd.0 %>% 
  map_df("daily") %>% 
  ggplot(aes(x = time, y = uvIndex)) +
  geom_line()

weather.ltd.0 %>% 
  map_df("daily") %>% 
  ggplot(aes(x = time, y = windSpeed)) +
  geom_line()

weather.ltd.0 %>% 
  map_df("daily") %>% 
  ggplot(aes(x = time, y = windGust)) +
  geom_line()




# JOINS ####
all.1 <- 
  tibble(
    dim_date = seq.Date(from = min(hrv.summary$dim_date), to = Sys.Date() - 1, by = "day")
  ) %>% 
  left_join(
    hrv.summary,
    by = "dim_date"
  ) %>% 
  left_join(
    scale.1 %>% select(dim_date, weight),
    by = "dim_date"
  ) %>% 
  #!!! This causes duplication since there may be multiple Polar rides per day
  #    Need to spread these Polar logs wide by day,
  #    else change join key to a dateTime
  #    Actually need to do this for all datasets (also Scale and HRV) since each might have multiple readings each day
  left_join(
    polar.1,
    by = "dim_date"
  ) %>% 
  left_join(
    tap.1,
    by = "dim_date"
  ) %>% 
  left_join(
    cron.1,
    by = "dim_date"
  ) %>% 
  left_join(
    weather.ltd.1 %>% mutate(dim_date = as.Date(paste(time), format = "%Y-%m-%d")) %>% select(dim_date, sunriseTime, sunsetTime, moonPhase, precipProbability, temperatureHigh, temperatureLow, humidity, pressure, windSpeed, windGust, windBearing, cloudCover, uvIndex, visibility),
    by = "dim_date"
  ) %>% 
  mutate(
    dayLength = as.numeric(difftime(sunsetTime, sunriseTime, units = "mins"))
  ) %>% 
  select(-sunsetTime, -sunriseTime)
freeze(all.1)


all.1 %>% 
  filter(dim_date > as.Date("2019-12-01")) %>% 
  #filter(dim_date < as.Date("2019-01-01")) %>% 
  melt(id.var = "dim_date") %>% 
  ggplot(aes(x = dim_date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = .25) +
  #geom_bar(stat = "identity") +
  facet_grid(variable ~ ., scales = "free_y") +
  theme_bw()

all.1 %>% 
  filter(dim_date > as.Date("2019-12-07")) %>% 
  #filter(dim_date < as.Date("2019-01-01")) %>% 
  select(-SDRR, -startTime, -stopTime, -distance, -duration) %>% 
  GGally::ggpairs(lower = list(continuous = "smooth"))

acf(all.1$meanHR)
pacf(all.1$meanHR)
acf(all.1$RMSSD)
pacf(all.1$RMSSD)
acf(all.1$weight, na.action = na.pass)
pacf(all.1$weight, na.action = na.pass)

ccf(all.1$meanHR, all.1$RMSSD, na.action = na.pass)
ccf(all.1$meanHR, all.1$weight, na.action = na.pass)
ccf(all.1$RMSSD, all.1$weight, na.action = na.pass)

ccf(diff(all.1$meanHR), diff(all.1$RMSSD), na.action = na.pass)
ccf(diff(all.1$meanHR), diff(all.1$weight), na.action = na.pass)
ccf(diff(all.1$RMSSD), diff(all.1$weight), na.action = na.pass)

plot(stl(ts(all.1$meanHR, frequency = 30), s.window = "periodic"))
plot(stl(ts(all.1$RMSSD, frequency = 30), s.window = "periodic"))
plot(stl(ts(na.omit(all.1$weight), frequency = 30), s.window = "periodic")) #seasonality affected by regular fasts


all.1 %>% 
  filter(year(dim_date) == 2018) %>%   
  select(meanHR, RMSSD, r, weight, temperatureHigh, pressure, cloudCover, uvIndex) %>% 
  na.omit() %>% 
  ccfMatrix()

all.2018 <- 
  all.1 %>% 
  filter(year(dim_date) == 2018)
acf(all.2018$RMSSD, na.action = na.pass)
acf(all.2018$temperatureHigh, na.action = na.pass)
plot(stl(ts(all.2018$RMSSD, frequency = 7), s.window = "periodic"))
plot(stl(ts(all.2018$temperatureHigh, frequency = 7), s.window = "periodic", na.action = na.omit))
ccf(all.2018$RMSSD, all.2018$temperatureHigh, na.action = na.pass)

all.1 %>% 
  filter(year(dim_date) == 2018) %>% 
  select(dim_date, meanHR, RMSSD, r, weight, temperatureHigh, pressure, cloudCover, uvIndex) %>% 
  melt(id.var = "dim_date") %>% 
  ggplot(aes(x = dim_date, y = value, group = variable)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y")

# Poincare plot: scatterplot of RR autocorrelation lag 1
# Tighter correlation would indicate lower variability
hrv.1 %>% 
  group_by(dim_date) %>% 
  mutate(
    RRlag1 = lead(RR, 1)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = RR, y = RRlag1, color = dim_date)) +
  geom_point(alpha = .1) +
  theme_bw()
  

# Smoothing...
qcc::ewma(na.omit(all.1$weight), lambda = .25)




# CLUSTERING ####

set.seed(1234)
c.0 <- 
  tibble(
    f1 = round(runif(10, 0, 1)),
    f2 = round(runif(10, 0, 1)),
    f3 = round(runif(10, 0, 1)),
    f4 = round(runif(10, 0, 1))
  )

#dist() computes the distance between all pairs of cases (rows in a df)
dist(c.0, method = "binary") %>% 
  #print()
  hclust(method = "complete") %>% 
  plot()


sh.get <- sheets_get("https://docs.google.com/spreadsheets/d/1CRMO5nmankLgRJb9FOMWUS9uA_MdAMtJ-Xh2zBxlEb4/edit#gid=0")
mvmts.0 <- sheets_read(ss = sh.get$spreadsheet_url, sheet = 1)

set.seed(1234)
mvmts.mat <- 
  mvmts.0 %>% 
  select(-Movement) %>% 
  as.matrix()
rownames(mvmts.mat) <- mvmts.0$Movement
mvmts.mat

mvmts.dist <- 
  mvmts.mat %>% 
  dist(method = "binary", diag = TRUE, upper = TRUE)
str(mvmts.dist)

# Note: the dist matrix computes all pairwise distances between cases
choose(nrow(mvmts.0), 2)

mvmts.dist %>% 
  #hclust(method = "ward.D") %>% 
  hclust(method = "complete") %>%   
  plot()


#/// for a given movement, choose 2 antagonists (least similar) and 1 agonist (similar)
mvmts.dist.df <- 
  mvmts.dist %>% 
  as.matrix() %>% 
  as.data.frame()

mvmts.dist.df %>% 
  rownames_to_column("mvmt_a") %>% 
  melt(id.var = "mvmt_a", variable.name = "mvmt_b", value.name = "distance") %>% 
  ggplot(aes(x = mvmt_a, y = mvmt_b, fill = distance)) +
  geom_raster()

mvmts.dist.df %>% 
  rownames_to_column("mvmt_a") %>%   
  select("mvmt_a", "Pullup") %>% 
  arrange(Pullup)


# > Markov Chain ####
# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, init_state = NULL, num.iters = 50 ) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)

  # initialize variable for first state 
  if (is.null(init_state)) {
    states[1] <- floor(runif(1, min = 1, max = dim(P)[1]))
  } else {
    states[1] <- init_state    
  }

  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}

# setup transition matrix 
# P <- t(matrix(c( 0.7, 0.2, 0.1, 
#                  0.4, 0.6,   0, 
#                  0,   1,   0  ), nrow=3, ncol=3))

P <- 
  mvmts.dist %>% 
  as.matrix()


num.chains     <- 10
num.iterations <- 4
init_state <- 7 #start with Straddle Planche
#init_state <- NULL

# each column stores the sequence of states for a single chains
chain.states  <- matrix(NA, ncol=num.chains, nrow=num.iterations)

# simulate chains
for(c in seq_len(num.chains)){
  chain.states[,c] <- run.mc.sim(P, init_state = init_state, num.iters = num.iterations)
}

matplot(chain.states, type='l', lty=1, col=1:5, ylim=c(1,dim(P)[1]), ylab='state', xlab='time')
# abline(h=1, lty=3)
# abline(h=3, lty=3)

chain.states %>% 
  as.data.frame() %>% 
  melt(id.var = NULL, variable.name = "iteration", value.name = "Movement_Index") %>% 
  left_join(
    mvmts.0 %>% 
      select(Movement) %>% 
      mutate(Movement_Index = row_number())
  ) %>% 
  View()
