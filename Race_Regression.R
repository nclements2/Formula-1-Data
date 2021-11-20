# Race Regression Analysis by Nick Clements

# import data
results <- read.csv("results.csv") # needed to get the position variable
pitstop <- read.csv("pitStops.csv") # needed to get the pitstop duration and number of stops
circuits <- read.csv("circuits.csv") # needed to get the track type 
races <- read.csv("races.csv") # needed to match circuits id and race id for the track types

# view datasets
summary(results)
summary(circuits)
summary(pitstop)
summary(races)

# merging circuits and races data sets
track <- merge(x=races[,c(1,4)], y=circuits[,c(1,10)], by.x ="circuitId", by.y = "ï..circuitId")
head(track)

# first data set :
## calculate the mean duration of stops
stops.length <- pitstop %>% group_by(raceId, driverId) %>%
  summarize(stops.length = mean(milliseconds, na.rm = TRUE)) %>%
  filter(stops.length < 60000) # exclude observations with mean pitstop larger than 60 seconds

head(stops.length)

# merge with track type and position in race
df1 <- merge(x=stops.length, y=track[,c(2,3)], by="raceId")
df.length <- merge(df1, results[,c(2,3,9)], by=c("driverId","raceId"))
head(df.length) # merged data set

# second data set :
# calculate the frequency of stops
freq.stops <- pitstop %>% group_by(raceId, driverId) %>%
  summarize(freq.stops = max(stop, na.rm = TRUE))

head(freq.stops)

# merge with track type and position in race
df2 <- merge(x=freq.stops, y=track[,c(2,3)], by="raceId")
df.freq <- merge(df2, results[,c(2,3,9)], by=c("driverId","raceId"))
head(df.freq) # merged data set

df <- merge(x=df.freq[,c(1,2,3)], y=df.length, by=c("driverId","raceId"))
head(df)

# Correlation analysis
cor.test(df.length$stops.length, df.length$positionOrder)

cor.test(df.freq$freq.stops, df.freq$positionOrder)

cor.test(df$freq.stops, df$stops.length)

# Regression models
# first model
model1 <- lm(positionOrder ~ tracktype + freq.stops-1, df.freq)
summary(model1) # summary of model

# model diagnosis
par(mfrow=c(2,2))
plot(model1)

# second model
model2 <- lm(positionOrder ~ tracktype + stops.length-1, df.length)
summary(model2)

# model diagnosis
par(mfrow=c(2,2))
plot(model2)

