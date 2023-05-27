###################################################
# Case Study Module 3 / Data Analysis             #
# Jan Hümmelink                                   #
###################################################

require(tidyverse)
require(GGally)
require(fpc)
require(factoextra)
require(dbscan)
# 1. Import the two data sets containing the trip information (trips-*.rds). Combine both data sets in a joint data set 
#    and augment the information contained therein with the associated day of the week and month.

t2022_11 = readRDS("material/trips-2022_11.rds")
t2022_12 = readRDS("material/trips-2022_12.rds")

trips = rbind(t2022_11,t2022_12)

trips$day_of_month = as.numeric(format(trips$time_start, "%d"))
trips$day_of_week = format(trips$time_start, "%A")
trips$month = as.numeric(format(trips$time_start, "%m"))
trips$day_of_week_num = match(trips$day_of_week, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

table(trips$day_of_week)
# 2. Conduct an exploratory data analysis ,i.e. , get an overview of the number of observations and the type and range of variables,
#    compute descriptive statistics, visualize the data, and examine it for noticeable patterns and/or correlations.


trips_numeric = select(trips, c("duration", "air_distance", "day_of_month", "month","day_of_week_num"))

ggpairs(trips_numeric)

#3. Check the data for univariate and multivariate outliers. If you find any conspicuous trips, 
#   decide how to deal with them (remove or keep them) and justify your decision.
summary(trips)
# big range between durations 3rd Quartile (19) and Max. (1231)
# big range between air distance 3rd Quartile (2.03) and Max. (12.24)

trips_cleaned = trips %>%
  filter(duration < 750) %>%
  filter(air_distance < 10)

trips_cleaned_numeric = select(trips_cleaned, c("duration", "air_distance", "day_of_month", "month","day_of_week_num"))

ggpairs(trips_cleaned_numeric)

# 4. In your materials,you will find another dataset (stations.rds) containing the
# coordinates of the nextbike stations. Add the locations to the map of the region shown above.
# Hint: The map is a ggplot object, and consequently can be extended with ggplot commands.
source("material/createMap.R")
map
stations = readRDS("material/stations.rds")
map_points = map +
  geom_point(data=stations, aes(x = lng, y = lat))

map_points

# 5. Find a meaningful grouping of the bike locations. For this purpose, 
#    try out different cluster algorithms and choose a suitable number of clusters.
# 6. Visualize your cluster results by color-coding the locations (see4.) according to their
#    cluster affiliation (see 5.). Which cluster result seems most plausible to you?
stations_coords = stations[,c(1:2)]

d = dist(stations_coords, method = 'euclidean')

ccompl = hclust(d,method = "complete")
plot(ccompl)


# Complete Linkage
stations$ccompl = as.character(cutree(ccompl, k = 6))
map +
  geom_point(data=stations, aes(x = lng, y = lat, col = ccompl))

# DBSCAN
kNNdistplot(stations_coords, k=3)
abline(h=0.024, col = "red")
cdbscan = fpc::dbscan(stations_coords, eps=0.024,MinPts = 3, method = "hybrid")
stations$cdbscan = as.character(cdbscan$cluster)
map +
  geom_point(data=stations, aes(x = lng, y = lat, col = cdbscan))


# kmeans
stations_coords_s = apply(stations_coords,2,scale)
apply(stations_coords_s,2,var)

n = nrow(stations_coords_s)
wss = rep(1:10)
for( i in 1:10){
  wss[i] = sum(kmeans(stations_coords_s, centers = i)$withinss)
}
plot(wss)

ergClust = kmeans(stations_coords_s, centers = 4)
stations$ckmeans = as.character(ergClust$cluster)

map +
  geom_point(data=stations, aes(x = lng, y = lat, col = ckmeans))

# 7. Let's assume that nextbike wants to set up a separate service station for each cluster. 
#    Where should these service stations be placed in order to be accessible as well as possible from all 
#    stations of the respective cluster? Mark the recommended locations on your map as well!

clusters = pull(distinct(stations,ccompl),ccompl)
empty = rep(0, length(clusters))
cluster_centers = tibble("cluster"=clusters,"lat"=empty, "lng"=empty ) 
c = "1"

averages = stations %>%
  group_by(ccompl) %>%
  summarise_all(mean)
averages

map +
  geom_point(data=stations, aes(x = lng, y = lat, col = ccompl)) +
  geom_point(data = averages, aes(x=lng,y=lat,col=ccompl), shape = 18, size = 5)


# 8. Intuitively, one would expect a relationship between trip duration and the other variables 
#    (especially traveled distance). Examine this assumption by modeling the relationship using various regression 
#    techniques. Which procedure is best suited for this purpose? Interpret your result(s)!

# Assuming that trip_duration is the dependent variable and distance is the independent variable
model_lm <- lm(duration ~ air_distance, data = trips)
# Logarithmic regression
model_log <- glm(duration ~ log(air_distance), data = trips, family = "gaussian")

# Exponential regression
model_exp <- glm(duration ~ exp(air_distance), data = trips, family = "gaussian")

# Polynomial regression
model_poly <- glm(duration ~ air_distance + I(air_distance^2), data = trips, family = "gaussian")

# Print summary of the models
summary(model_lm)
summary(model_log)
summary(model_exp)
summary(model_poly)


# 9. For urban planning, one of the areas of interest is the traffic between different areas of a city. 
#    Consider the clusters identified in 5. as separate areas of the city and compare several classifiers 
#    that model the drop-off station cluster as a function of the other metrics in a benchmark study. 
#    Interpret your result.
drop_off = merge(trips_cleaned, stations, by.x = "station_stop", by.y = "station_number")["ccompl"] %>%
  pull()
start = merge(trips_cleaned, stations, by.x = "station_start", by.y = "station_number")["ccompl"] %>%
  pull()
trips_relevant_move = select(trips_cleaned, c("day_of_week_num","day_of_month", "month"))

trips_relevant_move$drop_off = as.factor(drop_off)
trips_relevant_move$start = as.numeric(start)

train_indices = sample(1:nrow(trips_relevant_move), nrow(trips_relevant_move) * 0.7)  # 70% for training
train_X = trips_relevant_move[train_indices, c("day_of_week_num","day_of_month", "month","start")]
train_Y = trips_relevant_move[train_indices,c("drop_off")]
test_X = trips_relevant_move[-train_indices, c("day_of_week_num","day_of_month", "month","start")]
test_Y = trips_relevant_move[-train_indices, c("drop_off")]

# Approach 1: Random forest
# Load the randomForest package
library(randomForest)

# Set seed for reproducibility
set.seed(123)  

# Set up and train the random forest classifier
rf_model = randomForest(x = train_X, y = pull(train_Y), ntree = 100)

# Print the summary of the random forest model
print(rf_model)

# Predict using the trained random forest model
rf_predictions = predict(rf_model, test_X)

# Evaluate the performance of the random forest model
confusion_matrix = table(Actual = pull(test_Y), Predicted = rf_predictions)
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))




# 10.Select one of the classifiers (from 9.) and perform a feature selection to determine which of the data set's 
#    features are most relevant.
#    Hint: Study the code chunks and associated slides in the feature selection chapter of your course 
#    materials and modify the code to match this task!
