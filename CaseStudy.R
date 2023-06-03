###################################################
# Case Study Module 3 / Data Analysis             #
# Jan Hümmelink                                   #
###################################################

require(tidyverse)
require(GGally)
require(fpc)
require(factoextra)
require(dbscan)
require(pheatmap)
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
str(trips)
summary(trips)

trips_numeric = select(trips, c("duration", "air_distance", "day_of_month", "month","day_of_week_num"))

ggpairs(trips_numeric)

# calculate correlations
correlation = round(cor(
  select_if(trips, is.numeric),
  use = "pairwise.complete.obs",
  method = "spearman"
),
2)
correlation = as.data.frame(correlation)
# produce a heatmap for the correlation
pheatmap(
  correlation,
  display_numbers = TRUE,
  main = "correlation heatmap",
  treeheight_row = 0,
  treeheight_col = 0
)

# produce distributions per variable
for (name in names(select_if(trips, is.numeric))) {
  data = as.matrix(select_if(trips, is.numeric)[name])
  par(mfrow = c(1, 3))
  plot.ecdf(data, main = paste(name, "ecdf", " "))
  boxplot(data, main = paste(name, "boxplot", " "))
  hist(data, main = paste(name, "histogram", " "))
}
#3. Check the data for univariate and multivariate outliers. If you find any conspicuous trips, 
#   decide how to deal with them (remove or keep them) and justify your decision.

summary(trips)
# big range between durations 3rd Quartile (19) and Max. (1231)
# big range between air distance 3rd Quartile (2.03) and Max. (12.24)

# calculate z-scores
trips$duration_z = (trips$duration - mean(trips$duration)) / sd(trips$duration)
trips$air_distance_z = (trips$air_distance - mean(trips$air_distance)) / sd(trips$air_distance)

# Visualize z-scores
hist(trips$duration_z, main = "Duration z-Score", xlab = "Duration z-Score")
hist(trips$air_distance_z, main = "Air Distance z-Score", xlab = "Air Distance z-Score")

trips_outliers = trips %>%
  filter(duration_z > 3.5 | air_distance_z > 3.5)
ggpairs(trips_outliers)

trips_cleaned = trips %>%
  filter(duration_z < 3.5 & air_distance_z < 3.5) %>%
  mutate("duration_z"=NULL,"air_distance_z"=NULL)

trips_cleaned_numeric = select(trips_cleaned, c("duration", "air_distance", "day_of_month", "month","day_of_week_num"))

# produce distributions per variable
for (name in c("duration","air_distance")) {
  data = as.matrix(select_if(trips_cleaned_numeric, is.numeric)[name])
  par(mfrow = c(1, 3))
  plot.ecdf(data, main = paste(name, "ecdf", " "))
  boxplot(data, main = paste(name, "boxplot", " "))
  hist(data, main = paste(name, "histogram", " "))
}

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

avg = hclust(d,method = "average")
par(mfrow = c(1, 1))

plot(avg)
avg


# Complete Linkage
stations$avg = as.character(cutree(avg, k = 3))
map +
  geom_point(data=stations, aes(x = lng, y = lat, col = avg))

# DBSCAN
minPts = 5
k = minPts-1

kNNdistplot(stations_coords, k=k)
abline(h=0.0265, col = "red")
cdbscan = fpc::dbscan(stations_coords, eps=0.0265,MinPts = minPts, method = "hybrid")
stations$cdbscan = as.character(cdbscan$cluster)
map +
  geom_point(data=stations, aes(x = lng, y = lat, col = cdbscan))


# kmeans
n = nrow(stations_coords)
wss = rep(1:10)
for( i in 1:10){
  wss[i] = sum(kmeans(stations_coords, centers = i)$withinss)
}
plot(wss, xlab = "Number of clusters", ylab = "within group sum of squares")

ergClust = kmeans(stations_coords, centers = 2)
stations$ckmeans = as.character(ergClust$cluster)

map +
  geom_point(data=stations, aes(x = lng, y = lat, col = ckmeans))

# 7. Let's assume that nextbike wants to set up a separate service station for each cluster. 
#    Where should these service stations be placed in order to be accessible as well as possible from all 
#    stations of the respective cluster? Mark the recommended locations on your map as well!

clusters = pull(distinct(stations,avg),avg)
empty = rep(0, length(clusters))
cluster_centers = tibble("cluster"=clusters,"lat"=empty, "lng"=empty ) 
c = "1"

averages = stations %>%
  select_at(c("lat","lng","avg")) %>%
  group_by(avg) %>%
  summarise_all(mean)
averages

map +
  geom_point(data=stations, aes(x = lng, y = lat, col = avg)) +
  geom_point(data = averages, aes(x=lng,y=lat,col=avg), shape = 18, size = 5)+
  labs(col='service stations')


# 8. Intuitively, one would expect a relationship between trip duration and the other variables 
#    (especially traveled distance). Examine this assumption by modeling the relationship using various regression 
#    techniques. Which procedure is best suited for this purpose? Interpret your result(s)!

plot(trips_cleaned$air_distance, trips_cleaned$duration)

# multiple linear regression model
model_mlm = lm(duration ~ air_distance + day_of_week_num + month + day_of_month, data = trips_cleaned)
summary(model_mlm)

# linear model
model_lm = lm(duration ~ air_distance, data = trips_cleaned)
summary(model_lm)


# 9. For urban planning, one of the areas of interest is the traffic between different areas of a city. 
#    Consider the clusters identified in 5. as separate areas of the city and compare several classifiers 
#    that model the drop-off station cluster as a function of the other metrics in a benchmark study. 
#    Interpret your result.
drop_off = merge(trips_cleaned, stations, by.x = "station_stop", by.y = "station_number")["avg"] %>%
  pull()
start = merge(trips_cleaned, stations, by.x = "station_start", by.y = "station_number")["avg"] %>%
  pull()
trips_relevant_move = select(trips_cleaned_numeric, c("day_of_week_num","day_of_month", "month", "air_distance", "duration"))

trips_relevant_move = trips_cleaned
trips_relevant_move$drop_off = as.factor(drop_off)
trips_relevant_move$start = as.factor(start)

train_indices = sample(1:nrow(trips_relevant_move), nrow(trips_relevant_move) * 0.7)  # 70% for training
train = as.data.frame(trips_relevant_move[train_indices,])
test = as.data.frame(trips_relevant_move[-train_indices,])

# Approach 1: svm
require(mlr)
task.svm = makeClassifTask(data = train, target = "drop_off", id = "drop_off" )
lrn.svm = makeLearner("classif.ksvm")
lrn.svm
mod.svm = train(learner = lrn.svm, task = task.svm)

predictions.svm = predict(object=mod.svm, newdata=test)
confusion_matrix.svm = table(Actual = pull(predictions.svm$data['truth']), Predicted = pull(predictions.svm$data['response']))
accuracy.svm = sum(diag(confusion_matrix.svm)) / sum(confusion_matrix.svm)
print(confusion_matrix.svm)
print(paste("Accuracy:", accuracy.svm))

# Approach 2: decision tree
require(mlr)
task.rpart = makeClassifTask(data = train, target = "drop_off", id = "drop_off" )
lrn.rpart = makeLearner("classif.rpart")
lrn.rpart
mod.rpart = train(learner = lrn.rpart, task = task.rpart)

predictions.rpart = predict(object=mod.rpart, newdata=test)
confusion_matrix.rpart = table(Actual = pull(predictions.rpart$data['truth']), Predicted = pull(predictions.rpart$data['response']))
accuracy.rpart = sum(diag(confusion_matrix.rpart)) / sum(confusion_matrix.rpart)
print(confusion_matrix.rpart)
print(paste("Accuracy:", accuracy.rpart))

# 10.Select one of the classifiers (from 9.) and perform a feature selection to determine which of the data set's 
#    features are most relevant.
#    Hint: Study the code chunks and associated slides in the feature selection chapter of your course 
#    materials and modify the code to match this task!

rinst.cv = makeResampleInstance("CV", iters = 10, task=task.svm)
ctrl.seq = makeFeatSelControlSequential(method = "sbs")
res.seq = selectFeatures(learner = lrn.svm, task = task.svm, resampling = rinst.cv, control = ctrl.seq)
colnames(train)
res.seq$x

res.seq$y

optpath = getOptPathX(res.seq$opt.path)
optpath$mmce = getOptPathY(res.seq$opt.path)
optpath
pheatmap(
  optpath,
  display_numbers = round(optpath,4),
  main = "iterations and misclassification error",
  treeheight_row = 0,
  treeheight_col = 0,
  cluster_cols = F,
  cluster_rows = F
)

