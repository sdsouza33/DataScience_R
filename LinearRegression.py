
datasets::cars  
'Dataset to cars[speed] & cars[dist]'

head(cars)

plot(cars)
scatter.smooth(x = cars$speed, y = cars$dist, main = "Graph Dist x Speed")

"Checking for Outliers"

par(mfrow = c(1, 2)) "Dividing the graph in 2 columns"
boxplot(cars$speed, main = "Speed", sub = paste("Outlier rows: ", boxplot.stats(cars$speed)$out))
boxplot(cars$dist, main = "Distance", sub = paste("Outlier rows: ", boxplot.stats(cars$dist)$out))


library(caTools) "Split data is on caTools so imported"

split <- sample.split(cars, SplitRatio = 0.7)
train <- subset(cars, split = "TRUE")
test <- subset(cars, split = "FALSE")

cor(x = train$speed,   
    y = train$dist)  "Calculating Correlation"

linearM = lm(formula = dist ~ speed, data = train)  "LinearRegression Model"
summary(linearM)

pred <- predict(linearM, test)
plot(test$dist, type = 'l', col = "Red")
plot(pred, type = 'l', col = "Blue")
actuals_preds <- data.frame(cbind(actuals = test$dist, predicteds = pred))


correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) "Mean Absolute Percentage Error"


