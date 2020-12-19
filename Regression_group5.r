# import the data
df <- read.csv("CO2 Emissions_Canada.csv", header=T)

# display column header
names(df)

# summary of dataset
summary(df)

# import plyr package
library(plyr)

# change the value of Transmission columns
df$Transmission <- mapvalues(df$Transmission, from = c("A4", "A5", "A6", "A7", "A8", "A9", "A10"), to = c("Automatic","Automatic","Automatic","Automatic","Automatic","Automatic", "Automatic"))

df$Transmission <- mapvalues(df$Transmission, from = c("AM5", "AM6", "AM7", "AM8", "AM9"), to = c("Automated Manual","Automated Manual","Automated Manual","Automated Manual","Automated Manual"))

df$Transmission <- mapvalues(df$Transmission, from = c("AS4", "AS5", "AS6", "AS7", "AS8", "AS9", "AS10"), to = c("Automatic with select shift","Automatic with select shift","Automatic with select shift","Automatic with select shift","Automatic with select shift","Automatic with select shift","Automatic with select shift"))

df$Transmission <- mapvalues(df$Transmission, from = c("AV", "AV6", "AV7", "AV8", "AV10"), to = c("Continuously Variable","Continuously Variable","Continuously Variable","Continuously Variable","Continuously Variable"))

df$Transmission <- mapvalues(df$Transmission, from = c("M5", "M6", "M7"), to = c("Manual","Manual","Manual"))

# change the value of Fuel Type columns
df$Fuel.Type <- mapvalues(df$Fuel.Type, from = c("Z","X", "D", "E", "N"), to = c("Premium Gasoline", "Regular Gasoline", "Diesel", "Ethanol (E85)", "Natural Gas"))

# summary of dataset
summary(df)

# delete datapoints when the engine size bigger than 8L since they are outliers
df <- subset(df, Engine.Size.L. <= 8)

# import dummies package
library(dummies)

# change categorial data of Transmission into dummy or binary values
df <- dummy.data.frame(df, names = c("Transmission"), sep="_")

# change categorial data of Fuel Type into dummy or binary values
df <- dummy.data.frame(df, names = c("Fuel.Type"), sep="_")

# display column header
names(df)

# summary of dataset
summary(df)

# drop unneed columns 
df <- subset(df, select = -c(Make, Model, Vehicle.Class))

# attach the data
attach(df)

# plot the data in Scatterplot to check if there is any relationship between the size of engine and CO2 emission?
plot(Engine.Size.L., CO2.Emissions.g.km., main="CO2 Emission vs Engine Size", col="blue", ylab="CO2 Emissions (g/km)", xlab="Engine Size (L)")

# plot the data in Scatterplot to check if there is any relationship between the number of cylinders and CO2 emission?
plot(Cylinders, CO2.Emissions.g.km., main="CO2 Emission vs Number of Cylinders", col="blue", ylab="CO2 Emissions (g/km)", xlab="Number of Cylinders")

# plot the data in Scatterplot to check if there is any relationship between fuel consumption city and CO2 emission?
plot(Fuel.Consumption.City..L.100.km., CO2.Emissions.g.km., main="CO2 Emission vs Fuel Consumption City", col="blue", ylim=c(90,530), xlim=c(4,31), ylab="CO2 Emissions (g/km)", xlab="Fuel Consumption City (L/100 km)")

# plot the data in Scatterplot to check if there is any relationship between fuel consumption highway and CO2 emission?
plot(Fuel.Consumption.Hwy..L.100.km., CO2.Emissions.g.km., main="CO2 Emission vs Fuel Consumption Highway", col="blue", ylim=c(90,530), xlim=c(4,31), ylab="CO2 Emissions (g/km)", xlab="Fuel Consumption Hwy (L/100 km)")

# plot the data in Scatterplot to check if there is any relationship between fuel consumption combination (mpg) and CO2 emission?
plot(Fuel.Consumption.Comb..mpg., CO2.Emissions.g.km., main="CO2 Emission vs Fuel Consumption Combination (mpg)", col="blue", ylim=c(90,530), xlim=c(11,70), ylab="CO2 Emissions (g/km)", xlab="Fuel Consumption Combination (mpg)")

# Correlation between the size of engine and CO2 emission
cor.test(Engine.Size.L., CO2.Emissions.g.km.)

# Correlation between the number of cylinders and CO2 emission
cor.test(Cylinders, CO2.Emissions.g.km.)

# Correlation between fuel consumption city (L/100 km) and CO2 emission

cor.test(Fuel.Consumption.City..L.100.km., CO2.Emissions.g.km.)

# Correlation between fuel consumption hwy (L/100 km) and CO2 emission
cor.test(Fuel.Consumption.Hwy..L.100.km., CO2.Emissions.g.km.)

# Correlation between fuel consumption comb (mpg) and CO2 emission
cor.test(Fuel.Consumption.Comb..mpg., CO2.Emissions.g.km.)


# split dataset as training and test
set.seed(1)
train_index <- sample(1:nrow(df), nrow(df) *0.8)
test_index <- setdiff(1:nrow(df), train_index)
trainingData = df[train_index, ]
testData = df[test_index,]

# build the linear regression model with all features (X variables)
lr_model <- lm(CO2.Emissions.g.km. ~ ., data=trainingData)

# get a summary of the model
summary(lr_model)

# get the prediction
predict <- predict(lr_model, testData)

actuals_preds <- data.frame(cbind(actuals=testData$CO2.Emissions.g.km., predicteds=predict))  

mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
#Min Max Accuracy: 98.78%

mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
#Mean absolute percentage error: 1.22%

plot(lr_model)
