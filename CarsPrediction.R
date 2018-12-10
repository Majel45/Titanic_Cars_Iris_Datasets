set.seed(123)

trainSize <- round(nrow(cars)*0.7)

testSize <- nrow(cars) - trainSize

training_indices <- sample(seq_len(nrow(cars)), size = trainSize)

trainSet <- cars[training_indices, ]

testSet <- cars [-training_indices, ]

LinearCarsReg <- lm(cars$`distance of car` ~ cars$`speed of car`, trainSet)

summary(LinearCarsReg)

PredictionsCars <- predict(LinearCarsReg, testSet)

PredictionsCars

hist(PredictionsCars)

plot(PredictionsCars)

abline(lm(cars$`distance of car` ~ cars$`speed of car`, trainSet))

plot(cars$`speed of car`, type = "s", col= "red",
       xlab = "Cars ID", ylab = "", bty = "n")

text(8, 14, "Speed", col = "red")

par(new=T)

plot(cars$`distance of car`, type = "s", bty = "n", col="darkblue", ann = F, axes = F)

axis(side = 4, col = "darkblue")       

text(37, 40, "Distance", col = "darkblue")

title(main = "Speed and Distance Correlation of Cars")

cars$Predictions <- PredictionsCars

plot(cars$Predictions, type = "s", bty = "n", 
     col = "darkgreen", xlab = "Cars ID", ylab = "")

text(15, 40, "Predicted Distance", col = "darkgreen")

par(new=T)

plot(cars$`distance of car`, type = "s", bty = "n",
     col = "darkred", ann = F, axes = F)

axis(side = 4, col = "darkred") 

text(37, 40, "Distance", col = "darkred")

title(main = "Predicted Distance based on Speed")
