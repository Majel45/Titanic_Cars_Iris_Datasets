iris$Species <- as.factor(iris$Species)

iris$SpeciesName <- iris$Species

iris$Species <- as.numeric(iris$Species)

hist(iris$Species, main = "Iris Species", xlab = "Species")

plot(iris$Sepal.Length)

qqnorm(iris$Petal.Length)

qqnorm(iris$Petal.Width)

set.seed(123)

trainSize <- round(nrow(iris)*0.2)

testSize <- nrow(iris) - trainSize

trainSize

training_indices <- sample(seq_len(nrow(iris)), size = trainSize)

set.seed(405)

trainSet <- iris[training_indices, ]

testSet <- iris[- training_indices, ]

LinearIrisModel <- lm(trainSet$Petal.Width ~ testSet$Petal.Length)

LinearIrisModel <- lm(trainSet$Petal.Width ~ trainSet$Petal.Length)

summary(LinearIrisModel)

PredictionIris <- predict(LinearIrisModel)

PredictionIris

plot(PredictionIris)

plot(iris)

plot(iris$Petal.Length)

plot(iris$Petal.Length, iris$Petal.Width,
     main = "Petal Length vs Width", col= c("red", "green", "blue")[unclass(iris$Species)],
     xlab = "Petal Length", ylab = "Petal Width")

abline(lsfit(iris$Petal.Length, iris$Petal.Width)$coefficients, col= "black")

legend("topright", c("1", "2", "3"), col=c("red", "green", "blue"), 
       cex = 0.8, fill = c("red", "green", "blue"))

plot(PredictionIris)

plot(testSet$PredictedLength, testSet$Petal.Width,
     main = "Predicted Petal Length vs Width", col= c("red", "green", "blue")[unclass(testSet$Species)],
     xlab = "Petal Length", ylab = "Petal Width")

legend("topright", c("1", "2", "3"), col=c("red", "green", "blue"), 
       cex = 0.8, fill = c("red", "green", "blue"))
