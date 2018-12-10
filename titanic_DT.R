library(readr)

titanic <- read.csv('train_and_test2', header = T, na.strings = c(""))

summary(titanic)

str(titanic)

names(titanic)

titanic$Passengerid

View(titanic)

install.packages("tree")

library(tree)

nrow(titanic$Age)

is.na(titanic)

na.omit(titanic)

titanic$zero <- NULL

titanic <- titanic [-c (6, 7, 8, 9, 10, 11)]

titanic <- titanic [-c (7, 8, 9, 10, 11, 12, 13, 14)]

titanic <- titanic [-c (8, 9, 11, 12)]

View(titanic)

library(rpart)

names(titanic) <- c ("Passengerid", "Age", "Fare", "Sex", "Siblings", "Parch", "PClass",
                     "Embarked", "Survived")

fit <- rpart(Survived ~., method = "class", 
             data = titanic)

printcp(fit)

plotcp(fit)

summary(fit)

plot(fit, uniform = TRUE)

text(fit, use.n = TRUE, all = TRUE, cex = 0.8)

is.na(titanic)

set.seed(123)

trainSize <- round(nrow(titanic)*0.7)

testSize <- nrow(titanic) - trainSize

training_indices <- sample(seq_len(nrow(titanic)), size = trainSize)

trainSet <- titanic[training_indices, ]

testSet <- titanic [-training_indices, ]

TitanicModel <- glm (Survived ~., family=binomial(link = "logit"), data = titanic)

summary(TitanicModel)

anova(TitanicModel, test = "Chisq")

fitted.results <- predict(TitanicModel, 
                newdata = subset(testSet, select = c(1, 2, 3, 4, 5, 6, 7, 8)), 
                type = "response")

fitted.results

fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

ClasificError <- mean(fitted.results !=testSet$Survived)

print(paste("Accuracy", 1 - ClasificError))

hist(fitted.results)
