# Matthew Bolding
# MATH 40883 Predictive Modeling
# Project 2 - .r file

dataset <- read.table("project2.data", header = T, sep = ";")

nrow(dataset)
dataset <- na.omit(dataset)

nrow(dataset)

library(caTools)
set.seed(1984)
index <- sample.split(dataset$y, SplitRatio = 0.7)
train <- subset(dataset, index == TRUE)
valid <- subset(dataset, index == FALSE)

success.index <- which(train$y > 0.5)
success <- train[success.index, ]
failure <- train[-success.index, ]

cor(train)

cor(train$x1, train$y)
cor(train$x3, train$y)

boxplot(success$x1, failure$x1,
        ylab = expression('x'[1] * " Values"),
        names = c("Successes", "Failures"))

boxplot(success$x2, failure$x2,
        ylab = expression('x'[2] * " Values"),
        names = c("Successes", "Failures"))

boxplot(success$x3, failure$x3,
        ylab = expression('x'[3] * " Values"),
        names = c("Successes", "Failures"))

boxplot(success$x4, failure$x4,
        ylab = expression('x'[4] * " Values"),
        names = c("Successes", "Failures"))

boxplot(success$x5, failure$x5,
        ylab = expression('x'[5] * " Values"),
        names = c("Successes", "Failures"))

hist(success$x2, breaks = 16, 
     xlim = c(-4, 4), 
     ylim = c(0, 40), 
     main = expression("Histogram of " * 'x'[2] * " Values (Success)"),
     xlab = expression('x'[2] * " Values (Success)"))

hist(failure$x2, breaks = 16, 
     xlim = c(-4, 4), 
     ylim = c(0, 40), 
     main = expression("Histogram of " * 'x'[2] * " Values (Failure)"),
     xlab = expression('x'[2] * " Values (Failure)"))

hist(success$x3, breaks = 16, 
     xlim = c(-4, 4), 
     ylim = c(0, 40), 
     main = expression("Histogram of " * 'x'[3] * " Values (Success)"),
     xlab = expression('x'[3] * " Values (Success)"))

hist(failure$x3, breaks = 16, 
     xlim = c(-4, 4), 
     ylim = c(0, 40), 
     main = expression("Histogram of " * 'x'[3] * " Values (Failure)"),
     xlab = expression('x'[3] * " Values (Failure)"))

mean(success$x1)
sd(success$x1)
mean(success$x2)
sd(success$x2)
mean(success$x3)
sd(success$x3)
mean(success$x4)
sd(success$x4)
mean(success$x5)
sd(success$x5)

mean(failure$x1)
sd(failure$x1)
mean(failure$x2)
sd(failure$x2)
mean(failure$x3)
sd(failure$x3)
mean(failure$x4)
sd(failure$x4)
mean(failure$x5)
sd(failure$x5)

n <- nrow(train)
folds <- sample(rep(1:8, length = n))

Linear.MSE <- NULL
for(k in 1:8) {
  sub.train <- train[folds!=k,]
  sub.valid <- train[folds==k,]
  
  logistic.mod <- glm(y ~ ., data = sub.train, family = binomial)
  logistic.pred <- predict(logistic.mod, newdata = sub.valid, type = "response")
  Linear.MSE[k] <- mean((logistic.pred - sub.valid$y)^2)
}
Linear.CV <- mean(Linear.MSE)
Linear.CV

quad.train <- train
quad.train$x1.2 <- quad.train$x1^2
quad.train$x2.2 <- quad.train$x2^2
quad.train$x3.2 <- quad.train$x3^2
quad.train$x4.2 <- quad.train$x4^2
quad.train$x5.2 <- quad.train$x5^2

Linear.Quad.MSE <- NULL
for(k in 1:8) {
  sub.train <- quad.train[folds!=k,]
  sub.valid <- quad.train[folds==k,]
  
  logistic.mod <- glm(y ~ ., data = sub.train, family = binomial)
  logistic.pred <- predict(logistic.mod, newdata = sub.valid, type = "response")
  Linear.Quad.MSE[k] <- mean((logistic.pred - sub.valid$y)^2)
}
Linear.Quad.CV <- mean(Linear.Quad.MSE)
Linear.Quad.CV

best.mod <- glm(y ~ ., data = quad.train, family = binomial)

quad.valid <- valid
quad.valid$x1.2 <- quad.valid$x1^2
quad.valid$x2.2 <- quad.valid$x2^2
quad.valid$x3.2 <- quad.valid$x3^2
quad.valid$x4.2 <- quad.valid$x4^2
quad.valid$x5.2 <- quad.valid$x5^2

best.pred <- predict(best.mod, newdata = quad.valid, type = "response")
best.pred <- ifelse(best.pred > 0.5, 1, 0)

Conf.Matrix <- table(Predicted = best.pred, True = valid$y)
Conf.Matrix

Mis.Class.Rate <- 1 - (sum(diag(Conf.Matrix))/sum(Conf.Matrix))
Mis.Class.Rate

False.Neg <- Conf.Matrix[2]/(Conf.Matrix[1] + Conf.Matrix[2])
False.Neg

False.Pos <- Conf.Matrix[4]/(Conf.Matrix[3] + Conf.Matrix[4])
False.Pos
