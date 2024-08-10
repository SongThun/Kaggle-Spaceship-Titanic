library(tidyverse)
library(caret)

data <- imp_df %>%
  mutate(Transported = factor(df$Transported, exclude = NA),
         Number = as.integer(Number)) %>%
  mutate_at(vars(Age, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck), scale)
  select(-Group)

head(data)
str(data)
summary(data)
unseen <- data[is.na(data$Transported),]
data <- data[!is.na(data$Transported),]
apply(data, 2, function(x) sum(is.na(x)))

set.seed(1)
test_index <- createDataPartition(data$Transported, times = 1, p = 0.3, list = FALSE)
test <- data[test_index, ]
train <- data[-test_index, ]

# glm
glm_all <- train(Transported ~ ., data = train, method = "glm")
predict(glm_all, test)
confusionMatrix(predict(glm_all, test), as.factor(test$Transported))

glm1 <- glm(Transported ~ ., data = train, family = "binomial")
pred1 <- factor(ifelse(predict(glm1, test) > 0.1, "True", "False"))
confusionMatrix(pred1, test$Transported)

step_glm1 <- step(glm1, direction = "backward", trace = 0)
summary(step_glm1)
pred_step_glm1 <- factor(ifelse(predict(step_glm1, test) > 0.1, "True", "False"))
confusionMatrix(pred_step_glm1, test$Transported)

coefs <- data.frame(coef = names(step_glm1$coefficients[-1]),
                    estimate = step_glm1$coefficients[-1])
coefs <- coefs %>%
  arrange(desc(abs(estimate)))  

ggplot(coefs) +
  geom_col(aes(abs(estimate), reorder(coef, abs(estimate), decreasing = FALSE))) +
  labs(title = "Predictor importance", x = "Estimate", y = "Predictor")

# glm with poly terms
glm2 <- glm(Transported ~ poly(Spa,6, raw=TRUE) + poly(VRDeck,6, raw=TRUE) + .,
            family = "binomial",
            data = train)
summary(glm2)

pred_glm2 <- factor(ifelse(predict(glm2, test) > 0.1, "True", "False"))
confusionMatrix(pred_glm2, test$Transported)

pred_train_glm2 <- factor(ifelse(predict(glm2, train) > 0.1, "True", "False"))
confusionMatrix(pred_train_glm2, train$Transported)$overall["Accuracy"]

# glm with interact terms
glm3 <- glm(Transported ~ poly(Spa,6, raw=TRUE) + poly(VRDeck,6, raw=TRUE) + 
              VIP * RoomService + VIP * FoodCourt + VIP * Spa + VIP * VRDeck + VIP * ShoppingMall + .,
            family = "binomial",
            data = train)

summary(glm3)

pred_glm3 <- factor(ifelse(predict(glm3, test) > 0.1, "True", "False"))
confusionMatrix(pred_glm3, test$Transported)

pred_train_glm3 <- factor(ifelse(predict(glm3, train) > 0.1, "True", "False"))
confusionMatrix(pred_train_glm3, train$Transported)$overall["Accuracy"]

# knn
knn_all <- train(Transported ~ ., 
              data = train,
              method = "knn",
              tuneGrid = data.frame(k = seq(3,9,2)))
knn_all$bestTune
confusionMatrix(predict(knn_all, test), test$Transported)

knn1 <- knn3(Transported ~ ., data = train, k = 9)
confusionMatrix(predict(knn1, test, type = "class"), test$Transported)

knn2 <- knn3(Transported ~ HomePlanet + CryoSleep + Destination + 
               Age + VIP + RoomService + FoodCourt + ShoppingMall + Spa + 
               VRDeck + Deck + Number + Side,
             data = train, k = 5)
confusionMatrix(predict(knn2, test, type = "class"), test$Transported)


# rpart
modelLookup("rpart")
rpart_all <- train(Transported ~ .,
                   data = train,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.5, 0.02)))
rpart_all$bestTune
confusionMatrix(predict(rpart_all, test), test$Transported)

library(rpart)
rpart1 <- rpart(Transported ~ ., data = train, control = rpart.control(cp = 0.4, minsplit = 10))
confusionMatrix(predict(rpart1, test, type = "class"), test$Transported)

rpart2 <- train(Transported ~ HomePlanet + CryoSleep + Destination + 
                  Age + VIP + RoomService + FoodCourt + ShoppingMall + Spa + 
                  VRDeck + Deck + Number + Side,
                data = train,
                method = "rpart",
                tuneGrid = data.frame(cp = seq(0, 0.5, 0.02)))
rpart2$bestTune
confusionMatrix(predict(rpart1, test, type = "class"), test$Transported)

# randomForest
library(randomForest)
rf_all <- train(Transported ~ .,
                data = train,
                method = "rf",
                tuneGrid = data.frame(mtry = seq(100,500,50)),
                trControl = trainControl(method = "cv", number = 10))
rf_all$bestTune
confusionMatrix(predict(rf_all, test), test$Transported)
confusionMatrix(predict(rf_all, train), train$Transported)


rf1 <- randomForest(Transported ~ ., data = train, mtry = 150, nodesize = 20)
confusionMatrix(predict(rf_all, test), test$Transported)
confusionMatrix(predict(rf_all, train), train$Transported)

rf2 <- train(Transported ~ HomePlanet + CryoSleep + Destination + 
               Age + VIP + RoomService + FoodCourt + ShoppingMall + Spa + 
               VRDeck + Deck + Number + Side,
             data = train,
             method = "rf",
             tuneGrid = data.frame(mtry = seq(100, 500, 50)),
             trControl = trainControl(method = "cv", number = 5))
rf2$bestTune
confusionMatrix(predict(rf2, test), test$Transported)
confusionMatrix(predict(rf2, train), train$Transported)

rf2_ <- randomForest(Transported ~ HomePlanet + CryoSleep + Destination + 
                       Age + VIP + RoomService + FoodCourt + ShoppingMall + Spa + 
                       VRDeck + Deck + Number + Side,
                     data = train,
                     mtry = 200, nodesize = 30, ntree = 500)
confusionMatrix(predict(rf2_, test), test$Transported)
confusionMatrix(predict(rf2_, train), train$Transported)

# ensemble
p_hat <- (predict(glm2, test, type="response") + predict(rf2_,test, type = "prob")[,2]) / 2
confusionMatrix(factor(ifelse(p_hat > 0.5, "True", "False")), test$Transported)
