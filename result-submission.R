test_set <- read.csv("test.csv")
head(test_set)
head(unseen)

p_hat_submit <- (predict(glm2, unseen, type = "response") + predict(rf2_, unseen, type = "prob")[2]) / 2
submission <- data.frame(
  PassengerId = test_set$PassengerId,
  Transported = ifelse(p_hat_submit > 0.5, "True", "False")
)
write.csv(submission, "submission_.csv", row.names = FALSE)
