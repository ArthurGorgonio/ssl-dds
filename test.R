# data <- read.arff("elecNormNew.arff")
#
# par(mfrow = c(3, 3))
#
# colnames <- dimnames(data)[[2]]
#
# for (i in 1:9) {
#   hist(as.numeric(data[, i]), main = colnames[i], probability = T)
#   d <- density(as.numeric(data[,i]))
#   lines(d, col = "red")
# }
#
# for (i in 1:9) {
#   barplot(as.numeric(data[,i]), main = colnames[i], probability = T)
# }
#
# for (i in 1:9) {
#   boxplot(as.numeric(data[,i]), main = colnames[i])
# }

folds <- stratifiedKFold(data, data$class)
for (fold in folds) {
  train <- data[-fold, ]
  test <- data[fold, ]
  model <- naiveBayes(class ~ ., train)
  cat(rep("*",15), "\n")
  mClass <- predict(model, test[,-1], type = "class")
  table(mClass, test$class)
  cat(rep("+",15), "\n")
  mRaw <- predict(model, test[,-1], type = "raw")
  table(mRaw, test$class)
}
