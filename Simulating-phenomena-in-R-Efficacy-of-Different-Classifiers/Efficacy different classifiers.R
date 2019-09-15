# Selection of the dataframe
library(MASS)
library(neuralnet)
library(nnet)
library(class)

data("iris")

# Function to determine the accuracy index of each technique
accuracy <- function(dataframe){
  agreement <- 0
  total_data <- 0
  x_axis <- dim(dataframe)[1]
  for(i in c(1:x_axis)){
    if(dataframe[i,1] == dataframe[i,2]){
      agreement <- c(agreement, dataframe[i,3])
    }
    total_data <- c(total_data, dataframe[i,3])
  }
  agreement <- sum(agreement)/sum(total_data)
  return(agreement)
}

# Creation of dataframe
data <- data.frame(iris)

print(summary(data))
print(boxplot(data[1:4]))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data[,1:4] <- lapply(data[,1:4], normalize)

classify <- function(dataset, proportion, output, neighbours, hidden){
  accuracy_output <- c()
  index <- sample(nrow(data), nrow(data)*proportion)
  training_set <- data[index, ]
  test_set <- data[-index, ]
  fit <- switch(output,
                "LDA" = lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=training_set),
                "QDA" = qda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=training_set),
                "LMR" = multinom(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=training_set),
                "KNN" = knn(train = training_set[,1:4], test = test_set[,1:4], cl = training_set[,5], k=neighbours, prob = TRUE), 
                "NN" = nnet(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=training_set, size=hidden, maxit=100)
  )
  
  if(output == "KNN"){
    table_accuracy <- table(actual=test_set[,5], predicted = fit)
  }else{
    if(output == "LDA" || output == "QDA"){
      predictions <- predict(fit, test_set[,1:4])$class
    }
    else if (output == "NN" || output == "LMR"){
      predictions <- predict(fit,test_set,type="class") 
    }
    table_accuracy <- as.data.frame(table(predictions, test_set[,5]))
  }
  
  accuracy_output <- accuracy(table_accuracy)
  return(accuracy_output)
}

# KNN performance study as a function of the neighbours
results <- c()
for (i in c(1:30)){
  results <- c(results, mean(replicate(10000, classify(data, 0.67, "KNN", i, 4))))
}
print(results)
plot(results, type="b")
axis(1, at = c(1:30), labels = seq(1, 30, by = 1))

#Study of neural network performance study as a function of neurons in the hidden layer
results <- c()
for (j in c(1:10)){
  results <- c(results, mean(replicate(10000, classify(data, 0.67, "NN", 11, j))))
}
plot(results, type="b")


# Results for the general performance of each method
results_lda <- mean(replicate(10000, classify(data, 0.67, "LDA", 11, 5)))
print(results_lda)

results_qda <- mean(replicate(10000, classify(data, 0.67, "QDA", 11, 5)))
print(results_qda)

results_lmr <- mean(replicate(10000, classify(data, 0.67, "LMR", 11, 5)))
print(results_lmr)

results_knn <- mean(replicate(10000, classify(data, 0.67, "KNN", 11, 5)))
print(results_knn)

results_nn <- mean(replicate(10000, classify(data, 0.67, "NN", 11, 5)))
print(results_nn)

# Accuracy as a function of the dataset size
dataset_change <- function(size, dataset){
  index <- sample(nrow(dataset), size)
  dataset <- dataset[index,]
}

results_dataset_lda <- c()
for (i in seq(25, 150, by=25)){
  results_dataset_lda <- c(results_dataset_lda, mean(replicate(10000, classify(dataset_change(i,data), 0.67, "LDA", 11, 5))))
}
print(results_dataset_lda)
plot(results_dataset_lda, xaxt="n", type = "b")
axis(1, at=c(1:6), labels = seq(25, 150, by=25))

results_dataset_qda <- c()
for (i in seq(25, 150, by=25)){
  results_dataset_qda <- c(results_dataset_qda, mean(replicate(10000, classify(dataset_change(i,data), 0.67, "QDA", 11, 5))))
}
print(results_dataset_qda)
plot(results_dataset_qda, xaxt="n", type = "b")
axis(1, at=c(1:6), labels = seq(25, 150, by=25))

results_dataset_lmr <- c()
for (i in seq(25, 150, by=25)){
  results_dataset_lmr <- c(results_dataset_lmr, mean(replicate(10000, classify(dataset_change(i,data), 0.67, "LMR", 11, 5))))
}
print(results_dataset_lmr)
plot(results_dataset_lmr, xaxt="n", type = "b")
axis(1, at=c(1:6), labels = seq(25, 150, by=25))

results_dataset_knn <- c()
for (i in seq(25, 150, by=25)){
  results_dataset_knn <- c(results_dataset_knn, mean(replicate(10000, classify(dataset_change(i,data), 0.67, "KNN", 11, 5))))
}
print(results_dataset_knn)
plot(results_dataset_knn, xaxt="n", type = "b")
axis(1, at=c(1:6), labels = seq(25, 150, by=25))

results_dataset_nn <- c()
for (i in seq(25, 150, by=25)){
  results_dataset_nn <- c(results_dataset_nn, mean(replicate(10000, classify(dataset_change(i,data), 0.67, "NN", 11, 5))))
}
print(results_dataset_nn)
plot(results_dataset_nn, xaxt="n", type = "b")
axis(1, at=c(1:6), labels = seq(25, 150, by=25))


# Results as a function of the relative size of training and test set
results_lda <- c()
for(i in c(0.25, 0.33, 0.5, 0.67, 0.75, 0.9)){
  results_lda <- c(results_lda, mean(replicate(10000, classify(data, i, "LDA", 11, 5))))
}
print(results_lda)
plot(results_lda, xaxt="n", type="b")
axis(1, at=c(1:6), labels = c(25, 33, 50, 67, 75, 90))

results_qda <- c()
for(i in c(0.5, 0.67, 0.75, 0.9)){
  results_qda <- c(results_qda, mean(replicate(10000, classify(data, i, "QDA", 11, 5))))
}
print(results_qda)
plot(results_qda, xaxt="n", type="b")
axis(1, at=c(1:4), labels = c(50, 67, 75, 90))

results_lmr <- c()
for(i in c(0.25, 0.33, 0.5, 0.67, 0.75, 0.9)){
  results_lmr <- c(results_lmr, mean(replicate(10000, classify(data, i, "LMR", 11, 5))))
}
print(results_lmr)
plot(results_lmr, xaxt="n", type="b")
axis(1, at=c(1:6), labels = c(25, 33, 50, 67, 75, 90))

results_knn <- c()
for(i in c(0.25, 0.33, 0.5, 0.67, 0.75, 0.9)){
  results_knn <- c(results_knn, mean(replicate(10000, classify(data, i, "KNN", 11, 5))))
}
print(results_knn)
plot(results_knn, xaxt="n", type="b")
axis(1, at=c(1:6), labels = c(25, 33, 50, 67, 75, 90))

results_nn <- c()
for(i in c(0.25, 0.33, 0.5, 0.67, 0.75, 0.9)){
  results_nn <- c(results_nn, mean(replicate(10000, classify(data, i, "NN", 11, 5))))
}
print(results_nn)
plot(results_nn, xaxt="n", type="b")
axis(1, at=c(1:6), labels = c(25, 33, 50, 67, 75, 90))