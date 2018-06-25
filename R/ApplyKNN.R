#' Apply KNN
#'
#' This function predicts categories on a 2D test data set using training data.
#' @param training.data training data table where columns 1 and 2 are 
#' Euclidean coordinates (numeric), and column 3 is the category 
#' @param test.data 2D test point, as Euclidean coordinates (numeric)
#' @param k
#' @return knn.table :table of test data and KNN predictions
#' @keywords KNN
#' @examples
#' iris.train <- iris[1:120,c(1,4,5)]
#' iris.test <- iris[121:150,c(1,4)]
#' ApplyKNN(iris.train, iris.test, 5)

FindCategory <- function(training.data, test.point, k) {
  # Finds a category for a given 2D test point, given training data
  #
  # Args:
  #    training.data:  training data table where columns 1 and 2 are 
  #           Euclidean coordinates (number), and column 3 is the category 
  #    test.point:     2D test point, as Euclidean coordinates (real/integer)
  #    k:              number of neighbors (integer)
  #
  # Returns:
  #    category:        category with highest count from neighbors (string)
  #
  # Find distances of training data from test point
  distance.table <- apply(training.data[,1:2], 1, function(x) CalculateDistance(x, test.point))
  distance.table <- data.frame(distance.table , training.data[,3])
  distance.table <- distance.table[order(distance.table[,1]),] 
  # Find nearest neighbors
  k.nearest.neighbors <- distance.table[1:k,]
  # Find category
  category.count <- table(k.nearest.neighbors[,2])
  max.list <- which(category.count == max(category.count))
  if (length(max.list) == 1) {
    category <- names(category.count[max.list])
  }
  else {
    random.category <- sample(max.list, 1)
    category <- names(category.count[random.category])
  }
  return(category)
}

ApplyKNN <- function(training.data, test.data, k) {
  # Finds a category for 2D test data, given training data
  #
  # Args:
  #    training.data: training data table where columns 1 and 2 are 
  #           Euclidean coordinates (number), and column 3 is the category.
  #           A matrix or data frame with 3 columns.
  #    test.data:  training data table with Euclidean coordinates. A matrix or
  #           data frame with 2 columns.
  #    k:              number of neighbors (integer)
  #
  # Returns:
  #    knn.table:  table of test data and KNN predictions. A matrix or data 
  #           frame with 3 columns.
  #
  # Find distances of training data from test point
  category.list <- apply(test.data, 1, function(x) FindCategory(training.data, x, k))
  knn.table <- data.frame(test.data, category.list)
  return(knn.table)
}
