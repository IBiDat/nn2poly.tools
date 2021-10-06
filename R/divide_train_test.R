#' Divide data in train and test
#'
#' @param data input data
#' @param train_proportion % of train
#'
#' @return list with train and test
#' @export
#'

divide_train_test <- function(data, train_proportion) {
  index <- sample(1:nrow(data), round(train_proportion * nrow(data)))
  train <- data[index, ]
  test <- data[-index, ]

  output <- vector(mode = "list", length = 2)
  output[[1]] <- train
  output[[2]] <- test
  names(output) <- c("train", "test")

  return(output)
}
