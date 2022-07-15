

#' Generate uniform data (X) and a polynomial response (Y) for testing purposes
#'
#' @param n_sample  number of samples
#' @param p dimensions
#' @param q_original degree of the original polynomial
#' @param mean_range range for the normal mean
#' @param beta_range range for the ebta coefficients
#' @param error_var variance of the added normal error
#'
#' @return list with data and orginal coefficients
#' @export
#'
generate_uniform_data <- function(n_sample, p, q_original, unif_range, error_var) {

  X <- matrix(0,n_sample,p)

  for (i in 1:p){
  X[,i] <- runif(n = n_sample, min = unif_range[1], max = unif_range[2])
  }

  # compute the needed number of betas with
  n_betas <- 0
  for (t in 0:q_original) {
    n_betas <- n_betas + choose(p + t - 1, t) # each time adding the number of possible combinations with repetition with length t
  }

  # Obtain the values for thebetas unformly distributed in the given range
  original_betas <- rep(0,n_betas)
  while(sum(abs(original_betas)) == 0){
  	original_betas <- sample(x=c(-2,-1,0,1,2), size=10, replace=TRUE)
  }


  # intialize the response vector
  Y <- rep(0, n_sample)
  # set a counter


  # loop over all the sample
  for (i in 1:n_sample) {
    # set up counter to know which beta we are using at each step
    counter <- 1

    Y[i] <- original_betas[1] # add the intercept first

    for (t in 1:q_original) {
      # Compute the possible combinations of length t and store the number of them.
      indexes <- gtools::combinations(p, t, repeats.allowed = TRUE)
      indexes.rows <- nrow(indexes)

      # loop over all combinations of length t
      for (ind in 1:indexes.rows) {
        # product of all the variables for a given combination
        product <- 1
        for (j in 1:length(indexes[ind, ])) {
          product <- product * X[i, indexes[ind, j]]
        }

        # add each term to the response
        Y[i] <- Y[i] + original_betas[counter + ind] * product
      }
      # update counter after all combinations of length t are computed
      counter <- counter + indexes.rows
    }
  }

  # finally we add some normal errors:
  Y <- Y + stats::rnorm(n_sample, 0, error_var)

  # Store all as a data frame
  data <- as.data.frame(cbind(X, Y))

  # Output includes the data and the original betas to comapre later
  output <- vector(mode = "list", length = 2)
  output[[1]] <- data
  output[[2]] <- original_betas
  names(output) <- c("data", "original_betas")
  return(output)
}
