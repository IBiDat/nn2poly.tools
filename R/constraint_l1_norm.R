
#' Define a custom keras constraint
#'
#' @param w The weights to which the constraint will be applied
#'
#' @return Constrained weights

constraint_l1_norm <- function(w) {
	norms = keras::k_sum(keras::k_abs(w), axis = 1, keepdims = TRUE)
	desired = keras::k_clip(norms, 0, 1)
	final = w*(desired/(keras::k_epsilon() + norms))
}
