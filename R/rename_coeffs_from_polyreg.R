#' Renames the coefficients from polyreg package to match our notation
#'
#' @param coeffs_polyreg vector with the coefficients of a PR obtained with polyreg
#'
#' @return a vector with the same coeffcients, renamed
#' @export

rename_coeffs_from_polyreg <- function(coeffs_polyreg) {

  # Obtain the names
  names_polyreg <- names(coeffs_polyreg)
  n <- length(names_polyreg)

  # Initialize a vector to store the new names
  names_betas <- rep("0", n)
  for (i in 2:n) {
    position <- stringi::stri_locate_all(pattern = "^", names_polyreg[i], fixed = TRUE)[[1]][, 2]
    vars <- strsplit(names_polyreg[i], "")[[1]][position - 1]
    exponents <- as.numeric(strsplit(names_polyreg[i], "")[[1]][position + 1])
    names_betas[i] <- paste(sort(rep(vars, times = exponents)), collapse = ",")
  }
  names(coeffs_polyreg) <- names_betas
  return(coeffs_polyreg)
}
