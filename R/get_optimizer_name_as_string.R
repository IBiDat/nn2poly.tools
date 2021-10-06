#' Get optimizer name as string
#'
#' @param my_optimizer a keras optimizer function
#'
#' @return string
#' @export
#'
get_optimizer_name_as_string <- function(my_optimizer) {
  temp_optimizer_name <- strsplit(as.character(my_optimizer), "")[[1]]
  temp_string_position <- rev(which(temp_optimizer_name == "."))[1] + 1

  temp_optimizer_name <- temp_optimizer_name[temp_string_position:(length(temp_optimizer_name) - 1)]

  output <- ""
  for (i in 1:length(temp_optimizer_name)) {
    output <- paste0(output, temp_optimizer_name[i])
  }

  return(output)
}
