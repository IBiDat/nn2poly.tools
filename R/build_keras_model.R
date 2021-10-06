

#' Title
#'
#' @param af_string_list a
#' @param h_neurons_vector a
#' @param p a
#' @param my_max_norm a
#'
#' @return a
#' @export
#'
#' @importFrom magrittr "%>%"
#'

build_keras_model <- function(p,
															 af_string_list,
															 h_neurons_vector,
															 my_max_norm) {
	###############################################
	# train and test expected to have p+1 columns,
	# with the last one being the response variable
	###############################################
	# Obtain parameters:
	L <- length(h_neurons_vector)

	# Create the nn model:
	nn <- keras::keras_model_sequential()

	# First layer:

	# Check the needed constraint
	if (my_max_norm[[1]] == "no_constraint") {
		nn %>%
			keras::layer_dense(units = h_neurons_vector[1],
									activation = af_string_list[[1]],
									input_shape = p)
	}  else if (my_max_norm[[1]] == "l2_norm"){
		nn %>%
			layer_combined_L2(units = h_neurons_vector[1]) %>%
			keras::layer_activation(activation = af_string_list[[1]])
	}  else if (my_max_norm[[1]] == "l1_norm"){
		nn %>%
			layer_combined_L1(units = h_neurons_vector[1]) %>%
			keras::layer_activation(activation = af_string_list[[1]])
	}


	if(L>2){
		# rest of the layers except the last one:
		for (l in 2:(L-1)){

			# Check the needed constraint
			if (my_max_norm[[1]] == "no_constraint") {
				nn %>%
					keras::layer_dense(units = h_neurons_vector[l],
											activation = af_string_list[[l]])
			} else if (my_max_norm[[1]] == "l2_norm"){
				nn %>%
					layer_combined_L2(units = h_neurons_vector[1]) %>%
					keras::layer_activation(activation = af_string_list[[1]])
			} else if (my_max_norm[[1]] == "l1_norm"){
				nn %>%
					layer_combined_L1(units = h_neurons_vector[1]) %>%
					keras::layer_activation(activation = af_string_list[[1]])
			}
		}
	}

	# Last layer with no constraints ( in classification case we need to rethink this.)
	nn %>%
		keras::layer_dense(units = h_neurons_vector[L],
								activation = af_string_list[[L]])


	return(nn)
}

