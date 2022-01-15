


Layer_Combined_No_Restrictions = R6::R6Class(
	classname = "No_Restrictions",
	inherit = keras$layers$Layer,
	public = list(
		initialize = function(units) {
			super$initialize()
			self$units = units
		},
		build = function(input_shape) {
			self$combined_w_b = self$add_weight(
				shape = shape(tail(input_shape, 1) + tf$ones(shape(1, 1), dtype = "int32"), self$units),
				initializer = "random_normal",
				trainable = TRUE
			)
		},
		call = function(inputs) {
			b = self$combined_w_b[1, , drop = TRUE]
			w = self$combined_w_b[2:NULL, ]
			tf$matmul(inputs, w) + b
		}
	)
)

# Create the layer wrapper:

#' @export
#' @noRd
layer_combined_no_restrictions <- keras::create_layer_wrapper(Layer_Combined_No_Restrictions)
