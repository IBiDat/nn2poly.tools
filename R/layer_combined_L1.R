

Layer_Combined_L1 = R6::R6Class(
	classname = "Layer_Combined_L1",
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
				trainable = TRUE,
				# Custom defined constraint with L1 norm
				constraint = constraint_l1_norm
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
layer_combined_L1 <- keras::create_layer_wrapper(Layer_Combined_L1)
