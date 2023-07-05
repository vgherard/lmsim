#' Simulate Linear Regression
#'
#' These functions are used to simulate the results of fitting a linear model on
#' data generated according to a specified process. They can be used to study
#' the distributions of regression coefficients, variance estimates, nominal
#' p-values, *etc.*.
#'
#' The simulations use \link[stats]{lm} to fit linear models, while the
#' definition of the true data generating process relies on the helper
#' \link[lmsim]{rdata_fun}.
#'
#' @param formula a symbolic description of the model to be fitted, same as the
#' `formula` argument of \link[stats]{lm}.
#' @param rdata a function, return value of a call to \link[lmsim]{rdata_fun}.
#' @param n a positive integer. Sample size of each simulated dataset (in the
#' case of random regressors).
#' @param fun a function to be applied on each of the simulated `lm` objects.
#' @param B a positive integer. Number of replicas for the simulation.
#' @param simplify `TRUE` or `FALSE`. Should the result be simplified to a
#' vector, matrix or higher dimensional array if possible? See
#' \link[base]{lapply} for more details.
#' @param ... further arguments passed on to \link[stats]{lm}.
#'
#' @return a vector (possibly a list, depending on the value of the
#' `simplify` argument).
#'
#' @examples
#' # Coefficient distribution of a misspecified linear model
#' set.seed(840)
#'
#' rxy <- rdata_fun(  # Y = X * max(X, 0) + gaussian noise
#' 	x = rnorm,
#' 	f = \(x) ifelse(x > 0, x^2, 0),
#' 	eps = \(x) rnorm(length(x), sd = .1)
#' )
#'
#' plot(rxy(20))
#'
#' sim_coefs <- lmsim(formula = y ~ x,
#' 							 rdata = rxy,
#' 							 n = 20,
#' 							 fun = coefficients,
#' 							 B = 1e3,
#' 							 simplify = TRUE)
#'
#' print(sim_coefs[,1:5])  # The first five simulations look as follows
#'
#' summary(sim_coefs["x", ])
#'
#' hist(sim_coefs["x", ],
#' 		 freq = FALSE,
#' 		 breaks = 30,
#' 		 main = "Y vs. X - Distribution of linear regression slopes",
#' 		 xlab = "Slope")
#' @export
lmsim <- function(formula,
									rdata,
									n,
									fun = coefficients,
									B = 1000,
									simplify = FALSE,
									...
									)
{
	if (!is(rdata, "rdata"))
		stop("'rdata' must be an object of class \"rdata\"")

	B <- as.integer(B)
	if (length(B) != 1 || is.na(B) || B <= 0) {
		stop("'B' must be a positive integer (number of simulations).")
	}

	xtype <- attr(rdata, "xtype")

	if (xtype == "random") {
		n <- as.integer(n)

		if (length(n) != 1 || is.na(n) || n <= 1) {
			msg <- paste("'n' must be an integer of greater than one",
									 "(number of observations in each simulated dataset)."
									 )
			stop(msg)
		}
	} else if (!missing(n)) {
		msg <- paste("'n' can only be specified for data generating processes with",
								 "random regressors, and will be ignored")
		warning(msg)
	}

	if (!is.function(fun))
		stop("'fun' must be a function.")

	simplify <- as.logical(simplify)
	if (length(simplify) != 1 || is.na(simplify))
		stop("'simplify' must be either TRUE or FALSE.")

	.rdata <- ifelse (xtype == "fixed", \() rdata(), \() rdata(n))

	res <- sapply(X = 1:B,
				 FUN = \(b) fun( lm(formula, data = .rdata(), ...) ),
				 simplify = simplify
				 )

	res <- structure(res,
									 formula = formula,
									 rdata = rdata,
									 fun = fun,
									 B = B,
									 simplify = FALSE,
									 class = "lmsim"
									)

	return(res)
}

#' @export
print.lmsim <- function(x, ...) {
	cat("An lmsim.")
	return(invisible(x))
}
