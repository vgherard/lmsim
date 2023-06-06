#' Data Generating Process
#'
#' @description
#' Generate datasets \eqn{\{(X_i, Y_i)\}_{i = 1, 2, \dots, n}} paired
#' observations according to a specified data generating process:
#'
#' \deqn{Y_i = f(X_i) + \varepsilon _i.}
#'
#' The building blocks of the data generating process, *i.e.* the function
#' \eqn{f} and the distributions of \eqn{X_i} and \eqn{\varepsilon _i} are
#' provided as arguments. Can handle both fixed and random regressors \eqn{X_i}.
#'
#' @param x either a numeric vector, a numeric matrix or a function. When called
#' on a scalar integer argument must return a vector of the same length.
#' See Details.
#' @param f,eps functions. When called on a single numeric vector or matrix must
#' return a numeric vector with the same number of observations (as given by
#' `NROW()`, *cf.* \link[base]{nrow}). See Details.
#'
#' @returns a function of a single integer argument `n`.
#'
#' @details
#' The only purpose of this function is to provide a convenience wrapper for
#' generating \eqn{XY} datasets according to the scheme described in the
#' description. `rdata_fun()` returns a function that can be called for this
#' purpose.
#'
#' ## Conditional mean and conditional noise
#'
#' The interpretation of the \eqn{f} and \eqn{\varepsilon} terms in the equation
#' above is the following:
#'
#' - \eqn{f(X_i) = \mathbb E(Y_i \vert X_i)}. This is the conditional
#' expectation of the responses \eqn{Y_i}, given the value of the regressors
#' \eqn{X_i}.
#' - \eqn{\varepsilon _i = Y_i\vert X_i - \mathbb E(Y_i \vert X_i)}. The noise
#' of the response, that may in principle depend on \eqn{X_i}.
#'
#' These terms are encoded, respectively, by the `f` and `eps` arguments of
#' `rdata_fun()`. These functions are supposed to be called with a single
#' numeric vector or matrix argument, and return a vector with the same number
#' of observations, as reported by `NROW()`.
#'
#' It is understood that the outputs of `f` and `eps` should be
#' deterministic and random, respectively, and that `eps` should have zero
#' conditional (on its argument) mean.
#' *No automatic check is performed to ensure these expectations are satisfied*,
#' and the correct definition of these argument is ultimately left to the user.
#' See examples below.
#'
#' ## Fixed vs. Random regressors
#'
#' As for the regressors, both cases of fixed and random \eqn{X_i} are
#' contemplated:
#'
#' - For *fixed* \eqn{X_i}'s, `x` should be specified as a numeric vector or
#' matrix, corresponding to the fixed values for the regressors.
#' - For *random* \eqn{X_i}'s, `x` should be specified as a function of a single
#' integer argument, a random generator for \eqn{X_i} observations (the argument
#' corresponding to the variable number of observations to draw from \eqn{X_i}
#' distribution).
#'
#' ## Returned value
#'
#' The returned value is a function of no arguments in the fixed \eqn{X} case,
#' and of an integer argument `n` in the random \eqn{X} case. Calls to this
#' function return a `data.frame` of observations drawn from the specified
#' \eqn{\{(X_i, Y_i)\}_{i = 1, 2, \dots, n}} distribution.
#'
#' @examples
#' # Linear model: Y = q + m X + eps
#' rxy <- rdata_fun(
#'   x = rnorm,
#'   f = \(x) 1 + 2*x,
#'   eps = \(x) rnorm(length(x), sd = 0.1)
#' )
#'
#' # calling:
#' rxy(10)
#' # does the same job as:
#' x <- rnorm(10)
#' y <- 1 + 2 * x + rnorm(10, sd = 0.1)
#' data.frame(x, y)
#'
#' @export
rdata_fun <- function(x, f, eps)
{
	f_eps_checks <- is.function(f) && is.function(eps)

	if (!(f_eps_checks))
		stop("'f' and 'eps' must be functions.")

	if (is.numeric(x))
		return(rdata_fun_fixed_x(x, f, eps))

	x_check <- is.function(x)
	if (!x_check)
		stop("'x' must be either a numeric, or a function.")

	return(rdata_fun_random_x(x, f, eps))
}

rdata_fun_fixed_x <- function(x, f, eps)
{
	res <- function() {
		y <- f(x) + eps(x)
		return(data.frame(x = x, y = y))
	}

	res <- structure(res, xtype = "fixed", class = "rdata")
	rdata_post_checks(res)

	return(res)
}

rdata_fun_random_x <- function(x, f, eps)
{
	res <- function(n) {
		xx <- x(n)
		y <- f(xx) + eps(xx)
		return(data.frame(x = xx, y = y))
	}

	res <- structure(res, xtype = "random", class = "rdata")
	rdata_post_checks(res)

	return(res)
}

rdata_post_checks <- function(rxy)
{
	n_test_random_x <- 10

	tryCatch({
		if (attr(rxy, "xtype") == "fixed")
			rxy()
		else
			rxy(n_test_random_x)
		},
		error = \(cond) {
			msg <- paste("Failed generation of example dataset.",
									 "Please check the definitions of 'x', 'f' and 'eps'.")
			stop(msg)
		}
	)
}

#' @export
print.rdata <- function(x, ...) cat("An rdata.")
