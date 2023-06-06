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
}
