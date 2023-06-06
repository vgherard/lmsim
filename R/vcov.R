# TODO:
# @export
lmsim_vcov <- function(formula,
											 rdata,
											 n,
											 B = 1000,
											 vcov = stats::vcov,
											 ...
											 )
{
	res <- lmsim(formula, rdata, n, fun = vcov, B = B, simplify = FALSE, ...)
	class(res) <- c(class(res), "lmsim_vcov")
	return(res)
}

#' @export
summary.lmsim_vcov <- function(object, ...) {
	B <- attr(object, "B")
	mean_vcov <- Reduce("+", object) / attr(object, "B")

	cat("Mean of vcov estimator (Average over", B, "simulations):\n\n")
	print(mean_vcov)
}
