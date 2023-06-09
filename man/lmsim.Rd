% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmsim.R
\name{lmsim}
\alias{lmsim}
\title{Simulate Linear Regression}
\usage{
lmsim(formula, rdata, n, fun = coefficients, B = 1000, simplify = FALSE, ...)
}
\arguments{
\item{formula}{a symbolic description of the model to be fitted, same as the
\code{formula} argument of \link[stats]{lm}.}

\item{rdata}{a function, return value of a call to \link[lmsim]{rdata_fun}.}

\item{n}{a positive integer. Sample size of each simulated dataset (in the
case of random regressors).}

\item{fun}{a function to be applied on each of the simulated \code{lm} objects.}

\item{B}{a positive integer. Number of replicas for the simulation.}

\item{simplify}{\code{TRUE} or \code{FALSE}. Should the result be simplified to a
vector, matrix or higher dimensional array if possible? See
\link[base]{lapply} for more details.}

\item{...}{further arguments passed on to \link[stats]{lm}.}
}
\value{
a vector (possibly a list, depending on the value of the
\code{simplify} argument).
}
\description{
These functions are used to simulate the results of fitting a linear model on
data generated according to a specified process. They can be used to study
the distributions of regression coefficients, variance estimates, nominal
p-values, \emph{etc.}.
}
\details{
The simulations use \link[stats]{lm} to fit linear models, while the
definition of the true data generating process relies on the helper
\link[lmsim]{rdata_fun}.
}
\examples{
# Coefficient distribution of a misspecified linear model
set.seed(840)

rxy <- rdata_fun(  # Y = X * max(X, 0) + gaussian noise
	x = rnorm,
	f = \(x) ifelse(x > 0, x^2, 0),
	eps = \(x) rnorm(length(x), sd = .1)
)

plot(rxy(20))

sim_coefs <- lmsim(formula = y ~ x,
							 rdata = rxy,
							 n = 20,
							 fun = coefficients,
							 B = 1e3,
							 simplify = TRUE)

print(sim_coefs[,1:5])  # The first five simulations look as follows

summary(sim_coefs["x", ])

hist(sim_coefs["x", ],
		 freq = FALSE,
		 breaks = 30,
		 main = "Y vs. X - Distribution of linear regression slopes",
		 xlab = "Slope")
}
