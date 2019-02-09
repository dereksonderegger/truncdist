#' Create a q-q plot for a truncated random variable.
#'
#' This function produces a q-q plot of sample quantiles against theoretical
#' quantiles for a truncated random variable
#'
#' @param x A numeric vector of sample values
#' @param spec  A character vector for the name of the distribution (e.g., "norm")
#' @param a The minimum value allowed in the distribution (inclusive).
#' @param b The maximum value allowed in the distribution (inclusive).
#' @param params A list of distribution parameter name/value pairs
#' @param ... Distribution parameter arguments. Only used if `params` is NULL.
#' @param title Plot title
#' @param xlabel Label for the X-axis
#' @param ylabel Label for the Y-axis
qqtrunc <- function( x, spec, a=-Inf, b=Inf, title="Truncated Distribution Q-Q Plot",
    xlabel="Theoretical Quantiles", ylabel="Sample Quantiles", params=NULL, ... ){

  if ( a >= b )
    stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)

  graphics::plot( qtrunc( stats::ppoints(x), spec, a=a, b=b,params=params),
        sort(x), main=title,
        xlab=xlabel, ylab=ylabel )
  graphics::abline( 0,1 )
    return( invisible( 0 ) )
}
