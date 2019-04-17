#' Expected value and Variance of a truncated continuous distribution.
#'
#' These function compute the expected value and variance of a truncated random variable
#' with the given specification over the truncated domain using numerical integration.
#' For discrete distributions, see extrunc_discrete and vartrunc_discrete.
#'
#' @param spec  A character vector for the name of the distribution (e.g., "norm")
#' @param a The minimum value allowed in the distribution (exclusive).
#' @param b The maximum value allowed in the distribution (exclusive).
#' @param params A list of distribution parameter name/value pairs
#' @param ... Distribution parameter arguments. Only used if `params` is NULL.
#' @export
extrunc <- function( spec, a = -Inf, b = Inf, params=NULL, ... ){
  if ( a >= b )
    stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)

  f <- function( x ) { x * dtrunc( x, spec, a = a, b = b, params=params)}
  return( stats::integrate( f, lower = a, upper = b )$value )
}




extrunc_discrete <- function( spec, a=-Inf, b=Inf, params=NULL, ...){
  if ( a >= b )
    stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)


}
