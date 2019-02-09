#' Density, distribution function, quantile function and random generation for a
#' truncated distribution.
#'
#' These functions compute the density function defined by the spec argument
#' for the vector of quantile values in x.  The random variable is truncated
#' to be in the interval ( a, b )
#'
#' The rational for having two different ways to pass the distribution parameters
#' is that the truncdist package originally passed parameters via the `...`
#' argument, but some distributions use x,p,q,n,a, or b as parameter names and
#' that causes issues (e.g. the hypergeometric distribution has an `n` parameter).
#' Therefore to maintain backwards compatability as well as handle the hypergeometric
#' distribution, two methods exist.
#'
#' @param x,q  A numeric vector of quantiles
#' @param p A vector of probabilities
#' @param n number of observations.
#' @param spec  A character vector for the name of the distribution (e.g., "norm" or "pois")
#' @param a The minimum value allowed in the distribution (exclusive).
#' @param b The maximum value allowed in the distribution (exclusive).
#' @param params A list of distribution parameter name/value pairs
#' @param ... Distribution parameter arguments. Only used if `params` is NULL.
#' @examples
#' # All of these are identical calculations
#' dpois(  2, lambda=3 )
#' dtrunc( 2, 'pois', lambda=3)
#' dtrunc( 2, 'pois', params=list(lambda=3) )
#'
#' dpois(  2, lambda=3 )
#' dtrunc(2, 'pois', lambda=3)
#' ppois(  2, lambda=3 )
#' ptrunc( 2, 'pois', lambda=3)
#' qpois(  .8, lambda=3 )
#' qtrunc( .8, 'pois', lambda=3)
#'
#' # Now consider the Poisson with the zero values removed
#' dtrunc( 2, 'pois', a=0, lambda=3 )
#' dtrunc( 2, 'pois', a=0, params=list(lambda=3) )
#' ptrunc( 2, 'pois', a=0, lambda=3 )
#' ptrunc( 2, 'pois', a=0, params=list(lambda=3) )
#' qtrunc( c(.8,.9), 'pois', a=0, lambda=3 )
#' qtrunc( .8, 'pois', a=0, params=list(lambda=3) )
#' rtrunc( 10, 'pois', a=0, lambda=2 )
#' rtrunc( 10, 'pois', a=0, params=list(lambda=2) )
#'
#' # Now to look at the hypergeometric
#' dhyper( 3, m=5, n=10, k=4 )
#' dtrunc( 3, 'hyper', m=5, n=10, k=4 )
#' dtrunc( 3, 'hyper', params=list(m=5, n=10, k=4) )
#' # rtrunc( 10, 'hyper', m=5, n=10, k=4 ) # error!
#' rtrunc( 10, 'hyper', params=list(m=5, n=10, k=4) )
#' @export
dtrunc <- function( x, spec, a = -Inf, b= Inf, params=NULL, ... ){
  if ( a >= b )
      stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)
  tt <- rep( 0, length( x ) )
  g <- get( paste( "d", spec, sep="" ), mode="function" )
  G <- get( paste( "p", spec, sep="" ), mode="function" )
  G.a <- do.call(G, append(list(a), params))
  G.b <- do.call(G, append(list(b), params))
  if ( G.a == G.b ) {
    stop( "Trunction interval is not inside the domain of the density function" )
  }
  tt[x >= a & x <= b] <-
    do.call( g, append(list(x[x >= a & x <= b]), params)) / ( G.b - G.a )
  return( tt )
}
