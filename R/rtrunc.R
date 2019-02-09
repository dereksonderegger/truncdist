#' @rdname dtrunc
#' @export
rtrunc <- function( n, spec, a = -Inf, b = Inf, params=NULL, ... ){
  if ( a >= b )
    stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)

  u <- stats::runif( n, min = 0, max = 1 )
  x <- qtrunc( u, spec, a = a, b = b, params=params )
  return( x )
}
