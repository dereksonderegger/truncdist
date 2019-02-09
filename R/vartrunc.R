#' @rdname dtrunc
#' @export
vartrunc <- function( spec, a = -Inf, b = Inf, params=NULL, ... )
{
  if ( a >= b )
    stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)

  ex <- extrunc( spec, a = a, b = b, params=params )
  f <- function( x ) { ( x - ex )^2 * dtrunc( x, spec, a = a, b = b, params=params ) }
  tt <- stats::integrate( f, lower = a, upper = b )$value
  tt <- stats::integrate( f, lower = a, upper = b )$value
  return( tt )
}
