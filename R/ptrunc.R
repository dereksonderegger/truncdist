#' @rdname dtrunc
#' @export
ptrunc <- function( q, spec, a = -Inf, b = Inf, params=NULL, ... ){
  if ( a >= b )
      stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)

  tt <- q
  aa <- rep( a, length( q ) )
  bb <- rep( b, length( q ) )
  G <- get( paste( "p", spec, sep="" ), mode="function" )
  temp <- apply( cbind( apply( cbind( q, bb ), 1, min ), aa ), 1, max )
  tt <- do.call(G, append(list(temp), params))
  tt <- tt -  do.call(G, append(list(aa), params))
  G.a <- do.call(G, append(list(aa), params))
  G.b <- do.call(G, append(list(bb), params))
  if ( any( G.a == G.b ) ) {
    stop( "Trunction interval is not inside the domain of the distribution function" )
  }
  #result <- tt / ( G( bb, ... ) - G ( aa, ... ) )
  result <- tt / ( G.b - G.a )
  return( result )
}

