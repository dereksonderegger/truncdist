#' @rdname dtrunc
#' @export
qtrunc <- function( p, spec, a = -Inf, b = Inf, params=NULL, ... ){
  if ( a >= b )
    stop( "argument a is greater than or equal to b" )
  if(is.null(params))
    params <- list(...)

  tt <- p
  G   <- get( paste( "p", spec, sep="" ), mode = "function" )
  Gin <- get( paste( "q", spec, sep="" ), mode = "function" )
  G.a <- do.call(G, append(list(a), params))
  G.b <- do.call(G, append(list(b), params))
  if ( any(G.a == G.b) ) {
    stop( "Trunction interval is not inside the domain of the quantile function" )
  }
  temp <- do.call(G, append(list(a), params) )  +
    p * ( do.call(G, append( list(b), params )) - do.call(G, append(list(a), params) ))
  result <- pmin( pmax( a, do.call(Gin, append(list(temp), params) ) ), b )
  return( result )
}

