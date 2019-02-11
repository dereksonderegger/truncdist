# library(truncdist2)
# library(testthat)

# Check to see if I get the same values in Poisson distribution
test_that('Unrestricted Poisson values correct', {
  expect_equal(dpois( 2, lambda=3 ),  dtrunc(2, 'pois', lambda=3) )
  expect_equal(ppois( 2, lambda=3 ),  ptrunc(2, 'pois', lambda=3) )
  expect_equal(qpois( .8, lambda=3 ), qtrunc(.8, 'pois', lambda=3) )
})

# Check to see if I get the same values in Exponential distribution
test_that('Unrestricted Exponential values correct', {
  expect_equal(dexp( 2, rate=3 ),  dtrunc(2, 'exp', rate=3) )
  expect_equal(pexp( 2, rate=3 ),  ptrunc(2, 'exp', rate=3) )
  expect_equal(qexp( .8, rate=3 ), qtrunc(.8, 'exp', rate=3) )
})


# Do I get the correct probabilities in truncated poisson case?
# Here we will consider removing 0 and 1
test_that('Restricted Poisson Correct Values',{
  expect_equal(dpois(3, lambda=2)/(1-ppois(1,lambda=2)),  dtrunc(3, 'pois', a=1, lambda=2))
  expect_equal(dpois(3, lambda=5)/(1-ppois(1,lambda=5)),  dtrunc(3, 'pois', a=1, lambda=5))
  expect_equal(dpois(5, lambda=5)/(1-ppois(4,lambda=5)),  dtrunc(5, 'pois', a=4, lambda=5))
})

# Do we get the correct probabilities in the Restricted Normal case?
test_that('Restricted Normal Case', {
  expect_equal( 2*dnorm(1),  dtrunc(1, 'norm', a=0) )
  expect_equal( 2*dnorm(1),  dtrunc(1, 'norm', a=0, mean=0, sd=1) )
  expect_equal( 2*dnorm(1),  dtrunc(1, 'norm', a=0, params=list(mean=0, sd=1)) )

  expect_equal( 2*dnorm(-1),  dtrunc(-1, 'norm', b=0) )
  expect_equal( 2*dnorm(-1),  dtrunc(-1, 'norm', b=0, mean=0, sd=1) )
  expect_equal( 2*dnorm(-1),  dtrunc(-1, 'norm', b=0, params=list(mean=0, sd=1)) )

  expect_equal( 0,  dtrunc(0, 'norm', a=0) )
  expect_equal( 0,  dtrunc(0, 'norm', b=0) )
})


# Can I send in vectors?
test_that('Sending multiple values to evaluate', {
  expect_equal( dnorm( -1:1 ), dtrunc( -1:1, 'norm' ) )
  expect_equal( dnorm( -1:1 ), dtrunc( -1:1, 'norm', mean=0, sd=1) )
  expect_equal( dnorm( -1:1 ), dtrunc( -1:1, 'norm', params=list(mean=0, sd=1)) )
  expect_equal( pnorm( -1:1 ), ptrunc( -1:1, 'norm' ) )
  expect_equal( pnorm( -1:1 ), ptrunc( -1:1, 'norm', mean=0, sd=1) )
  expect_equal( pnorm( -1:1 ), ptrunc( -1:1, 'norm', params=list(mean=0, sd=1)) )
  expect_equal( qnorm( 1:3/4 ), qtrunc( 1:3/4, 'norm' ) )
  expect_equal( qnorm( 1:3/4 ), qtrunc( 1:3/4, 'norm', mean=0, sd=1 ) )
  expect_equal( qnorm( 1:3/4 ), qtrunc( 1:3/4, 'norm', params=list(mean=0, sd=1)) )
})

# Does the Hypergeometric distribution work?
test_that( 'Hypergeometric distribution', {
  expect_equal( dhyper(3, m=5, n=10, k=4), dtrunc(3, 'hyper', m=5, n=10, k=4))
  expect_equal( dhyper(3, m=5, n=10, k=4), dtrunc(3, 'hyper', params=list(m=5, n=10, k=4)) )
  expect_equal( dhyper(c(3,4), m=5, n=10, k=4), dtrunc(c(3,4), 'hyper', params=list(m=5, n=10, k=4)) )
  expect_equal( phyper(3, m=5, n=10, k=4), ptrunc(3, 'hyper', m=5, n=10, k=4))
  expect_equal( phyper(3, m=5, n=10, k=4), ptrunc(3, 'hyper', params=list(m=5, n=10, k=4)) )
  expect_equal( phyper(c(3,4), m=5, n=10, k=4), ptrunc(c(3,4), 'hyper', params=list(m=5, n=10, k=4)) )
  expect_equal( qhyper(.8, m=5, n=10, k=4), qtrunc(.8, 'hyper', m=5, n=10, k=4))
  expect_equal( qhyper(.8, m=5, n=10, k=4), qtrunc(.8, 'hyper', params=list(m=5, n=10, k=4)) )
  expect_equal( qhyper(c(.7,.8), m=5, n=10, k=4), qtrunc(c(.7,.8), 'hyper', params=list(m=5, n=10, k=4)) )
})

# Testing the random number generation
test_that( 'Testing random number generation - right length:', {
  expect_equal( length(rtrunc(1000, 'norm')), 1000 )
  expect_equal( length(rtrunc(1000, 'norm', mean=5, sd=2)), 1000)
  expect_equal( length(rtrunc(1000, 'norm', params=list(mean=5, sd=2))), 1000)
  expect_equal( length(rtrunc(1000, 'norm', a=0, params=list(mean=5, sd=2))), 1000)
  expect_equal( length(rtrunc(1000, 'hyper', params=list(m=5, n=10, k=4))), 1000 )
  expect_equal( length(rtrunc(1000, 'hyper', a=0, params=list(m=5, n=10, k=4))), 1000 )
})

# Testing the random number generation
p <- seq(.2, .8, by=.2)
test_that( 'Testing random number generation - right distribution:', {
  expect_lt( sum( abs( quantile(rtrunc(100000, 'norm'), p) - qtrunc(p, 'norm') ) )/4,  0.1)
  expect_lt( sum( abs( quantile(rtrunc(100000, 'norm', a=0), p) - qtrunc(p, 'norm', a=0) ) )/4,  0.1)
  expect_lt( sum( abs( quantile(rtrunc(100000, 'hyper', params=list(m=5,n=10,k=4)), p) - qtrunc(p, 'hyper', params=list(m=5,n=10,k=4))) )/4,  0.1)
})


# Testing can we send in vectors of parameters
test_that( 'Testing sending vectors of parameters', {
  expect_equal(dnorm(c(-1,0,1), mean=c(-.5, 0, .5), sd=c(4,2,1) ), dtrunc(x=c(-1,0,1), spec='norm', mean=c(-.5, 0, .5), sd=c(4,2,1)))
  expect_equal(dnorm(c(-1,0,1), mean=c(-.5, 0, .5), sd=1), dtrunc(c(-1,0,1), 'norm', params=list(mean=c(-.5,0,.5), sd=1)))
  expect_equal(pnorm(c(-1,0,1), mean=c(-.5, 0, .5), sd=c(4,2,1) ), ptrunc(c(-1,0,1), 'norm', mean=c(-.5, 0, .5), sd=c(4,2,1)))
  expect_equal(pnorm(c(-1,0,1), mean=c(-.5, 0, .5), sd=1), ptrunc(c(-1,0,1), 'norm', params=list(mean=c(-.5,0,.5), sd=1)))
  expect_equal(qnorm(c(.1, .5, .8), mean=c(-.5, 0, .5), sd=c(4,2,1) ), qtrunc(c(.1, .5, .8), 'norm', mean=c(-.5, 0, .5), sd=c(4,2,1)))
  expect_equal(qnorm(c(.1, .5, .8), mean=c(-.5, 0, .5), sd=1), qtrunc(c(.1, .5, .8), 'norm', params=list(mean=c(-.5,0,.5), sd=1)))
  expect_equal(rtrunc(5, 'norm', params=list(mean=1:5, sd=0)), 1:5)
  expect_equal(rtrunc(3, 'hyper', params=list(m=c(2,2,2), n=c(0,0,0), k=2)), c(2,2,2) )

  expect_equal(dnorm( 3, mean=-1:1), dtrunc(3, 'norm', params=list(mean=-1:1)))
  expect_equal(pnorm( 3, mean=-1:1), ptrunc(3, 'norm', params=list(mean=-1:1)))
  expect_equal(qnorm( .2, mean=-1:1), qtrunc(.2, 'norm', params=list(mean=-1:1)))
})
