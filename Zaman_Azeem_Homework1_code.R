# these are various varialbes that need to be set
# some choices are arbitrary
start.point <- 0 # lower bound for x values
end.point <- 3 # upper bound for x (theta) values
n <- 10 # number of observed RVs
x.sum <- 50 # sum of these observed RVs
seq.len <- 1000 # number of points to plot
shape <- 3 # parameter for gamma dist
rate <- 3 # parameter for gamma dist

# create a sequence of theta values
# these will be the domian values in the plots we make
theta.seq <- seq(from = start.point,
                 to = end.point,
                 length.out = seq.len)
# this function just computes values of the likelihood function
compExpLik <- function(theta, n, x.sum){
  return(theta^n*exp(-theta*x.sum))
}



makeGamSeqs <- function(theta.seq, shape, rate, n, x.sum){
#######################################
# This function makes of list of vectors that can be used to plot
# the prior, likelihood, and posterior
#
# Args
#   theta.seq:  a vector containing domain values
#   shape:  shape param for gamma dist
#   rate: rate param for gamma dist
#   n:  number of observed RVs
#   x.sum:  sum of observed RVs, a sufficient statistic
#
# Output:
#   This function returns a list.  The first item in the list is the 
# input theta.seq (for convinience later).  The second is the range values
# for the gamma prior with given shape and rate.  The third is the likelihood 
# domain values for the theta.seq range values.  The last is the range values
# for the posterior
#######################################
  # compute prior range values
  prior.seq <- dgamma(theta.seq,
                      shape = shape,
                      rate = rate)
  # compute posterior range values
  post.seq  <- dgamma(theta.seq,
                      shape = shape + n,
                      rate = rate + x.sum)
  # compute likelihood range values
  lik.seq <-   compExpLik(theta.seq, n, x.sum)
  # return a list with calculated vectors
  return(list(theta.seq, prior.seq, lik.seq, post.seq))
}

plotPrLiPo <- function(supp.seq,
                       prior.den.seq,
                       like.seq,
                       post.den.seq,
                       title = "Prior, Likelihood, Posterior",
                       x.lab = "theta",
                       y.lab = "density",
                       plot.leg = TRUE,
                       leg.pos = "topright"){
#######################################
# This function plots the prior, likelihood, and posterior on the same plot
#
# Args
#   supp.seq: sequence of support (domian) values for x-axis
#   prior.den.seq: density values for prior in vector
#   like.seq: density values for likelihood in vector
#   post.den.seq: density values for posterior
#   NOTE: ALL 4 OF THESE ARGUEMENTS SHOULD HAVE SAME LENGTH!
#   title:  a string with title for graph
#   x.lab:  label for x-axis (unknow parameter)
#   y.lab: label for y-axis (density)
#   plot.leg: boolean, whether to add legend to plot
#   leg.pos:  string or vector specifying legend position
#
# Output:
#   This function outputs a single graph with the prior, posterior, and
# likelihood on the same plot.  No axes are plotted.  Lines are distingushed
# by dots, dashes, and unbroken
#######################################
  # plot prior
  plot(supp.seq, # x values
       prior.den.seq, # y values
      type = "l",
      lty = 1, 
      main = title, # title is input
      axes = FALSE,
      frame.plot = TRUE,
      xlab = x.lab, # name x axis
      ylab = y.lab) # name y axis
  if (plot.leg){ # plot legend if desired
    legend(leg.pos,
         c("Prior", "Likelihood", "Posterior"),
         lty = c(1,2,3))
  }
  par(new = TRUE)
  # plot likelihood
  plot(supp.seq,
       like.seq,
       type = "l",
       lty = 2,
       xlab = "",
       ylab = "",
       axes = FALSE)
  par(new = TRUE)
  # plot posterior
  plot(supp.seq,
       post.den.seq,
       lty = 3,
       typ = "l",
       axes = FALSE,
       xlab = "",
       ylab = "")
}


varyParams <- function(param.mat, theta.seq, n, x.sum){
#######################################
# This function creates a series of plots, each corresponding to a set
# of hyperparameters taken from an input matrix
#
# Args:
#   param.mat: a matrix with various shape and rate combinations
#              first column contains shapes, second rates
#   theta.seq:  domain values for plot
#   n:  number of observations
#   x.sum: sum of these obervations
#
# Output:
#   A series of plots.  Can set grid size using par to make them appear in
# a single grid
#######################################
  apply(param.mat, 1, function(x){
    # compute seqs for this pair of params
    list.seqs <- makeGamSeqs(theta.seq, x[1], x[2], n, x.sum)
    # make title for graph
    main = paste0("shape = ", as.character(x[1]),
                  ", rate = ", as.character(x[2]))
    # plot densities for this set of params
    plotPrLiPo(list.seqs[[1]], 
               list.seqs[[2]], 
               list.seqs[[3]], 
               list.seqs[[4]],
               title = main,
               x.lab = "",
               y.lab = "", 
               plot.leg = FALSE)
  })
}
# this calcultes values needed to create plot
list.seqs <- makeGamSeqs(theta.seq, shape, rate, n, x.sum)
# create plot with single prior, likelihood, and posterior
pdf(file = "jointplot.pdf")
par(mfrow = c(1,1))
plotPrLiPo(list.seqs[[1]], list.seqs[[2]], list.seqs[[3]], list.seqs[[4]])
dev.off()

# create grid with varying hyperparams
params <- c(2,3,4)
pair.mat <- expand.grid(params, params)
pdf(file = "gridplot.pdf") # save as pdf in working dir
par(mfrow = c(length(params), length(params)))
varyParams(pair.mat, theta.seq, n, x.sum)
dev.off()
# create grid with varying hyperparams and LARGE n
pdf(file = "gridplotbign.pdf") # save as pdf in working dir
par(mfrow = c(length(params), length(params)))
varyParams(pair.mat, theta.seq, n = 100, x.sum = 100)
dev.off()


