start.point <- 0
end.point <- 10
n <- 10
x.sum <- 10
seq.len <- 1000
shape <- 2
rate <- 3

theta.seq <- seq(from = start.point,
                 to = end.point,
                 length.out = seq.len)

compExpLik <- function(theta, n, x.sum){
  return(theta^n*exp(-theta*x.sum))
}

plot(theta.seq, 
     dgamma(theta.seq,
                       shape = shape,
                       rate = rate),
     type = "l",
     lty = 1, 
     main = "Prior, Likelihood, Posterior",
     axes = FALSE,
     frame.plot = TRUE,
     xlab = "theta",
     ylab = "density")
legend("topright",
       c("Prior", "Likelihood", "Posterior"),
       lty = c(1,2,3))
par(new = TRUE)
plot(theta.seq,
     compExpLik(theta.seq, n, x.sum),
     type = "l",
     lty = 2,
     xlab = "",
     ylab = "",
     axes = FALSE)
par(new = TRUE)
plot(theta.seq,
     dgamma(theta.seq,
            shape = 4,
            rate = 5),
     lty = 3,
     typ = "l",
     axes = FALSE,
     xlab = "",
     ylab = "")