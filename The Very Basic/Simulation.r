library(foreign)
data <- read.dta("pop.dta")
View(data)

rangex <- range(data$X) 
rangey <- range(data$Y)
## determine the range of X and Y, 
## and use the lowest and highest bounds of the more spreadout range to set the xlim
## of the X axis of X's and Y's histograms


pdf("histxy.pdf")
par(mfrow = c(1,2)) ## to array the two histograms in a row  
hist(data$X, col = "grey", xlab = c("X"), freq = F, main = c("Histogram of X"), ylim = c(0, 0.2), xlim = c(rangey[1],rangey[2]))
curve(dnorm(x, mean = mean(data$X), sd = sd(data$X)), add = T, col = "black", lwd = 4)

hist(data$Y,col = "grey",xlab = c("Y"), freq = F, main = c("Histogram of Y"), ylim = c(0,0.2))
curve(dnorm(x, mean = mean(data$Y), sd = sd(data$Y)), add = T, col = "black", lwd = 4)
dev.off()

## to create a function that returns the sample means of X with different sample sizes 
smean_x <- function(n){
  A <- 1:1000
  set.seed(11223344)
  for (i in 1:1000) {
    A[i] <- mean(sample(data$X, n, replace = FALSE, prob = NULL))
  }
  return(A)
}

rangesamplex <- range(smean_x(10)) 
## determine the range of sample means with sample size of 10. 
## The lowest and highest bars of the range would be 
## used to set the xlim for all the four density plots 

pdf("histxsample.pdf")
par(mfrow = c(2,2))
hist(smean_x(10), col = "grey", xlab = c("sample means of X (sample size of 10)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0,0.6), xlim= c(rangesamplex[1],rangesamplex[2]))
curve(dnorm(x, mean= mean(smean_x(10)), sd = sd(smean_x(10))), col = "black", add = TRUE, lwd = 4)

hist(smean_x(50), col = "grey", xlab = c("sample means of X (sample size of 50)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0,1.3), xlim= c(rangesamplex[1],rangesamplex[2]))
curve(dnorm(x, mean= mean(smean_x(50)), sd = sd(smean_x(50))), col = "black", add = TRUE, lwd = 4)

hist(smean_x(250), col = "grey", xlab = c("sample means of X (sample size of 250)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0,2.5), xlim= c(rangesamplex[1],rangesamplex[2]))
curve(dnorm(x, mean= mean(smean_x(250)), sd = sd(smean_x(250))), col = "black", add = TRUE, lwd = 4)

hist(smean_x(500), col = "grey", xlab = c("sample means of X (sample size of 500)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0, 4), xlim= c(rangesamplex[1],rangesamplex[2]))
curve(dnorm(x, mean= mean(smean_x(500)), sd = sd(smean_x(500))), col = "black", add = TRUE, lwd = 4)
dev.off()

## to create a function that returns the sample means of Y with different sample sizes 
smean_y <- function(n){
  A <- 1:1000 ## creat a vector to store the result of every randoming sampling
  set.seed(11223344)
  for (i in 1:1000) {
    A[i] <- mean(sample(data$Y, n, replace = FALSE, prob = NULL))
  }
  return(A)
}

rangesampley <- range(smean_y(10)) 
## determine the range of sample means with sample size of 10. 
## The lowest and highest bars of the range would be 
## used to set the xlim for all the four density plots 

pdf("histysample.pdf")
par(mfrow = c(2,2))
hist(smean_y(10), col = "grey", xlab = c("sample means of Y (sample size of 10)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0,0.6), xlim= c(rangesampley[1],rangesampley[2]))
curve(dnorm(x, mean= mean(smean_y(10)), sd = sd(smean_y(10))), col = "black", add = TRUE, lwd = 4)

hist(smean_y(50), col = "grey", xlab = c("sample means of Y (sample size of 50)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0,1.3), xlim= c(rangesampley[1],rangesampley[2]))
curve(dnorm(x, mean= mean(smean_y(50)), sd = sd(smean_y(50))), col = "black", add = TRUE, lwd = 4)

hist(smean_y(250), col = "grey", xlab = c("sample means of Y (sample size of 250)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0,2.5), xlim= c(rangesampley[1],rangesampley[2]))
curve(dnorm(x, mean= mean(smean_y(250)), sd = sd(smean_y(250))), col = "black", add = TRUE, lwd = 4)

hist(smean_y(500), col = "grey", xlab = c("sample means of Y (sample size of 500)"), freq= FALSE, main = c("Histogram of sample means"), ylim = c(0, 4), xlim= c(rangesampley[1],rangesampley[2]))
curve(dnorm(x, mean= mean(smean_y(500)), sd = sd(smean_y(500))), col = "black", add = TRUE, lwd = 4)
dev.off()
