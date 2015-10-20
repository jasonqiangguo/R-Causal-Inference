#########################################################
## Weighted survey regression and sensitivity analysis ##
#########################################################


data<- read.dta("mydata.dta")
require(survey)

## create a dataset with abducted == 0 and found == 1
datanonabd <- data[which(data$abd==0 & data$found == 1),] 

## sensitivity analysis of the relation between abd and vote05

data2$abd_1 <- scale(data2$abd) 
data2$vote05_1 <- scale(data2$vote05)
alpha <- rnorm(nrow(data2), 0, 1) ## Generate alphas from a normal distribution
delta <- rnorm(nrow(data2), 0, 1) ## Generate deltas from a normal distribution
X <- "age + age2 + age3 + fthr_ed + mthr_ed + hh_size96 + hh_wealth96"
out<- lm(paste0("vote05_1~abd_1+", X), data2) ## regression without unobserved confounder U

## use the properties of logit transformation to generate the unobserved confounder U
genConfound<- function (x, y) {
  e <- rnorm(nrow(data2), 0, 1)
  w <- x * data2$abd_1 + y * data2$vote05_1 + e
  p <- exp(w)/(1+exp(w))
  U <- rbinom(nrow(data2), 1, p)
  return(U)
}


## generate the estimated TE including the unobserved confounding covariate U
results <- NULL
for (i in seq_len(length(alpha))) {
  U <- genConfound(alpha[i], delta[i])
  corD <- cor(U, data2$abd_1, use="pairwise.complete.obs")
  corY <- cor(U, data2$vote05_1, use="pairwise.complete.obs")
  estTE <- coef(lm(paste0("vote05_1 ~ abd_1 + U +", X), data2))["abd_1"]
  names(estTE) <- NULL
  res <- c(estTE = estTE, corD = corD, corY = corY)
  results <- rbind(results, res)
}
head(results)

pdf('sensitivity1.pdf')
color <- ifelse(results[, "estTE"] <= 0.5 * coef(out)["abd_1"], "red", NA)
color <- ifelse(is.na(color) & results[, "estTE"] >= 1.5 * coef(out)["abd_1"],"blue", color)  
color <- ifelse(is.na(color), "green", color)
pch <- ifelse(results[, "estTE"] <= 0.5 * coef(out)["abd_1"], 2, NA)
pch <- ifelse(is.na(pch) & results[, "estTE"] >= 1.5 * coef(out)["abd_1"],5, pch)  
pch <- ifelse(is.na(pch), 1, pch)

plot(results[, "corD"], results[, "corY"], col = color, pch = pch, xlab = "correlation with D",
     ylab = "correlation with voting", xlim = c(0, 1), ylim = c(0, 1), main = "FIGURE 1. Impact of Relaxing the Assumption of Unconfoundedness")
mtext(side = 2, text = "(Increase in R-squared)", line = 2)
mtext(side = 1, text = "(Increase in R-squared)", line = 4)
legend("topright",legend = c("Set of correlation pairs that does not overturn the result","Set of correlation pairs that overturns the result","Influence of pre-war covariates on voting and abduction"), pch=c(1,2,3), col = c("green","red","black"), cex=0.8)
points_observed <- read.csv("V2V-fig1.csv")
points_observed <- points_observed[1:9,]
points(points_observed$RwP, points_observed$RyP, pch = "+", col = "black", cex = 1)
text(0.18319667, 0.34574641, label = "Year and location", cex=1, pos=4, col="Black", lwd=10)
text(0.15589219, 0.28866525, label = "Year of birth", cex=1, pos=4, col="Black", lwd=10)
text(0.03053662, 0.10892849, label = "Location of birth", cex=1, pos=4, col="Black", lwd=10)
text(0.00255776, 0.07770063, label = "HH Size", cex=1, pos=4, col="Black", lwd=10)
text(0.00981271, 0.01160790, label = "Cattle", cex=1, pos=4, col="Black", lwd=10)
dev.off()


## sensitivity analysis of the relation between abd and comm_mobil 
data2$comm_mobil_1 <- scale(data2$comm_mobil)
alpha <- rnorm(nrow(data2), 0, 1) ## Generate alphas from a normal distribution
delta <- rnorm(nrow(data2), 0, 1) ## Generate deltas from a normal distribution
X <- "age + age2 + age3 + fthr_ed + mthr_ed + hh_size96 + hh_wealth96"
out2<- lm(paste0("data2$comm_mobil_1~abd_1+", X), data2) ## regression without unobserved confounder U

## use the properties of logit transformation to generate the unobserved confounder U
genConfound2<- function (x, y) {
  e <- rnorm(nrow(data2), 0, 1)
  w <- x * data2$abd_1 + y * data2$comm_mobil_1 + e
  p <- exp(w)/(1+exp(w))
  U <- rbinom(nrow(data2), 1, p)
  return(U)
}

## generate the estimated TE including the unobserved confounding covariate U
results1 <- NULL
for (i in seq_len(length(alpha))) {
  U <- genConfound2(alpha[i], delta[i])
  corD <- cor(U, data2$abd_1, use="pairwise.complete.obs")
  corY <- cor(U, data2$comm_mobil_1, use="pairwise.complete.obs")
  estTE <- coef(lm(paste0("comm_mobil_1 ~ abd_1 + U +", X), data2))["abd_1"]
  names(estTE) <- NULL
  res <- c(estTE = estTE, corD = corD, corY = corY)
  results1 <- rbind(results1, res)
}
head(results1)

pdf('sensitivity2.pdf')
color <- ifelse(results1[, "estTE"] <= 0.5 * coef(out2)["abd_1"], "red", NA)
color <- ifelse(is.na(color) & results1[, "estTE"] >= 1.5 * coef(out2)["abd_1"],"blue", color)  ##why?
color <- ifelse(is.na(color), "green", color)
pch <- ifelse(results1[, "estTE"] <= 0.5 * coef(out2)["abd_1"], 2, NA)
pch <- ifelse(is.na(pch) & results1[, "estTE"] >= 1.5 * coef(out2)["abd_1"],5, pch)  
pch <- ifelse(is.na(pch), 1, pch)
plot(results1[, "corD"], results1[, "corY"], col = color, pch = pch, xlab = "correlation with D",
     ylab = "correlation with community mobilizer", xlim = c(0, 1), ylim = c(0, 0.6), main = "FIGURE 2. Impact of Relaxing the Assumption of Unconfoundedness")
mtext(side = 2, text = "(Increase in R-squared)", line = 2)
mtext(side = 1, text = "(Increase in R-squared)", line = 4)
legend("topright",legend = c("Set of correlation pairs that does not overturn the result","Set of correlation pairs that overturns the result","Influence of pre-war covariates on voting and abduction"), pch=c(1,2,3), col = c("green","red","black"), cex=0.8)
points_observed <- read.csv("V2V-fig1.csv")
points_observed <- points_observed[1:9,]
points(points_observed$RwP, points_observed$RyP, pch = "+", col = "black", cex = 1)
text(0.18319667, 0.34574641, label = "Year and location", cex=1, pos=4, col="Black", lwd=10)
text(0.15589219, 0.28866525, label = "Year of birth", cex=1, pos=4, col="Black", lwd=10)
text(0.03053662, 0.10892849, label = "Location of birth", cex=1, pos=4, col="Black", lwd=10)
text(0.00255776, 0.07770063, label = "HH Size", cex=1, pos=4, col="Black", lwd=10)
text(0.00981271, 0.01160790, label = "Cattle", cex=1, pos=4, col="Black", lwd=10)
dev.off()
