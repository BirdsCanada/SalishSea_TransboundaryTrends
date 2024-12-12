Xm <- model.matrix(~ -1 + BasisYR1 + BasisYR2+ BasisYR3+ BasisYR4 + BasisDay1 +BasisDay2+ BasisDay3 + DurationInHours,
                   data = dat)
Xm <- as.matrix(Xm)
dim(Xm)

# Repeat the whole thing 1000 times.
NSim <- 1000
SimData <- inla.posterior.sample(n = NSim, result = model)
N  <- nrow(dat)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)

MyParams <- rownames(model$summary.fixed)
MyID <- function(x){ which(rownames(SimData[[1]]$latent) == x) }

RowNum.Betas<-lapply(MyParams, function(x) grep(x, rownames(SimData[[1]]$latent), fixed=TRUE))
RowNum.Betas<-as.numeric(RowNum.Betas)

MyParams2 <- paste("kappa_k:", levels(dat$kappa_k), sep = "")
RowNum.ai<-lapply(MyParams2, function(x) grep(x, rownames(SimData[[1]]$latent), fixed=TRUE))
RowNum.ai<-unlist(RowNum.ai)
RowNum.ai <- unlist(RowNum.ai)

for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[RowNum.Betas]
  ai    <- SimData[[i]]$latent[RowNum.ai]
  FixedPart   <- Xm %*% Betas
  aiPart      <- ai[as.numeric(dat$kappa_k)]
  mu.i[,i]    <- exp(FixedPart + aiPart)
  Ysim[,i]    <- rpois(n = nrow(dat), lambda = mu.i[,i]) ##This needs to be changed to a negative binomial distribution
  
  #The size parameter is the dispersion parameter p<-nrow(model$summary.fixed + 1) # +1 for the idd random effect
}

# We could calculate the number of zeros in each of the 1,000
# data sets.
zeros <- vector(length = NSim)
for(i in 1:NSim){
  zeros[i] <- sum(Ysim[,i] == 0)
}

table(zeros)

# Let's plot this as a table
# Figure 20.24
par(mfrow = c(2,1), mar = c(5,5,2,2), cex.lab = 1.5)

T1 <- gam(E1 ~ s(ObservationCount), 
          data = dat)

plot(T1, ylim = c(-4,5), lwd = 2, cex.lab = 1.5,
     xlab = "Observation Count")
abline(h = 0, lty = 2)
points(x = dat$ObservationCount,
       y = E1, 
       pch = 16,
       cex = 0.5,
       col = grey(0.1))



plot(table(zeros), 
     #axes = FALSE,
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in simulated data sets",
     xlim = c(0, 170),
     main = "Simulation results")
points(x = sum(dat$NCalls == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
# The red dot is the number of zeros in the original data set.
# The data simulated from the Poisson model
# contains to many zeros.
#########################################
