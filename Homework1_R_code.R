# define a function to calculate the empirical CDF of standard normal distribution
empirical_cdf_sdnormal <- function (n,t)
{
  x <- rnorm(n, mean = 0, sd = 1)
  b = 0
  for (i in 1:n) {
    if ( x[i] <= t) {b=b+1}
  }
  empirical_cdf <- b/n
  
  return(empirical_cdf)
  
}

# test the function above
y <- empirical_cdf_sdnormal(10,0)

# right the nexted loop for testing n={100,1000,10000} and t={}
Phi_hat <- matrix(0,3,900)  # each row represents a n value, each 100 clo represents a t value
bias <- matrix(0,3,900) # bias matrix

n <- c(100,1000,10000)
t <- c(0.0,0.67,0.84,1.28,1.65,2.32,2.58,3.09,3.72)

for (i in 1:length(n)) {
  for (j in 1: length(t)) {
     for (k in (1:100)) {
       Phi_hat[i,100*(j-1)+k] <- empirical_cdf_sdnormal(n[i],t[j])  # 100*(j-1)+k, for the each 100 elements corresponding to the same value of t
       bias[i,100*(j-1)+k] <- Phi_hat[i,100*(j-1)+k]-pnorm(t[j])    # calculate the bias
       
       }
    
  } 
  
}


# look at the outputs Phi_hat[1,1:100]; Phi_hat[2,];Phi[3,]

# reshape the result to do the boxplot
a<-c(1,2,3,4,5,6)
b<-matrix(a,3,2,byrow = FALSE)  # when reshaping a matrix, reading the original matrix by col, no matter you fill the new matrix by row or col
output <- matrix(Phi_hat, 300,9, byrow = FALSE) # reshape estimated matrix such that each col represents a t value and whatever n it is
output_bias <- matrix(bias, 300,9, byrow = FALSE)
colnames(output_bias) <- c("0", "0.67","0.84","1.28","1.65","2.32","2.58","3.09","3.72")
df<-data.frame(output_bias)
ab <-melt(df)
# do the boxplot
boxplot(df)

####### experiment with melt and ggplot

meltData <- melt(df)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

