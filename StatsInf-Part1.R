library(ggplot2)

lambda     <- 0.2
n          <- 40
sim        <- 1000 
set.seed(1234)

exp_data   <- replicate(sim, mean(rexp(n, lambda)))

th_mean <- 1/lambda
th_var  <- 1/(lambda^2*n)
th_sd   <- (1/lambda)/sqrt(n)

sim_mean   <- mean(exp_data)
sim_var    <- var(exp_data)
sim_median <- median(exp_data) 
sim_sd     <- sd(exp_data)

sim_mean
th_mean

sim_var 
th_var 

hist   <- ggplot(data.frame(exp_data), aes(x = exp_data)) 
hist   <- hist + geom_histogram(aes(y = ..density..), colour = "black", fill = "green", alpha = .3,binwidth=.2)
hist   <- hist + stat_function(fun = "dnorm", args = list(mean = tmean, sd = th_sd))
hist   <- hist + geom_vline(xintercept=th_mean,size=1,colour="red")
hist   <- hist + xlab("Mean")+ylab("Density")
hist   <- hist + scale_x_continuous(breaks = c(1:10))
hist

sim_mean + (c(1,-1)*1.96 * (ssd/sqrt(n)))

