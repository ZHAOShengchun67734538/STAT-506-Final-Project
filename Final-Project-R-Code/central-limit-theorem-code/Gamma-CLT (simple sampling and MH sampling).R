### Gamma Distribution One Sample t-test###
# Plot the PDF of the gamma
library(ggplot2)
x_lower_g = 0
x_upper_g = 20

ggplot(data.frame(x = c(x_lower_g, x_upper_g)), aes(x = x)) + 
  xlim(c(x_lower_g, x_upper_g)) +
  ylim(0, 0.4) +
  stat_function(fun = dgamma, args = list(shape = 2, rate = 1/2), 
                aes(color = "Shape = 2, Rate = 0.5")) +
  stat_function(fun = dgamma, args = list(shape = 9, rate = 2), 
                aes(color = "Shape = 9, Rate = 2")) +
  stat_function(fun = dgamma, args = list(shape = 0.5, rate = 1), 
                aes(color = "Shape = 0.5, Rate = 1")) +
  labs(
    x = "\n x", 
    y = "f(x) \n", 
    title = "Gamma Distribution", 
    color = "Parameters" 
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )



# Gamma(2, 0.5) function
n_values = c(5,10,50,100,200,500)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = rgamma(n, shape = 2, rate = 0.5)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Gamma(2,0.5) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))


# Gamma(0.5, 1) function
n_values = c(5,10,50,100,200,500)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = rgamma(n, shape = 0.5, rate = 1)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Gamma(0.5,1) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))



# Gamma(9, 2) function
n_values = c(5,10,50,100,200,500)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = rgamma(n, shape = 9, rate = 2)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Gamma(9,2) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))


#################################
### Compare with MH Algorithm ###
#################################

# Define the target (Gamma distribution) and proposal distribution
target1 = function(x)
{
  if (x <= 0) return(0)
  dgamma(x, shape = 2, rate = 0.5)
}

target2 = function(x)
{
  if (x <= 0) return(0)
  dgamma(x, shape = 9, rate = 2)
}

target3 = function(x)
{
  if (x <= 0) return(0)
  dgamma(x, shape = 0.5, rate = 1)
}


# Metropolis-Hastings Sampler
metropolis_hastings = function(n_samples,target) 
{
  B = 100
  size = n_samples+B
  samples = c(1:size)*0
  samples[1] = 0 # Initial value
  for (i in 2:size) 
  {
    current_x = samples[i-1]
    x_star = current_x + rnorm(1,mean=0,sd=1)
    alpha = min(1, target(x_star) / target(current_x))
    if(is.na(alpha))
    {
      samples[i] = current_x
      next
    }
    if(runif(1) < alpha) 
    {
      samples[i] = x_star
    }else{
      
      samples[i] = current_x
    }
  }
  return(samples[(B+1):size])
}


# Target 1 function
n_values = c(5,10,50,100,500,1000)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = metropolis_hastings(n_samples=n, target=target1)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Gamma(2,0.5) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))



# Target 2 function
n_values = c(5,10,50,100,500,1000)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = metropolis_hastings(n_samples=n, target=target2)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Gamma(9,2) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))



# Target 3 function
n_values = c(5,10,50,100,500,1000)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = metropolis_hastings(n_samples=n, target=target3)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Gamma(0.5,1) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))

















































































