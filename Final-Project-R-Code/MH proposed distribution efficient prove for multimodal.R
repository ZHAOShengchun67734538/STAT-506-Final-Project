# Define the target density: Mixture of Gaussian
# Target Function
target1 = function(x)
{
  value=(0.5/sqrt(2*pi))*exp(-(x^2)/2)+(0.5/sqrt(2*pi))*exp(-((x-3)^2)/2)
  return(value)
}
target2 = function(x)
{
  value=(0.75/sqrt(2*pi))*exp(-((x+1)^2)/2)+(0.25/sqrt(pi))*exp(-(x-3)^2)
  return(value)
}
target3 = function(x)
{
  value=(0.5/sqrt(1.5*pi))*exp(-(x^2)/1.5)+(0.15/sqrt(pi))*exp(-(x-3)^2)+
    (0.35/sqrt(3*pi))*exp(-((x+4)^2)/3)
  return(value)
}

# Define the proposal function (mixture of local and global proposals)
propose = function(x, sigma) 
{
  if (runif(1) < 0.8) 
  {
    # Local proposal: Gaussian
    rnorm(1, mean = x, sd = sigma)
  } else {
    # Global proposal: Uniform
    runif(1, min = -10, max = 10)
  }
}

# Metropolis-Hastings Sampler
metropolis_hastings = function(n_samples, sigma, target_density) 
{
  B = n_samples/2
  samples = c(1:n_samples)*0
  samples[1] = 0 # Initial value
  for (i in 2:n_samples) 
  {
    current_x = samples[i-1]
    x_star = propose(current_x, sigma)
    alpha = min(1, target_density(x_star) / target_density(current_x))
    if (runif(1) < alpha) 
    {
      samples[i] = x_star
    }else{
      
      samples[i] = current_x
    }
  }
  return(samples[(B+1):n_samples])
}

samples = metropolis_hastings(10000, sqrt(0.5),target_density = target3)

# Plot the results
library(ggplot2)
library(dplyr)

# Create a histogram of the samples
sample_df = data.frame(samples = samples)
x_vals = seq(-10, 10, length.out = 5000)
target_vals = sapply(x_vals, target3)

ggplot(sample_df, aes(x = samples)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "lightblue", alpha = 0.7) +
  geom_line(aes(x = x_vals, y = target_vals), color = "red", size = 1) +
  labs(
    title = "Metropolis-Hastings Sampling from Mixture of Gaussians Model 3",
    x = "x",
    y = "Density"
  ) +
  theme_minimal()































































