### multimodal distribution ###
library(ggplot2)
x = seq(-6,6,by=0.01)
y1 = (0.5/sqrt(2*pi))*exp(-(x^2)/2)+(0.5/sqrt(2*pi))*exp(-((x-3)^2)/2)
y2 = (0.75/sqrt(2*pi))*exp(-((x+1)^2)/2)+(0.25/sqrt(pi))*exp(-(x-3)^2)
y3 = (0.5/sqrt(1.5*pi))*exp(-(x^2)/1.5)+(0.15/sqrt(pi))*exp(-(x-3)^2)+
     (0.35/sqrt(3*pi))*exp(-((x+4)^2)/3)
# Create data frames for each density
df1 = data.frame(x = x, y = y1, Distribution = "model 1")
df2 = data.frame(x = x, y = y2, Distribution = "model 2")
df3 = data.frame(x = x, y = y3, Distribution = "model 3")

# Combine the data frames
df = rbind(df1, df2, df3)

ggplot(df, aes(x = x, y = y, color = Distribution)) +
  geom_line(size = 0.7) + # Increase line thickness for better visibility
  labs(
    title = "Multimodal Normal Distributions",
    x = "X",
    y = "Density"
  ) +
  scale_color_manual(
    values = c("blue", "red", "brown"),
    labels = c("Mode 1: µ=0, σ=1", "Mode 2: µ=2, σ=0.5", 
               "Mode 3: µ=-1, σ=1.5") 
  ) +
  theme_minimal(base_size = 10) + 
  theme(
    panel.background = element_rect(fill = "grey93"),
    panel.grid.major = element_line(color = "grey100"), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"), 
    legend.title = element_blank(), 
    legend.position = "right", 
    legend.text = element_text(size = 12), 
    plot.title = element_text(hjust = 0.5,  size = 16), 
    axis.title = element_text(size = 12 ), 
    axis.text = element_text(size = 12) 
  )


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
metropolis_hastings = function(n_samples,sigma=sqrt(0.5),target) 
{
  B = 100
  size = n_samples+B
  samples = c(1:size)*0
  samples[1] = 0 # Initial value
  for (i in 2:size) 
  {
    current_x = samples[i-1]
    x_star = propose(current_x, sigma)
    alpha = min(1, target(x_star) / target(current_x))
    if (runif(1) < alpha) 
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
  qqnorm(sample_means, main = paste("model 1 sample size n = ", n),
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
  qqnorm(sample_means, main = paste("model 2 sample size n = ", n),
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
  qqnorm(sample_means, main = paste("model 3 sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))






































































































































