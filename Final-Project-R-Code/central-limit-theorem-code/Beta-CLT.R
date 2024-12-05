### Beta Distribution CLT ###
# Plot the PDF of the beta
library(ggplot2)
x_lower = 0
x_upper = 1

ggplot(data.frame(x = c(x_lower, x_upper)), aes(x = x)) + 
  xlim(c(x_lower, x_upper)) +
  stat_function(fun = dbeta, args = list(shape1 = 0.5, shape2 = 0.5), 
                aes(color = "Shape1 = 0.5, Shape2 = 0.5")) +
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 1), 
                aes(color = "Shape1 = 5, Shape2 = 1")) +
  stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 3), 
                aes(color = "Shape1 = 1, Shape2 = 3")) +
  labs(
    x = "\n x", 
    y = "f(x) \n", 
    title = "Beta Distribution", 
    color = "Parameters"  # Title of the legend
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )


# Beta(0.5, 0.5) function
n_values = c(5,10,25,50,75,100)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = rbeta(n, shape1 = 0.5, shape2 = 0.5)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Beta(0.5,0.5) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))



# Beta(1, 3) function
n_values = c(5,10,25,50,75,100)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = rbeta(n, shape1 = 1, shape2 = 3)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Beta(1,3) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))




# Beta(5, 1) function
n_values = c(5,10,25,50,75,100)
par(mfrow = c(2, 3))
for(n in n_values)
{
  iteration = 1000
  x_samples = matrix(0, nrow = iteration, ncol = n)
  for (i in 1:iteration)
  {
    x_samples[i,] = rbeta(n, shape1 = 5, shape2 = 1)
  }
  sample_means = rowMeans(x_samples)
  qqnorm(sample_means, main = paste("Beta(5,1) sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))




























































































