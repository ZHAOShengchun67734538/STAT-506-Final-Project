### Example 1:Beta-Binomial simulation ###
n_values = c(5,10,50,100,500,1000)
par(mfrow = c(2, 3))
for(n in n_values)
{
  a=1; b=3;
  iteration =2000
  th=rep(NA,iteration)
  x_samples = matrix(0, nrow = iteration, ncol = n)
  # set initial value
  th[1]=0.5
  x_samples[1,] = rbinom(n,size=16,prob=th[1])
  # Perform Gibbs iterations
  for (i in 2:iteration)
  {
    x_samples[i,] =rbinom(n,size=16,prob=th[i-1])
    th[i] = rbeta(1,a+sum(x_samples[i,]),b+16*n-sum(x_samples[i,]))
  }
  sample_means = rowMeans(x_samples)
  # Burn in
  B = iteration/2
  sample_means = sample_means[(B+1):iteration]
  qqnorm(sample_means, main = paste("Beta-Binom sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))







### Normal-Gamma Example ###
n_values = c(5,10,50,100,500,1000)
par(mfrow = c(2, 3))
for(n in n_values)
{
  mu_0 = 0;tau2 = 10
  alpha = 2;beta = 1
  n_iter = 2000  # Number of iterations
  
  # Initialize values
  y = rnorm(n, mean = 5, sd = 2)  # Initial guess for y
  mu = 5
  sigma2 = 4
  
  # Store samples
  y_samples = matrix(0, nrow = n_iter, ncol = n)  # For sampled y
  mu_samples = numeric(n_iter)
  sigma2_samples = numeric(n_iter)
  
  y_samples[1, ] = y
  mu_samples[1] = mu
  sigma2_samples[1] = sigma2
  
  # Gibbs sampling loop
  for (i in 2:n_iter) 
  {
    # Update mu from conditional posterior
    mu_var = 1 / (n / sigma2_samples[i-1] + 1/tau2)
    mu_mean = mu_var*(sum(y_samples[(i-1),])/sigma2_samples[i-1] + mu_0 / tau2)
    mu = rnorm(1, mean = mu_mean, sd = sqrt(mu_var))
    
    # Update sigma2 from conditional posterior
    alpha_post = alpha + n / 2
    beta_post = beta + sum((y_samples[(i-1),] - mu)^2) / 2
    sigma2 = 1 / rgamma(1, shape = alpha_post, rate = beta_post)
    
    # Update y from conditional posterior
    y = rnorm(n, mean = mu, sd = sqrt(sigma2))
    
    # Store samples
    y_samples[i, ] = y
    mu_samples[i] = mu
    sigma2_samples[i] = sigma2
  }
  sample_means = rowMeans(y_samples)
  # Burn in
  B = n_iter/2
  sample_means = sample_means[(B+1):n_iter]
  qqnorm(sample_means, main = paste("Nom-IG sample size n = ", n),
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         col = "blue", pch = 20)
  qqline(sample_means, col = "red", lwd = 2)
  
}
par(mfrow = c(1, 1))





























































