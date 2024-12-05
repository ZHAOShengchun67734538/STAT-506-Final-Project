### Multimodal Distribution One Sample t-test###
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
    labels = c("Mode 1: µ=3/2,  σ^2=1/2", "Mode 2: µ=0,  σ^2=19/32", 
               "Mode 3: µ=-19/20,  σ^2=153/400") 
  ) +
  theme_minimal(base_size = 10) + 
  theme(
    panel.background = element_rect(fill = "grey100"),
    panel.grid.major = element_line(color = "grey85"), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"), 
    legend.title = element_blank(), 
    legend.position = "right", 
    legend.text = element_text(size = 12), 
    plot.title = element_text(hjust = 0.5,  size = 14), 
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
metropolis_hastings = function(n_samples,sigma,target) 
{
  B = 1000
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


# Do the MC to estimate the Type I error
times = 100; alpha = 0.05
n1=10; n2=50; n3=100; n4=500
typeI1_n1=c(1:B)*0; typeI2_n1=c(1:B)*0; typeI3_n1=c(1:B)*0;typeI4_n1=c(1:B)*0
typeI1_n2=c(1:B)*0; typeI2_n2=c(1:B)*0; typeI3_n2=c(1:B)*0;typeI4_n2=c(1:B)*0
typeI1_n3=c(1:B)*0; typeI2_n3=c(1:B)*0; typeI3_n3=c(1:B)*0;typeI4_n3=c(1:B)*0
typeI1_n4=c(1:B)*0; typeI2_n4=c(1:B)*0; typeI3_n4=c(1:B)*0;typeI4_n4=c(1:B)*0

# N = n1 = 10
for(j in 1:times)
{
  rej1 = 0; rej2 = 0; rej3 = 0
  for(i in 1:n1)
  {
    # Sample from the population
    s1 = metropolis_hastings(n1,sqrt(0.5),target=target1)
    s2 = metropolis_hastings(n1,sqrt(0.5),target=target2)
    s3 = metropolis_hastings(n1,sqrt(0.5),target=target3)
    
    t1 = (mean(s1)-(3/2))/(sd(s1)/sqrt(n1))
    p1 = 2 * pt(-abs(t1), df=(n1-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2)-(0))/(sd(s2)/sqrt(n1))
    p2 = 2 * pt(-abs(t2), df=(n1-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3)-(-0.95))/(sd(s3)/sqrt(n1))
    p3 = 2 * pt(-abs(t3), df=(n1-1))
    if(p3 < alpha){rej3 = rej3+1}

  }
  
  typeI1_n1[j] = rej1/n1
  typeI2_n1[j] = rej2/n1
  typeI3_n1[j] = rej3/n1
}
# N = n2 = 50
for(j in 1:times)
{
  rej1 = 0; rej2 = 0; rej3 = 0
  for(i in 1:n2)
  {
    # Sample from the population
    s1 = metropolis_hastings(n2, sqrt(0.5),target=target1)
    s2 = metropolis_hastings(n2, sqrt(0.5),target=target2)
    s3 = metropolis_hastings(n2, sqrt(0.5),target=target3)
    
    t1 = (mean(s1)-(3/2))/(sd(s1)/sqrt(n2))
    p1 = 2 * pt(-abs(t1), df=(n2-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2)-(0))/(sd(s2)/sqrt(n2))
    p2 = 2 * pt(-abs(t2), df=(n2-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3)-(-0.95))/(sd(s3)/sqrt(n2))
    p3 = 2 * pt(-abs(t3), df=(n2-1))
    if(p3 < alpha){rej3 = rej3+1}
  }
  
  typeI1_n2[j] = rej1/n2
  typeI2_n2[j] = rej2/n2
  typeI3_n2[j] = rej3/n2
}
# N = n3 = 100
for(j in 1:times)
{
  rej1 = 0; rej2 = 0; rej3 = 0
  for(i in 1:n3)
  {
    # Sample from the population
    s1 = metropolis_hastings(n3, sqrt(0.5),target=target1)
    s2 = metropolis_hastings(n3, sqrt(0.5),target=target2)
    s3 = metropolis_hastings(n3, sqrt(0.5),target=target3)
    
    t1 = (mean(s1)-(3/2))/(sd(s1)/sqrt(n3))
    p1 = 2 * pt(-abs(t1), df=(n3-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2)-(0))/(sd(s2)/sqrt(n3))
    p2 = 2 * pt(-abs(t2), df=(n3-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3)-(-0.95))/(sd(s3)/sqrt(n3))
    p3 = 2 * pt(-abs(t3), df=(n3-1))
    if(p3 < alpha){rej3 = rej3+1}
  }
  
  typeI1_n3[j] = rej1/n3
  typeI2_n3[j] = rej2/n3
  typeI3_n3[j] = rej3/n3
}

# N = n4 = 500
for(j in 1:times)
{
  rej1 = 0; rej2 = 0; rej3 = 0
  for(i in 1:n4)
  {
    # Sample from the population
    s1 = metropolis_hastings(n4, sqrt(0.5),target=target1)
    s2 = metropolis_hastings(n4, sqrt(0.5),target=target2)
    s3 = metropolis_hastings(n4, sqrt(0.5),target=target3)
    
    t1 = (mean(s1)-(3/2))/(sd(s1)/sqrt(n4))
    p1 = 2 * pt(-abs(t1), df=(n4-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2)-(0))/(sd(s2)/sqrt(n4))
    p2 = 2 * pt(-abs(t2), df=(n4-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3)-(-0.95))/(sd(s3)/sqrt(n4))
    p3 = 2 * pt(-abs(t3), df=(n4-1))
    if(p3 < alpha){rej3 = rej3+1}
  }
  
  typeI1_n4[j] = rej1/n4
  typeI2_n4[j] = rej2/n4
  typeI3_n4[j] = rej3/n4
}

# Draw the result plot
library(tidyr)
data = data.frame(
  x_val=c("10", "50", "100", "500"),
  y1=c(mean(typeI1_n1), mean(typeI1_n2), mean(typeI1_n3), mean(typeI1_n4)),
  y2=c(mean(typeI2_n1), mean(typeI2_n2), mean(typeI2_n3), mean(typeI2_n4)),
  y3=c(mean(typeI3_n1), mean(typeI3_n2), mean(typeI3_n3), mean(typeI3_n4))
)
data$x_val = factor(data$x_val, levels = c("10", "50", "100", "500"))
data_long = pivot_longer(data, cols = y1:y3, names_to = "variable", values_to = "y")
data_long = as.data.frame(data_long)

# Create the Power plot
ggplot(data_long, aes(x = x_val, y = y, color = variable, group = variable)) +
  geom_point(size = 3) +                   
  geom_line(size = 1) + 
  scale_color_manual(
    name = "Multi-modal Normal Distribution",                      
    values = c("y1" = "purple", "y2" = "red",
               "y3" = "green"),
    labels = c("model 1", "model 2", "model 3")
  ) +
  labs(x = "X", y = "Type I Error", 
       title = "Type I error of one sample t-test") +
  theme_minimal()



























































































