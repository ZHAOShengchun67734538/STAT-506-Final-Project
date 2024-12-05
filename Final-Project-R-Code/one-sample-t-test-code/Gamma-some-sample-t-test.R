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
  stat_function(fun = dgamma, args = list(shape = 7.5, rate = 1), 
                aes(color = "Shape = 7.5, Rate = 1")) +
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



# Do the MC to estimate the Type I error
B = 100; alpha = 0.05
n1=10; n2=50; n3=100; n4=500
typeI1_n1=c(1:B)*0; typeI2_n1=c(1:B)*0; typeI3_n1=c(1:B)*0;typeI4_n1=c(1:B)*0
typeI1_n2=c(1:B)*0; typeI2_n2=c(1:B)*0; typeI3_n2=c(1:B)*0;typeI4_n2=c(1:B)*0
typeI1_n3=c(1:B)*0; typeI2_n3=c(1:B)*0; typeI3_n3=c(1:B)*0;typeI4_n3=c(1:B)*0
typeI1_n4=c(1:B)*0; typeI2_n4=c(1:B)*0; typeI3_n4=c(1:B)*0;typeI4_n4=c(1:B)*0

# N = n1 = 10
for(j in 1:B)
{
  rej1 = 0; rej2 = 0; rej3 = 0; rej4 = 0
  for(i in 1:n1)
  {
    s1gamma = rgamma(n=n1,shape=2,rate=1/2)
    s2gamma = rgamma(n=n1,shape=9,rate=2)
    s3gamma = rgamma(n=n1,shape=7.5,rate=1)
    s4gamma = rgamma(n=n1,shape=0.5,rate=1)
    t1 = (mean(s1gamma)-(4))/(sd(s1gamma)/sqrt(n1))
    p1 = 2 * pt(-abs(t1), df=(n1-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2gamma)-(4.5))/(sd(s2gamma)/sqrt(n1))
    p2 = 2 * pt(-abs(t2), df=(n1-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3gamma)-(7.5))/(sd(s3gamma)/sqrt(n1))
    p3 = 2 * pt(-abs(t3), df=(n1-1))
    if(p3 < alpha){rej3 = rej3+1}
  
    t4 = (mean(s4gamma)-(0.5))/(sd(s4gamma)/sqrt(n1))
    p4 = 2 * pt(-abs(t4), df=(n1-1))
    if(p4 < alpha){rej4 = rej4+1}
  }

  typeI1_n1[j] = rej1/n1
  typeI2_n1[j] = rej2/n1
  typeI3_n1[j] = rej3/n1
  typeI4_n1[j] = rej4/n1
}
# N = n2 = 50
for(j in 1:B)
{
  rej1 = 0; rej2 = 0; rej3 = 0; rej4 = 0
  for(i in 1:n2)
  {
    s1gamma = rgamma(n=n2,shape=2,rate=1/2)
    s2gamma = rgamma(n=n2,shape=9,rate=2)
    s3gamma = rgamma(n=n2,shape=7.5,rate=1)
    s4gamma = rgamma(n=n2,shape=0.5,rate=1)
    t1 = (mean(s1gamma)-(4))/(sd(s1gamma)/sqrt(n2))
    p1 = 2 * pt(-abs(t1), df=(n2-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2gamma)-(4.5))/(sd(s2gamma)/sqrt(n2))
    p2 = 2 * pt(-abs(t2), df=(n2-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3gamma)-(7.5))/(sd(s3gamma)/sqrt(n2))
    p3 = 2 * pt(-abs(t3), df=(n2-1))
    if(p3 < alpha){rej3 = rej3+1}
    
    t4 = (mean(s4gamma)-(0.5))/(sd(s4gamma)/sqrt(n2))
    p4 = 2 * pt(-abs(t4), df=(n2-1))
    if(p4 < alpha){rej4 = rej4+1}
  }
  
  typeI1_n2[j] = rej1/n2
  typeI2_n2[j] = rej2/n2
  typeI3_n2[j] = rej3/n2
  typeI4_n2[j] = rej4/n2
}
# N = n3 = 100
for(j in 1:B)
{
  rej1 = 0; rej2 = 0; rej3 = 0; rej4 = 0
  for(i in 1:n3)
  {
    s1gamma = rgamma(n=n3,shape=2,rate=1/2)
    s2gamma = rgamma(n=n3,shape=9,rate=2)
    s3gamma = rgamma(n=n3,shape=7.5,rate=1)
    s4gamma = rgamma(n=n3,shape=0.5,rate=1)
    t1 = (mean(s1gamma)-(4))/(sd(s1gamma)/sqrt(n3))
    p1 = 2 * pt(-abs(t1), df=(n3-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2gamma)-(4.5))/(sd(s2gamma)/sqrt(n3))
    p2 = 2 * pt(-abs(t2), df=(n3-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3gamma)-(7.5))/(sd(s3gamma)/sqrt(n3))
    p3 = 2 * pt(-abs(t3), df=(n3-1))
    if(p3 < alpha){rej3 = rej3+1}
    
    t4 = (mean(s4gamma)-(0.5))/(sd(s4gamma)/sqrt(n3))
    p4 = 2 * pt(-abs(t4), df=(n3-1))
    if(p4 < alpha){rej4 = rej4+1}
  }
  
  typeI1_n3[j] = rej1/n3
  typeI2_n3[j] = rej2/n3
  typeI3_n3[j] = rej3/n3
  typeI4_n3[j] = rej4/n3
}

# N = n4 = 500
for(j in 1:B)
{
  rej1 = 0; rej2 = 0; rej3 = 0; rej4 = 0
  for(i in 1:n4)
  {
    s1gamma = rgamma(n=n4,shape=2,rate=1/2)
    s2gamma = rgamma(n=n4,shape=9,rate=2)
    s3gamma = rgamma(n=n4,shape=7.5,rate=1)
    s4gamma = rgamma(n=n4,shape=0.5,rate=1)
    t1 = (mean(s1gamma)-(4))/(sd(s1gamma)/sqrt(n4))
    p1 = 2 * pt(-abs(t1), df=(n4-1))
    if(p1 < alpha){rej1 = rej1+1}
    
    t2 = (mean(s2gamma)-(4.5))/(sd(s2gamma)/sqrt(n4))
    p2 = 2 * pt(-abs(t2), df=(n4-1))
    if(p2 < alpha){rej2 = rej2+1}
    
    t3 = (mean(s3gamma)-(7.5))/(sd(s3gamma)/sqrt(n4))
    p3 = 2 * pt(-abs(t3), df=(n4-1))
    if(p3 < alpha){rej3 = rej3+1}
    
    t4 = (mean(s4gamma)-(0.5))/(sd(s4gamma)/sqrt(n4))
    p4 = 2 * pt(-abs(t4), df=(n4-1))
    if(p4 < alpha){rej4 = rej4+1}
  }
  
  typeI1_n4[j] = rej1/n4
  typeI2_n4[j] = rej2/n4
  typeI3_n4[j] = rej3/n4
  typeI4_n4[j] = rej4/n4
}


# Draw the result plot
library(tidyr)
data = data.frame(
  x_val = c("10","50","100","500"),
  y1=c(mean(typeI1_n1), mean(typeI1_n2), mean(typeI1_n3), mean(typeI1_n4)),
  y2=c(mean(typeI2_n1), mean(typeI2_n2), mean(typeI2_n3), mean(typeI2_n4)),
  y3=c(mean(typeI3_n1), mean(typeI3_n2), mean(typeI3_n3), mean(typeI3_n4)),
  y4=c(mean(typeI4_n1), mean(typeI4_n2), mean(typeI4_n3), mean(typeI4_n4))
)
data$x_val = factor(data$x_val, levels = c("10", "50", "100", "500"))
data_long = pivot_longer(data, cols = y1:y4, names_to = "variable", values_to = "y")
data_long = as.data.frame(data_long)
# Create the plot
ggplot(data_long, aes(x = x_val, y = y, color = variable, group = variable)) +
  geom_point(size = 3) +                   
  geom_line(size = 1) +                     
  scale_color_manual(
    name = "Gamma(shape, rate)",                      
    values = c("y1" = "yellow", "y2" = "red", 
               "y3" = "green", "y4" = "blue"), # Custom colors
    labels = c("gamma(2, 0.5)", "gamma(9, 2)", 
               "gamma(7.5, 1)", "gamma(0.5, 1)")
  ) +
  labs(x = "X", y = "Type I Error", 
       title = "Type I error of one sample t-test") +
  theme_minimal()





































































































































































































































