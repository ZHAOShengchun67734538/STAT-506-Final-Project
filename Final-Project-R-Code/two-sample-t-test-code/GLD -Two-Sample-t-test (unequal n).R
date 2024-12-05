### Generalized Lambda Distribution
library(ggplot2)
# Define the percentile function (quantile function) R(p)
R_p = function(p, lambda1, lambda2, lambda3, lambda4) 
{
  if (lambda2 == 0) stop("lambda2 must be non-zero")
  result = lambda1+(p^lambda3-(1-p)^lambda4)/lambda2
  return(result)
}

# Define the PDF function f(R(p))
f_R_p = function(p, lambda1, lambda2, lambda3, lambda4) 
{
  if (lambda2 == 0) stop("lambda2 must be non-zero")
  result = lambda2*(lambda3*p^(lambda3-1)+lambda4*(1-p)^(lambda4-1))^(-1)
  return(result)
}

lambda1 = 0.166;lambda2 = 0.5901;lambda3 = 1.7680;lambda4 = 1.1773  
p_vals = runif(10000) 
R = R_p(p_vals, lambda1, lambda2, lambda3, lambda4)
density = f_R_p(p_vals, lambda1, lambda2, lambda3, lambda4)
data = data.frame(R = R, Density = density)
# Draw the density plot
ggplot(data, aes(x = R, y = Density)) +
  geom_line(color = "red", size = 1) +
  labs(title = " pdf of gld with λ1 = 0.166, λ2 = 0.5901, λ3 = 1.7680, λ4 = 1.1773",
       x = "R(p)", y = "Density") +
  theme_minimal()


lambda1 = 0;lambda2 = 1;lambda3 = 2;lambda4 = 3  
p_vals = runif(10000)
R = R_p(p_vals, lambda1, lambda2, lambda3, lambda4)
density = f_R_p(p_vals, lambda1, lambda2, lambda3, lambda4)
data = data.frame(R = R, Density = density)
# Draw the density plot
ggplot(data, aes(x = R, y = Density)) +
  geom_line(color = "orange", size = 1) +
  labs(title = "pdf of gld with λ1 = 0, λ2 = 1, λ3 =2, λ4 = 3",
       x = "R(p)", y = "Density") +
  theme_minimal()



### Generalized Lambda Sampling ###
lambda1 = 0.166;lambda2 = 0.5901;lambda3 = 1.7680;lambda4 = 1.1773      
p_vals = runif(10000)
gld_samples = R_p(p_vals, lambda1, lambda2, lambda3, lambda4)

# Do the MC to estimate the Type I error
B = 100; alpha = 0.05
n1=10; n2=50; n3=100; n4=500
typeI1_n1=c(1:B)*0; typeI2_n1=c(1:B)*0; 
typeI1_n2=c(1:B)*0; typeI2_n2=c(1:B)*0; 
typeI1_n3=c(1:B)*0; typeI2_n3=c(1:B)*0; 
typeI1_n4=c(1:B)*0; typeI2_n4=c(1:B)*0; 

# N = n1 = 10
for(j in 1:B)
{
  rej1 = 0; rej2 = 0
  for(i in 1:n1)
  {
    p11 = runif(n1)
    s11 = gld_samples = R_p(p11, 0.166, 0.5901 , 1.7680, 1.1773)
    p12 = runif(n1/2)
    s12 = gld_samples = R_p(p12, 0.166, 0.5901 , 1.7680, 1.1773)
    
    p21 = runif(n1)
    s21 = gld_samples = R_p(p21, 0, 1, 2, 3)
    p22 = runif(n1/2)
    s22 = gld_samples = R_p(p22, 0, 1, 2, 3)
    
    
    t1 = t.test(x=s11,y=s12,var.equal = TRUE)
    if(t1$p.value < alpha){rej1 = rej1+1}
    
    t2 = t.test(x=s21,y=s22,var.equal = TRUE)
    if(t2$p.value < alpha){rej2 = rej2+1}
    
  }
  
  typeI1_n1[j] = rej1/n1
  typeI2_n1[j] = rej2/n1
  
}

# N = n2 = 50
for(j in 1:B)
{
  rej1 = 0; rej2 = 0
  for(i in 1:n2)
  {
    p11 = runif(n2)
    s11 = gld_samples = R_p(p11, 0.166, 0.5901 , 1.7680, 1.1773)
    p12 = runif(n2/2)
    s12 = gld_samples = R_p(p12, 0.166, 0.5901 , 1.7680, 1.1773)
    
    p21 = runif(n2)
    s21 = gld_samples = R_p(p21, 0, 1, 2, 3)
    p22 = runif(n2/2)
    s22 = gld_samples = R_p(p22, 0, 1, 2, 3)
    
    
    t1 = t.test(x=s11,y=s12,var.equal = TRUE)
    if(t1$p.value < alpha){rej1 = rej1+1}
    
    t2 = t.test(x=s21,y=s22,var.equal = TRUE)
    if(t2$p.value < alpha){rej2 = rej2+1}
    
  }
  
  typeI1_n2[j] = rej1/n2
  typeI2_n2[j] = rej2/n2
  
}

# N = n3 = 100
for(j in 1:B)
{
  rej1 = 0; rej2 = 0
  for(i in 1:n3)
  {
    p11 = runif(n3)
    s11 = gld_samples = R_p(p11, 0.166, 0.5901 , 1.7680, 1.1773)
    p12 = runif(n3/2)
    s12 = gld_samples = R_p(p12, 0.166, 0.5901 , 1.7680, 1.1773)
    
    p21 = runif(n3)
    s21 = gld_samples = R_p(p21, 0, 1, 2, 3)
    p22 = runif(n3/2)
    s22 = gld_samples = R_p(p22, 0, 1, 2, 3)
    
    
    t1 = t.test(x=s11,y=s12,var.equal = TRUE)
    if(t1$p.value < alpha){rej1 = rej1+1}
    
    t2 = t.test(x=s21,y=s22,var.equal = TRUE)
    if(t2$p.value < alpha){rej2 = rej2+1}
    
  }
  
  typeI1_n3[j] = rej1/n3
  typeI2_n3[j] = rej2/n3
  
}


# N = n4 = 500
for(j in 1:B)
{
  rej1 = 0; rej2 = 0
  for(i in 1:n4)
  {
    p11 = runif(n4)
    s11 = gld_samples = R_p(p11, 0.166, 0.5901, 1.7680, 1.1773)
    p12 = runif(n4/2)
    s12 = gld_samples = R_p(p12, 0.166, 0.5901, 1.7680, 1.1773)
    
    p21 = runif(n4)
    s21 = gld_samples = R_p(p21, 0, 1, 2, 3)
    p22 = runif(n4/2)
    s22 = gld_samples = R_p(p22, 0, 1, 2, 3)
    
    
    t1 = t.test(x=s11,y=s12,var.equal = TRUE)
    if(t1$p.value < alpha){rej1 = rej1+1}
    
    t2 = t.test(x=s21,y=s22,var.equal = TRUE)
    if(t2$p.value < alpha){rej2 = rej2+1}
    
  }
  
  typeI1_n4[j] = rej1/n4
  typeI2_n4[j] = rej2/n4
  
}



# Draw the result plot
library(tidyr)
data = data.frame(
  x_val = c("10","50","100","500"),
  y1=c(mean(typeI1_n1), mean(typeI1_n2), mean(typeI1_n3), mean(typeI1_n4)),
  y2=c(mean(typeI2_n1), mean(typeI2_n2), mean(typeI2_n3), mean(typeI2_n4))
)
data$x_val = factor(data$x_val, levels = c("10", "50", "100", "500"))
data_long = pivot_longer(data, cols = y1:y2, names_to = "variable", values_to = "y")
data_long = as.data.frame(data_long)
# Create the plot
ggplot(data_long, aes(x = x_val, y = y, color = variable, group = variable)) +
  geom_point(size = 3) +                   
  geom_line(size = 1) +                     
  scale_color_manual(
    name = "gld(λ1, λ2, λ3, λ4)",                      
    values = c("y1" = "orange", "y2" = "red"),
    labels = c("gld(0.166, 0.5901, 1.7680, 1.1773)", 
               "gld(0, 1, 2, 3)")
  ) +
  labs(x = "X", y = "Type I Error", 
       title = "Type I error of two sample t-test (n1=n, n2=n1/2)") +
  theme_minimal()

























































































