require(EnvStats)

set.seed(123)

#################### Problem 1a ####################

lower <- 20 #(a)
peak <- 85 #(c)
upper <- 350 #(b)

exp <- (lower+upper+peak)/3
exp_ssd <-sqrt((lower^2+upper^2+peak^2-lower*upper-lower*peak-upper*peak)/18) 

exp_hosp <- c(exp*0.2,exp*0.3,exp*0.15,exp*0.2,exp*0.15)
names(exp_hosp) <- c("beth","tufts","mass","boston","bw")

hosp <- c(0.2,0.3,0.15,0.2,0.15)
names(hosp) <- c("beth","tufts","mass","boston","bw")

my_sim <- function(num,i) {
  set.seed(12)
  obs_hos <- vector()
  x_values <- vector()
  for (val in seq(1:num)){  
    
    r <- runif(1)
    A <- lower + sqrt((upper-lower)*(peak-lower)*r)
    B <- upper - sqrt((upper-lower)*(upper-peak)*(1-r))
    C <- (peak - lower) / (upper - lower)
    
    x <- ifelse(r <= C, A, B)
    x_values <- append(x_values,x)

    temp <- x * hosp[[i]]
               
    obs_hos <- append(obs_hos,temp)
  }

  return(list(mean =mean(obs_hos),std = sd(x_values)))

}

to_simulate <- c(50,100,500,1000,5000)

find_sim <- function(i){
  
  obs_sim <- vector()
  obs_std <- vector()
  for (num in to_simulate){
    obs_sim <- append(obs_sim,my_sim(num,i)$mean)
    obs_std <- append(obs_std,my_sim(num,i)$std)
  }
  
  
  plot(x = to_simulate,y=obs_sim,ylim = c(min(obs_sim-5),max(obs_sim)+5),main=paste("Simulation Result for ",names(hosp)[i]),xlab="Simulation size",ylab = "Sampling Mean")
  abline(h =exp_hosp[[i]] ,col = "red")
  
  plot(x = to_simulate,y=obs_std)
  abline(h =exp_ssd ,col = "red")
  
  print("Expected vs Observed foe means")
  print(exp*hosp[[i]])
  print(mean(obs_sim))
  
  print("Expected vs observed for variance")
  print(exp_ssd)
  print((obs_std))

}

find_sim(1)
find_sim(2)
find_sim(3)
find_sim(4)
find_sim(5)

############################## Problem 1b ######################

hosp_time <- c(15,8,25,10,12)
names(hosp_time) <- c("beth","tufts","mass","boston","bw")


my_sim_time <- function(num,i){
 
  obs_time <- vector()
  std_time_values <- vector()
  
  for (val in seq(1:num)){  
    
    r <- runif(1)
    A <- lower + sqrt((upper-lower)*(peak-lower)*r)
    B <- upper - sqrt((upper-lower)*(upper-peak)*(1-r))
    C <- (peak - lower) / (upper - lower)
    
    x <- ifelse(r <= C, A, B)
    
    temp <- x * hosp[[i]]
    
    obs_time <- append(obs_time,sum(rexp(temp,rate=1/hosp_time[[i]])))
    std_time_values <- append(std_time_values,sd(rexp(temp,rate=1/hosp_time[[i]])))
    
 
}
  
  return(list(mean=mean(obs_time),std=mean(std_time_values)))
  
}


find_sim_time <- function(i){
  
  obs_sim_time <- vector()
  obs_std_time <- vector()
  
  for (num in to_simulate){
    obs_sim_time <- append(obs_sim_time,my_sim_time(num,i)$mean)
    obs_std_time <- append(obs_std_time,my_sim_time(num,i)$std)
  }
  
  
  plot(x = to_simulate,y=obs_sim_time/60,ylim = c(min(obs_sim_time/60-1),max(obs_sim_time/60)+2),main=paste("Simulation Result for ",names(hosp)[i]),xlab="Simulation size",ylab = "Sampling Mean")
  abline(h =exp_hosp[[i]]*hosp_time[[i]]/60 ,col = "red")
  
  #plot(x = to_simulate,y=obs_std)
  #abline(h =exp_ssd ,col = "red")
  
  
  print("Expected vs sampling for means")
  print(exp*hosp[[i]]*hosp_time[[i]]/60)
  print(mean(obs_sim_time)/60)
  
  
  print("Expected vs observed for variance")
  print(hosp_time[[i]])
  print(mean(obs_std_time))
  
  
}
(find_sim_time(1))
(find_sim_time(2))
(find_sim_time(3))
(find_sim_time(4))
(find_sim_time(5))

#################### problem 1c ####################

find_sim(1)



#################### problem 1d ####################

my_sim_confidence <- function(num){
    
    obs_conf <- vector()
    std_conf <- vector()
    temp_values <- vector()
    
    for (val in seq(1:num)){  
      
      r <- runif(1)
      A <- lower + sqrt((upper-lower)*(peak-lower)*r)
      B <- upper - sqrt((upper-lower)*(upper-peak)*(1-r))
      C <- (peak - lower) / (upper - lower)
      
      x <- ifelse(r <= C, A, B)
      
      temp <- x * hosp[[1]]
      temp_values <- append(temp_values,temp)
      
      obs_conf <- append(obs_conf,sum(rexp(temp,rate=1/hosp_time[[1]])))

      #std_time_values <- sd(rexp(temp,rate=1/hosp_time[[i]]))
      
      #total_time <- mean(obs_time)
    }
    obs_conf <- obs_conf/60
    sdd_conf <-sd(obs_conf)
    sam_mean <- mean(obs_conf)
    hist(obs_conf)
    print(sam_mean)
    
    CI <- c(sam_mean-1.96*sdd_conf/sqrt(mean(temp_values)),sam_mean+1.96*sdd_conf/sqrt(mean(temp_values)))
    print(CI)
    #print(mean(std_time_values))
    return((obs_conf))
    
  }


for (val in to_simulate){
  print("Sampling means and CI - actual expected values is equal to 7.55 hours")
  print(my_sim_confidence(val))
}

values_1 <- my_sim_confidence(5000)
values_1 <- (values_1 - mean(values_1))/sd(values_1)

hist(values_1)

value_d <- vector()
r <- runif(length(values_1))
A <- lower + sqrt((upper-lower)*(peak-lower)*r)
B <- upper - sqrt((upper-lower)*(upper-peak)*(1-r))
C <- (peak - lower) / (upper - lower)

x <- ifelse(r <= C, A, B)
value_d <- append(value_d,x)

value_d <- (value_d - mean(value_d))/sd(value_d)

qqPlot(values_1,value_d,add.line = TRUE)


my_test_1 <- chisq.test(values_1,value_d)
my_test_1$p.value
#################### problem 1e ####################

my_sim_total <- function(num){
  
  obs_t <- vector()
  std_t <- vector()
  x_values <- vector()
  for (val in seq(1:num)){  
    
    r <- runif(1)
    A <- lower + sqrt((upper-lower)*(peak-lower)*r)
    B <- upper - sqrt((upper-lower)*(upper-peak)*(1-r))
    C <- (peak - lower) / (upper - lower)
    
    x <- ifelse(r <= C, A, B)
    x_values <- append(x_values,x)
    
    avg_t <- (sum(rexp(x*0.2,rate = 1/15)) +sum(rexp(x*0.3,rate = 1/8))+sum(rexp(x*0.15,rate = 1/25))+sum(rexp(x*0.2,rate = 1/10))+sum(rexp(x*0.15,rate = 1/12)))/(x-1)
    
    obs_t <- append(obs_t,avg_t)
    
    #std_time_values <- sd(rexp(temp,rate=1/hosp_time[[i]]))
    
    #total_time <- mean(obs_time)
  }
  print(mean(obs_t))
  hist(obs_t)

  CI <- c(mean(obs_t)-1.96*sd(obs_t)/sqrt(mean(x_values)),mean(obs_t)+1.96*sd(obs_t)/sqrt(mean(x_values)))
  print(CI)
  #print(mean(std_time_values))
  return(obs_t)
  
}

for (val in to_simulate){
  print("Expected time is 12.95 minutes")
  print(my_sim_total(val))
}

values <- my_sim_total(5000)
values <- (values-mean(values))/sd(values)


qqPlot(values,rnorm(length(values)),add.line = TRUE)

my_test_2 <- chisq.test(values,rnorm(length(values)))
my_test_2$p.value


######################################################################
######################################################################
######################################################################

set.seed(123)

#################### Problem 2a ####################

avg = 175
std = 63


my_sim_2 <- function(num,i) {
  set.seed(12)
  obs_hos <- vector()
  x_values <- vector()
  for (val in seq(1:num)){  
    
    x <- rnorm(1,mean=avg,sd = std)
    x_values <- append(x_values,x)
    
    temp <- x * hosp[[i]]
    
    obs_hos <- append(obs_hos,temp)
  }
  
  return(list(mean =mean(obs_hos),std = sd(x_values)))
  
}

to_simulate_2 <- c(5000)

find_sim_2 <- function(i){
  
  obs_sim <- vector()
  obs_std <- vector()
  for (num in to_simulate_2){
    obs_sim <- append(obs_sim,my_sim_2(num,i)$mean)
    obs_std <- append(obs_std,my_sim_2(num,i)$std)
  }
  
  
  #plot(x = to_simulate,y=obs_sim,ylim = c(min(obs_sim-5),max(obs_sim)+5),main=paste("Simulation Result for ",names(hosp)[i]),xlab="Simulation size",ylab = "Sampling Mean")
  #abline(h =hosp[[i]]*175 ,col = "red")
  
  #plot(x = to_simulate,y=obs_std)
  #abline(h =63 ,col = "red")
  print("Mean comparision")
  print(mean(obs_sim))
  print(175*hosp[[i]])
  print("Standard deviations")
  print((obs_std))
  print(63)
  
  
}

find_sim_2(1)
find_sim_2(2)
find_sim_2(3)
find_sim_2(4)
find_sim_2(5)


############################## Problem 2b ######################
hosp_std <- c(3,2,6,3.5,2.5)
names(exp_hosp) <- c("beth","tufts","mass","boston","bw")



my_sim_time_2 <- function(num,i){
  
  obs_time <- vector()
  std_time_values <- vector()
  
  for (val in seq(1:num)){  
    
    x <- rnorm(1,mean=avg,sd = std)
    
    temp <- abs(x) * hosp[[i]]
    
    obs_time <- append(obs_time,sum(rnorm(temp,hosp_time[[i]],hosp_std[[i]])))
    std_time_values <- append(std_time_values,sd(rnorm(temp,hosp_time[[i]],hosp_std[[i]]))*temp)
    
    
  }
  total_time <- mean(obs_time,na.rm=TRUE)/60
  
  print("Expected vs Observed means")
  print(175*hosp[[i]]*hosp_time[[i]]/60)
  print(total_time)
  
  print("Expected vs observed standard deviations")
  print(175*hosp[[i]]*hosp_std[[i]])
  print(mean(std_time_values,na.rm = TRUE))
  
  
}


my_sim_time_2(5000,1)
my_sim_time_2(5000,2)
my_sim_time_2(5000,3)
my_sim_time_2(5000,4)
my_sim_time_2(5000,5)



#################### problem 1c ####################

find_sim_2(1)


#################### problem 1d ####################

my_sim_confidence_2 <- function(num){
  
  obs_conf <- vector()
  std_conf <- vector()
  temp_values <- vector()
  
  for (val in seq(1:num)){  
    
    
    x <- rnorm(1,avg,std)
    
    temp <- abs(x) * hosp[[1]]
    temp_values <- append(temp_values,temp)
    
    obs_conf <- append(obs_conf,sum(rnorm(temp,15,3)))
    std_conf <- append(std_conf,temp*3/60)
    #std_time_values <- sd(rexp(temp,rate=1/hosp_time[[i]]))
    
    #total_time <- mean(obs_time)
  }
  obs_conf <- obs_conf/60
  sam_mean <- mean(obs_conf)
  hist(obs_conf)
  print(sam_mean)
  
  CI <- c(sam_mean-1.96*mean(std_conf)/sqrt(mean(temp_values)),sam_mean+1.96*mean(std_conf)/sqrt(mean(temp_values)))
  print(CI)
  #print(mean(std_time_values))
  return((obs_conf))
  
}


my_sim_confidence_2(5000)

values <- my_sim_confidence_2(5000)
values <- (values-mean(values))/sd(values)
val <-hist(values)
d <- density(values)
plot(d)

qqPlot(values,rnorm(length(values)),add.line = TRUE)



#prob <- c(pnorm(-6),pnorm(-5)-pnorm(-6),pnorm(-4)-pnorm(-5),pnorm(-3)-pnorm(-4),pnorm(-2)-pnorm(-3),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),pnorm(3)-pnorm(2),pnorm(4)-pnorm(3),pnorm(5)-pnorm(4),pnorm(6)-pnorm(5),1-pnorm(6))
#prob2 <- c(pt(-5,1),pt(-4,1)-pt(-5,1),pt(-3,1)-pt(-4,1),pt(-2,1)-pt(-3,1),pt(-1,1)-pt(-2,1),pt(0,1)-pt(-1,1),pt(1,1)-pt(0,1),pt(2,1)-pt(1,1),pt(3,1)-pt(2,1),pt(4,1)-pt(3,1),pt(5,1)-pt(4,1),pt(6,1)-pt(5,1),1-pt(6,1))

my_test_3 <- chisq.test(values,rnorm(length(values)))
my_test_3$p.value

#################### problem 1e ####################

my_sim_total_2 <- function(num){
  
  obs_t <- vector()
  std_t <- vector()
  x_values <- vector()
  for (val in seq(1:num)){  
    
    x <- rnorm(1,avg,std)
    
    x_values <- append(x_values,x)
    if (x>0){
      avg_t <- (sum(rnorm(x*0.2,15,3)) +sum(rnorm(x*0.3,8,2))+sum(rnorm(x*0.15,25,6))+sum(rnorm(x*0.2,10,3.5))+sum(rnorm(x*0.15,12,2.5)))/x
      if (avg_t>12 && avg_t<14){
        obs_t <- append(obs_t,avg_t)
      }
    }
    #std_time_values <- sd(rexp(temp,rate=1/hosp_time[[i]]))
    
    #total_time <- mean(obs_time)
  }
  print(mean(obs_t))
  hist((obs_t))
  
  CI <- c(mean(obs_t)-1.96*sd(obs_t)/sqrt(mean(x_values)),mean(obs_t)+1.96*sd(obs_t)/sqrt(mean(x_values)))
  print(CI)
  #print(mean(std_time_values))
  return(((obs_t)))
  
}

for_test <- my_sim_total_2(5000)
for_test<- (for_test-mean(for_test))/sd(for_test)

qqPlot(for_test,rnorm(length(for_test)),add.line = TRUE)

k1 <-hist(for_test,breaks=c(-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5))

#prob <- c(pnorm(-2.5),pnorm(-2)-pnorm(-2.5),pnorm(-1.5)-pnorm(-2),pnorm(-1)-pnorm(-1.5),pnorm(-0.5)-pnorm(-1),pnorm(0)-pnorm(-0.5),pnorm(0.5)-pnorm(0),pnorm(1)-pnorm(0.5),pnorm(1.5)-pnorm(1),pnorm(2)-pnorm(1.5),pnorm(2.5)-pnorm(2),pnorm(3)-pnorm(2.5),pnorm(3.5)-pnorm(3),1-pnorm(3.5))


my_test_4 <- chisq.test(x=for_test,y=rnorm(length(for_test)))
my_test_4$p.value


