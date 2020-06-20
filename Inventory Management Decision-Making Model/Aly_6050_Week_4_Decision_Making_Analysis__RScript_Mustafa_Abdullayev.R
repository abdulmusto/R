## Setting seed for reprocdubility
set.seed(123)

## Parameters given in the assignment
unit_cost_amount <- 82
order_cost_amount <- 212
holding_cost_amount <- 0.169


## Triangular distribution parameters
lower <- 13900
peak <- 16000
upper <- 18000


## Function to create custom amount of triangular values
my_sim <- function(num) {
  
  demand_values <- vector()
  
  for (val in seq(1:num)){  
    
    r <- runif(1)
    A <- lower + sqrt((upper-lower)*(peak-lower)*r)
    B <- upper - sqrt((upper-lower)*(upper-peak)*(1-r))
    C <- (peak - lower) / (upper - lower)
    
    demand <- ifelse(r <= C, A, B)
    demand_values <- append(demand_values,demand)

  }
  return(demand_values)
}

## Creating 1000 triangular values
demand_values <-my_sim(1000)

## Distribution of demand values 
hist(demand_values)


## Calcuşatng total cost
total_cost <- function(order_quantity,demand){
  
  order_number <- (demand/order_quantity)

  holding_cost <- (order_quantity/2)*unit_cost_amount*holding_cost_amount
  order_cost <- order_number * order_cost_amount
  total_cost <- holding_cost + order_cost
  
  return(total_cost)
  
}

## Defining custom vectors
quantity_values <- vector()
order_number_values <- vector()
total_cost_values <- vector()


## Optimizing total cost for different demand values
for (i in demand_values){
  test <- optimize(total_cost,i,interval = c(100,1000))
  quantity_values <- append(quantity_values,round(test$minimum))
  order_number_values <- append(order_number_values,round(i/test$minimum))
  total_cost_values <- append(total_cost_values,test$objective)
}


## Histogram of minimum total cost, order quantity and annual number of orders
hist(total_cost_values,col="cornflowerblue", main = "Probability Distribution of Total Cost", xlab = "Total Cost")
hist(quantity_values,col="cornflowerblue",main = "Probability Distribution of Order Amount", xlab = "Order Amount")
hist(order_number_values,col="cornflowerblue",main = "Probability Distribution of Number of Orders", xlab = "Order Count", breaks = 21:24)



