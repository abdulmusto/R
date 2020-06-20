require(lpSolveAPI)
require(lpSolve)


## Set the coefficients of the decision variables -> C

my_lp_solver <- function(cost){
  
  C <- c(119.99, 359.99, 224.99,712.95)
  
  # Create constraint martix B
  A <- matrix(c(320, 360, 415,637,
                3.2, 8,3.2,0.8,
                3, 8, 0,0,
                0, 0, 1,-10), nrow=4, byrow=TRUE)
  
  # Right hand side for the constraints
  B <- c(cost, 2688, 725.76, 0)
  
  
  # Set 4 constraints and 4 decision variables
  lprec <- make.lp(nrow = 4, ncol = 4)
  
  # Set the type of problem we are trying to solve
  lp.control(lprec, sense="max")
  
  # Set type of decision variables
  set.type(lprec, 1:4, type=c("integer"))
  
  # Set objective function coefficients vector C
  set.objfn(lprec, C)
  
  # Add constraints
  add.constraint(lprec, A[1, ], "<=", B[1])
  add.constraint(lprec, A[2, ], "<=", B[2])
  add.constraint(lprec, A[3, ], ">=", B[3])
  add.constraint(lprec, A[4, ], ">=", B[4])
  
  # Solve problem
  solve(lprec)
  
  print(get.variables(lprec))
  # Get the decision variables values
  return(get.objective(lprec))
  
}

my_lp_solver(176000)


cost_values <- vector()
profit_values <- vector()

for (i in seq(from =176000, to=1000000, by = 20000)) {
  
  cost_values <- append(cost_values, i)
  profit_values <- append(profit_values,my_lp_solver(i))
  
}


plot.default(x = cost_values/1000, y = profit_values)

############ Warehouse area ########

used_area <- 0*5*3 + 268*5*8 + 166*5*3 + 4*5*3
used_area

excess_area <- 84*32*5 - used_area
excess_area
