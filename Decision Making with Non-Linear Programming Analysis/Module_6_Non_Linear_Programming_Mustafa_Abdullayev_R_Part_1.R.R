require(lpSolveAPI)
require(lpSolve)


## Set the coefficients of the decision variables -> C

C <- c(23,17,20,
       18,22,
       25,20,
       19,21,17,
       8,12,9,
       10,12,8,
       14,12,15)
  
# Create constraint martix B
A <- matrix(c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,
              1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
              0,2,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,
              0,0,1,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,
              1,0,0,1,0,0,0,1,0,0,-1,-1,-1,0,0,0,0,0,0,
              0,1,0,0,0,1,0,0,1,0,0,0,0,-1,-1,-1,0,0,0,
              0,0,1,0,1,0,1,0,0,1,0,0,0,0,0,0,-1,-1,-1
              ), nrow=14, byrow=TRUE)
  
# Right hand side for the constraints
B <- c(32500, 41200, 18000, 22500,
       22500,35000,39700, 16800, 
       50000, 50000, 50000, 
       0, 0, 0)
  
  
# Set 4 constraints and 4 decision variables
lprec <- make.lp(nrow = 14, ncol = 19)
  
# Set the type of problem we are trying to solve
lp.control(lprec, sense="min")
  
# Set type of decision variables
set.type(lprec, 1:19, type=c("integer"))
  
# Set objective function coefficients vector C
set.objfn(lprec, C)
  
# Add constraints for supply
add.constraint(lprec, A[1, ], "<=", B[1])
add.constraint(lprec, A[2, ], "<=", B[2])
add.constraint(lprec, A[3, ], "<=", B[3])
add.constraint(lprec, A[4, ], "<=", B[4])

# Add constraints for demand
add.constraint(lprec, A[5, ], "=", B[5])
add.constraint(lprec, A[6, ], "=", B[6])
add.constraint(lprec, A[7, ], "=", B[7])
add.constraint(lprec, A[8, ], "=", B[8])

# Add constraints for distribution points
add.constraint(lprec, A[9, ], "<=", B[9])
add.constraint(lprec, A[10, ], "<=", B[10])
add.constraint(lprec, A[11, ], "<=", B[11])

add.constraint(lprec, A[12, ], ">=", B[12])
add.constraint(lprec, A[13, ], ">=", B[13])
add.constraint(lprec, A[14, ], ">=", B[14])
  
# Solve problem
solve(lprec)
  
decision_variables <-(get.variables(lprec))
# Get the decision variables values

#### For the naming, I will use two letters : first one is where 
#### an item is coming crom and second is where it is going. For example, az means
#### it is from supply point A to Distribution point Z. 

names(decision_variables) <- c("ax","ay","az", 
                               "bx", "bz",
                               "cy", "cz",
                               "dx","dy","dz",
                               "xp","xq","xr",
                               "yp","yr","ys",
                               "zq","zr","zs")

print(decision_variables)

print((get.objective(lprec)))
