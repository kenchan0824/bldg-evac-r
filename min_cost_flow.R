library(lpSolve)

min_cost_flow <- function(nodelist, edgelist) {
  options(warn=-1)
  
  nedge <- nrow(edgelist)
  constraints <- NULL
  I <- diag(nedge)
  # constraints: all flow <= capacity
  # 1 0 0 0   0 0 0 0 <= 10
  # 0 1 0 0   0 0 0 0 <= 40
  constraints$lhs <- I
  constraints$dir <- rep('<=', nedge)
  constraints$rhs <- edgelist$capacity

  # constraints: flow out - flow in = supply - demand
  # 1 1 0 0   0 0 0 0 = 40
  # 0-1-1-1   0 1 1 0 = 0
  for (i in 1:nrow(nodelist)) {
    node.name <- as.character(nodelist[i, 'name'])
    flow_out <- I[which(edgelist$from == node.name), , drop=FALSE]
    flow_in <- I[which(edgelist$to == node.name), , drop=FALSE]
    lhs <- colSums(flow_out) - colSums(flow_in)
    rhs <- nodelist$net.supply[i]
    constraints$lhs <- rbind(constraints$lhs, lhs)
    constraints$dir <- c(constraints$dir, '==')
    constraints$rhs <- c(constraints$rhs, rhs)
  }

  solution <- lp(
    direction = 'min',
    objective.in = edgelist$cost,
    const.mat = constraints$lhs,
    const.dir = constraints$dir,
    const.rhs = constraints$rhs
  )

  options(warn=0)
  return(solution)
}

# # tesing
# # create nodelist
# nodelist <- data.frame(
#   name = c('F1', 'F2', 'F3', 'DC', 'C1', 'C2'),
#   net.supply = c(40, 20, 60, 0, -80, -40)
# )
# 
# # create edgelist
# edgelist <- data.frame(
#   from = c('F1', 'F1', 'F2', 'F3', 'F3', 'DC', 'DC', 'C2'), 
#   to = c('C1', 'DC', 'DC', 'DC', 'C2', 'C1', 'C2', 'C1' ),
#   capacity = c(10, 40, 30, 50, 15, 60, 120, 100),
#   cost = c(150, 100, 120, 200, 360, 80, 300, 250)
# )
# 
# solution <- min_cost_flow(nodelist, edgelist)
# solution
# solution$solution
