library(igraph)

# make graph object and plot
g <- graph_from_edgelist(
  as.matrix(edgelist[,c('from','to')])
)

E(g)$capacity <- edgelist$capacity
E(g)$time <- edgelist$time

plot(g, edge.label = E(g)$capacity)
plot(g, edge.label = E(g)$time)

# make graph object and plot
g.tx <- graph_from_edgelist(
  as.matrix(edgelist.tx[ ,c('from','to')])
)
E(g.tx)$capacity <- edgelist.tx$capacity
tkplot(g.tx, vertex.color="white", edge.label=E(g.tx)$capacity)

E(g.tx)$cost <- edgelist.tx$cost
tkplot(g.tx, vertex.color="white", edge.label=E(g.tx)$cost)

E(g.tx)$flow <- solution$solution
tkplot(g.tx, vertex.color="white", edge.label=E(g.tx)$flow)

maxflow <- max_flow(g.tx, source=1, target=8)
E(g.tx)[maxflow$cut]
