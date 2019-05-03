source('time_expansion.R')
source('min_cost_flow.R')

nodelist <- read.csv('nodelist.csv')
edgelist <- read.csv('edgelist.csv')
timeperiod <- 36

graph.tx <- time_expand(nodelist, edgelist, timeperiod)
nodelist.tx <- graph.tx$nodelist
edgelist.tx <- graph.tx$edgelist

solution <- min_cost_flow(nodelist.tx, edgelist.tx)
ncontents <- sum(nodelist$init.contents) 
avgtime <- solution$objval / ncontents

library(dplyr)
edgelist.tx$flow <- solution$solution
maxtime <- edgelist.tx %>% filter(to == 't*' & flow != 0) %>% 
              summarise(max(from.t)) %>% as.numeric()

library(ggplot2)
summary <- edgelist.tx %>% filter(to == 't*') %>% 
  group_by(from.t) %>% summarise(count=sum(flow))
df <- data.frame(timestep=rep(summary$from.t, summary$count))
ggplot(df, aes(timestep)) + geom_bar()

library(igraph)
graph.tx <- time_expand(nodelist, edgelist, 33)
nodelist.tx <- graph.tx$nodelist
edgelist.tx <- graph.tx$edgelist
g.tx <- graph_from_edgelist(
  as.matrix(edgelist.tx[ ,c('from','to')])
)
E(g.tx)$capacity <- edgelist.tx$capacity
maxflow <- max_flow(g.tx, source=1, target=length(V(g.tx)))
edgelist.tx[maxflow$cut, ] %>% filter(from!='s*' & to!='t*') %>%
  group_by(from.static, to.static) %>% count()

# utilization
edgelist.tx %>% filter(from != 's*' & to != 't*') %>%
  group_by(from.static, to.static) %>% 
  summarise(util=mean(flow / capacity)) %>% arrange(desc(util))
