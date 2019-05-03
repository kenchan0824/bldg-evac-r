library(dplyr)

# time expansion
build_edges <- function(from.name, from.t, to.name, to.t, capacity) {
  if (length(from.name) == 0) return(NULL)
  edges <- NULL
  edges$from <- sprintf("%s.%d", from.name, from.t)
  edges$from.static <- from.name
  edges$from.t <- from.t
  edges$to <- sprintf("%s.%d", to.name, to.t)
  edges$to.static <- to.name
  edges$to.t <- to.t
  edges$capacity = capacity
  return (data.frame(edges))
}

time_expand <- function(nodelist, edgelist, timeperiod, maxflow=999) {
  options(warn=-1)
  edgelist.tx <- NULL

  # build edges s* --- sources.0
  sources <- nodelist %>% filter(init.contents > 0) 
  edgelist.tx <- build_edges('s*', NA, sources$name, 0, sources$init.contents)
  edgelist.tx$from <- 's*'

  for (t in seq(0, timeperiod-1)) {
    from <- edgelist.tx %>% filter(to.t == t) %>% 
                    select(from=to, from.static=to.static) %>% unique()
    
    # build edges from.t --- from.t+1
    nonsink <- from %>% inner_join(nodelist %>% filter(!destination), 
                                by=c('from.static'='name')) 
    temp <- build_edges(
      nonsink$from.static, t, nonsink$from.static, t+1, nonsink$capacity)
    edgelist.tx <- rbind(edgelist.tx, temp) 

    # build edges from.t --- to.t+time
    edges <- from %>% inner_join( 
                edgelist %>% rename(from.static=from, to.static=to) %>%
                mutate(to.t=t+time) %>% filter(to.t <= timeperiod), 
              by='from.static') 
    
    temp <- build_edges(
        edges$from.static, t, edges$to.static, edges$to.t, edges$capacity)
    edgelist.tx <- rbind(edgelist.tx, temp) 
  }
  
  # build edges sinks.t --- t*
  from <- edgelist.tx %>% inner_join(nodelist %>% filter(destination),
            by=c('to.static'='name')) %>%
            select(from.static=to.static, from.t=to.t) %>% unique()
  temp <- build_edges(from$from.static, from$from.t, 't*', NA, maxflow)
  if (!is.null(temp)) { temp$to <- 't*' }
  edgelist.tx <- rbind(edgelist.tx, temp) 
  
  # remove redundant edges
  for (t in seq(timeperiod, 1, -1)) {
    edgelist.tx <- edgelist.tx %>% filter(to %in% from | to == 't*')
  }
  
  # add supply and cost
  nodelist.tx <- edgelist.tx %>% 
                    select(name=from, static=from.static, timestep=from.t) %>% 
                    unique()
  supply.total = sum(nodelist$init.contents)                     
  nodelist.tx <- nodelist.tx %>% mutate(
                    net.supply = ifelse(name == 's*', supply.total, 0)) 
  nodelist.tx <- rbind(nodelist.tx, data.frame(
                    name = 't*', static = 't*', timestep = NA, 
                    net.supply = -supply.total)) 
  edgelist.tx <- edgelist.tx %>% mutate(cost = ifelse(to=='t*', from.t, 0)) 
  
  options(warn=0)
  return(list(nodelist=nodelist.tx, edgelist=edgelist.tx))
}

# # tesing
# nodelist <- data.frame(
#   name = c('1', '2', '3', '4'),
#   init.contents = c(3, 4, 3, 0),
#   capacity = c(5, 8, 20, 999),
#   destination = c(F, F, F, T)
# )
# nodelist
# 
# edgelist <- data.frame(
#   from = c('1', '1', '2', '2', '3'),
#   to = c('2', '3', '3', '4', '4'),
#   capacity = c(2, 2, 2, 3, 2),
#   time = c(1, 1, 1, 2, 1)
# )
# edgelist
# 
# graph.tx <- time_expand(nodelist, edgelist, 4)
# nodelist.tx <- graph.tx$nodelist
# edgelist.tx <- graph.tx$edgelist
# edgelist.tx
