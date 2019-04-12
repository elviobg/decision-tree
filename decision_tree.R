entropy <- function(nPositive, nNegative) {
  t = nPositive+nNegative
  e = -nPositive/t * log2(nPositive/t) -nNegative/t * log2(nNegative/t)
  if (is.nan(e))
    return(0)
  return(e)
}

gain <- function(entropy, data, labels) {
  
  c <- data.frame(value=data, label=labels)
  s <- length(labels)
  e <- c()
  g <- entropy
  
  for (level in levels(data)) {
    t <- which(c$value == level & c$label== 'TRUE')
    f <- which(c$value == level & c$label== 'FALSE')
    g = g - entropy(length(f), length(t)) * (length(f) + length(t)) / s
  }
  return(g)
}

define_node <- function(data, labels) {
  
  t <- length(which(labels== 'TRUE'))
  f <- length(which(labels== 'FALSE'))
  eS = entropy(f, t)
  cat("entropia base: ", eS, "\n")
  g <- c()
  
  for (col in data) {
    cg <- gain(eS, col, labels)
    g <- c(g, cg)
  }
  
  print(g)
  print(max(g))
  print(which.max(g))
  print(colnames(data[which.max(g)]))
  return(which.max(g))
}

get_data <- function() {
  #0.94
  tempo = c('e','e','n','c','c','c','n','e','e','c','e','n','n','c');
  #0.247
  temperatura = c('q','q','q','m','f','f','f','m','f','m','m','m','q','m');
  #0.029
  umidade = c('a','a','a','a','n','n','n','a','n','n','n','a','n','a');
  #0.985
  vento = c('-','+','-','-','-','+','+','-','-','-','+','+','-','+');
  #0.048
  data.frame(tempo, temperatura, umidade, vento)
}

get_label <- function() {
  c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE);
}