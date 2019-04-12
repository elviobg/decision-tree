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

define_node <- function(eS, data, labels) {
  #cat("entropia base: ", eS, "\n")
  g <- c()
  
  for (col in data) {
    cg <- gain(eS, col, labels)
    g <- c(g, cg)
  }
  return(which.max(g))
}

id3 <- function(data, labels, print_level) {
  t <- length(which(labels== 'TRUE'))
  f <- length(which(labels== 'FALSE'))
  eS = entropy(f, t)
  for(i in 0:print_level){cat('  ')}
  if(eS > 0){
    root <- define_node(eS, data, labels)
    cat(colnames(data[root]),'\n')
    print_level = print_level + 1
    for (level in levels(data[,root])) {
      l <- which(data[,root] == level)
      data2 <- subset(data, data[,root] == level)
      label2 <- labels[l]
      
      for(i in 0:print_level){cat('  ')}
      cat(level, '\n')
      
      id3(data2, label2, print_level+1)
    }
  }else{
    cat(labels[1], "\n")
  }
}

get_data <- function() {
  tempo = c('ensolarado','ensolarado','nublado','chuva','chuva','chuva','nublado','ensolarado','ensolarado','chuva','ensolarado','nublado','nublado','chuva');
  temperatura = c('quente','quente','quente','moderado','frio','frio','frio','moderado','frio','moderado','moderado','moderado','quente','moderado')
  umidade = c('alta','alta','alta','alta','normal','normal','normal','alta','normal','normal','normal','alta','normal','alta')
  vento = c('fraco','forte','fraco','fraco','fraco','forte','forte','fraco','fraco','fraco','forte','forte','fraco','forte')
  data.frame(tempo, temperatura, umidade, vento)
}

get_labels <- function() {
  c('FALSE', 'FALSE', 'TRUE', 'TRUE', 'TRUE', 'FALSE', 'TRUE', 'FALSE', 'TRUE', 'TRUE', 'TRUE', 'TRUE', 'TRUE', 'FALSE')
}