entropy <- function(nPositive, nNegative) {
  t = nPositive+nNegative;
  -nPositive/t * log2(nPositive/t) -nNegative/t * log2(nNegative/t);
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