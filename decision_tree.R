entropy <-function(nPositive, nNegative){
  t = nPositive+nNegative;
  -nPositive/t * log2(nPositive/t) -nNegative/t * log2(nNegative/t);
}
