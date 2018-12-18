calc = function(df){
  return(colSums(is.na(df)/nrow(odi)*100))
}