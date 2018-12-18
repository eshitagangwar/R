calc = function(df){
  return(colSums(is.na(df)/nrow(odi)*100))
}
cal_nlevels = function(df){
  dc =c()
  temp =list()
  for(col in colnames(df)){
    t =n_distinct(df[,col])
    dc = c(dc,t)
    temp[col] = t
    
  }
  
  return(t)
}

cal_max = function(df){
  dc = list()
  for(col in colnames(df)){
    data = table(as.vector(df[,col]))
    t = names(data)[data == max(data)]
    dc[col] = t
  }
  return(dc)
}

#cal_n = function(df){
 # dc = list()
  #for(col in colnames(df)){
   # level_name = labels(sort(table(df[,col]),decreasing = TRUE)[1])
    #dc[col] = level_name
  #}
  #return(dc)
#}

library(dplyr)
#dp = function(df,col,col1){
 # k = df%>%group_by_(col)%>%summarise_(total = sum(col1))
  #return(k)

#}

#######################
summary_df_cat = function(df){
  fact = colnames(df)[sapply(df, is.factor)]
  factors =df[,fact]
  row3 = calc(factors)
  row2 = cal_nlevels(factors)
  row1 = cal_max (factors)
  result = rbind(most_freq =row1,row2,row3,fact)
  dc = data.frame(result)
  return(dc)
}


#summary_df_num = function(df){
#   str
#   cm=c()
#   ce =c()
#   ce1 =c()
#   cmean =c()
#   ce2 = c()
#   cmax =c()
#   for(col in colnames(df)){
#     
#     mini = min(df[,col],na.rm = T) 
#     cm = c(cm,mini)
#     e25 =  quantile(df[,col],na.rm = T,0.25)
#     ce = c(ce,e25)
#     e50 =  median(df[,col],na.rm = T)
#     ce1 = c(ce1,e50)
#     meann = mean(df[,col],na.rm = T)
#     cmean = c(cmean, meann)
#     e70 = quantile(df[,col],na.rm = T,0.75)
#     ce2 = c(ce2,e70)
#     maxx =  max(df[,col],na.rm=T)
#     cmax = c(cmax,maxx)
#     
#   }
#   k = rbind(cm,ce,ce1,cmean,ce2,cmax)
#    return(k)
#   
#   
# }
############################
summary_df_num = function(df){
  numerics = colnames(df)[sapply(df, is.numeric)]
  minn = sapply(df[,numerics],min,na.rm=T )
   maxx = sapply(df[,numerics],max,na.rm=T )
   quan25 = sapply(df[,numerics], quantile,prob=0.25,na.rm=T)
   quan50 = sapply(df[,numerics], quantile,prob=0.50, na.rm=T)
   quan75 = sapply(df[,numerics], quantile,prob=0.75, na.rm=T)
   me = sapply(df[,numerics], median,na.rm=T )
   skeww = sapply(me>quan50, function(z) if_else(z,"Right","left"))
   k = rbind(numerics,
            minn,
            maxx,
            quan25,
            quan50,
            quan75,
            me,
            skeww)
  return(data.frame(k))
  
  
}
summary_df = function(df){
  x = summary_df_num(df)
  y = summary_df_cat(df)
  result = list(factors = x, numerics = y, factor_col = colnames(x), numerics_cols= colnames(y))
  return(result)
  print(y)
  
}
