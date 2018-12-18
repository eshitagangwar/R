
                               ###################FUNCTION############
odi = read.csv('C:/Users/Administrator/Desktop/datasets/odi-batting.csv')
dim(odi)
is.na(odi$Country)


#missing value of the col
#is.na to count missing values
table(is.na(odi$Country))
table(odi$Country)
#only two values T and F
table(is.na(odi$Runs))
#only missing values
sum(is.na(odi$Runs))
########




#$----> will search for odi$col same name
#it  will return NULL
# we will use the subsetting
for(col in colnames(odi)){
  
  print(c(col,sum(is.na(odi[,col]))))
  
}
#Column wise summation 
View(is.na(odi))
colSums(is.na(odi))
# absolute values arev given
colSums(is.na(odi)/nrow(odi)*100)
class(colSums(is.na(odi)))



###########

#calc_per = function(df){
 # return(colSums(is.na(df)/nrow(odi)*100))
#}
#save the function in any other 
#require() and source()
#require('abc')
source("C:/Users/Administrator/Documents/abc.r")
calc(odi)
class(calc(odi))

summary_categorical_cols(odi)

# create a function df as input, how nay unique value

dim(odi[,c('Country',"Player")])
k = odi[,c('Country',"Player")]
t =n_distinct(k$Country)
t
source("C:/Users/Administrator/Documents/utils.r")
#cal_nlevels_1 = function(df){
 # dc =c()
  #for(col in colnames(df)){
   # t =n_distinct(df[,col])
    #dc = c(dc,t)
    
  #}
  
  
 # return(dc)
#}

data = table(as.vector(odi$Country))
names(data)[data == max(data,na.rm = T)]


cal_max(odi[,c('Country',"Player")])
cal_nlevels_2(odi[,c('Country',"Player")])
cal_n(odi[,c('Country',"Player")])

cal_max = function(odi[,c('Country',"Player")]){
  dc = list()
  for(col in colnames(odi[,c('Country',"Player")])){
    data = table(as.vector(odi[,c('Country',"Player")][,col]))
    t =names(data)[data == max(data,na.rm = T)]
    dc[col] = t
  }
  return(dc)
}

#cal_max_1 = function(df){
#  dc = list()
#  for(col in colnames(df)){
#    data = table(as.vector(df[,col]))
#    t = names(data)[data == max(data)]
#    dc[col] = t
#  }
#  return(dc)
#}

cal_max(odi[,c('Country',"Player")])

library(dplyr)

dp = function(df, groupby_col, summarize_col){
  k = df%>%group_by(groupby_col)%>%summarise(total = sum(summarize_col))
  return(k)
}
dp(odi[,c('Country',"Player","Runs")],'Player', 'Runs')
str(odi)

k = odi%>%group_by(Player)%>%summarise(total = sum_(Runs))

summary_df(odi[,c('Country',"Player","Ground","Versus")])


library(dplyr)

########################
k =sapply(odi, is.factor)# vector
lapply(odi,is.factor)#list
colnames(odi)[k]


sapply(odi$Runs, min)
summary_df_cat(odi)
summary_df_num1(odi)
View(summary_df(odi))
