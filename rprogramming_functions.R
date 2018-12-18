
path = '/Users/skathirmani/Documents/datasets/odi-batting.csv'
odi = read.csv(path)

parliament = read.csv('/Users/skathirmani/Documents/datasets/parliament.csv')

# Percentage of missing values
View(is.na(odi$Country))

table(is.na(odi$Runs))
table(odi$Country)

sum(is.na(odi$Runs))

for (col in colnames(odi)){
  print(c(col, sum(is.na(odi[,col]))))
}

View(is.na(odi))
colSums(is.na(odi))
View(odi[1:10,c('Runs','ScoreRate')])

colSums(is.na(odi)) / nrow(odi) * 100
colSums(is.na(parliament)) / nrow(parliament) * 100


source("/Users/skathirmani/Documents/rsnippets/utils.R")

calc_perc_na(odi)
calc_perc_na(parliament)
library(dplyr)
n_distinct(odi)

calc_nlevels = function(df){
  result = list()
  for(col in colnames(df)){
    curr_nlevels = n_distinct(df[,col])
    result[col] = curr_nlevels
  }
  return (result)
}

# -------------------------------------------
most_freq_level = function(df){
  # your code goes here
  result = list()
  for (col in colnames(df)){
    level_name = labels(sort(table(df[,col]),
                             decreasing = TRUE)[1])
    result[col] = level_name
  }
  return (result)
}

summary_df_factors = function(df){
  factors = colnames(df)[sapply(df, is.factor)]
  df_subset = df[, factors]
  row_3 = calc_perc_na(df_subset)
  row_2 = calc_nlevels(df_subset)
  row_1 = most_freq_level(df_subset)
  result = rbind(most_freq=row_1,
                 unique_levels=row_2,
                 perc_na=row_3)
  return (data.frame(result))
}


summary_df_numerics = function(df){
  numerics = colnames(df)[sapply(df, is.numeric)]
  df_subset = df[,numerics]
  col_mins = sapply(df_subset, min, na.rm=T)
  col_maxs = sapply(df_subset, max, na.rm=T)
  col_means = sapply(df_subset, mean, na.rm=T)
  col_medians = sapply(df_subset, median, na.rm=T)
  which_skewness = col_means > col_medians
  col_skewness = sapply(which_skewness,
                        function(x) if_else(x, 'Right Skewed',
                                            'Left Skewed'))
  quantile_25 = sapply(df_subset, 
         function(x) quantile(x, 0.25, na.rm=T))
  
  result = rbind(min=col_mins,
                 max=col_maxs,
                 quantile_25=quantile_25,
                 distribution=col_skewness)
  return(data.frame(result))
}

summary_df = function(df){
  x = summary_df_factors(df)
  y = summary_df_numerics(df)
  result = list(factors=x,
                numerics=y,
                factors_cols=colnames(x),
                numerics_cols=colnames(y))
  return(result)
}

s = summary_df(odi)
View(s$numerics)

