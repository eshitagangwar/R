#x = 1
#y = 30
#class(x)
#
name = 'abc'
class(name)

score = 100.54
class(score)

is_null = TRUE
class(is_null)

is_nul = F
class(is_nul)

# vector, same type , if mix type it will convert into one of them 
#ages = c(50,10,'a')
# ("50","10","a") converted into the char
ages = c(55,23,50,10)
class(ages)

# identify the total number of the elements
length(ages)
#Element the vector, vector is the homogenous in nature
#if element is not present  it will not throw error it will show not available ages[10]
#acces the last element if dont know what is the size is, ages[length(ages)]
ages[1]
ages[length(ages)]
ages[10]
# concatination
ages = c(ages,'b')
ages = c("50","10","a")
ages = c(ages,'b')
ages
#subset of the vector
ages[1:3]
#Access one element at at a time
cities = c('banglore','chennai','mumbai','dehi')
#no. will be printed increment one by one
class(seq(1,4))
seq(1,length(cities))
#for loop , used when the two vectors are used. assuming that both have the same size
for( i in seq(length(cities))){
  print(i)
}
# escape to come out in between the incomplete code, esc in the console not in the notepad
for( i in seq(length(cities))){
  print(cities[i])
}
# for loop without the position , it is good for single vector 
for(i in cities){
  print (i)
}

# acess vector from two vectors
population = c(100,200,400,300)
for( i in seq(length(cities)+1)){
  print(cities[i])
  print(population[i])
}
# wap to print the cities whose population is more than 200 if so print the name of the city
for( i in seq(length(cities))){
  if(population[i]>200){
    print(cities[i])
  }
}

sum=0
#wap a program to cal the sum of population
for( i in seq(length(cities))){
  sum = sum + population[i]
}
sum
#wap to find the distinct city in the cities
# get the unique element in the vector and compare two vector use as x %in% collection
cities = c('banglore','mumbai','banglore','chennai','mumbai')
un = c()
for( i in cities){
  if(!(i %in% un)){
    un = c(un,i)
  }
}

un
#wap to find no. of time banglore apperaring
ck = 0
cities = c('banglore','mumbai','banglore','chennai','mumbai')
n = "banglore" 
for( i in cities){
  if((i == n)){
    ck=ck+1
  }
}

for( i in seq(length(cities))){
  if((n == cities[i])){
    ck=ck+1
  }
}
ck

#List
#vector homogenous but list can have  hetrogenous
emp =list(name='eshita', age = 20)
class (emp)
emp$name
#label are case sensitive. $ to access the attribute
# new detail in the list
emp['city'] = 'chennai' 
emp



#Read CSV files. Delimiter
#Comma seperate files, flat files  os indepentent
#TSV Tab seperte file.
# rows - observattion
odi = read.csv('C:/Users/Administrator/Desktop/datasets/odi-batting.csv')
class(odi)#
#dimension of the csv file
dim(odi)
#number of  rows columnn
length(odi)
#no. of rows
nrow(odi)
#preview of the csvfile
View(odi)
# structure of the csv file
str(odi)


                                           #DATA VARIABLE
#QUALITATIVE                                                         QUANTITATIVE(Number)
#Factor                                                         Int              Float
#Categorised
#Dimension
#Group                                                               

# Get column names
colnames(odi)

#Acess the column using the head() by default six values are printed

head(odi$Ground)
odi$Player[1]
#first value of first row and column
odi$Player[1][1]
#acess all the column of the csv file of on e row
odi[1,]
# value of ground column
odi[1:5,'Ground']
#value of first two column and rows
odi[1:5,c('Player','Runs')]


#  Wap Get unique countries
#odi$Country will be treated as vector 

un = c()
for( i in odi$Country){
  if(!(i %in% un)){
    un = c(un,i)
  }
}

un
# WAP to get No. of times India is get repeated
ck = 0

n = "India" 
for( i in odi$Country){
  if((i == n)){
    ck=ck+1
  }
}

ck
#Get total "Runs" scored by "Sachin R tendulkar"
# We  will use the position method 
ck = 0

n = "Sachin R Tendulkar" 
for( i in seq(1,nrow(odi))){
  if(( odi$Player[i] == n)){
    ck=ck+odi$Runs[i]
  }
}

ck
#
ck = 0

n = "Sachin R Tendulkar" 
for( i in seq(1,nrow(odi))){
  k=odi$Player[i]
  if(k == n & odi$Runs[i]>99){
    ck=ck+1
    }
      
}

ck
#missed centuries
ck = 0

n = "Sachin R Tendulkar" 
for( i in seq(1,nrow(odi))){
  k=odi$Player[i]
  if(k == n & odi$Runs[i]>90 & odi$Runs[i]<100){
    ck=ck+1
  }
  
}

ck
# mean of the sachin runs
ck = 0
j=0
n = "Sachin R Tendulkar" 
for( i in seq(1,nrow(odi))){
  if(( odi$Player[i] == n)){
    j=j+1
    ck=ck+odi$Runs[i]
  }
}

total=0
total = ck/j
total
# how to add the 
players = list(sachin = 0, dhoni = 0)
runs = 100
#
players['sachin']= players['sachin'][[1]]+runs
players
class(labels(players))#vector

#adding to the list
players = list()
for( i in seq(1,nrow(odi))){
  pn =as.character(odi$Player[i])#conversion as the character
  pr=odi$Runs[i]
  if(pn %in% labels(players)){
    players[pn] = players[pn][[1]]+ pr
    
  }
  else{
    players[pn]=pr
  }
    
}
players    
class(players)
#odi = read.csv('C:/Users/Administrator/Desktop/datasets/odi-batting.csv')
hr = read.csv('C:/Users/Administrator/Desktop/datasets/HR Analytics.csv')
  
#1 identify total no. of rows and col
#2 under job role unique elements
#3identify % of females and males
#4 calculate avg age of male and female employees 
#5calculate maximum age of male and female employee

nrow(hr)
length(hr)

uhr = c()
for( i in seq(1,nrow(hr))){
  u =as.character(hr$JobRole[i])#conversion as the character
  
  if(u %in% uhr){
    #players[pn] = players[pn][[1]]+ pr
    
  }
  else{
    uhr =c(uhr,u)
  }
  
}
uhr 
str(hr)

f=0
m=0
for( i in seq(1,nrow(hr))){
  u =as.character(hr$Gender[i])#conversion as the character
  
  if(u =='Female'){
    #players[pn] = players[pn][[1]]+ pr
    f=f+1
  }
  else{
    m=m+1
  }
  
}

m/nrow(hr)
fnrow(hr)
nrow(hr)

ma=0
fa=0
f=0
m=0
for( i in seq(1,nrow(hr))){
  u =as.character(hr$Gender[i])#conversion as the character
  
  if(u =='Female'){
    fa = fa+ hr$Age[i]
    f=f+1
  }
  else{
    ma = ma+ hr$Age[i]
    m=m+1
  }
  
}
ma/m
fa/f


f=0
m=0
for( i in seq(1,nrow(hr))){
  u =as.character(hr$Gender[i])#conversion as the character
  
  if(u =='Female'){
    if(f<hr$Age[i]){
      f=hr$Age[i]
    }
    
    
  }
  else{
    if(m<hr$Age[i]){
      m=hr$Age[i]
    }
  }
  
}
f
m





library(dplyr)
View(hr)
