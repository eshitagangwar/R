
for(i in seq(1,10)){
  if(i%%2 ==1){
    cat('odd',i,'\t' )
    #print(i)
  }
  
}

library('dplyr')
odi = read.csv('C:/Users/Administrator/Desktop/datasets/odi-batting.csv')
dim(odi)

odi$Century <- ifelse(odi$Runs > 99, TRUE, FALSE)
odi$Fifties <- ifelse(odi$Runs > 50 || odi$Runs<100, TRUE, FALSE)
playerwise = odi%>%group_by(Player)%>%  
  summarise(total_matches = n(),
            centuries = sum(Runs>99,na.rm = T,T),
            fifty = sum(Runs>50 || Runs<99, na.rm = T,T),
            ducks = sum(Runs==0,na.rm = T,T))
View(playerwise)

performance =  odi%>%filter(Country =='India')%>%group_by(Versus)%>%
  summarise(total_matches = n_distinct(MatchDate),
            centuries = sum(Runs>99,na.rm = T,T),
            Total_Run = sum(Runs, na.rm = T,T))
View(performance)

higest = odi%>%group_by(Player)%>%
  summarise(score = max(Runs))%>%arrange(-score)
View(higest)


########JOINS###########
p = data.frame(id= c(101,102,103,104), name = c("a","b","c","d"))
q = data.frame(id= c(103,104,105,106), age = c(20,23,33,45))
merge(p,q)#inner join
merge(p,q,all.x = T)#left joinmerge(p,q,all.y  = T)#right join
q1 = data.frame(id1 = c(107,103,105,106), n = c(21,23,24,25))
k = merge(p,q1,by.x = "id",by.y = "id1")
merge(q,k)
seq(20,1,by = -0.5)
rep(1,5)
rep(1:5,c(3,4,1,5,2))

#####
p = data.frame(id= c(101,102,103,104), name = c("a","b","c","d"))
q = data.frame(id= c(103,104,105,106), age = c(20,23,33,45))

q1 = data.frame(id = c(107,103,105,106), n = c(21,23,24,25))
merge(p,q,all.x = T)
#######

swa = read.csv('C:/Users/Administrator/Desktop/datasets/Source_wise_Addmission.csv')
swl = read.csv('C:/Users/Administrator/Desktop/datasets/Source_wise_lead.csv')
colnames(swa)
colnames(swl)
View(swa)
#inner join
merge(swa,swl)
#left join
View(merge(swa,swl,all.x =  T))
merge(swl,swa,all.x =  T)
#right join
View(merge(swa,swl,all.y =   T))
merge(swl,swa,all.y =   T)
######
library(readxl)
hs = read_excel("")
a = read_excel('C:/Users/Administrator/Desktop/datasets/City and State.xlsx')
b = read_excel('C:/Users/Administrator/Desktop/datasets/Country Name.xlsx')
c=read_excel('C:/Users/Administrator/Desktop/datasets/Hospital Address.xlsx')
d=read_excel('C:/Users/Administrator/Desktop/datasets/Hospital Name.xlsx')
colnames(a)
colnames(b)
colnames(c)
colnames(d)
m1=merge(d,c,by.x= "ID" , by.y ="Provider ID")
#View(merge(d,c,by.x= "ID" , by.y ="Provider ID"))
View(m1)
colnames(m1)
m11 = merge(d,b , by.x= "ZIP Code" , by.y ="Pin Code",all.x = T)
View(m11)
m3 = merge(m2,a , by.x= "ZIP Code" , by.y ="ZIP Code")
View(m3)
dim(m3)
View(merge(m3,c , by.x ="ID" , by.y ="Provider ID", all.y = T))
dim(a)
View(d)



####LIST#######
vec1 = c(1,2,3)
vec2 = c(TRUE,FALSE,TRUE)
vec3 = c('dog', 'cat','rat')
my_list = list(vec1,vec2,vec3)
my_list[[2]]
my_list[1]
names(my_list) = c("integer","char","animal")
my_list
my_list$integer
my_list[c(1,3)]
my_list[c("integer" , "char")]
unlist(my_list)
my_new_list = unlist(my_list)
b =c("a","b","c")
my_list[[6]] = c("a","b","c")
my_list$money = c(100,200,300)
my_list[[7]] = c(1,2,3)
my_list[9]=c("a","b","c")
my_list

#my_list_1 =
######
na = c('eshita','shivangi', 'steni','shrestha','manpreet','dakshita','raj')
dep = c('cs','ec','phy','cs','mba','ec','mba')
age =c(22,26,24,23,27,24,29)
mob =c(123,234,345,456,567,678,789)
datascience = data.frame(na,dep,age,mob)
datascience
datascience[2]
datascience[1:3,]
age_desc = datascience[order(-age),]
age_desc
datasets::mtcars
s = mtcars
dim(s)
str(mtcars)
colnames(mtcars)
mtcars[order(-mtcars$mpg),]
top_10 = head(mtcars,10)
top_10
datanew = data.frame(mtcars$mpg, mtcars$cyl, mtcars$disp ,mtcars$qsec , mtcars$gear)
View(datanew)


library(hflights)
str(hflights)
dim(hflights)
f = tbl_df(hflights)
View(f)
View(hflights)
f
hflights
k = hflights
hflights%>%filter(DayofMonth == 1, Month ==1)%>%View()
hflights%>%filter(UniqueCarrier =="AA"  || UniqueCarrier =="UA" )%>%View()
hflights%>%select(DepTime, ArrTime)%>%View()
hflights%>%filter(DepDelay>60 | ArrDelay >60)%>%select(UniqueCarrier, DepDelay)%>%View()
hflights %>%   mutate(speed = Distance /AirTime ) %>%  View()
str(hflights)
hflights%>%group_by(Year, Month,DayofMonth)%>%summarise(total = n_distinct(FlightNum))%>%View()
hflights%>%group_by(Dest)%>%summarise(total = n_distinct(FlightNum))%>%View()
