#read csv files
hr = read.csv('C:/Users/Administrator/Desktop/datasets/HR Analytics.csv')
odi = read.csv('C:/Users/Administrator/Desktop/datasets/odi-batting.csv')
# Filter data
#Filter for Sachin R Tendulkar
#Library should always be loaded for all the function
library(dplyr)
#
odi%>%dplyr::filter(Player) # :: if overlapping of the function are used. filter(column name)
# column name are case sensitive . column name are written wiothout the ''
#if we use this method we need not bring the 
#single column single value filtering 
sachin = odi%>%dplyr::filter(Player=='Sachin R Tendulkar')# it will be dataframe that will be returned
nrow(sachin)
class(sachin)
#multiple value in the single coulmn .we need to create the vector of same type
indian_player= c('Sachin R Tendulkar','Ashish Nehra','Virender Sehwag')
#pass data from one function to another function
sachin_no.=odi%>%dplyr::filter(Player=='Sachin R Tendulkar')%>%head(5)
View(sachin_no.)
indian_playerall=odi%>%dplyr::filter(Player %in% indian_player)
nrow(indian_playerall)

#Filter using more than one column
sachin_vs_aus = odi%>%filter(Player=='Sachin R Tendulkar', Versus =='Australia')
nrow(sachin_vs_aus)

#Grouping operations
#Compute player wise total runs and identify top 10 players
#group by is used so that we can get small dataframe
#alphabetic order is used
top = odi%>%group_by(Player)%>%summarise(total_runs = sum(Runs))
nrow(top)
#numeric order is used
top1 = odi%>%group_by(Player)%>%summarise(total_runs = sum(Runs)) %>% arrange(total_runs)%>%head(10)
nrow(top1)
View(top1)
#by default arrange has ascending order
#arrange(desc(total_runs))
#OR
#arrange(-total_runs)
#head(10)
top1 = odi%>%group_by(Player)%>%summarise(total_runs = sum(Runs)) %>% arrange(-total_runs)%>%head(10)
View(top1)
indiatop= odi%>%filter(Country == 'India')%>%group_by(Player)%>%summarise(max_runs =max(Runs))%>%arrange(-max_runs)%>%head(10)
# Top 10 players who have played better against australia based on their total
topaus=  odi%>%filter(Versus == 'Australia')%>%group_by(Player)%>%summarise(max_runs =max(Runs))%>%arrange(-max_runs)%>%head(10)
View(topaus)
topaus_vs_ind = odi %>% filter(Versus == 'Australia')%>%
  group_by(Country,Player)%>%
  summarise(sume =sum(Runs))%>%
  arrange(-sume)%>%
  head(10)
topaus_vs_ind
#Top 10 players based on total
#no numerical data, summaarise on the base character

player_total_matches = odi%>% group_by(Player)%>%summarise(total_matches =n())%>% arrange(-total_matches)%>%head(10)
View(player_total_matches)
#when we dont need to store the variable anywhere
odi%>% group_by(Player)%>%summarise(total_matches =n())%>% arrange(-total_matches)%>%head(10)%>% View()


#Another way
player_total_matches =odi%>% group_by(Player)%>% summarise(totalmatches =n())
t_10 = player_total_matches%>%arrange((-totalmatches))%>%head(10)
View(t_10)

##Summarize many metrics
#Playerwise get the following
#-Total runs
#-Total matches
#-Centuries, ducks, fifties, missed centuries

player_summary = odi%>%group_by(Country,Player)%>%summarise(total_runs = sum(Runs,na.rm=T),total_matches =n())
View(player_summary)


odi$Century = ifelse(odi$Runs ==0, TRUE,FALSE)
odi$Fifty = ifelse(odi$Runs>49 & odi$Runs<99 , TRUE,FALSE)
View(odi)


#players_summary = odi$

  
  
  
  
  
  
  
  
x="2000/02/19"  
x_date = as.Date(x,'%Y/%m/%d')

format(x_date,'%Y')
format(x_date,'%y')
format(x_date,'%d')
format(x_date,'%m')
format(x_date,'%b')
format(x_date,'%B')
format(x_date,'%a')
format(x_date,'%A')
format(x_date,'%H')
format(x_date,'%M')
format(x_date,'%S')
format(x_date,'%Y-%B')


##calculate year-wise total runs scored by Sachin R Tendulkar
sachin_year_perf = odi%>%filter(Player=='Sachin R Tendulkar')%>%group_by(Year)%>%
  summarise(total_runs = sum(Runs, na.rm=T),total_matches = n(), centuries = sum(Century,na.rm= T),
            ducks=sum(Ducks,na.rm = T), fiftes = sum(Fifty,na.rm = T))
