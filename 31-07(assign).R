library(dplyr)
library(readxl)
bb = read_excel('C:/Users/Administrator/Desktop/datasets/Case study_2/Data Set for Case study/Ball_by_Ball.xlsx')
m = read_excel('C:/Users/Administrator/Desktop/datasets/Case study_2/Data Set for Case study/Match.xlsx') 
  
p = read_excel('C:/Users/Administrator/Desktop/datasets/Case study_2/Data Set for Case study/Player.xlsx')

pp = read_excel('C:/Users/Administrator/Desktop/datasets/Case study_2/Data Set for Case study/Player_Match.xlsx')
s = read_excel('C:/Users/Administrator/Desktop/datasets/Case study_2/Data Set for Case study/Season.xlsx')
t= read_excel('C:/Users/Administrator/Desktop/datasets/Case study_2/Data Set for Case study/Team.xlsx')

str(bb)  
str(p)

player_bb = merge(bb[,c("Striker_Id","Batsman_Scored","Match_Id")],p[,c("Player_Id","Player_Name")] ,by.x = 'Striker_Id', by.y = 'Player_Id',all.x =TRUE)

q1 = player_bb%>% 
  group_by(Player_Name)%>%
  summarise(total_run = sum(as.integer(Batsman_Scored), na.rm = TRUE))%>%
  head(10)
View(q1)

q2 = player_bb%>% group_by(Player_Name)%>%summarise(ti = sum(Batsman_Scored==6,na.rm = T, T)) %>%head(10)
View(q2) 



q3 = player_bb%>%group_by(Player_Name,Striker_Id,Match_Id)%>%
  summarise(runs = sum(as.integer(Batsman_Scored),na.rm = T))%>%
  group_by(Player_Name,Striker_Id)%>%
  summarise(hunds = sum(runs >99))%>%
  arrange(-hunds)%>%
  head(10)
View(q3)
str(t)
###############################ques 2########
q4 = m%>%group_by(Match_Winner_Id)%>%summarise(win =n())
View(q4)
str(q4)
str(t)
m_t = merge(q4[,c("Match_Winner_Id","win"),],t[,c("Team_Id","Team_Short_Code")],by.x ='Match_Winner_Id',by.y = 'Team_Id',all.x =TRUE)
m_t
m_t = merge(m[,c("Toss_Winner_Id","Match_Winner_Id","is_result"),],t[,c("Team_Id","Team_Short_Code")],by.x ='Toss_Winner_Id',by.y = 'Team_Id',all.x =TRUE)
q4 = m_t%>%group_by(Toss_Winner_Id,Match_Winner_Id, Team_Short_Code)%>%summarise(win =sum(is_result == ))%>%arrange()%>%head(10)
View(q4)
m_t = merge(m[,c("Toss_Winner_Id","Match_Winner_Id","win"),],t[,c("Team_Id","Team_Short_Code")],by.x ='Toss_Winner_Id',by.y = 'Team_Id',all.x =TRUE)
m_t
