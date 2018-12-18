library(dplyr)
odi = read.csv('C:/Users/Administrator/Desktop/datasets/odi-batting.csv')
dim(odi) 
                        ###########SUB-SETTING##########
#SQUARE BRACKET NOTATION
# odi[r,c]
x = odi[20000,3]
View(x)


#Square bracket indexing
x =odi[,2:4]
View(x)

# Problem with this indexing is that u have to check the index
# Without dplyr will give you col indexing
x = odi[2:3,c(2:3,5,7)]
View(x)


# Select - search based indexing 
# Subset the data
x = odi%>% select(Player , Runs , Versus)
View(x)

## To  remove the col 
x = odi%>% select(-c(Player, Runs, Versus))
View(x)

#? Ground wise no. of centuries
#? No. of matches played by india on year basis
#? no. of matches in which India scored more than 300 runs against each country
# format is used to extract the particular thing
str(odi)

x = odi%>%group_by(Ground)%>%
  summarise(total_runs = sum(Runs),
            centuries = sum(Runs>99,na.rm = T,T))%>%arrange(-centuries)
View(x)
odi$Century = ifelse(odi$Runs > 99 , "Cent","No")
ground_wise = odi%>% filter(Century =='Cent')%>%
  group_by(Ground)%>%
  summarise(no._century = n())%>%arrange(-no._cent)
View(odi)
odi$Date = as.Date(odi$MatchDate, "%m-%d-%Y") 
odi$Year = format(odi$Date,'%Y')
x = odi%>%filter(Country =='India')%>%group_by(Year)%>%summarise(total = n_distinct(MatchDate))
View(x)

x = odi%>%group_by(Country, MatchDate , Versus)%>%summarise(Match_score = sum(Runs))%>%
  filter(Country =='India' & Match_score >299)%>%
  group_by(Versus)%>%
  summarise(no_of_Match = n())
View(x)
###########################################ggPLOT#####################################################
################################################################################################
# Layer by layer architecture
# ggplot(x,y)
#     aes(x,y)
#++
#geom ----(what kind of chart) Eg: geom_bar



#freq by
#alphabetic order


### file ggplot.r by sirS


odi%>% filter(Country =='India')%>%group_by(Year)%>%select(Country , Player, Year)%>%arrange(Year)%>%View()
























