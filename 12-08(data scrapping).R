install.packages("ggmap")
library(ggplot2)
library(ggmap)
library(BatchGetSymbols)
# Stock Market Price since past one year
############ it will give the 
geocode("Banglore")


geocode(c('Mumbai','UK','Middleearth'))

out = BatchGetSymbols(c('GOOGL','APPL') , first.date = '2018-01-01' , last.date = '2018-07-30')
View(out$df.tickers)

library("rvest")
#########
library(dplyr)
url_page = "https://medium.freecodecamp.org/if-you-want-to-learn-data-science-start-with-one-of-these-programming-classes-fb694ffe780c"
url_page ="https://medium.com/topic/data-science"
page = read_html(url_page)

#page%>%html_nodes('p')%>%html_text()%>%View()
#page%>%html_nodes('p')%>%html_text()%>%View()
########################################## start ###################################################################
url_page ="https://medium.com/topic/data-science"
page = read_html(url_page)
html_nodes(page,xpath = "//*[@class='bu bv bw w bx x ck ea eb q ct ec ed']")%>%html_text()
html_nodes(page,xpath = "//*[@class='w b x y z ab q bu bv']")%>%html_text()
html_nodes(page,xpath = "//*[@class='el q']")%>%html_text()
page %>% 
  html_nodes(xpath = "//*[@class='k p r cq ek']") %>%
  xml_attr("value")






#######################################################################################################################
url_wiki = "https://en.wikibooks.org/wiki/Data_Science:_An_Introduction" 
page1 = read_html(url_wiki)
page1%>%html_nodes('p')%>%html_text()

url_wiki1 = "https://en.wikipedia.org/wiki/India"
page2 =read_html(url_wiki1)
page%>%html_nodes('p.graf')%>%html_text()

url_wiki = "https://en.wikipedia.org/wiki/Demographics_of_India" 
page1 = read_html(url_wiki)
l =page1%>%html_nodes('table')
View(l)
d =page1%>%html_nodes('table.wikitable')%>%html_table(fill = TRUE) #
View(d[[10]])
d

url_wiki = "https://medium.com/towards-data-science/data-science/home" 
page1 = read_html(url_wiki)
l =page1%>%html_nodes('p')
View(l)
d =page1%>%html_nodes('h3')%>%html_text()
d =page1%>%html_nodes("a.ds-link--styleSubtle")%>%html_text()
d =page1%>%html_nodes("a.ds-link--styleSubtle")%>%html_text()
View(d[[10]])
d


###################################################################################################################
#practice
############ TABLE##################
url1 = "https://en.wikipedia.org/wiki/Faith_(2012_TV_series)"
u = read_html(url1)
r = u%>%html_nodes('table.wikitable')%>%html_table(fill = TRUE)
r
View(r[[2]])
###########Heading########
url1 = "https://en.wikipedia.org/wiki/BTS_(band)"
u = read_html(url1)
r = u%>%html_nodes('h2')%>%html_text()
r
View(r)
####################Aurthor##############################
url1 = "https://en.wikipedia.org/wiki/BTS_(band)"
u = read_html(url1)
r = u%>%html_nodes('img')
r
View(r)
#######################################
url_medium = "https://medium.com/topic/data-science"
u = read_html(url_medium)
rt = u%>%html_nodes('h3') %>%html_text()
rt2=u %>% html_nodes('span.w') %>% html_text()
rt2
