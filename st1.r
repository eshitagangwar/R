#To read the datasets that are already present  inthe R
datasets::faithful
data1 = faithful
data1
str(data1)
#to understand what the meaning of the data
?faithful
help("faithful")
dim(data1)
colnames(data1)
range(data1$eruptions)
mi = min(data1$eruptions)
ma = max(data1$eruptions)
bins = seq(1.5, 5.5 , by = 0.5)
intervals = cut(data1$eruptions,bins)
freq2 = transform(table(intervals))
View(freq2)
