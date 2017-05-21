library(binr)
vid = read.csv("Video_Store.csv")
vid = data.frame(vid)
bin.age = bins(vid$Age, target.bins = 12, max.breaks = 18)
bin.age

binned.age = cut(vid$Age, breaks = c(0,19,21,24,25,30,32,33,35,38,45,47,70), labels = c('15-16','18-19',
                                                                                         '20-21','22-24','25-25','28-30',
                                                                                         '32-33','35-35','36-38','40-45',
                                                                                         '46-47','52-70'))
binned.age
vid$binned.age = binned.age
vid

min.max.income = (vid$Income - min(vid$Income))/(max(vid$Income)- min(vid$Income))
vid$min.max.income = min.max.income
vid

znorm.rental = scale(vid$Rentals, center = TRUE, scale = TRUE)
vid$znorm.rental = znorm.rental
vid

category.income = rep(NA, length(vid$Income))
category.income[vid$Income>=60000] = "High"
category.income[vid$Income >=25000 & vid$Income<60000] = "Mid"
category.income[vid$Income < 25000] = "Low"

vid$category.income = category.income
vid
