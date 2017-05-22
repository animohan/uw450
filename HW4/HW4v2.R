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
write.csv(vid, file = "Part_a_d_Video_Store.csv", sep = ',')



vid = read.csv("Video_Store.csv")
vid = data.frame(vid)
names(vid)

GenderM = rep(0,length(vid$Gender))
GenderM[vid$Gender == "M"] = 1

GenderF = rep(0,length(vid$Gender))
GenderF[vid$Gender == "F"] = 1

IncidentalsNo = rep(0, length(vid$Incidentals))
IncidentalsNo[vid$Incidentals == "No"] = 1

IncidentalsYes = rep(0, length(vid$Incidentals))
IncidentalsYes[vid$Incidentals == "Yes"] = 1

GenreComedy = rep(0, length(vid$Genre))
GenreComedy[vid$Genre == 'Comedy'] = 1

GenreAction = rep(0, length(vid$Genre))
GenreAction[vid$Genre == 'Action'] = 1

GenreDrama = rep(0, length(vid$Genre))
GenreDrama[vid$Genre == 'Drama'] = 1


newvid = data.frame(cbind(vid$Cust.ID, GenderM, GenderF, vid$Income, vid$Age, vid$Rentals, vid$Avg.Per.Visit,
               IncidentalsNo, IncidentalsYes, GenreDrama,GenreAction, GenreComedy))
names(newvid)

names(newvid) = c("Cust.ID","GenderM","GenderF","Income","Age","Rentals","Avg.Per.Visit",
                  "IncidentalsNo","IncidentalsYes","GenreDrama","GenreAction","GenreComedy")
newvid

write.csv(newvid, file = "Part_e_Video_Store.csv")
library(ellipse)
vidcor = cor( newvid[,c("GenderM","GenderF","Income","Age","Rentals","Avg.Per.Visit",
                       "IncidentalsNo","IncidentalsYes","GenreDrama","GenreAction","GenreComedy") ], method = "pearson")

library(corrplot)
corrplot(vidcor, method="circle", type='lower')

xtabs(newvid$GenderM+newvid$GenderF ~ newvid$GenreAction + newvid$GenreDrama + newvid$GenreComedy)

FemaleComedy = xtabs(~ newvid$GenderF + newvid$GenreComedy)
MaleComedy = xtabs(~ newvid$GenderM + newvid$GenreComedy)
GenreComedyCol = rep(NA,2)
GenreComedyCol[1] = FemaleComedy[4]
GenreComedyCol[2] = MaleComedy[4]

FemaleDrama = xtabs(~ newvid$GenderF + newvid$GenreDrama)
MaleDrama = xtabs(~ newvid$GenderM + newvid$GenreDrama)
GenreDramaCol = rep(NA,2)
GenreDramaCol[1] = FemaleDrama[4]
GenreDramaCol[2] = MaleDrama[4]

FemaleAction= xtabs(~ newvid$GenderF + newvid$GenreAction)
MaleAction = xtabs(~ newvid$GenderM + newvid$GenreAction)
GenreActionCol = rep(NA,2)
GenreActionCol[1] = FemaleAction[4]
GenreActionCol[2] = MaleAction[4]

GenderGenre = cbind(GenreComedyCol,GenreActionCol,GenreDramaCol)
row.names(GenderGenre) = c("Female","Male")
GenderGenre

vid[vid$Gender == "F" && vid$Genre == "Comedy"]

#High Rentals.

HighRentals = vid[vid$Rentals >= 30,]
summary(HighRentals)
#Actions are much more likely to be rented.
#Average and Median income of folks who rent more movies are lower.
# Average age is lower.

HighIncidentals = vid[vid$Incidentals == "Yes",]
summary(HighIncidentals)
summary(vid)

# Males are more likely to by incidentals.
# Folks who rent Action or Drama are more likely; comedy is less likely.
#
