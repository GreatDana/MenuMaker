#### Daily menu making program, Dec 2015 version
# new stuff is preventing items in close proximity, sunday start for google calendar, all menu items
food<-c("EatOut/Takeaway","PerogiesMeatballs","Meatloaf&Tots","Saimon&Wonton","SpanishNite","ChickenStuffingCorn","Lasagna","Spaghetti","Pizza","MacNCheese","Hamburgers","Steaks","Tacos","Surprise","Leftovers","Grilled Cheese","ChickenCaesar","Soup","Sandwich","Fish","CrockPotMeat","Asian","PizzaRolls","CucumberSalad")
initial<-c("Ordr","Prgy","Loaf","SWon","Tapa","Mich","Lsga","Spag","Piza","McCz","HamB","Stks","Taco","Surp","Left","GrCz","Caes","Soup","Sand","Fish","CPMt","Asia","PRol","CSal")
newfoods<-c()
foodini<-data.frame(cbind(food,initial)) ## get the initials and items together
colnames(foodini)<-c("Food","Initial")
np<-length(food)
d<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
# Assign some relative weights based on ad hoc family voting
fp<-c(2,2,1,3,2,2,1,3,4,5,2,1,3,4,3,2,2,2,1,0.3,0.7,1,1,3)
numweeks<-16

repeat {
  menu3<-matrix(NA,nrow=1,ncol=3)
  names(menu3)<-c("Week","Day","Food")
  for(i in 1:numweeks) {
    menu<-sample(food,7,prob=fp/np,replace=F)
    menu2<-cbind(rep(i,7),d,menu)
    menu3<-rbind(menu3,menu2) }
  
  menu4<-data.frame(menu3[-1,])
  names(menu4)<-c("Week","Day","Food")
  bb<-match(menu4$Food,food)
  bbdif<-c(diff(bb,lag=1),diff(bb,lag=2),diff(bb,lag=3),diff(bb,lag=4))  ### preventing close proximity menu items
   if(!0%in%bbdif)
     if(length(unique(bb))==(length(food))) break  ## assuring all menu items make the cut
}
menu5<-menu4
menu6 <- merge(menu5, foodini, by.x="Food")
 
### make plots
library(ggplot2)

### Get the levels right for plot order
menu6$Day<-factor(menu6$Day, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
menu6$Week<-factor(menu6$Week, levels=seq(numweeks,1))
### Could make better by putting the actual dates on the side of plot 
ggplot(menu6, aes(x=Day,y=Week))+
  geom_tile(aes(fill=Food))+
  geom_text(data = menu6, aes(x=Day, y=Week, label=Initial), size=4)+
  labs(y="Week", x="Day of Week")+
  coord_fixed() + theme(axis.text.x=element_text(angle = 90))+
   ggtitle("Lets eat fatties!")


#Horizontal version for fridgerator

menu6$Week = with(menu6, factor(Week, levels = rev(levels(Week))))
menu6$Day = with(menu6, factor(Day, levels = rev(levels(Day))))
### reasonably good size png for printing
png(filename = "menu2.png", width = 1920, height = 1080, units = "px", pointsize = 30,    bg = "white")

ggplot(menu6, aes(x=rev(Week),y=rev(Day))) +
  geom_tile(aes(fill=Food))+
  geom_text(data = menu6, aes(x=rev(Week), y=rev(Day), label=Initial), size=11)+
  labs(x="Week", y="Day of Week")+
  coord_fixed() + theme(axis.text.x=element_text(angle = 90))+
  ggtitle("Lets eat fatties!") +
 theme_grey(base_size = 40)

dev.off()

### Make google calendar file for importing, could figure out how to get it to start on sunday
### of the upcoming week
oneday<-60*24*60
daystillsunday<-7-as.numeric(format(as.Date(Sys.time()),"%w"))
  menustart<-Sys.time()+daystillsunday*oneday
googdates<-rep(menustart,numweeks*7)
for(i in 1:length(googdates)) googdates[i]<-Sys.time()+i*oneday

googdates2 <- seq(as.Date(menustart), by = "day", length.out = numweeks*7)


nms <- c('Subject','Start Date','Start Time', 'End Date', 'End Time','All Day Event',
  'Description','Location','Private')
mat <- matrix(nrow = length(googdates), ncol = length(nms))
mat <- data.frame(mat)
colnames(mat) <- nms

mat$Subject <- menu4$Food
mat$"Start Date" <- googdates2
mat$"End Date" <- googdates2
mat$"All Day Event" <- "True"
mat$Description <- "Hanselmenu"
mat$Location <- "Glacier Hwy"
mat$Private <- "False"

starts <- strftime(googdates, format="%H:%M:%S %p")
ends <- strftime(googdates+60*30, format="%H:%M:%S %p")

mat$"Start Time" <- starts
mat$"End Time" <- ends

write.csv(mat,file="menudates.csv",quote=FALSE,row.names=FALSE)

#############