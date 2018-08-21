foodinmouth <- function(numweeks){
  require(lubridate)
  require(ggplot2)
  require(RColorBrewer)
  require(plyr)
  numweeks=numweeks
### set up ggplot theme
  theme_set(theme_bw(base_size=16)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
### enter long food names
  Food<-c("EatOut/Takeaway","Meatballs&Tots","Noodles","SpanishNite","ChickenStuffingCorn","Lasagna",
          "Spaghetti","Pizza","MacNCheese","Hamburgers","Steaks","Tacos","Surprise","Leftovers","GrilledCheese","Salad","Soup","Sandwich",
          "Fish","CrockPotMeat","Asian","PizzaRolls","CucumberSalad","Grill","Jambalaya","Sushi","Nachos")
### make initials for the plot
  Initial<-c("Ordr","Ball","Nood","Tapa","Mich","Lsga","Spag","Piza","McCz","HamB","Stks","Taco","Surp","Left","GrCz","Salad",
             "Soup","Sand","Fish","CPMt","Asia","PRol","CSal","Grll","Jamb","Sush","Nach")
   ### create data frame with long and short names
  foodini<-data.frame(cbind(Food,Initial)) ## get the initials and items together
  days=7
  np<-length(Food)
  fp<-c(2,2,2,2,2,1,3,4,5,2,1,3,4,3,2,2,2,1,0.3,0.7,1,1,3,1.5,1,0.5,1)# Assign some relative weights based on ad hoc family voting
  ## this does the sampling of the vectors with above weights
  menu <- data.frame(Food=c(replicate(numweeks,sample(Food,days,prob=fp/np,replace=F ))), 
                     day=now() -days(1) + c(1:(numweeks*days)) * days(1))
  menu$Week <- round_date(menu$day, "week")
  menu$Day <- weekdays(menu$day)
  menu <- merge(menu, foodini, by="Food")
  menu$labels <- floor_date(menu$day,"week")
  bb<-match(menu$Meal,Food)
  ### use differencing to ensure no repeats
  bbdif<-c(diff(bb,lag=1),diff(bb,lag=2),diff(bb,lag=3),diff(bb,lag=4))  ### preventing close proximity menu items
  if(!0%in%bbdif)
    if(length(unique(bb))==(length(Food))) break  ## assuring all menu items make the cut
  
  #plotting stuff   
  menu <- arrange(menu, Week)
  menucols <- length(factor(menu$Food))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))(menucols)
  ### Get the levels right for plot order
  menu$Day<-factor(menu$Day,levels=
                     c("Saturday","Friday","Thursday","Wednesday","Tuesday","Monday","Sunday"))
  
   ## plot it up
   ggplot(menu, aes(labels,Day, fill=Food))+geom_tile()+ 
    scale_fill_manual(values = getPalette)+
    ggtitle("Lets eat, fatties!") + xlab("Week")+ylab("")+
    geom_text(aes(x=labels, y=Day, label=Initial))
  ggsave("menu.png",width=14,height=10,dpi=300)

  ### start it on the day the program is run...
  oneday<-60*24*60
  daystillsunday<-7-as.numeric(format(as.Date(Sys.time()),"%w"))
  #menustart<-Sys.time()+daystillsunday*oneday
  menustart<-menu$day[1]
  
  googdates<-rep(menustart,numweeks*7)
  for(i in 1:length(googdates)) googdates[i]<-Sys.time()+i*oneday
  
  googdates2 <- seq(as.Date(menustart), by = "day", length.out = numweeks*7)
  
  ## setting up calendar friendly data frame.
  
  nms <- c('Subject','Start Date','Start Time', 'End Date', 'End Time','All Day Event',
           'Description','Location','Private')
  mat <- matrix(nrow = length(googdates), ncol = length(nms))
  mat <- data.frame(mat)
  colnames(mat) <- nms
  
  mat$Subject <- menu$Food
  mat$"Start Date" <- strftime(menu$day,format = "%Y-%m-%d")
  mat$"End Date" <- strftime(menu$day,format = "%Y-%m-%d")
  mat$"All Day Event" <- "True"
  mat$Description <- "Hanselmenu"
  mat$Location <- "Glacier Hwy"
  mat$Private <- "False"
  
  starts <- strftime(googdates, format="%H:%M:%S %p")
  ends <- strftime(googdates+60*30, format="%H:%M:%S %p")
  
  mat$"Start Time" <- starts
  mat$"End Time" <- ends
  ### output file ready to import into google calendar.
  write.csv(mat,file="menudates.csv",quote=FALSE,row.names=FALSE)
  
  }

foodinmouth(16)
