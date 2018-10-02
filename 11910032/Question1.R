
#######################################Question1###################################################

install.packages("Matrix")## if not already installed
install.packages("arules") ## if not already installed
install.packages("arulesViz") ## if not already installed; necessary for visualization purpose

library("Matrix")
library("arules")
library("arulesViz")


cosmetics <-read.csv("cosmetics.csv")
head(cosmetics)

getwd()
dir()

rules = apriori(as.matrix(cosmetics[,2:15]), parameter=list(support=0.15, confidence=0.7,minlen=2)) ## the first column in mydata has transaction id


inspect(rules)

#######################################Question2###################################################
Blush<-cosmetics[cosmetics$Blush==1,]
Eye.shadow=Blush[Blush$Eye.shadow==1,]
nrow(Eye.shadow[Eye.shadow$Mascara==1,])


#######################################Question3###################################################

Confidence=nrow(Eye.shadow[Eye.shadow$Mascara==1,])/nrow(Eye.shadow)
Confidence


###################################Question4######################################

lift_ration=Confidence/(nrow(cosmetics[cosmetics$Mascara==1,])/1000)
lift_ration
