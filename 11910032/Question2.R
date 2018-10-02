#########################################Question1#############################

install.packages("igraph") # install the package, if not already installed


library(igraph)


airports <- read.csv("airports.dat", header=FALSE) ## source: http://openflights.org/data.html
colnames(airports) <- c("Airport ID","Name","City","Country","IATA_FAA","ICAO","Latitude","Longitude","Altitude","Timezone","DST","Tz database timezone")
nrow(airports)

airline_routes <- read.csv("routes.dat", header=FALSE) ## source: http://openflights.org/data.html
##colnames(airline_routes) <- c("Airline", "Airline ID", "Source Airport","Source Airport ID","Destination Airport","Destination Airport ID","Codeshare","Stops","Equipment")
colnames(airline_routes) <- c("Airline", "Airline ID", "Source Airport","Source Airport ID","Destination Airport","Destination Airport ID","Stops","Equipment")
head(airline_routes)


AirlineNW <- graph.edgelist(as.matrix(airline_routes[,c(3,5)]), directed=FALSE)


#########################################Question3#############################

plot(AirlineNW)
lec <- cluster_leading_eigen(AirlineNW)
lec

cluster_leading_eigen(AirlineNW, start=membership(lec))

max(membership(lec))

#########################################Question5#############################






outdegree <- degree(AirlineNW,mode="out")
indegree <- degree(AirlineNW,mode="in")
closeness_in <- closeness(AirlineNW, mode="in",normalized = TRUE)
btwn <- betweenness(AirlineNW,normalized = TRUE)



centralities <- cbind(indegree, outdegree, closeness_in, btwn)
colnames(centralities) <- c("inDegree","outDegree","closenessIn","betweenness")
head(centralities)

X <- kmeans(centralities,centers=25)
X
kmean_clust<- cbind(centralities, X$cluster)
subset(kmean_clust,X$cluster==1)
############################################Question6###################################################


Cluster_Variability <- matrix(nrow=20, ncol=1)
for (i in 1:20) Cluster_Variability[i] <- kmeans(centralities,centers=i, nstart=4)$tot.withinss
plot(1:20, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")


