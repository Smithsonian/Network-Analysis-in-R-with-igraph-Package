
##Network graphs using "igraph" package
library(igraph)

#read in edge data. First two columns are used as ID. Subsequent columns used as edge properties. This can include weights.
rawedges<-read.csv(file.choose())

#read in vertices (nodes) data. First column is ID, subsequent columns are vertices properties
rawedges<-read.csv(file.choose())

#Create 'igraph' object from vertices and edges. Can be directed or undirected
Net<-graph.data.frame(vertices=rawnodes, d=rawedges, directed=TRUE)

#Get measures of centrality. Here we use 'degree' and 'betweenness'
#other measures are available, see package manual

Degree<-degree(Net,v=V(Net),mode="all", normalized = TRUE)

Between<-betweenness(Net,v=V(Net),directed=TRUE, normalized = TRUE)

#Create a layout object for how we want the graph displayed. Here we use the Fruchterman-Reingold layout algorithm 
#which is basically a 'spring embedding' type layout. See function help for details
#We can use this layout object in plotting later
FR.Layout <- layout_with_fr(Net)

#We can add or edit vertices properties which will help with graphing and analysis later
#View the vertices IDs
V(Net)
#View the properties of the network with 'summary'. The notation "v/c' indicates a vertices property that is in character format
#"e/n" indicates an edge property that is in numeric format
summary(Net)


#Here we make sure that the 'Year' vertices propety is a factor
V(Net)$Year<- as.factor(V(Net)$Year)

#We can add new properties such as 'color' and 'labels' that will feed into the plotting function later. 

#Set up a colour pallette for distinguishing between two release groups
mycol<-c("violetred3", "darkorange2" )

#Set up a colour pallette for distinguishing between four seasons
season.pallette<-c("chocolate4","chartreuse4","gold1","dodgerblue")


#add a property that assigns one of the above collars dependent on the 'Year' property

V(Net)$color <- mycol[as.factor(V(Net)$Year)]
#check that it has worked properly
head(V(Net)$color)

#add a property that assigns a label based on the animal group (in this case the 'Harem')
V(Net)$label<-V(Net)$Harem


#we can also do the same with edge properties to help us colour and label the edges

E(Net)$color <- season.pallette[as.factor(E(Net)$Season)]
head(E(Net)$color)

#if we need to get rid of those colors we can reset this property to a single colour
##E(Net)$color <-'darkgrey'


#####Plotting
#Now we bring togetehr all the objects and properties created above and plot the network
#We set 'vertex.size' based on centrality measures and set the layout based on the layout we chose above 
#set the label sizes, placement and rotation of labels and margins and select our colour pallette. 
#See function help file for details on other arguments

#Plot by 'degree'. The absolute values may be too small or too large for clear plotting
#so we sometimes need to add a multiplier in order to see the vertices clearly on the plot
plot(Net, vertex.size=Degree*0.3,vertex.label.cex=3.5, layout=FR.Layout, rescale=TRUE, vertex.label.degree=0, edge.arrow.size=0.5, margin=-0.13, palette=mycol, main="All Data - Degree")

#Add a legend for the release group
legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")

#add another legend for the season if applicable
legend(x=-2, y=0.7, c("Autumn","Spring","Summer","Winter"),pch=15, col =season.pallette,  cex=2.5,  ncol=1, bty="n", title="Season")

#Plot by 'betweeness' 
plot(Net, vertex.size=Between*2000,vertex.label.cex=3.5, layout=FR.Layout, rescale=TRUE, vertex.label.degree=0, edge.arrow.size=0.5, margin=-0.16, palette=mycol,main= "All Data - Betweeness")

#Add a legend for the release group
legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")

#add another legend for the season if applicable
legend(x=-2, y=0.7, c("Autumn","Spring","Summer","Winter"),pch=15, col =season.pallette,  cex=2.5,  ncol=1, bty="n", title="Season")


##Splitting into Seasons

#We can subset the network based on the edge property "Season", deleting vertices that are not present in that subset of edges

Net.Spring<- subgraph.edges(graph=Net, eids=which(E(Net)$Season=="Spring"), delete.vertices = TRUE)
Net.Summer<- subgraph.edges(graph=Net, eids=which(E(Net)$Season=="Summer"), delete.vertices = TRUE)
Net.Autumn<- subgraph.edges(graph=Net, eids=which(E(Net)$Season=="Autumn"), delete.vertices = TRUE)
Net.Winter<- subgraph.edges(graph=Net, eids=which(E(Net)$Season=="Winter"), delete.vertices = TRUE)

#We will need to re-calculate the measures of centrality for each season
Degree.Spring<-degree(Net.Spring,v=V(Net.Spring),mode="all", normalized = TRUE)
Between.Spring<-betweenness(Net.Spring,v=V(Net.Spring),directed=TRUE, normalized = TRUE)

Degree.Summer<-degree(Net.Summer,v=V(Net.Summer),mode="all", normalized = TRUE)
Between.Summer<-betweenness(Net.Summer,v=V(Net.Summer),directed=TRUE, normalized = TRUE)

Degree.Autumn<-degree(Net.Autumn,v=V(Net.Autumn),mode="all", normalized = TRUE)
Between.Autumn<-betweenness(Net.Autumn,v=V(Net.Autumn),directed=TRUE, normalized = TRUE)

Degree.Winter<-degree(Net.Winter,v=V(Net.Winter),mode="all", normalized = TRUE)
Between.Winter<-betweenness(Net.Winter,v=V(Net.Winter),directed=TRUE, normalized = TRUE)


#Similarly we need to recalculate the 'layout' object for each season
FR.Layout.Spring <- layout_with_fr(Net.Spring)
FR.Layout.Summer <- layout_with_fr(Net.Summer)
FR.Layout.Autumn <- layout_with_fr(Net.Autumn)
FR.Layout.Winter <- layout_with_fr(Net.Winter)

#The edge and node properties we set in the original network should still be present in the subets
summary(Net.Spring)


#Plot the same graphs as above but broken out into season
#Degree plots
#set up grid of plots
par(mfrow=c(2,2))

#Plot each season
plot(Net.Spring, vertex.size=Degree.Spring*1.2,vertex.label.cex=3.5, layout=FR.Layout.Spring, rescale=TRUE, vertex.label.degree=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Spring - Degree")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")


plot(Net.Summer, vertex.size=Degree.Summer*1.2,vertex.label.cex=3.5, layout=FR.Layout.Summer, rescale=TRUE, vertex.label.degree=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Summer - Degree")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")


plot(Net.Autumn, vertex.size=Degree.Autumn*1.2,vertex.label.cex=3.5, layout=FR.Layout.Autumn, rescale=TRUE, vertex.label.degree=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Autumn - Degree")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")


plot(Net.Winter, vertex.size=Degree.Winter*1.2,vertex.label.cex=3.5, layout=FR.Layout.Winter, rescale=TRUE, vertex.label.degree=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Winter - Degree")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")


#Between plots
#set up grid of plots
par(mfrow=c(2,2))

#Plot each season
plot(Net.Spring, vertex.size=Between.Spring*500,vertex.label.cex=2.5, layout=FR.Layout.Spring, rescale=TRUE, vertex.label.Between=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Spring - Between")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")


plot(Net.Summer, vertex.size=Between.Summer*200,vertex.label.cex=2.5, layout=FR.Layout.Summer, rescale=TRUE, vertex.label.Between=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Summer - Between")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")


plot(Net.Autumn, vertex.size=Between.Autumn*500,vertex.label.cex=2.5, layout=FR.Layout.Autumn, rescale=TRUE, vertex.label.Between=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Autumn - Between")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")


plot(Net.Winter, vertex.size=Between.Winter*500,vertex.label.cex=2.5, layout=FR.Layout.Winter, rescale=TRUE, vertex.label.Between=0, edge.arrow.size=0.2, margin=-0.13, palette=mycol, main="Winter - Between")

#Add a legend for the release group
#legend(x=1., y=0.2, c("2016","2017"),pch=19, col =mycol,  cex=2.5,  ncol=1, bty="n", title="Release")



