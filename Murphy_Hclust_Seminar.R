rm(list = ls())

#Data location: http://www.stat.berkeley.edu/classes/s133/data/cars.tab

#space delimited data requires read.delim
cars = read.delim('http://www.stat.berkeley.edu/classes/s133/data/cars.tab',stringsAsFactors = FALSE)

#check out the data we are working with
head(cars)

#variables are measured in different scales, we need to standardize the data before analysis
cars.use = cars[,-c(1,2)] #remove first 2 columns as 'country' and 'name' are not meaningful for our analysis
medians = apply(cars.use,2,median)#(1 is for rows, 2 is for columns) we have selected to use the columns to calculate the median value of each column
mads = apply(cars.use,2,mad)#using the columns again, we have calculated the mean average deviation for each 
cars.use = scale(cars.use,center=medians,scale=mads)#here we are subtracting the median from each value of x and dividing by the mad
#############Any ideas for when we might want to use different scaling methods?#########################################
head(cars.use)

#calculate euclidian distance using 'dist' 
#For a data set with n observations, the distance matrix will have n rows and n columns; 
#the (i,j)th element of the distance matrix will be the difference between observation i and observation j
cars.dist = dist(cars.use)
cars.dist
#perform hierarchical cluster method, default is complete linkage
#Using this method, when a cluster is formed, its distance to other objects is computed as the 
#maximum distance between any object in the cluster and the other object
cars.hclust = hclust(cars.dist)

#pass results of 'hclust' to plot to view dendogram
plot(cars.hclust,labels=cars$Car,main='Default from hclust')

#how many obs in each cluster?
#One of the first things we can look at is how many cars are in each of the groups. 
#We'd like to do this for both the two cluster and three cluster solutions. You can create a vector showing 
#the cluster membership of each observation by using the cutree function.

groups.3 = cutree(cars.hclust,3)

table(groups.3)

#check counts for various numbers of clusters
#Notice that you can get this information for many different groupings at once by combining the calls
#to cutree and table in a call to sapply. For example, to see the sizes of the clusters for solutions 
#ranging from 2 to 6 clusters, we could use:
  
counts = sapply(2:6,function(ncl)table(cutree(cars.hclust,ncl)))
names(counts) = 2:6
counts

#use subscripting on the vector of car names to choose just the observations from a particular cluster
cars$Car[groups.3 == 1]

#use sapply to perform the same operation on all groups in one step
sapply(unique(groups.3),function(g)cars$Car[groups.3 == g])

#same process using 4 cluster solution
groups.4 = cutree(cars.hclust,4)
sapply(unique(groups.4),function(g)cars$Car[groups.4 == g])

#The table function can be used, this time passing two arguments, to produce a cross-tabulation of cluster group 
#membership and country of origin:
table(groups.3,cars$Country)

#A very useful method for characterizing clusters is to look at some sort of summary statistic, 
# like the median, of the variables used to perform the cluster analysis, broken down by the groups 
# that the cluster analysis identified. 
#The aggregate function is well suited for this task, since it will perform summaries on many 
# variables simultaneously. Let's look at the median values for the variables we've used in the 
# cluster analysis, broken up by the cluster groups. One oddity of the aggregate function is that 
# it demands that the variable(s) used to divide up the data are passed to it in a list, even if 
# there's only one variable:
#Note aggregate ftn requires all values passed to be in a list,even single values

#because the data is scaled the negative numbers mean "lower than most" and positive numbers mean "higher than most"
aggregate(cars.use,list(groups.3),median)

#data in original scales may be more meaningful for interpretation of the clusters
aggregate(cars[,-c(1,2)],list(groups.3),median)

#add the number of obs in each group, using a data frame for ease of manipulation
a3 = aggregate(cars[,-c(1,2)],list(groups.3),median)
data.frame(Cluster=a3[,1],Freq=as.vector(table(groups.3)),a3[,-1])

#same procedure for 4 cluster solution
a4 = aggregate(cars[,-c(1,2)],list(groups.4),median)
data.frame(Cluster=a4[,1],Freq=as.vector(table(groups.4)),a4[,-1])

