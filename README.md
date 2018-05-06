# Data-Mining
#import data
dataset=read.csv('Airplane.csv')

#making time coloumn null
dataset$Time <- NULL
#making flightno coloumn null
dataset$Flight.. <- NULL
dataset$Date<-NULL
dataset$Type<-NULL
dataset$Registration<-NULL
dataset$cn.In<-NULL

#deletion of a row having aboard, ground and fatalities as NA
dataset<-dataset[!(is.na(dataset$Aboard)) & !(is.na(dataset$Fatalities)), ]
dataset<-dataset[!(is.na(dataset$Aboard)) & !(is.na(dataset$Ground)) & !(is.na(dataset$Fatalities)), ]
dataset<-na.omit(dataset$Location)
na.omit(dataset$Operator)
dataset<-na.omit(dataset$Route)
na.omit(dataset$Aboard)
na.omit(dataset$Fatalities)
na.omit(dataset$Ground)

dataset<-na.omit(dataset)
#kmean
x=dataset[1:2]
#elbow method for optimal no. of clusters/
set.seed(6)
wcss=vector()

for(i in 1:10) wcss[i]= sum(kmeans(x,i)$withinss)

plot(1:10,wcss,type = 'b', main=paste('clusters of client'),xlab='nummber of clusters',ylab = 'wcss')
#applying k mean
set.seed(10)
kmeans= kmeans(x,2,iter.max=300, nstart=10)
#visulising clusters
clusplot(x,
         kmeans$cluster,
         lines=0,
         shade=TRUE, color=TRUE, labels=2,
         plotchar=FALSE,span=TRUE,main=paste('clusters of cluster'),
         xlab="fatalities", ylab="values")

#hierarchical
dendrogram=hclust(dist(x,method = 'euclidean'),method = 'ward.D')
plot(dendrogram, main=paste('clusters of client'),xlab='nummber of clusters',ylab = 'wcss')


