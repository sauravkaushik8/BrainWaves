
train<-read.csv("train.csv",stringsAsFactors= T)
test<-read.csv("test.csv",stringsAsFactors = T)

train$Time<-NULL
train$Y<-NULL
train_transposed<-t(train)

distances<-dist(train_transposed,method="euclidean")
cluster<-hclust(distances,method="ward.D")
plot(cluster)
clusterGroups<-cutree(cluster,k=5)

c<-data.frame(Cluster=clusterGroups)

write.csv(c,"clst.csv")

#Opend excel and put the names of rows as Asset.
