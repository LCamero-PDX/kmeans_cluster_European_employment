# Cluster in FIN*SER view;
# Specify 3 Clusters;
finser.k3 <- kmeans(x=label.data[,c('FIN','SER')],centers=3);
names(finser.k3)
finser.k3df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=finser.k3$cluster,
                                  FIN=label.data$FIN,SER=label.data$SER));
finser.k3tab <- table(finser.k3df$Group,finser.k3df$Cluster);
finser.k3ac <- sum(apply(finser.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.k3tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(label.data$SER,label.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17),col='white')
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(finser.k3$centers[,2],finser.k3$centers[,1],labels=seq(1,3,1),col='black',cex=1)
points(finser.k3$centers[,2],finser.k3$centers[,1],col='black',cex=2.5)
text(finser.k3df$SER,finser.k3df$FIN,labels=finser.k3df$Cluster,col='grey',cex=0.75);
title('k-Means with 3 Clusters')

# Specify 6 Clusters;
finser.k6 <- kmeans(x=label.data[,c('FIN','SER')],centers=6);
names(finser.k6)
finser.k6df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=finser.k6$cluster,
                                  FIN=label.data$FIN,SER=label.data$SER));
finser.k6tab <- table(finser.k6df$Group,finser.k6df$Cluster);
finser.k6ac <- sum(apply(finser.k6tab[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.k6tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(label.data$SER,label.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17),col='white')
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(finser.k6$centers[,2],finser.k6$centers[,1],labels=seq(1,6,1),col='black',cex=1)
points(finser.k6$centers[,2],finser.k6$centers[,1],col='black',cex=2.5)
text(finser.k6df$SER,finser.k6df$FIN,labels=finser.k6df$Cluster,col='grey',cex=0.75);
title('k-Means with 6 Clusters')

# Cluster in PC1*PC2 view;
# Specify 3 Clusters;
pca.k3 <- kmeans(x=my.pca[,-c(1,2)],centers=3);
names(pca.k3)
pca.k3df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.k3$cluster,pc1=my.pca$pc1,pc2=my.pca$pc2));
pca.k3tab <- table(pca.k3df$Group,pca.k3df$Cluster);
pca.k3ac <- sum(apply(pca.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k3tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(my.pca$pc1,my.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2',
     xlim=c(-60,20),ylim=c(-22,25),col='white')
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,pos=4,col='green')
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,pos=4,col='blue')
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,pos=4,col='red')
#text(other.pca$pc1,other.pca$pc2,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(pca.k3$centers[,1],pca.k3$centers[,2],labels=seq(1,3,1),col='black',cex=1)
points(pca.k3$centers[,1],pca.k3$centers[,2],col='black',cex=2.5)
text(pca.k3df$pc1,pca.k3df$pc2,labels=pca.k3df$Cluster,col='grey',cex=0.75);
title('k-Means with 3 Clusters')
# Specify 6 Clusters;
pca.k6 <- kmeans(x=my.pca[,-c(1,2)],centers=6);
names(pca.k6)
pca.k6df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.k6$cluster,pc1=my.pca$pc1,pc2=my.pca$pc2));
pca.k6tab <- table(pca.k6df$Group,pca.k6df$Cluster);
pca.k6ac <- sum(apply(pca.k6tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k6tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(my.pca$pc1,my.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2',
     xlim=c(-60,25),ylim=c(-25,30),col='white')
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,pos=4,col='green')
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,pos=4,col='blue')
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,pos=4,col='red')
#text(other.pca$pc1,other.pca$pc2,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(pca.k6$centers[,1],pca.k6$centers[,2],labels=seq(1,6,1),col='black',cex=1)
points(pca.k6$centers[,1],pca.k6$centers[,2],col='black',cex=2.5)
text(pca.k6df$pc1,pca.k6df$pc2,labels=pca.k6df$Cluster,col='grey',cex=0.75);
title('k-Means with 6 Clusters')

kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));
kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));
kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));
kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));

finser.accuracy3
finser.accuracy6
pca.accuracy3
pca.accuracy6
pca.t6 <- table(pca.6df$Group,pca.6df$Cluster)

pca.accuracyk3 <- sum(apply(pca.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k3tab[1:3,],FUN=sum,MARGIN=2));

# Loop through 1-20 clusters using all dimensions;
# Compute the accuracy for each cluster, store, and plot;
# Set the maximum number of clusters to consider;
k.max <- 20;
# Initialize the accuracy arrays for storage;
accuracy.hier <- rep(NA,k.max);
accuracy.kmeans <- rep(NA,k.max);
# Fit the hierarchical clustering model outside of the loop for efficiency;
all.h <- hclust(d=dist(label.data[,-c(1,2)]),method='complete');
# Loop through different cluster sizes and compute classification accuracy;
for (j in 1:k.max){
  # Fit hierarchical cluster model of size j;
  hier.j <- cutree(all.h,k=j);
  hier.df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=hier.j));
  hier.table <- table(hier.df$Group,hier.df$Cluster);
  # Cannot use apply() on a vector;
  if (j==1){
    accuracy.hier[j] <- max(hier.table[1:3,])/sum(hier.table[1:3,]);
  }else{
    accuracy.hier[j] <- sum(apply(hier.table[1:3,],FUN=max,MARGIN=2))/sum(apply(hier.table[1:3,],FUN=sum,MARGIN=2));
  }#end if-else;
  # Fit k-means clustering model of size j;
  kmeans.j <- kmeans(x=label.data[,-c(1,2)],centers=j);
  kmeans.df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=kmeans.j$cluster));
  kmeans.table <- table(kmeans.df$Group,kmeans.df$Cluster);
  # Cannot use apply() on a vector;
  if (j==1){
    accuracy.kmeans[j] <- max(kmeans.table[1:3,])/sum(kmeans.table[1:3,]);
  }else{
    accuracy.kmeans[j] <- sum(apply(kmeans.table[1:3,],FUN=max,MARGIN=2))/sum(apply(kmeans.table[1:3,],FUN=sum,MARGIN=2));
  }#end if-else;
} #end j loop;

plot(seq(1,k.max,1),accuracy.hier,ylim=c(0,1),xlab='# Clusters',ylab='Accuracy',cex.axis=1,type='l',lwd=2,col='red')
points(seq(1,k.max,1),accuracy.hier,ylim=c(0,1),cex=1.5,type='p',col='red',pch=19)
points(seq(1,k.max,1),accuracy.kmeans,ylim=c(0,1),type='l',lwd=2,col='blue')
points(seq(1,k.max,1),accuracy.kmeans,ylim=c(0,1),cex=1.5,type='p',col='blue')
title('Classification Accuracy')
legend(1,0.2,legend=c('Hierarchical','k-Means'),col=c('red','blue'),lwd=2)

finser.k3ac
finser.k6ac
pca.k3ac
pca.k6ac
