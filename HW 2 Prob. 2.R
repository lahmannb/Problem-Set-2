#Problem 2

data(iris)
names(iris)
table(iris$Species)
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)
#making the tree
library(tree)
tree1<-tree(Species~Sepal.Width+Petal.Width,data=iris)
summary(tree1)
plot(tree1)
text(tree1)
plot(iris$Petal.Widt,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)
#predicting values
set.seed(32313)
newdata<-data.frame(Petal.Width=runif(20,0,2.5),Sepal.Width=runif(20,2,4.5))
pred1<-predict(tree1,newdata)
pred1
pred1<-predict(tree1,newdata,type="class")
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19)
partition.tree(tree1,"Species",add=TRUE)
#Cars Examples for Pruning Trees
data(Cars93,package="MASS")
head(Cars93)
treeCars<-tree(DriveTrain~MPG.city+MPG.highway+AirBags+
                 EngineSize+Width+Length+Weight+Price+Cylinders+
                 Horsepower+Wheelbase,data=Cars93)
plot(treeCars)
text(treeCars)
par(mfrow=c(1,2))
plot(cv.tree(treeCars,FUN=prune.tree,method="misclass"))
plot(cv.tree(treeCars))
pruneTree<-prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)
#Show resubstitution error
table(Cars93$DriveTrain,predict(pruneTree,type="class"))
table(Cars93$DriveTrain,predict(treeCars,type="class"))

