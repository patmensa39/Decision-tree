# Decision-tree
Produced decision tree including pruning. 


```{r}
### Reading the baseball salary dataset (Hitters)
philant.baseball<-read.csv("Hitters.csv", header = T, sep = ",")
#### Removing all the N/A values in the salary column
philant.baseball2<-philant.baseball[!is.na(philant.baseball$Salary),]

### Scatter plots for Hits and years
library(ggplot2)
ggplot(data = philant.baseball2, mapping = aes(x=Years, y=Hits, color="Salary", main="Scatter plot for Years and Hits"))+geom_point()
```

```{r}
### Using log to build a regression decision tree with base on years and previous year's hit of the baseball players.
###install.packages("rpart")
###install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

philant.tree<-rpart(Salary~Hits+Years, data = philant.baseball2)
a<-data.frame(Hits=c(100), Years=c(4.5))
result<-predict(philant.tree,a)
print(result)
rpart.plot(philant.tree)

```


```{r}
### With heart disease example.
philant.heart<-read.csv("heartdata.csv", header = FALSE, colClasses = c(rep("numeric", 14)))
### This has no header so we fix that.
names(philant.heart)<-c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope","ca", "thal", "status")

### Setting status as a factor where 0= no heart disease and the remaining is there is heart disease. 
philant.heart$status<-as.factor(philant.heart$status>0)
philant.heart<-na.omit(philant.heart)
philant_heart.tree<-rpart(status~.,data = philant.heart, method = "class")
rpart.plot(philant_heart.tree)
```


```{r}
### Pruning the tree
philant_prune<-rpart(status~.,data = philant.heart, method = "class")
philant_heart.tree<-prune(philant_prune, cp=0.1)
plot(philant_heart.tree)
```
