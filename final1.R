library(data.table)
library(reshape2)
library(dplyr)
library(rpart)
library(rpart.plot)
#Read data from file
movies <- read.csv("C:/Users/Nikila/Desktop/reccomender/movies.csv")
ratings <- read.csv("C:/Users/Nikila/Desktop/reccomender/ratings.csv")

#Clean rating data and order to required format
movieIds <- length(unique(movies$movieId))
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) 
write.csv(ratingmat,"C:/Users/Pooja s/Desktop/reccomender/ratingmat.csv")

#Accept user id and find similar user 
current_user <- 1
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))
head(rated_items)
selected_users <- which(apply(!is.na(ratingmat[,rated_items]), 1, sum) >= 80)
tmp <- ratingmat[selected_users,rated_items]
rownames(tmp) <- selected_users;
write.csv(tmp,"C:/Users/Nikila/Desktop/reccomender/tmp.csv")

#Create correlation matrix for the current user and similar users 
cormat <- rep(0,length(selected_users))
corna <- function(x,y)
{
 tmp1 <- data.frame(x,y);
 tmp1 <- tmp1[(which(!is.na(tmp1$y))),];
 c <- cor(tmp1$x,tmp1$y)
 return(c);
}
for (i in 1:length(selected_users))
{
  cormat[i] <- corna(tmp[1,],tmp[i,]);
}
cormatgraph <- data.frame(selected_users,cormat);
colnames(cormatgraph) <- c("User","Correlation");
cormatgraph <- cormatgraph[order(-cormatgraph$Correlation),]
print("Correlation matrix");
print(t(cormatgraph));

#Select users with the highest correlation
x <- which(rownames(tmp) %in% as.character(head(cormatgraph$User)),tmp)
tp <- tmp[x,]
write.csv(tp,"C:/Users/Nikila/Desktop/reccomender/tp.csv");

#Clean data for the decision tree
final <- tmp[,colMeans(is.na(tmp)) == 0]
final <- t(final)
rownames(final) <- dimnames(final)[[1]]
colnames(final) <- dimnames(final)[[2]]
final <- data.frame(final) 
write.csv(final,"C:/Users/Nikila/Desktop/reccomender/final.csv");

#Fit decision tree and plot it
fit <- rpart(final$X1~.,final,method="class");
rpart.plot(fit)

#Create testing matrix
test <- ratingmat[selected_users,-rated_items]
test <- test[2:nrow(test),]
rownames(test) <- selected_users[2:length(selected_users)];
final_test <- test[,colMeans(is.na(test)) == 0]
final_test <- t(final_test)
final_test <- data.frame(final_test)
write.csv(final_test,"C:/Users/Nikila/Desktop/reccomender/final_test.csv");

#Prediction using the testing matrix
tr <- predict(fit,final_test,type="class");
tr <- data.frame(tr);

#Recommender 
movie_name <- dimnames(tr)[[1]];
tr <- as.integer(tr$tr)
tr <- data.frame(movie_name,tr)
colnames(tr) <- c("MovieId","Rating")
flag <- which(tr$Rating == 4);
id <- as.character(tr$MovieId[flag]);
flag <- match(id,as.character(movies$movieId))
print(paste("The movies recommended for user ",current_user))
predicted <- data.frame(as.character(movies$title[flag]),as.character(movies$genres[flag]))
colnames(predicted) <- c("Movie","Generes")
print(predicted)