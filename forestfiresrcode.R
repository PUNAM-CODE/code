##################nurral network###############
###########forestfires###############
 library(readr)
 forestfires <- read_csv("forestfires.csv")
View(forestfires1)

str(forestfires)

forestfires1<-forestfires[-c(1,2)]

##########normalizatio of data#######
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm<-as.data.frame(lapply(forestfires1[,-11],FUN=normalize))
#no need normaization beacaue data is binary from
head(forestfires1)
forestfires1$size_category<-ifelse(forestfires$size_category=="small",1,0)
summary(forestfires1$size_category)

#bind the data
forestfiresbind<- cbind(forestfires1,forestfires1$size_category)
colnames(forestfires1)[11] <- "size_category"
forestfires_train<-forestfires1[1:362,]
forestfires_test<-forestfires1[363:517,]

##############data is classification problem hencee used the nnet package
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification
dim(forestfires)
dimnames(forestfires1)
forestfires1<-nnet(size_category~.,data = startups_norm,hidden=10,linear.output = F))
plot(forestfires1)

forestfires1<-nnet(forestfires1$size_category~.,data=forestfires_norm,subset = samp, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)

forestfires_model <- nnet(Size_Category ~ .-size_category,data = forestfires_train,hidden=10,linear.output = F)

#Neural Network
library(neuralnet)
nn <- neuralnet(size_category ~ . , data=forestfires_train, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
str(nn)
plot(nn)


#Test the resulting outp
forest_test <- subset(forestfires, select = c("fcfps","earnings_growth", "de", "mcap", "current_ratio"))
head(temp_test)
nn.results <- compute(nn, temp_test)
dim(forestfires1)

model_results <- compute(nn,forestfires_test[1:28])
#results <- data.frame(actual = testset$dividend, prediction = nn.results$net.result)

str(model_results)
predicted_Profit <- model_results$net.result

cor(predicted_Profit,forestfires_test$size_category)
plot(predicted_Profit,forestfires_test$size_category)

roundedresults<-sapply(model_results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)


model_5<-neuralnet(size_category ~ . , data=forestfires_train, hidden=c(3,5), linear.output=FALSE, threshold=0.01)
plot(model_5)
model_5_res<-compute(model_5,forestfires_test[1:28])
pred_5<-model_5_res$net.result
cor(pred_5,forestfires_test$size_category)
plot(pred_profit_5,forestfires_test$size_category)
