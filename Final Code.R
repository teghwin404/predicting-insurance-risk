# installing all packages needed
install.packages('tree')
install.packages('rpart.plot')
install.packages("randomForest")
install.packages("gbm")
install.packages("MASS")
install.packages("klaR")
install.packages("readr")
install.packages("GGally")
install.packages("ggfortify")
library(ggfortify)
library(GGally)
library(readr)
library(boot)
library(MASS)
library(klaR)
library(ggplot2)
library(gbm)
library(randomForest)
library(tree)
library(rpart)
library(rpart.plot)
require(caTools)
require(methods)

#_____________________________________________________

# importing dataset
auto_raw = read_csv(#file path here (omitted to ensure anonymity) ,
                                              col_types = cols(symboling = col_number(),
                                                                 normalized_losses = col_number(),
                                                                 wheel_base = col_number(), length = col_number(),
                                                                 width = col_number(), height = col_number(),
                                                                 curb_weight = col_number(), engine_size = col_number(),
                                                                 bore = col_number(), stroke = col_number(),
                                                                 compression_ratio = col_number(),
                                                                 horsepower = col_number(), peak_rpm = col_number(),
                                                                 city_mpg = col_number(), highway_mpg = col_number(),
                                                                 price = col_number()))  
auto=auto_raw # making a copy

attach(auto)

#_____________________________________________________

## converting categorical variables as factor

auto$make=as.factor(auto$make)
auto$fuel_type=as.factor(auto$fuel_type)
auto$aspiration=as.factor(auto$aspiration)
auto$num_of_doors=as.factor(auto$num_of_doors)
auto$body_style=as.factor(auto$body_style)
auto$drive_wheels=as.factor(auto$drive_wheels)
auto$engine_location=as.factor(auto$engine_location)
auto$engine_type=as.factor(auto$engine_type)
auto$num_of_cylinders=as.factor(auto$num_of_cylinders)
auto$fuel_system=as.factor(auto$fuel_system)

#_____________________________________________________

## checking for NAs

summary(auto)

auto=auto[ -c(2) ] # dropped normalized losses column because 20% is NA
auto=na.omit(auto) # removing NAs
auto=auto[-c(27,57),] # removing ? from num_of_doors column

attach(auto)

#_____________________________________________________

## exploring the dataset
symboling=as.factor(symboling) # converting symboling as categorical to make a bar plot
barplot(summary(symboling), main="Symboling Frequency",ylab = "Frequencies",col = 'orange')

symboling=as.numeric(symboling) # converting back to numeric 
hist(price)

#_____________________________________________________

## checking for multicolineariy 

attach(auto)

quantvars=auto[,c(1,9,10,11,12,13,16,18,19,20,21,22,23,24,25)]
  
corr_matrix=data.frame(cor(quantvars))
write.csv(corr_matrix, 
          #file path here (omitted to ensure anonymity), 
          row.names=TRUE) # code to export table

## 1. Using PCA to pre-assess model

pca_auto=prcomp(na.omit(quantvars), scale=TRUE)
quantvars$safety=ifelse(auto_pca$symboling<=-1,"Safe",ifelse(auto_pca$symboling<=1,"Neutral","High Risk")) # assigning strings based on rating to make graph
autoplot(pca_auto, 
         data = na.omit(quantvars), 
         loadings = TRUE, 
         loadings.colour = 'dark grey', 
         loadings.label.colour='dark grey',
         colour="safety", 
         loadings.label = TRUE)
         
#_____________________________________________________

## 2. Decision Trees 

tree0=rpart(symboling~.,data=auto,control=rpart.control(cp=0.01)) # initial tree
rpart.plot(tree0)

tree1=rpart(symboling~., data=auto,control=rpart.control(cp=0.00000001)) # most complex tree
rpart.plot(tree1)
summary(tree1)
printcp(tree1)
plotcp(tree1)

opt_cp=tree1$cptable[which.min(tree1$cptable[,'xerror']),"CP"] # finding optimal CP
opt_cp

tree1x=rpart(symboling~., data=auto,control=rpart.control(cp=opt_cp)) # most optimal tree
rpart.plot(tree1x, col = c("green","blue", "red" ), cex = 0.5)

#_____________________________________________________

## 3. Random forest 
auto2=auto
forest1=randomForest(symboling~make+fuel_type+aspiration+num_of_doors+body_style+drive_wheels+engine_location+wheel_base+length+width+height+curb_weight+engine_type+num_of_cylinders+engine_size+fuel_system+bore+stroke+compression_ratio+horsepower+peak_rpm+city_mpg+highway_mpg+price,
                     ntree=3000, data=auto2, importance=TRUE, na.action = na.omit) 
forest1
imp_forest1=data.frame(importance(forest1))
imp_forest1
write.csv(imp_forest1,
          #file path here (omitted to ensure anonymity),
          row.names=TRUE)

varImpPlot(forest1, col='orange')

  #### testing out of bag error 
  test_forest1=randomForest(symboling~make+fuel_type+aspiration+num_of_doors+body_style+drive_wheels+engine_location+wheel_base+length+width+height+curb_weight+engine_type+num_of_cylinders+engine_size+fuel_system+bore+stroke+compression_ratio+horsepower+peak_rpm+city_mpg+highway_mpg+price
                            , ntree=10000, data=auto2, importance=TRUE, na.action = na.omit, do.trace=200) 
  
  #### finding RSS
  auto2$predicted_symbol=predict(forest1, newdata=auto2, n.trees=1200)
  mean((auto2$predicted_symbol - auto2$symboling)^2)

#_____________________________________________________
  
## 4. Boosting

#### splitting the dataset
sample = sample(c(TRUE, FALSE), nrow(auto), replace=TRUE, prob=c(0.7,0.3))
train=subset(auto, sample==TRUE)
test=subset(auto, sample==FALSE)

set.seed(1)

# boosted1a: all variables with training dataset
boosted1a=gbm(symboling~.,data=train, distribution="gaussian", n.trees=20000, interaction.depth=5)
summary(boosted1a)

  #### finding MSE
  test$predicted_symbol=predict(boosted1a, newdata=test, n.trees=20000)
  round(mean((test$predicted_symbol - test$symboling)^2),4)
  
######

# boosted1b: to find RSS on full dataset
auto3=auto
boosted1b=gbm(symboling~.,data=auto3, distribution="gaussian", n.trees=20000, interaction.depth=5)
summary(boosted1b, col='orange')
summary_1b

write.csv(summary_1b, 
          #file path here (omitted to ensure anonymity), 
          row.names=FALSE) # command to export table

  #### RSS
  auto3$predicted_symbol=predict(boosted1b, newdata=auto3, n.trees=20000)
  mean((auto3$predicted_symbol - auto3$symboling)^2)

######

# boosted2: removing problematic variables (engine_location, fuel_type, aspiration, drive_wheels,engine_type,fuel_system)
boosted2=gbm(symboling~make+num_of_doors+body_style+wheel_base+length+width+height+curb_weight+num_of_cylinders+engine_size++bore+stroke+compression_ratio+horsepower+peak_rpm+city_mpg+highway_mpg+price
            ,data=train,distribution="gaussian", n.trees=20000, interaction.depth=5)
summary(boosted2)

  #### finding MSE
  test$predicted_symbol=predict(boosted2, newdata=test, n.trees=20000)
  round(mean((test$predicted_symbol - test$symboling)^2),4)

######

# boosted3: all variables with full dataset using cross validation
boosted3=gbm(symboling~.,data=auto, distribution="gaussian", 
             n.trees=20000, interaction.depth=5, cv.folds=50)
summary(boosted3)

  #### finding MSE
  best=which.min(boosted3$cv.error)
  mse=boosted3$cv.error[best]
  mse 

#_____________________________________________________
# MSE for random forest using the same train-test dataset

forest_test=randomForest(symboling~make+fuel_type+aspiration+num_of_doors+body_style+drive_wheels+engine_location+wheel_base+length+width+height+curb_weight+engine_type+num_of_cylinders+engine_size+fuel_system+bore+stroke+compression_ratio+horsepower+peak_rpm+city_mpg+highway_mpg+price,
                      ntree=1200, data=train, importance=TRUE, na.action = na.omit)
  
  #### finding MSE
  test$predicted_symbol=predict(forest_test, newdata=test, n.trees=20000) # making predictions
  round(mean((test$predicted_symbol - test$symboling)^2),4) # calculating MSE

  
#_____________________________________________________

# 5. Clustering

auto_c=auto[,c("symboling", "wheel_base")]

km.3=kmeans(auto_c,3)
km.3

auto_c$cluster=as.factor(km.3$cluster)
plot=ggplot(auto_c,aes(y=height, x=symboling))
plot+geom_point(aes(colour=cluster))


