olympic <- read.csv("C:/Users/luyif/OneDrive/Desktop/olympic dataset.csv",header=TRUE, sep=",")
View(olympic)
summary(olympic)
library(ggplot2)

# Data cleansing -- dealing with missing values(Figure 1 Missing Data)
sum(is.na(olympic))
library(mice)
par(mfrow = c(1, 1))
md.pattern(olympic, plot = TRUE)

#Due to the randomness of medal types, medal players are uniformly marked as 1 and others are marked as 0.
olympic$Medal[!is.na(olympic$Medal)] <- 1

olympic$Medal[is.na(olympic$Medal)] <- 0
# Delete other data with missing values
olympic <- olympic[complete.cases(olympic), ]
olympic_backup<-olympic

x <- table(olympic$Medal)
Medal_percent <- round(100 * x / sum(x), 1)
Medal_percent <- paste(Medal_percent, "%", sep = "")
Medal_percent
count_m1 <- sum(olympic$Medal == 1)
count_m0 <- sum(olympic$Medal == 0)
count_df <- data.frame(
  Gender = c("Medal", "No_Medal"),
  Count = c(count_m1, count_m0)
)

#Figure 2 Comparison of the amount of data on whether medals were won or not
ggplot(count_df, aes(x = Gender, y = Count)) +
  geom_bar(stat = "identity") +
  labs(x = "Medal or not", y = "Count", title = "Count of Medal") +
  theme_minimal()

#Sex, age, height, weight distribution
options(scipen = 1)
count_m <- sum(olympic$Sex == "M")
count_f <- sum(olympic$Sex == "F")
count_df <- data.frame(
  Gender = c("M", "F"),
  Count = c(count_m, count_f)
)
#Figure 3 Percentage of athletes of different genders
ggplot(count_df, aes(x = Gender, y = Count)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Count", title = "Count of M and F") +
  theme_minimal()

#Figure 4 Age difference of athletes of different genders
ggplot(olympic, aes(group = Sex,x=Sex, y = Age)) +
  scale_y_log10() +
  geom_boxplot()

#Figure 5 Difference in height of athletes of different genders
ggplot(olympic, aes(group = Sex,x=Sex, y = Height)) +
  scale_y_log10() +
  geom_boxplot()

#Figure 6 Difference in weight of athletes of different genders
ggplot(olympic, aes(group = Sex,x=Sex, y = Weight)) +
  scale_y_log10() +
  geom_boxplot()

summary(olympic)

# Basic data analysis

library(ggplot2)
library(RColorBrewer)
library(Rmisc)
olympic$number<-1

# Sex impact on award rates
groupdata <- aggregate(
  x = olympic[c("number")],
  by = list(sex = olympic$Sex, Medal = olympic$Medal),
  FUN = sum
)
summary(groupdata)
groupdata$Medal <- as.factor(groupdata$Medal)

groupdata[which(groupdata$sex == "F"), 1] <- "Female"
groupdata[which(groupdata$sex == "M"), 1] <- "Male"
groupdata$sex <- as.factor(groupdata$sex)

groupdata

#Figure 7 Probability of winning an award by sex
p1 <- ggplot(
  data = groupdata,
  aes(x = sex, y = number, fill = Medal)
) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_text(aes(label = number), position = position_dodge(width = 1), vjust = -0.5, color = "black", size = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, colour = "black", size = 20),
    axis.text.y = element_text(size = 16, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain"),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black", size = 1),
    legend.text = element_text(face = "italic", colour = "black", size = 16),
    legend.title = element_text(face = "italic", colour = "black", size = 18),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_line() +
  labs(x = "Sex", y = "Number", title = "Medal rate for different sex") +
  labs(fill = "Medal")
p1

# Country impact on award rates
groupdata <- aggregate(
  x = olympic[c("number")],
  by = list(NOC = olympic$NOC, Medal = olympic$Medal),
  FUN = sum
)
groupdata$Medal <- as.factor(groupdata$Medal)
table(olympic$NOC)
groupdata$number[groupdata$Medal == 1]
tabledata <- 1-groupdata$number[groupdata$Medal == 0]/table(olympic$NOC)
#Figure 8 Probability of winning an award by country
barplot(tabledata, space = 0.7)

#Clustering the body data of the metal players using K-Means
medal_player <- subset(olympic, Medal == 1)
# Body data clustering
data <- medal_player[,c(3,4,5,6)] 
data$Sex <-as.numeric(as.character(factor(data$Sex, labels = c(0, 1))))
summary(data)
data_scale <- scale(data) 
View(data_scale)

#Unsupervised learning - data is divided into 4 categories
k <- kmeans(data_scale, 4)
k$cluster
medal_player
medal_player$classification<-k$cluster

#Percentage of males
class1<-subset(medal_player, classification == 1)
class2<-subset(medal_player, classification == 2)
class3<-subset(medal_player, classification == 3)
class4<-subset(medal_player, classification == 4)
Mrate <- c(sum(class1$Sex == "M")/nrow(class1),sum(class2$Sex == "M")/nrow(class2),sum(class3$Sex == "M")/nrow(class3),sum(class4$Sex == "M")/nrow(class4))
Mrate

#Body Data Comparison
#Figure 10 Age distribution of the four groups of data
ggplot(medal_player, aes(group = classification,x=classification, y = Age)) +
  scale_y_log10() +
  geom_boxplot()

#Figure 11 Height distribution of the four data sets
ggplot(medal_player, aes(group = classification,x=classification, y = Height)) +
  scale_y_log10() +
  geom_boxplot()

#Figure 12 Weight distribution of the four data sets
ggplot(medal_player, aes(group = classification,x=classification, y = Weight)) +
  scale_y_log10() +
  geom_boxplot()
  
# Awarded sports
table_result1 <- as.data.frame(table(class1$Sport))
table_result2 <- as.data.frame(table(class2$Sport))
table_result3 <- as.data.frame(table(class3$Sport))
table_result4 <- as.data.frame(table(class4$Sport))
table_result1
library(wordcloud2)
#Figure 13 Word cloud statistics for the four data sets
wordcloud2(table_result1, size = 1, fontFamily = NULL, fontWeight = 'normal', color =
             'random-dark', backgroundColor = "white", minRotation = -pi/4, maxRotation 
           = pi/4, rotateRatio = 0.4, shape = 'circle')
wordcloud2(table_result2, size = 1, fontFamily = NULL, fontWeight = 'normal', color =
             'random-dark', backgroundColor = "white", minRotation = -pi/4, maxRotation 
           = pi/4, rotateRatio = 0.4, shape = 'circle')
wordcloud2(table_result3, size = 1, fontFamily = NULL, fontWeight = 'normal', color =
             'random-dark', backgroundColor = "white", minRotation = -pi/4, maxRotation 
           = pi/4, rotateRatio = 0.4, shape = 'circle')
wordcloud2(table_result4, size = 1, fontFamily = NULL, fontWeight = 'normal', color =
             'random-dark', backgroundColor = "white", minRotation = -pi/4, maxRotation 
           = pi/4, rotateRatio = 0.4, shape = 'circle')


#basketball players
basketball<-subset(olympic,Sport=="Basketball")

# Model feature extraction, selecting gender, age, height, weight as independent variables and whether medal as dependent variable
basketball<-basketball[,c(3,4,5,6,15)]



basketball$Sex <-as.numeric(as.character(factor(basketball$Sex, labels = c(0, 1))))

basketball$Medal <- as.factor(basketball$Medal)

# Model building
# Build training and test datasets
library(caret)
sss <- createDataPartition(y = basketball$Medal, p = 0.80, list = FALSE)
train <- basketball[sss, ] 
test <- basketball[-sss, ] 



#evaluation function
result <- data.frame(row.names = c("recall", "precision", "F1"))

pre <- function(table, rname) {
  recall <- table[2, 2] / (table[2, 2] + table[2, 1])
  precision <- table[2, 2] / (table[2, 2] + table[1, 2])
  F1 <- 2 * table[2, 2] / (2 * table[2, 2] + table[2, 1] + table[1, 2])
  result <<- cbind(result, c(recall, precision, F1)) 
  names(result) <<- rname
  print(result)
}




#Random Forest
library(randomForest)
rname <- c("Random Forest")
rf_model <- randomForest(Medal ~ ., data = train, ntree = 500,mtry=2,importance=TRUE,proximity=TRUE)
rf_predict <- predict(rf_model, test)
rf_table <- table(actual = test$Medal, predict = rf_predict)
pre(rf_table, rname)
#Logistic Regression
rname <- cbind(rname, c("LogisticRegression"))
glm_model <- glm(Medal ~ ., data = train, family = binomial(link = "logit"))
glm_predict <- predict(glm_model, test)
glm_predict <- round(exp(glm_predict) / (1 + exp(glm_predict)))
glm_table <- table(actual = test$Medal, predict = glm_predict)
pre(glm_table, rname)
#Decision Tree
library(C50)
rname <- cbind(rname, c("Decision Tree"))
dt_model <- C5.0(x = train[-18], y = train$Medal)
dt_predict <- predict(dt_model, test)
dt_table <- table(actual = test$Medal, predict = dt_predict)
pre(dt_table, rname)