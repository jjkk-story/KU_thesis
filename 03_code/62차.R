library(dplyr)
library(nnet)
library(rpart)
library(rpart.plot)
library(randomForest)

data62 <- read.csv("C:/Users/samsung/Desktop/(NBS) 53-67차 데이터/data62.csv")

data62_1 <- select(data62, -c(ID, SQ1, AGE, Q2_1, Q3, Q6, WT))
data62_1 <- data62_1[c(12,1,2,3,4,13,6,7,8,9,10,11,5)]

for(i in 1:ncol(data62_1)){
  data62_1[,i] <- as.factor(data62_1[,i])
}

data62_te <- subset(data62_1, Q4 == 98 | Q4 == 99)
data62_tr <- subset(data62_1, Q4 != 98 & Q4 != 99)

data62_tr$Q4 <- droplevels(data62_tr$Q4)

### 다항 로지스틱 회귀분석 (없다 & 모름/무응답)
all <- multinom(Q4 ~ 0 + ., data=data62_tr)
pred <- predict(all, data62_te)

### 의사결정나무
tree <- rpart(Q4 ~ ., data = data62_tr, method = "class", cp = -1)
pred_tree <- predict(tree, data62_te, type = "class")

rpart.plot(tree)
tree$variable.importance

### 랜덤포레스트
set.seed(11)

rf <- randomForest(Q4 ~ ., data = data62_tr, ntree = 100, mtry = 12)
pred_rf <- predict(rf, data62_te, type = "class")

############# +안철수

data62_te3 <- subset(data62_1, Q4 == 98 | Q4 == 99 | Q4 == 4)
data62_tr3 <- subset(data62_1, Q4 != 98 & Q4 != 99 & Q4 != 4)

data62_tr3$Q4 <- droplevels(data62_tr3$Q4)

### 다항 로지스틱 회귀분석 (없다 & 모름/무응답 & 안철수)
all3 <- multinom(Q4 ~ 0 + ., data=data62_tr3)
pred3 <- predict(all3, data62_te3)

### 의사결정나무
tree3 <- rpart(Q4 ~ ., data = data62_tr3, method = "class", cp = -1)
pred_tree3 <- predict(tree3, data62_te3, type = "class")

rpart.plot(tree3)
tree3$variable.importance

### 랜덤포레스트
set.seed(13)

rf3 <- randomForest(Q4 ~ ., data = data62_tr3, ntree = 100, mtry = 12)
pred_rf3 <- predict(rf3, data62_te3, type = "class")

#############

### 다항 로지스틱 회귀분석
data62_te$Q4 <- pred
result <- rbind(data62_tr, data62_te)
write.csv(result, "C:/Users/samsung/Desktop/table/result.csv")
result <- read.csv("C:/Users/samsung/Desktop/table/result.csv")
result <- result %>% arrange(X)
result <- cbind(result, data62$WT)
colnames(result)[15] <- "WT"
result <- result[,-1]
a <- sum(result$WT)
s1 <- data.frame(result %>% group_by(Q4) %>%
                   summarise(freq = sum(WT)/a))
s1 <- data.frame(matrix(s1[,2], nrow = 1))

data62_te3$Q4 <- pred3
result3 <- rbind(data62_tr3, data62_te3)
write.csv(result3, "C:/Users/samsung/Desktop/table/result3.csv")
result3 <- read.csv("C:/Users/samsung/Desktop/table/result3.csv")
result3 <- result3 %>% arrange(X)
result3 <- cbind(result3, data62$WT)
colnames(result3)[15] <- "WT"
result3 <- result3[,-1]
a <- sum(result3$WT)
s2 <- data.frame(result3 %>% group_by(Q4) %>%
                   summarise(freq = sum(WT)/a))
s2 <- data.frame(matrix(s2[,2], nrow = 1))

### 의사결정나무
data62_te$Q4 <- pred_tree
result_tree <- rbind(data62_tr, data62_te)
write.csv(result_tree, "C:/Users/samsung/Desktop/table/result_tree.csv")
result_tree <- read.csv("C:/Users/samsung/Desktop/table/result_tree.csv")
result_tree <- result_tree %>% arrange(X)
result_tree <- cbind(result_tree, data62$WT)
colnames(result_tree)[15] <- "WT"
result_tree <- result_tree[,-1]
a <- sum(result_tree$WT)
s3 <- data.frame(result_tree %>% group_by(Q4) %>%
                   summarise(freq = sum(WT)/a))
s3 <- data.frame(matrix(s3[,2], nrow = 1))

data62_te3$Q4 <- pred_tree3
result_tree3 <- rbind(data62_tr3, data62_te3)
write.csv(result_tree3, "C:/Users/samsung/Desktop/table/result_tree3.csv")
result_tree3 <- read.csv("C:/Users/samsung/Desktop/table/result_tree3.csv")
result_tree3 <- result_tree3 %>% arrange(X)
result_tree3 <- cbind(result_tree3, data62$WT)
colnames(result_tree3)[15] <- "WT"
result_tree3 <- result_tree3[,-1]
a <- sum(result_tree3$WT)
s4 <- data.frame(result_tree3 %>% group_by(Q4) %>%
                   summarise(freq = sum(WT)/a))
s4 <- data.frame(matrix(s4[,2], nrow = 1))

### 랜덤포레스트
data62_te$Q4 <- pred_rf
result_rf <- rbind(data62_tr, data62_te)
write.csv(result_rf, "C:/Users/samsung/Desktop/table/result_rf.csv")
result_rf <- read.csv("C:/Users/samsung/Desktop/table/result_rf.csv")
result_rf <- result_rf %>% arrange(X)
result_rf <- cbind(result_rf, data62$WT)
colnames(result_rf)[15] <- "WT"
result_rf <- result_rf[,-1]
a <- sum(result_rf$WT)
s5 <- data.frame(result_rf %>% group_by(Q4) %>%
                   summarise(freq = sum(WT)/a))
s5 <- data.frame(matrix(s5[,2], nrow = 1))

data62_te3$Q4 <- pred_rf3
result_rf3 <- rbind(data62_tr3, data62_te3)
write.csv(result_rf3, "C:/Users/samsung/Desktop/table/result_rf3.csv")
result_rf3 <- read.csv("C:/Users/samsung/Desktop/table/result_rf3.csv")
result_rf3 <- result_rf3 %>% arrange(X)
result_rf3 <- cbind(result_rf3, data62$WT)
colnames(result_rf3)[15] <- "WT"
result_rf3 <- result_rf3[,-1]
a <- sum(result_rf3$WT)
s6 <- data.frame(result_rf3 %>% group_by(Q4) %>%
                   summarise(freq = sum(WT)/a))
s6 <- data.frame(matrix(s6[,2], nrow = 1))

#############

t1 <- read.csv("C:/Users/samsung/Desktop/table/freq.csv")
t2 <- read.csv("C:/Users/samsung/Desktop/table/freq3.csv")
t3 <- read.csv("C:/Users/samsung/Desktop/table/freq_tree.csv")
t4 <- read.csv("C:/Users/samsung/Desktop/table/freq_tree3.csv")
t5 <- read.csv("C:/Users/samsung/Desktop/table/freq_rf.csv")
t6 <- read.csv("C:/Users/samsung/Desktop/table/freq_rf3.csv")

s1 <- rbind(t1, s1)
s2 <- rbind(t2, s2)
s3 <- rbind(t3, s3)
s4 <- rbind(t4, s4)
s5 <- rbind(t5, s5)
s6 <- rbind(t6, s6)

write.csv(s1, file="C:/Users/samsung/Desktop/table/freq.csv", row.names = F)
write.csv(s2, file="C:/Users/samsung/Desktop/table/freq3.csv", row.names = F)
write.csv(s3, file="C:/Users/samsung/Desktop/table/freq_tree.csv", row.names = F)
write.csv(s4, file="C:/Users/samsung/Desktop/table/freq_tree3.csv", row.names = F)
write.csv(s5, file="C:/Users/samsung/Desktop/table/freq_rf.csv", row.names = F)
write.csv(s6, file="C:/Users/samsung/Desktop/table/freq_rf3.csv", row.names = F)
