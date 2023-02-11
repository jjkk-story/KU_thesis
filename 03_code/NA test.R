
### NA 데이터프레임 생성
a <- data.frame(t(rep(NA, 12)))
a <- rbind(a, a)

### 53~57차
names(a) <- c("region", "SQ2", "SQ3", "Q1", "Q2", "agree",
              "Q6", "Q7", "DQ1", "DQ2", "DQ3", "DQ4")

### 58~59차, 61~66차
names(a) <- c("region", "SQ2", "SQ3", "Q1", "Q2", "agree",
              "Q7", "Q8", "DQ1", "DQ2", "DQ3", "DQ4")

### 67차
names(a) <- c("region", "SQ2", "SQ3", "Q7", "Q8", "agree",
              "Q5", "Q6", "DQ1", "DQ2", "DQ3", "DQ4")

for(i in 1:ncol(a)){
  a[,i] <- as.factor(a[,i])
}

for(i in 1:ncol(a)){
  levels(a[,i]) <- levels(data67_tr[,i]) 
}

### 다항 로지스틱 회귀분석
predict(all, a)

### 의사결정나무
predict(tree, a, type="class")

### 랜덤포레스트
predict(rf, a, type="class")

############# +안철수

### 다항 로지스틱 회귀분석
predict(all3, a)

### 의사결정나무
predict(tree3, a, type="class")

### 랜덤포레스트
predict(rf3, a, type="class")
