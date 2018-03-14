### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
iris

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris,6)

# 使用tail() 回傳iris的後六列
tail(iris,6)

# 使用str() 
str(iris)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)
for (i in c(1:9)){
  for (j in c(1:9)){
   print(paste0(i,'x',j,"=",i*j)) 
  }
}


########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
nums<- sample(c(10:100),10)

# 查看nums
nums 

# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。
for (i in nums){
  if (i>50 & i%%2==0){
    print(paste("偶數且大於50",":",i))
  }
  if (i==66){
    print("太66666666666了")
    break
  }
}
  
  
  

########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年

year <- 2000 #請在此輸入西元年分

if(year%%4 !=0){
  print(paste(year,"年為平年"))
}else if(year%%4==0 & year%%100!=0){
  print(paste(year,"年為閏年"))
}else if(year%%100==0 & year%%400!=0){
  print(paste(year,"年為平年"))
}else if(year%%400==0){
  print(paste(year,"年為閏年"))
}


########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

#設定亂數4個不重複四位數
ans <- sample(0:9,4)
ans
ct <- 0

while (TRUE){
  print("請輸入4個不重複的數")
  gus = scan(nmax=4)
  ct <- ct+1
  a <- 0
  b <- 0
  for(i in seq(1,4)){
    if(gus[i]==ans[i]){
      a<-a+1
    }
    for(j in seq(1,4)){
      if (gus[j]==ans[i] & i!=j){
        b<-b+1
      }
    }
  }
  print(paste0(a,"A",b,"B"))
  if(a==4 & b==0){
    print("恭喜你猜中了!")
    print(paste("您總共輸入",ct,"次"))
    break
  }
}





