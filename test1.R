#导入数据并定义#

licai <- read.table("C:/Users/17621/Desktop/理财副本.csv", header=TRUE,sep=",")
library(plyr)
licai <- rename(licai,c(性别="gender",文化程度="education level",省="province",婚姻状况="married",个人是否有经管类专业学习经历="experience of finance",
                          投资风险厌恶水平="The level of risk reverse",金融知识1="finance problem1",金融知识2="finance problem2",
                          金融知识题回答情况="The condition of the answer of financial problems",
                          房地产="real estate",基金="fund",股票="stock",债券="bond",互联网理财="internet planning",金融理财="financial planning",金融衍生品="derivative",黄金="gold",
                          非人民币资产="no RMB asset",养老保险="pension insurance",
                          医疗保险="medical issurance",商业保险="business issurance",教育投资="educational investment"))
myvar <- c("gender","education level","province","married","experience of finance",
           "The level of risk reverse","finance problem1","finance problem2","The condition of the answer of financial problems",
           "real estate","fund","stock","bond","internet planning","financial planning","derivative","gold","no RMB asset","pension insurance",
           "medical issurance","business issurance","educational investment")
licai1 <- licai[myvar]
licai1$`education level`<- as.numeric(licai1$`education level`)
licai1$`experience of finance`<- as.numeric(licai1$`experience of finance`)
licai1$married <- as.numeric(licai1$married)
licai1$`real estate` <- factor(licai1$`real estate`,
                               levels=c(0,1),
                               labels = c("No","Yes"))
licai1$fund <- factor(licai1$fund,
                      levels=c(0,1),
                      labels = c("No","Yes"))
licai1$stock <- factor(licai1$stock,
                       levels=c(0,1),
                       labels = c("No","Yes"))
licai1$bond <- factor(licai1$bond,
                      levels=c(0,1),
                      labels = c("No","Yes"))
licai1$derivative <- factor(licai1$derivative,
                            levels=c(0,1),
                            labels = c("No","Yes"))
licai1$gold <- factor(licai1$gold,
                      levels=c(0,1),
                      labels = c("No","Yes"))
licai1$`internet planning` <- factor(licai1$`internet planning`,
                                     levels=c(0,1),
                                     labels = c("No","Yes"))
licai1$`financial planning` <- factor(licai1$`financial planning`,
                                      levels=c(0,1),
                                      labels = c("No","Yes"))
licai1$`pension insurance` <- factor(licai1$`pension insurance`,
                                     levels=c(0,1),
                                     labels = c("No","Yes"))
licai1$`medical issurance`<- factor(licai1$`medical issurance`,
                                    levels=c(0,1),
                                    labels = c("No","Yes"))
licai1$`business issurance` <- factor(licai1$`business issurance`,
                                      levels=c(0,1),
                                      labels = c("No","Yes"))
licai1$`educational investment` <- factor(licai1$`educational investment`,
                                          levels=c(0,1),
                                          labels = c("No","Yes"))
licai1$`no RMB asset` <- factor(licai1$`no RMB asset`,
                                levels=c(0,1),
                                labels = c("No","Yes"))
licai1$`finance problem1`<- factor(licai1$`finance problem1`,
                                   levels=c(0,1),
                                   labels = c("No","Yes"))
licai1$`finance problem2`<- factor(licai1$`finance problem2`,
                                   levels=c(0,1),
                                   labels = c("No","Yes"))


#检验模型#

fit <- glm(stock~licai1$`The level of risk reverse`+licai1$`The condition of the answer of financial problems`,data = licai1,family = binomial())
summary(fit)
par(mfrow=c(2,2))
plot(fit)


fit1 <- glm(stock~licai1$`The condition of the answer of financial problems`+licai1$`The level of risk reverse`+
              licai1$gender+licai1$`education level`+licai1$`experience of finance`,data = licai1,family = binomial())
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)


fit2 <- glm(stock~licai1$`The condition of the answer of financial problems`+licai1$`The level of risk reverse`+
              licai1$`education level`,data = licai1,family = binomial()) 
anova(fit1,fit2)
AIC(fit1,fit2)

library(leaps)
leap <- regsubsets(stock~licai1$`The condition of the answer of financial problems`+licai1$`The level of risk reverse`+
                     licai1$gender+licai1$`education level`+licai1$`experience of finance`+married,data = licai1, nbest = 6)
plot(leap,scale="adjr2")

leap1 <- regsubsets(stock~licai1$`The condition of the answer of financial problems`+licai1$`The level of risk reverse`+
                      licai1$`education level`,data = licai1,nbest = 3)
plot(leap1,scale="adjr2")
