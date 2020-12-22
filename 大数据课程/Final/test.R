###  Load Packages
library(plyr)
library(VGAM)
library(stargazer)
library(AER)
library(car)


###  导入数据   ===================================

licai <- read.table("理财副本.csv", header=TRUE,sep=",")
licai <- rename(licai,c(性别="gender",文化程度="education level",省="province",婚姻状况="marital status",个人是否有经管类专业学习经历="experience of finance",
                          投资风险厌恶水平="The level of risk reverse",金融知识1="finance problem 1",金融知识2="finance problem 2",
                          金融知识题回答情况="The condition of the answer of financial problems",
                          教育投资="educational investment", 参与金融市场 = "financial market",
                          是否工作 = "work",房地产 = "real estate", 股票 = "stock", 风险资产占比 = "financial asset",股票资产占比 = "stock assset"))
myvar <- c("The level of risk reverse","The condition of the answer of financial problems",
           "stock","financial asset", "stock assset","financial market",
           "province","gender","education level","marital status","educational investment",
           "experience of finance",
           "real estate", "work","finance problem 1","finance problem 2")
licai1 <- licai[myvar]

licai1$`education level` <- as.numeric(licai1$`education level`)
licai1$`marital status` <- as.numeric(licai1$`marital status`)
licai1$`experience of finance` <- as.numeric(licai1$`experience of finance`)
df <- licai1

### 回归分析   ======================================
## Probit Model =====================================

fit1 <- lm(df$`The level of risk reverse`~df$gender+df$work+df$`education level`+df$`marital status`+
            df$`experience of finance`+df$`real estate` +df$`educational investment`,data = df)

fit2 <- lm(df$`The condition of the answer of financial problems`~df$gender+df$work+df$`education level`+df$`marital status`+
             df$`experience of finance`+df$`real estate` +df$`educational investment`,data = df)

probit_stock_sim <- glm(df$stock~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`,
                        data = df,family = binomial(link = "probit"))

probit_stock <- glm(df$stock~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
            df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
            df$`real estate`+ df$`educational investment`,data = df,family = binomial(link = "probit"))


probit_market_sim <- glm(df$`financial market`~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`,
                        data = df,family = binomial(link = "probit"))

probit_market <- glm(df$`financial market`~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
              df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
              df$`real estate`+ df$`educational investment`,data = df,family = binomial(link = "probit"))

ivprobit_stock <- ivreg(df$stock~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
                          df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                          df$`real estate`+ df$`educational investment` | df$`The level of risk reverse`+df$`finance problem 2`+
                          df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                          df$`real estate`+ df$`educational investment`,data = df)

ivprobit_market <- ivreg(df$`financial market`~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
                           df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                           df$`real estate`+ df$`educational investment` | df$`The level of risk reverse`+df$`finance problem 2`+
                           df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                           df$`real estate`+ df$`educational investment`,data = df)

summary(ivprobit_stock,diagnostics = T)
summary(ivprobit_market,diagnostics = T)


stargazer(probit_stock_sim,probit_stock,ivprobit_stock,probit_market_sim,probit_market,ivprobit_market,title = "金融知识、风险厌恶水平对股票市场参与和金融市场参与的影响", 
          align = T, dep.var.labels = c( "参与股票市场", "参与金融市场")
          ,covariate.labels = c("风险厌恶水平","金融知识","性别","是否工作","教育水平","婚姻状况",
                                "经管类专业学习经历","有房","有小孩","常数项"),
          omit.stat = c("LL", "aic","ser"), no.space = T, type = "html", out = "Probit.html")

stargazer(probit_stock_sim,probit_stock,ivprobit_stock,probit_market_sim,probit_market,ivprobit_market,title = "金融知识、风险厌恶水平对股票市场参与和金融市场参与的影响", 
          align = T, dep.var.labels = c("参与股票市场", "参与金融市场" )
          ,covariate.labels = c("风险厌恶水平","金融知识","性别","是否工作","教育水平","婚姻状况",
                                "经管类专业学习经历","有房","有小孩","常数项"),
          omit.stat = c("LL", "aic","ser"), no.space = T, type = "html", out = "Probit.doc")

##  Tobit Model =================================

tobit_stockasset <- tobit(df$`stock assset`~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
                            df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                            df$`real estate`+ df$`educational investment`,data = df)

tobit_financialasset <- tobit(df$`financial asset`~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
                                df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                                df$`real estate`+ df$`educational investment`,data = df)

ivtobit_stockasset <- ivreg(df$`stock assset`~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
                              df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                              df$`real estate`+ df$`educational investment` | df$`The level of risk reverse`+df$`finance problem 2`+
                              df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                              df$`real estate`+ df$`educational investment`,data = df)

ivtobit_financialasset <- ivreg(df$`financial asset`~df$`The level of risk reverse`+df$`The condition of the answer of financial problems`+
                              df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                              df$`real estate`+ df$`educational investment` | df$`The level of risk reverse`+df$`finance problem 2`+
                              df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                              df$`real estate`+ df$`educational investment`,data = df)

summary(ivtobit_stockasset,diagnostics = T)
summary(ivtobit_financialasset,diagnostics = T)

stargazer(tobit_stockasset,ivtobit_stockasset,tobit_financialasset,ivtobit_financialasset,title = "金融知识、风险厌恶水平对家庭资产选择的影响", 
          align = T, dep.var.labels = c("股票资产占比", "风险资产占比" )
          ,covariate.labels = c("风险厌恶水平","金融知识","性别","是否工作","教育水平","婚姻状况",
                                "经管类专业学习经历","有房","有小孩","常数项"),
          omit.stat = c("LL", "aic","ser"), no.space = T, type = "html", out = "Tobit.html")

stargazer(tobit_stockasset,ivtobit_stockasset,tobit_financialasset,ivtobit_financialasset,title = "金融知识、风险厌恶水平对家庭资产选择的影响", 
          align = T, dep.var.labels = c("股票资产占比", "风险资产占比" )
          ,covariate.labels = c("风险厌恶水平","金融知识","性别","是否工作","教育水平","婚姻状况",
                                "经管类专业学习经历","有房","有小孩","常数项"),
          omit.stat = c("LL", "aic","ser"), no.space = T, type = "html", out = "Tobit.doc")



###   描述性统计  ====================================

stargazer(df,title = "描述性统计", align = T,
          covariate.labels = c("风险厌恶水平","金融知识","参与股票市场","参与金融市场","股票资产占比","金融资产占比",
                               "性别","教育水平","婚姻状况","有小孩","经管类专业学习经历","有房",
                               "有工作","利率问题","通货膨胀问题"),out = "描述性统计.html")

stargazer(df,title = "描述性统计", align = T,
          covariate.labels = c("风险厌恶水平","金融知识","参与股票市场","参与金融市场","股票资产占比","金融资产占比",
                               "性别","教育水平","婚姻状况","有小孩","经管类专业学习经历","有房",
                               "有工作","利率问题","通货膨胀问题"),type = "html",out = "描述性统计.doc")

###   Finance Problem ROBUST =======================================

probit_stock_robust_fin <- glm(df$stock~df$`finance problem 1`+df$`finance problem 2`+df$`The level of risk reverse`+
                             df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                             df$`real estate`+ df$`educational investment`,data = df,family = binomial(link = "probit"))


probit_market_robust_fin <- glm(df$`financial market`~df$`finance problem 1`+df$`finance problem 2`+df$`The level of risk reverse`+
                                 df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                                 df$`real estate`+ df$`educational investment`,data = df,family = binomial(link = "probit"))


tobit_stockasset_robust_fin <- tobit(df$`stock assset`~df$`finance problem 1`+df$`finance problem 2`+df$`The level of risk reverse`+
                                       df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                                       df$`real estate`+ df$`educational investment`,data = df)


tobit_financialasset_robust_fin <- tobit(df$`financial market`~df$`finance problem 1`+df$`finance problem 2`+df$`The level of risk reverse`+
                                       df$gender+df$work+df$`education level`+df$`marital status`+df$`experience of finance`+
                                       df$`real estate`+ df$`educational investment`,data = df)



stargazer(probit_stock_robust_fin, probit_market_robust_fin, tobit_stockasset_robust_fin, tobit_financialasset_robust_fin,
          title = "金融知识对参与金融市场与家庭资产选择的影响：稳健性检验",align = T,dep.var.labels = c("参与股票市场", "参与金融市场", "股票资产占比", "风险资产占比"),
          covariate.labels = c("利率问题","通货膨胀问题","风险厌恶水平","性别","是否工作","教育水平","婚姻状况",
                               "经管类专业学习经历","有房","有小孩","常数项"),omit.stat = c("LL","aic"), 
          no.space = T,type = "html",out = "金融知识-稳健性检验.html")

stargazer(probit_stock_robust_fin, probit_market_robust_fin, tobit_stockasset_robust_fin, tobit_financialasset_robust_fin,
          title = "金融知识对参与金融市场与家庭资产选择的影响：稳健性检验",align = T,dep.var.labels = c("参与股票市场", "参与金融市场", "股票资产占比", "风险资产占比"),
          covariate.labels = c("利率问题","通货膨胀问题","风险厌恶水平","性别","是否工作","教育水平","婚姻状况",
                     "经管类专业学习经历","有房","有小孩","常数项"),omit.stat = c("LL","aic"), 
          no.space = T,type = "html",out = "金融知识-稳健性检验.doc")


###  Experience of Finance  ROBUST ==================
df1 <- subset(df,df$`experience of finance` > 0)

probit_stock_robust_exp <- glm(df1$stock~df1$`The level of risk reverse`+df1$`The condition of the answer of financial problems`+
                                 df1$gender+df1$work+df1$`education level`+df1$`marital status`+
                                 df1$`real estate`+ df1$`educational investment`,data = df1,family = binomial(link = "probit"))


probit_market_robust_exp <- glm(df1$`financial market`~df1$`The level of risk reverse`+df1$`The condition of the answer of financial problems`+
                                 df1$gender+df1$work+df1$`education level`+df1$`marital status`+
                                 df1$`real estate`+ df1$`educational investment`,data = df1,family = binomial(link = "probit"))

tobit_stockasset_robust_exp <- tobit(df1$`stock assset`~df1$`The level of risk reverse`+df1$`The condition of the answer of financial problems`+
                                 df1$gender+df1$work+df1$`education level`+df1$`marital status`+
                                 df1$`real estate`+ df1$`educational investment`,data = df1)

tobit_financialasset_robust_exp <- tobit(df1$`financial asset`~df1$`The level of risk reverse`+df1$`The condition of the answer of financial problems`+
                                       df1$gender+df1$work+df1$`education level`+df1$`marital status`+
                                       df1$`real estate`+ df1$`educational investment`,data = df1)

stargazer(probit_stock_robust_exp, probit_market_robust_exp, tobit_stockasset_robust_exp, tobit_financialasset_robust_exp,
          title = "金融知识对参与金融市场与家庭资产选择的影响(去除有经管专业学习经历的家庭)",align = T,dep.var.labels = c("参与股票市场", "参与金融市场", "股票资产占比", "风险资产占比"),
          covariate.labels = c("风险厌恶水平","金融知识","性别","是否工作","教育水平","婚姻状况",
                               "有房","有小孩","常数项"),omit.stat = c("LL","aic"), 
          no.space = T,type = "html",out = "稳健性检验(去除有经管专业学习经历的家庭).html")

stargazer(probit_stock_robust_exp, probit_market_robust_exp, tobit_stockasset_robust_exp, tobit_financialasset_robust_exp,
          title = "金融知识对参与金融市场与家庭资产选择的影响(去除有经管专业学习经历的家庭)",align = T,dep.var.labels = c("参与股票市场", "参与金融市场", "股票资产占比", "风险资产占比"),
          covariate.labels = c("风险厌恶水平","金融知识","性别","是否工作","教育水平","婚姻状况",
                              "有房","有小孩","常数项"),omit.stat = c("LL","aic"), 
          no.space = T,type = "html",out = "稳健性检验(去除有经管专业学习经历的家庭).doc")
