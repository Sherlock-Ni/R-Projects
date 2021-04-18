library(tidyverse)
library(haven)
library(AER)
library(stargazer)

master_17 <- read_dta("master_17.dta", encoding = "utf-8")
master_17 <- master_17 %>% 
  filter(qc == 0) 
ind_17 <- read_dta("ind_17.dta",encoding = "utf-8")
ind_17 <- ind_17 %>% 
  filter(hhead == 1,qc == 0)
hh_17 <- read_dta("hh_17.dta",encoding = "utf-8")


m_17 <- master_17 %>% 
  mutate(prov = prov_CHN,id = paste(hhid,pline,sep = "_")) %>% 
  select(id,hhid,prov,rural)

d_17 <- ind_17 %>% 
  mutate(hhid_d = hhid,id = paste(hhid,pline,sep = "_"),male = 2 - a2003, age = 2017 - a2005, edu = a2012,
         marriage = a2024, industry = a3110, 
         pension = ifelse(is.na(f1001a) == T, 0,ifelse(f1001a == 7788,0,ifelse(f1001a == ".d",0,ifelse(f1001a == ".r",0,ifelse(f1001a == ".e",0,ifelse(f1001a == ".n",0,1)))))),
         busi_pe = ifelse(is.na(f6001a) == T, 0,ifelse(f6001a == 7788,0,ifelse(f6001a == ".d",0,ifelse(f6001a == ".r",0,ifelse(f6001a == ".e",0,ifelse(f6001a == ".n",0,1))))))) %>%
  filter(age > 16) %>% 
  select(id,hhid_d,male,age,edu,marriage,industry,pension,busi_pe)

h_17 <- hh_17 %>% 
  mutate(edu_p1 = a2032_1, edu_p2 = a2032_2,
         risk_aver = ifelse(h3104 == "NA","NA",ifelse(h3104 == ".d",6,ifelse(h3104 == ".r",0,ifelse(h3104 == ".e",0,ifelse(h3104 == ".n",0,h3104))))),
         risk_p = 2-ifelse(h3103 == "NA","NA",ifelse(h3103 == 1,1,ifelse(h3103 == ".d",0,2))), 
         rate_p = ifelse(h3105 == "NA","NA",ifelse(h3105 == 2,1,ifelse(h3105 == ".d",2,0))),
         inf_p = ifelse(h3106 == "NA","NA",ifelse(h3106 == 1,1,ifelse(h3106 == ".d",2,0))), 
         risk_div = 2-ifelse(h3115 == "NA","NA",ifelse(h3115 == 1,1,ifelse(h3115 == ".d",0,2))), 
         finnews = 5-ifelse(h3101 == "NA","NA",ifelse(h3101 == 1,1,ifelse(h3101 == 2,2,ifelse(h3101 == 3,3,ifelse(h3101 == 4,4,5))))),
         house = 2-ifelse(c2001 == "NA","NA",ifelse(c2001 == 1,1,2)), 
         stock = 2-ifelse(d3101 == "NA","NA",ifelse(d3101 == 1,1,2)), 
         fund = 2-ifelse(d5102 == "NA","NA",ifelse(d5102 == 1,1,2)),
         bond = 2-ifelse(d7113_1_mc == "NA","NA",ifelse(d7113_1_mc == 1,1,2)),
         finmag = 2-ifelse(d7109 == "NA","NA",ifelse(d7109 == 1,1,2)), 
         deriv = 2-ifelse(d7113_2_mc == "NA","NA",ifelse(d7113_2_mc == 1,1,2)), 
         gold = 2-ifelse(d7113_3_mc == "NA","NA",ifelse(d7113_3_mc == 1,1,2)), 
         nonrmb = 2-ifelse(d7113_4_mc == "NA","NA",ifelse(d7113_4_mc == 1,1,2)), 
         stock_v = ifelse(d3109_imp == "NA","NA",ifelse(d3109_imp > 0,d3109_imp,0)), 
         bond_v = ifelse(d4103_1_imp == "NA","NA",ifelse(d4103_1_imp > 0,d4103_1_imp)), 
         fund_v = ifelse(d5107_imp == "NA","NA",ifelse(d5107_imp > 0,d5107_imp,0)), 
         finmag_v = ifelse(d7110a_imp == "NA","NA",ifelse(d7110a_imp > 0,d7110a_imp,0)),
         deriv_v = ifelse(d6100a_imp== "NA","NA",ifelse(d6100a_imp > 0,d6100a_imp,0)), 
         gold_v = ifelse(d9103_imp == "NA","NA",ifelse(d9103_imp > 0,d9103_imp,0)), 
         nonrmb_v = ifelse(d8104_imp == "NA","NA",ifelse(d8104_imp > 0,d8104_imp,0)), 
         cash = ifelse(k1101_imp == "NA","NA",ifelse(k1101_imp > 0,k1101_imp,0)),
         save = ifelse(d1105_imp == "NA","NA",ifelse(d1105_imp > 0,d1105_imp,0)), 
         deposit = ifelse(d2104_imp == "NA","NA",ifelse(d2104_imp > 0,d2104_imp,0)), 
         project_v = ifelse(b2003d_imp == "NA","NA",ifelse(b2003d_imp > 0,b2003d_imp,0)),
         total_income = ifelse(total_income == 0,0,log(abs(total_income)))) %>% 
  select(hhid,track,edu_p1,edu_p2,risk_aver,risk_p,rate_p, inf_p,risk_div, finnews, house, stock,bond,fund, finmag,deriv,gold,nonrmb,
         stock_v, bond_v,fund_v,finmag_v,deriv_v,gold_v,nonrmb_v,cash,save,deposit,project_v,total_income)

df1 <- d_17 %>% left_join(m_17,by = "id") %>% 
  filter(prov == c("上海市","江苏省","浙江省"))
df1 <- df1 %>% left_join(h_17, by = "hhid")


hh_15 <- read_dta("hh_15.dta",encoding = "utf-8")

h_15 <- hh_15 %>% 
  mutate(finnews = 5-ifelse(a4002a == "NA","NA",ifelse(a4002a == 1,1,ifelse(a4002a == 2,2,ifelse(a4002a == 3,3,ifelse(a4002a == 4,4,5))))),
         risk_aver = ifelse(a4003 == "NA","NA",ifelse(a4003 == ".d",6,ifelse(a4003 == ".r",0,ifelse(a4003 == ".e",0,ifelse(a4003== ".n",0,a4003))))), 
         rate_p = ifelse(a4004a == "NA","NA",ifelse(a4004a == 2,1,ifelse(a4004a == ".d",2,0))), 
         inf_p = ifelse(a4005a== "NA","NA",ifelse(a4005a == 1,1,ifelse(a4005a == ".d",2,0)))) %>% 
  select(hhid,finnews,risk_aver,rate_p,inf_p)
df2 <- h_15

df <- df1 %>% left_join(df2,by = "hhid")



df <- df %>% 
  mutate(finnews = ifelse(is.na(finnews.x)== T,finnews.y,finnews.x),
         risk_aver = ifelse(is.na(risk_aver.x)== T,risk_aver.y,risk_aver.x),
         rate_p = ifelse(is.na(rate_p.x)== T,rate_p.y,rate_p.x),
         inf_p = ifelse(is.na(inf_p.x)== T,inf_p.y,inf_p.x),
         stock_v = ifelse(is.na(stock_v)== T,0,stock_v),
         bond_v = ifelse(is.na(bond_v)== T,0,bond_v),
         fund_v = ifelse(is.na(fund_v)== T,0,fund_v),
         finmag_v = ifelse(is.na(finmag_v)== T,0,finmag_v),
         deriv_v = ifelse(is.na(deriv_v)== T,0,deriv_v),
         gold_v = ifelse(is.na(gold_v)== T,0,gold_v),
         nonrmb_v = ifelse(is.na(nonrmb_v)== T,0,nonrmb_v),
         cash = ifelse(is.na(cash)== T,0,cash),
         save = ifelse(is.na(save)== T,0,save),
         deposit = ifelse(is.na(deposit)== T,0,deposit),
         project_v = ifelse(is.na(project_v) == T, 0,project_v),
         edu_p1 = ifelse(is.na(edu_p1)== T,0,edu_p1),
         edu_p2 = ifelse(is.na(edu_p2)== T,0,edu_p2),
         house = ifelse(is.na(house)== T,0,house),
         total_income = ifelse(is.na(total_income)== T,0,total_income)) %>%
  select(hhid,prov,male,age,edu,marriage,edu_p1,edu_p2,risk_aver,risk_p,rate_p,inf_p,risk_div,finnews,house,stock, fund,bond, finmag,deriv,
         gold, nonrmb,stock_v, bond_v,fund_v, finmag_v, deriv_v,gold_v, nonrmb_v,cash, save, deposit,
         project_v,total_income,rural,industry,pension,busi_pe)




df_t <- df %>% 
  mutate(fin_asset = stock_v+bond_v+fund_v+finmag_v+deriv_v+gold_v+nonrmb_v+cash+save+deposit,
         risk_asset = stock_v+bond_v+fund_v+finmag_v+deriv_v+gold_v+nonrmb_v,
         risk_asset_per = ifelse(fin_asset == 0,0,risk_asset/fin_asset),
         stock_per = ifelse(risk_asset == 0,0,stock_v/risk_asset),
         fin_market = ifelse(stock+bond+fund+finmag+deriv+gold+nonrmb == 0,0,1),
         project = ifelse(project_v == 0,0,1),
         knowledge = ifelse(is.na(risk_p)==T,0,ifelse(risk_p == 2,0,risk_p))+ifelse(is.na(risk_div)==T,0,ifelse(risk_div == 2,0,risk_div))+
           ifelse(is.na(rate_p)==T,0,ifelse(rate_p == 2,0,rate_p))+ifelse(is.na(inf_p)==T,0,ifelse(inf_p == 2,0,inf_p)),
         edu_par = ifelse(edu_p1>edu_p2,edu_p1,edu_p2)) %>%
  select(hhid,prov,male,age,edu,marriage,edu_par,risk_aver,risk_p,rate_p,inf_p,risk_div,knowledge,finnews,house, project,stock, fund,bond, finmag,deriv,
         gold, nonrmb, fin_asset,risk_asset,risk_asset_per,stock_per,fin_market,stock_v, bond_v,fund_v, finmag_v, deriv_v,gold_v, nonrmb_v,cash, save, deposit,
         total_income,rural,industry,pension,busi_pe)

write.csv(df_t,"df_t.csv")


p1 <- df_t %>%
  filter(risk_aver > 0) %>%
  ggplot(aes(x=risk_aver,fill = prov))+geom_bar(stat = "count",width = 0.4,position = "dodge")+
  ggtitle('长三角地区家庭金融风险厌恶水平')+scale_x_continuous(breaks = seq(1,6,1))
p1 + labs(title = "长三角地区家庭风险厌恶水平", x = "风险厌恶水平", y = "Obs")+theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1,6,1)) + scale_fill_brewer(palette = "Paired")  


p2 <- df_t %>%
  ggplot(aes(x = knowledge,fill = prov))+geom_bar(stat = "count", width = 0.4, position = position_dodge(0.4))
p2 + labs(title = "长三角地区家庭金融知识回答情况", x = "金融知识问题答对个数", y = "Obs")  + theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0,4,1)) + scale_fill_brewer(palette = "Paired")




### 回归 ===================================
# 线性 ================================

attach(df_t)

fit1 <- lm(stock~risk_aver+knowledge)
fit2 <- lm(fin_market~risk_aver+knowledge)

probit_stock <- glm(stock~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,
                    data = df_t,family = binomial(link = "probit"))
probit_fin <- glm(fin_market~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,
                  data = df_t,family = binomial(link = "probit"))

ivprobit_stock <- ivreg(stock~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural
                        | risk_aver+knowledge+male+age+edu_par+marriage+house+total_income+rural,data = df_t)
ivprobit_fin <- ivreg(fin_market~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural
                      | risk_aver+knowledge+male+age+edu_par+marriage+house+total_income+rural,data = df_t)


tobit_stock_per <- tobit(stock_per~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,data = df_t)
tobit_fin_per <- tobit(risk_asset_per~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,data = df_t)


ivtobit_stock_per <- ivreg(stock_per~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural
                           | risk_aver+knowledge+male+age+edu_par+marriage+house+total_income+rural,data = df_t)
ivtobit_risk_per <- ivreg(risk_asset_per~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural
                          | risk_aver+knowledge+male+age+edu_par+marriage+house+total_income+rural,data = df_t)

## 稳健性检验 =================================
# without fin worker ==================
df_t1 <- subset(df_t,industry != 10)


probit_stock_rob1 <- glm(stock~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,
                         data = df_t1,family = binomial(link = "probit"))
probit_fin_rob1 <- glm(fin_market~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,
                       data = df_t1,family = binomial(link = "probit"))
tobit_stock_per_rob1 <- tobit(stock_per~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,data = df_t1)
tobit_risk_per_rob1 <- tobit(risk_asset_per~risk_aver+knowledge+male+age+edu+marriage+house+total_income+rural,data = df_t1)





# problem ==================================
# risk_p
probit_stock_rob2 <- glm(stock~risk_aver+risk_p+male+age+edu+marriage+house+total_income+rural,
                         data = df_t,family = binomial(link = "probit"))
probit_fin_rob2 <- glm(fin_market~risk_aver+risk_p+male+age+edu+marriage+house+total_income+rural,
                       data = df_t,family = binomial(link = "probit"))
tobit_stock_per_rob2 <- tobit(stock_per~risk_aver+risk_p+male+age+edu+marriage+house+total_income+rural,data = df_t)
tobit_risk_per_rob2 <- tobit(risk_asset_per~risk_aver+risk_p+male+age+edu+marriage+house+total_income+rural,data = df_t)


#rate_p
probit_stock_rob3 <- glm(stock~risk_aver+rate_p+male+age+edu+marriage+house+total_income+rural,
                         data = df_t,family = binomial(link = "probit"))
probit_fin_rob3 <- glm(fin_market~risk_aver+rate_p+male+age+edu+marriage+house+total_income+rural,
                       data = df_t,family = binomial(link = "probit"))
tobit_stock_per_rob3 <- tobit(stock_per~risk_aver+rate_p+male+age+edu+marriage+house+total_income+rural,data = df_t)
tobit_risk_per_rob3 <- tobit(risk_asset_per~risk_aver+rate_p+male+age+edu+marriage+house+total_income+rural,data = df_t)


#inf_P
probit_stock_rob4 <- glm(stock~risk_aver+inf_p+male+age+edu+marriage+house+total_income+rural,
                         data = df_t,family = binomial(link = "probit"))
probit_fin_rob4 <- glm(fin_market~risk_aver+inf_p+male+age+edu+marriage+house+total_income+rural,
                       data = df_t,family = binomial(link = "probit"))
tobit_stock_per_rob4 <- tobit(stock_per~risk_aver+inf_p+male+age+edu+marriage+house+total_income+rural,data = df_t)
tobit_risk_per_rob4 <- tobit(risk_asset_per~risk_aver+inf_p+male+age+edu+marriage+house+total_income+rural,data = df_t)


#risk_div
probit_stock_rob5 <- glm(stock~risk_aver+risk_div+male+age+edu+marriage+house+total_income+rural,
                         data = df_t,family = binomial(link = "probit"))
probit_fin_rob5 <- glm(fin_market~risk_aver+risk_div+male+age+edu+marriage+house+total_income+rural,
                       data = df_t,family = binomial(link = "probit"))
tobit_stock_per_rob5 <- tobit(stock_per~risk_aver+risk_div+male+age+edu+marriage+house+total_income+rural,data = df_t)
tobit_risk_per_rob5 <- tobit(risk_asset_per~risk_aver+risk_div+male+age+edu+marriage+house+total_income+rural,data = df_t)


  
num_knowledge <- df_t %>% 
  group_by(prov,knowledge)%>% 
  summarize(n = n()) %>% 
  mutate(per = n / sum(n)*100)
p3 <- num_knowledge %>%
  ggplot(aes(x = knowledge, y = per, fill = prov))+geom_bar(stat = "identity", width = 0.4, position = position_dodge(0.4))
p3 + labs(title = "长三角地区家庭金融知识回答情况", x = "金融知识问题答对个数", y = "Percent")  + theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0,4,1)) +scale_y_continuous(limits = c(0,100),breaks = seq(0,100,20))+ scale_fill_brewer(palette = "Paired")


num_risk_aver <- df_t %>% 
  filter(!is.na(risk_aver)) %>% 
  group_by(prov,risk_aver) %>% 
  summarize(n = n()) %>% 
  mutate(per = n/sum(n))

p4 <- num_risk_aver %>%
  filter(risk_aver > 0) %>%
  ggplot(aes(x=risk_aver,y = per,fill = prov))+geom_bar(stat = "identity",width = 0.4,position = "dodge")+
  ggtitle('长三角地区家庭金融风险厌恶水平')+scale_x_continuous(breaks = seq(1,6,1))
p4 + labs(title = "长三角地区家庭风险厌恶水平", x = "风险厌恶水平", y = "Obs")+theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1,6,1)) + scale_fill_brewer(palette = "Paired")
p4+coord_flip()
p4+coord_polar()