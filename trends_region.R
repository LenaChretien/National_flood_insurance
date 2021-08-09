library(ggplot2)
library(dplyr)
library(usmap)
library(maps)
library(timetk)
library(tidyverse) 
library(lubridate)
library(forecast)
library(reshape2)  
library(zoo)
library(scales)

### Plot yearly amounts. 

# Make a graph of the yearly insurance cost by region

################################################################
#### Make yearly timeseries by removing seasonal cycle first.
############################################################

NE = c('ME','NH','MA','RI','VT','NY','PA','NJ','DE','CT')
for (i in (1:length(NE))){
  load(paste('data/', NE[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , NE[i], sep = '')))
  eval(parse(text = paste('remove(', NE[i],')', sep = '')))
  
  if (i == 1){
    NE_df = st
  } else {
    NE_df = rbind(NE_df,st)
  }
}
NE_df = NE_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost)) 
#num = n(),
#amt = mean(building_ins + content_ins)) %>%

ts_st = ts(NE_df$cost, start = as.yearmon(NE_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_adj = df_adj

lm_NE = lm(cost_adj ~ time, data = NE_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_seas = df_adj



MW = c('ND','SD','NE','KS','MO','IA','MN','WI','IL','IN','MI','OH')
for (i in (1:length(MW))){
  load(paste('data/', MW[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , MW[i], sep = '')))
  eval(parse(text = paste('remove(', MW[i],')', sep = '')))
  
  if (i == 1){
    MW_df = st
  } else {
    MW_df = rbind(MW_df,st)
  }
}
MW_df = MW_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost)) 

ts_st = ts(MW_df$cost, start = as.yearmon(MW_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_adj = df_adj

lm_MW = lm(cost_adj ~ time, data = MW_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_seas = df_adj




S = c('WV','VA','MD','DC','NC','KY','TN','SC','GA','AL','MS','LA','AR','OK','TX','FL')
for (i in (1:length(S))){
  load(paste('data/', S[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , S[i], sep = '')))
  eval(parse(text = paste('remove(', S[i],')', sep = '')))
  
  if (i == 1){
    S_df = st
  } else {
    S_df = rbind(S_df,st)
  }
}
S_df = S_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost)) 

ts_st = ts(S_df$cost, start = as.yearmon(S_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_adj = df_adj

lm_S = lm(cost_adj ~ time, data = S_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_seas = df_adj




MD = c('MT','ID','WY','CO','NM','AZ','UT','NV')
for (i in (1:length(MD))){
  load(paste('data/', MD[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , MD[i], sep = '')))
  eval(parse(text = paste('remove(', MD[i],')', sep = '')))
  
  if (i == 1){
    MD_df = st
  } else {
    MD_df = rbind(MD_df,st)
  }
}
MD_df = MD_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost)) 

ts_st = ts(MD_df$cost, start = as.yearmon(MD_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_adj = df_adj

lm_MD = lm(cost_adj ~ time, data = MD_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_seas = df_adj








PA = c('WA','OR','CA')
for (i in (1:length(PA))){
  load(paste('data/', PA[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , PA[i], sep = '')))
  eval(parse(text = paste('remove(', PA[i],')', sep = '')))
  
  if (i == 1){
    PA_df = st
  } else {
    PA_df = rbind(PA_df,st)
  }
}
PA_df = PA_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost)) 

ts_st = ts(PA_df$cost, start = as.yearmon(PA_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_adj = df_adj

lm_PA = lm(cost_adj ~ time, data = PA_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_seas = df_adj







MD_adj = MD_adj %>% mutate(region = "MD") 
MD_adj = MD_adj[c(1,7,8)]
MD_seas = MD_seas %>% mutate(region = "MD") 
MD_seas = MD_seas[c(1,7,8)]

MW_adj = MW_adj %>% mutate(region = "MW") 
MW_adj = MW_adj[c(1,7,8)]
MW_seas = MW_seas %>% mutate(region = "MW") 
MW_seas = MW_seas[c(1,7,8)]

NE_adj = NE_adj %>% mutate(region = "NE")
NE_adj = NE_adj[c(1,7,8)]
NE_seas = NE_seas %>% mutate(region = "NE") 
NE_seas = NE_seas[c(1,7,8)]

PA_adj = PA_adj %>% mutate(region = "PA")
PA_adj = PA_adj[c(1,7,8)]
PA_seas = PA_seas %>% mutate(region = "PA") 
PA_seas = PA_seas[c(1,7,8)]

S_adj = S_adj %>% mutate(region = "S")
S_adj = S_adj[c(1,7,8)]
S_seas = S_seas %>% mutate(region = "S") 
S_seas = S_seas[c(1,7,8)]

region_adj = rbind(MD_adj,MW_adj,NE_adj,PA_adj,S_adj)
region_seas = rbind(MD_seas,MW_seas,NE_seas,PA_seas,S_seas)


rm_seas = region_seas %>% group_by(region) %>%
  summarise_by_time(.date_var = time, .by= "month", 
                    cost = mean(cost_seas)) 

rm_adj = region_adj %>% group_by(region) %>%
  summarize_by_time(.date_var = time, .by = "month",
                    cost = mean(cost_adj))


rm_adj_yr = region_adj %>% group_by(region) %>%
  summarize_by_time(.date_var = time, .by = "year",
                    cost = mean(cost_adj))





rm_one = rm_seas %>% group_by(region) %>%
  filter(row_number()<=24) 

ggplot(inherit.aes = FALSE, data = rm_one,
       aes(x = time, y= cost, group = region, color = region)) + 
  theme_bw() + 
  geom_line()+
  facet_wrap(~region, ncol = 1) +
  scale_x_date(date_labels = "%m", date_breaks = "2 months") +
  labs(y = "Month",
       x = "Dollar [$]", 
       title = "Seasonal variability of cost")



ggplot(inherit.aes = FALSE, data = rm_adj,
       aes(x = time, y= cost, group = region), color = '#999999') + 
  theme_bw() + 
  geom_line()+
  scale_x_date(date_labels = "%Y", date_breaks = "years") +
  geom_line(data = rm_adj_yr, aes(x = time, y = cost, group = region, color = region),
            size = 1.5, alpha = 0.7 ) +
  labs(x = "Year",
       y = "Dollar [$]", 
       title = "Yearly variability of cost")



#### MAKE TABLE OF CHANGES
year = as.character(2014)
month = as.character(1)
day = as.character(1)
ymd = as.Date(paste(year,month,day,sep = '-'))
p1 =data.frame(ymd)
colnames(p1) = "time" 

year = as.character(2019)
month = as.character(8)
day = as.character(1)
ymd = as.Date(paste(year,month,day,sep = '-'))
p2 =data.frame(ymd)
colnames(p2) = "time" 

MD_diff = predict(lm_MD,p2) - predict(lm_MD,p1)
MW_diff = predict(lm_MW,p2) - predict(lm_MW,p1)
NE_diff = predict(lm_NE,p2) - predict(lm_NE,p1)
PA_diff = predict(lm_PA,p2) - predict(lm_PA,p1)
S_diff = predict(lm_S,p2) - predict(lm_S,p1)

regions = c("MD","MW","NE","PA","S")
cost_incr = c(MD_diff, MW_diff, NE_diff, PA_diff, S_diff)
diff_df = data.frame(cost_incr,regions)


p1 = diff_df %>% 
      ggplot(aes(cost_incr, reorder(regions, -cost_incr),
                 group = regions, fill = regions)) + 
      geom_col()+
      labs(y = "Region",
           x = "Dollar [$]", 
           title = "") +
      geom_text(aes(x = cost_incr, 
                    y = regions,
                    label = round(cost_incr, 0)))

p1
ggsave("figures/change_of_cost_region.jpg", width = 4, height = 4)





p1 = ggplot(inherit.aes = FALSE, data = rm_adj,
       aes(x = time, y= cost, group = region), alpha = 0.5) + 
  theme_bw() + 
  geom_line()+
  scale_x_date(date_labels = "%Y", date_breaks = "years")


p_trend = p1 + stat_smooth(inherit.aes = FALSE, data = region_adj, 
              aes(x = time, 
                  y = cost_adj, 
                  group = region,
                  colour = region),
              method = "lm", se=FALSE) +
  labs(y = "Cost [$]", x = "Year", 
  title = "Yearly variability of cost")

p_trend
ggsave("figures/linear_change_of_cost_region.jpg", width =6, height = 5)





##############################################################################
##################### Change in cost per 1000$ insured #######################
##############################################################################
NE = c('ME','NH','MA','RI','VT','NY','PA','NJ','DE','CT')
for (i in (1:length(NE))){
  load(paste('data/', NE[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , NE[i], sep = '')))
  eval(parse(text = paste('remove(', NE[i],')', sep = '')))
  
  if (i == 1){
    NE_df = st
  } else {
    NE_df = rbind(NE_df,st)
  }
}
NE_df = NE_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost),
                    amt = mean(building_ins + content_ins)) %>%
  mutate(cost_per = (cost/amt)*1000)
       
       
       
ts_st = ts(NE_df$cost_per, start = as.yearmon(NE_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_adj = df_adj

lm_NE = lm(cost_adj ~ time, data = NE_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_seas = df_adj



MW = c('ND','SD','NE','KS','MO','IA','MN','WI','IL','IN','MI','OH')
for (i in (1:length(MW))){
  load(paste('data/', MW[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , MW[i], sep = '')))
  eval(parse(text = paste('remove(', MW[i],')', sep = '')))
  
  if (i == 1){
    MW_df = st
  } else {
    MW_df = rbind(MW_df,st)
  }
}
MW_df = MW_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost),
                    amt = mean(building_ins + content_ins)) %>% 
  mutate(cost_per = (cost/amt)*1000)





ts_st = ts(MW_df$cost_per, start = as.yearmon(MW_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_adj = df_adj

lm_MW = lm(cost_adj ~ time, data = MW_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_seas = df_adj




S = c('WV','VA','MD','DC','NC','KY','TN','SC','GA','AL','MS','LA','AR','OK','TX','FL')
for (i in (1:length(S))){
  load(paste('data/', S[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , S[i], sep = '')))
  eval(parse(text = paste('remove(', S[i],')', sep = '')))
  
  if (i == 1){
    S_df = st
  } else {
    S_df = rbind(S_df,st)
  }
}
S_df = S_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost),
                    amt = mean(building_ins + content_ins)) %>% 
  mutate(cost_per = (cost/amt)*1000)




ts_st = ts(S_df$cost_per, start = as.yearmon(S_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_adj = df_adj

lm_S = lm(cost_adj ~ time, data = S_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_seas = df_adj




MD = c('MT','ID','WY','CO','NM','AZ','UT','NV')
for (i in (1:length(MD))){
  load(paste('data/', MD[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , MD[i], sep = '')))
  eval(parse(text = paste('remove(', MD[i],')', sep = '')))
  
  if (i == 1){
    MD_df = st
  } else {
    MD_df = rbind(MD_df,st)
  }
}
MD_df = MD_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost),
                    amt = mean(building_ins + content_ins)) %>% 
  mutate(cost_per = (cost/amt)*1000)

ts_st = ts(MD_df$cost_per, start = as.yearmon(MD_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_adj = df_adj

lm_MD = lm(cost_adj ~ time, data = MD_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_seas = df_adj








PA = c('WA','OR','CA')
for (i in (1:length(PA))){
  load(paste('data/', PA[i], '.Rda', sep = ''))
  eval(parse(text = paste('st= ' , PA[i], sep = '')))
  eval(parse(text = paste('remove(', PA[i],')', sep = '')))
  
  if (i == 1){
    PA_df = st
  } else {
    PA_df = rbind(PA_df,st)
  }
}
PA_df = PA_df %>%
  filter(policydate < as.Date("2019-08-01")) %>%
  summarise_by_time(.date_var = policydate, .by= "day", 
                    cost = mean(policycost),
                    amt = mean(building_ins + content_ins)) %>% 
  mutate(cost_per = (cost/amt)*1000)


ts_st = ts(PA_df$cost_per, start = as.yearmon(PA_df$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st)

st_seasAdj = ts_st - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_adj = df_adj

lm_PA = lm(cost_adj ~ time, data = PA_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_seas = df_adj




MD_adj = MD_adj %>% mutate(region = "MD") 
MD_adj = MD_adj[c(1,7,8)]
MD_seas = MD_seas %>% mutate(region = "MD") 
MD_seas = MD_seas[c(1,7,8)]

MW_adj = MW_adj %>% mutate(region = "MW") 
MW_adj = MW_adj[c(1,7,8)]
MW_seas = MW_seas %>% mutate(region = "MW") 
MW_seas = MW_seas[c(1,7,8)]

NE_adj = NE_adj %>% mutate(region = "NE")
NE_adj = NE_adj[c(1,7,8)]
NE_seas = NE_seas %>% mutate(region = "NE") 
NE_seas = NE_seas[c(1,7,8)]

PA_adj = PA_adj %>% mutate(region = "PA")
PA_adj = PA_adj[c(1,7,8)]
PA_seas = PA_seas %>% mutate(region = "PA") 
PA_seas = PA_seas[c(1,7,8)]

S_adj = S_adj %>% mutate(region = "S")
S_adj = S_adj[c(1,7,8)]
S_seas = S_seas %>% mutate(region = "S") 
S_seas = S_seas[c(1,7,8)]

region_adj = rbind(MD_adj,MW_adj,NE_adj,PA_adj,S_adj)
region_seas = rbind(MD_seas,MW_seas,NE_seas,PA_seas,S_seas)


rm_seas = region_seas %>% group_by(region) %>%
  summarise_by_time(.date_var = time, .by= "month", 
                    cost = mean(cost_seas)) 

rm_adj = region_adj %>% group_by(region) %>%
  summarize_by_time(.date_var = time, .by = "month",
                    cost = mean(cost_adj))


rm_adj_yr = region_adj %>% group_by(region) %>%
  summarize_by_time(.date_var = time, .by = "year",
                    cost = mean(cost_adj))


rm_one = rm_seas %>% group_by(region) %>%
  filter(row_number()<=24) 



ggplot(inherit.aes = FALSE, data = rm_one,
       aes(x = time, y= cost, group = region, color = region)) + 
  theme_bw() + 
  geom_line()+
  facet_wrap(~region, ncol = 1) +
  scale_x_date(date_labels = "%m", date_breaks = "2 months") +
  labs(y = "Month",
       x = "Dollar [$]", 
       title = "Seasonal variability of cost")



ggplot(inherit.aes = FALSE, data = rm_adj,
       aes(x = time, y= cost, group = region), color = '#999999') + 
  theme_bw() + 
  geom_line()+
  scale_x_date(date_labels = "%Y", date_breaks = "years") +
  geom_line(data = rm_adj_yr, aes(x = time, y = cost, group = region, color = region),
            size = 1.5, alpha = 0.7 ) +
  labs(x = "Year",
       y = "Dollar/1000K [$]", 
       title = "Yearly variability of cost/1000K insured")

ggsave("figures/yearly_cost_per_insured_region.jpg", width = 6, height = 5)

#### MAKE TABLE OF CHANGES
year = as.character(2014)
month = as.character(1)
day = as.character(1)
ymd = as.Date(paste(year,month,day,sep = '-'))
p1 =data.frame(ymd)
colnames(p1) = "time" 

year = as.character(2019)
month = as.character(8)
day = as.character(1)
ymd = as.Date(paste(year,month,day,sep = '-'))
p2 =data.frame(ymd)
colnames(p2) = "time" 

MD_diff = predict(lm_MD,p2) - predict(lm_MD,p1)
MW_diff = predict(lm_MW,p2) - predict(lm_MW,p1)
NE_diff = predict(lm_NE,p2) - predict(lm_NE,p1)
PA_diff = predict(lm_PA,p2) - predict(lm_PA,p1)
S_diff = predict(lm_S,p2) - predict(lm_S,p1)

regions = c("MD","MW","NE","PA","S")
cost_incr = c(MD_diff, MW_diff, NE_diff, PA_diff, S_diff)
diff_df = data.frame(cost_incr,regions)


p1 = diff_df %>% 
  ggplot(aes(cost_incr, reorder(regions, -cost_incr),
             group = regions, fill = regions)) + 
  geom_col()+
  labs(y = "Region",
       x = "Dollar [$]", 
       title = "Change in cost") +
  geom_text(aes(x = cost_incr, 
                y = regions,
                label = round(cost_incr, 2)))

p1

ggsave("figures/change_of_cost_per_insured_region.jpg", width = 4, height = 4)





p1 = ggplot(inherit.aes = FALSE, data = rm_adj,
            aes(x = time, y= cost, group = region), alpha = 0.5) + 
  theme_bw() + 
  geom_line()+
  scale_x_date(date_labels = "%Y", date_breaks = "years")


p_trend = p1 + stat_smooth(inherit.aes = FALSE, data = region_adj, 
                           aes(x = time, 
                               y = cost_adj, 
                               group = region,
                               colour = region),
                           method = "lm", se=FALSE) +
  labs(y = "Cost [$]", x = "Year", 
       title = "")

p_trend



ggsave("figures/linear_change_of_cost_per_insured_region.jpg", width = 6, height = 5)







##############################################################################
##################### Change in number per population  #######################
##############################################################################
# census = read.csv("data/nst-est2019-01.csv", header = F, sep = ",", fill = TRUE)
# census = census %>% filter(V1 !="Alaska") %>% filter(V1 != "Hawaii") 
# 
# census = census[c(7,8,9,10,11,12,13)]
# colnames(census) = c("2013","2014","2015","2016","2017","2018","2019")
# census$mean_cns = rowMeans(census)
# census = census[8]
# census = cbind(census,state_list)
# colnames(census) = c("mean_cns","state")
# 
# #usstates_census = left_join(usstates,census, by = "state")
# #(number/mean_cns)*100))
# 
# 
# NE = c('ME','NH','MA','RI','VT','NY','PA','NJ','DE','CT')
# for (i in (1:length(NE))){
#   load(paste('data/', NE[i], '.Rda', sep = ''))
#   eval(parse(text = paste('st= ' , NE[i], sep = '')))
#   eval(parse(text = paste('remove(', NE[i],')', sep = '')))
#   
#   if (i == 1){
#     NE_df = st
#   } else {
#     NE_df = rbind(NE_df,st)
#   }
# }
# 
# NE_df = NE_df %>%
#   filter(policydate < as.Date("2019-08-01")) %>%
#   summarise_by_time(.date_var = policydate, .by= "day", 
#                     number = n())
# #cens = census%>% filter(state %in% NE) %>% 
# #  summarize(total_cens = mean(mean_cns))
# #NE_df = NE_df %>% mutate(num_per = ((number/cens)*100))
# 
# 
# ts_st = ts(NE_df$number, start = as.yearmon(NE_df$policydate[1]), freq = 24*15)
# decom_st = decompose(ts_st)
# 
# st_seasAdj = ts_st - decom_st$seasonal
# df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# NE_adj = df_adj
# 
# lm_NE = lm(cost_adj ~ time, data = NE_adj)
# 
# 
# df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# NE_seas = df_adj
# 
# 
# 
# MW = c('ND','SD','NE','KS','MO','IA','MN','WI','IL','IN','MI','OH')
# for (i in (1:length(MW))){
#   load(paste('data/', MW[i], '.Rda', sep = ''))
#   eval(parse(text = paste('st= ' , MW[i], sep = '')))
#   eval(parse(text = paste('remove(', MW[i],')', sep = '')))
#   
#   if (i == 1){
#     MW_df = st
#   } else {
#     MW_df = rbind(MW_df,st)
#   }
# }
# 
# MW_df = MW_df %>%
#   filter(policydate < as.Date("2019-08-01")) %>%
#   summarise_by_time(.date_var = policydate, .by= "day", 
#                     number = n())
# #cens = census%>% filter(state %in% MW) %>% 
# #  summarize(total_cens = mean(mean_cns))
# 
# 
# #MW_df = MW_df %>% mutate(num_per = ((number/cens)*100))
# 
# 
# 
# ts_st = ts(MW_df$number, start = as.yearmon(MW_df$policydate[1]), freq = 24*15)
# decom_st = decompose(ts_st)
# 
# st_seasAdj = ts_st - decom_st$seasonal
# df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# MW_adj = df_adj
# 
# lm_MW = lm(cost_adj ~ time, data = MW_adj)
# 
# 
# df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# MW_seas = df_adj
# 
# 
# 
# 
# S = c('WV','VA','MD','DC','NC','KY','TN','SC','GA','AL','MS','LA','AR','OK','TX','FL')
# for (i in (1:length(S))){
#   load(paste('data/', S[i], '.Rda', sep = ''))
#   eval(parse(text = paste('st= ' , S[i], sep = '')))
#   eval(parse(text = paste('remove(', S[i],')', sep = '')))
#   
#   if (i == 1){
#     S_df = st
#   } else {
#     S_df = rbind(S_df,st)
#   }
# }
# 
# S_df = S_df %>%
#   filter(policydate < as.Date("2019-08-01")) %>%
#   summarise_by_time(.date_var = policydate, .by= "day", 
#                     number = n())
# #cens = census%>% filter(state %in% S) %>% 
# #  summarize(total_cens = mean(mean_cns))
# 
# 
# #S_df = S_df %>% mutate(num_per = ((number/cens)*100))
# 
# 
# 
# 
# ts_st = ts(S_df$number, start = as.yearmon(S_df$policydate[1]), freq = 24*15)
# decom_st = decompose(ts_st)
# 
# st_seasAdj = ts_st - decom_st$seasonal
# df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# S_adj = df_adj
# 
# lm_S = lm(cost_adj ~ time, data = S_adj)
# 
# 
# df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# S_seas = df_adj
# 
# 
# 
# 
# MD = c('MT','ID','WY','CO','NM','AZ','UT','NV')
# for (i in (1:length(MD))){
#   load(paste('data/', MD[i], '.Rda', sep = ''))
#   eval(parse(text = paste('st= ' , MD[i], sep = '')))
#   eval(parse(text = paste('remove(', MD[i],')', sep = '')))
#   
#   if (i == 1){
#     MD_df = st
#   } else {
#     MD_df = rbind(MD_df,st)
#   }
# }
# 
# MD_df = MD_df %>%
#   filter(policydate < as.Date("2019-08-01")) %>%
#   summarise_by_time(.date_var = policydate, .by= "day", 
#                     number = n())
# #cens = census%>% filter(state %in% MD) %>% 
# #  summarize(total_cens = mean(mean_cns))
# 
# 
# #MD_df = MD_df %>% mutate(num_per = ((number/cens)*100))
# 
# 
# 
# ts_st = ts(MD_df$number, start = as.yearmon(MD_df$policydate[1]), freq = 24*15)
# decom_st = decompose(ts_st)
# 
# st_seasAdj = ts_st - decom_st$seasonal
# df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# MD_adj = df_adj
# 
# lm_MD = lm(cost_adj ~ time, data = MD_adj)
# 
# 
# df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# MD_seas = df_adj
# 
# 
# 
# 
# 
# 
# 
# 
# PA = c('WA','OR','CA')
# for (i in (1:length(PA))){
#   load(paste('data/', PA[i], '.Rda', sep = ''))
#   eval(parse(text = paste('st= ' , PA[i], sep = '')))
#   eval(parse(text = paste('remove(', PA[i],')', sep = '')))
#   
#   if (i == 1){
#     PA_df = st
#   } else {
#     PA_df = rbind(PA_df,st)
#   }
# }
# 
# PA_df = PA_df %>%
#   filter(policydate < as.Date("2019-08-01")) %>%
#   summarise_by_time(.date_var = policydate, .by= "day", 
#                     number = n())
# #cens = census%>% filter(state %in% PA) %>% 
# #  summarize(total_cens = mean(mean_cns))
# 
# 
# #PA_df = PA_df %>% mutate(num_per = ((number/cens)*100))
# 
# 
# ts_st = ts(PA_df$number, start = as.yearmon(PA_df$policydate[1]), freq = 24*15)
# decom_st = decompose(ts_st)
# 
# st_seasAdj = ts_st - decom_st$seasonal
# df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# PA_adj = df_adj
# 
# lm_PA = lm(cost_adj ~ time, data = PA_adj)
# 
# 
# df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
# df_adj$year = as.numeric(trunc(df_adj$date))
# df_adj$month = (df_adj$date - df_adj$year) * 12 +1
# df_adj$month2 = as.numeric(trunc(df_adj$month))
# df_adj$day = (df_adj$month - df_adj$month2) * 30
# df_adj$day = round(as.numeric(df_adj$day + 1),0)
# df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
# df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
# PA_seas = df_adj
# 
# 
# 
# 
# MD_adj = MD_adj %>% mutate(region = "MD") 
# MD_adj = MD_adj[c(1,7,8)]
# MD_seas = MD_seas %>% mutate(region = "MD") 
# MD_seas = MD_seas[c(1,7,8)]
# 
# MW_adj = MW_adj %>% mutate(region = "MW") 
# MW_adj = MW_adj[c(1,7,8)]
# MW_seas = MW_seas %>% mutate(region = "MW") 
# MW_seas = MW_seas[c(1,7,8)]
# 
# NE_adj = NE_adj %>% mutate(region = "NE")
# NE_adj = NE_adj[c(1,7,8)]
# NE_seas = NE_seas %>% mutate(region = "NE") 
# NE_seas = NE_seas[c(1,7,8)]
# 
# PA_adj = PA_adj %>% mutate(region = "PA")
# PA_adj = PA_adj[c(1,7,8)]
# PA_seas = PA_seas %>% mutate(region = "PA") 
# PA_seas = PA_seas[c(1,7,8)]
# 
# S_adj = S_adj %>% mutate(region = "S")
# S_adj = S_adj[c(1,7,8)]
# S_seas = S_seas %>% mutate(region = "S") 
# S_seas = S_seas[c(1,7,8)]
# 
# region_adj = rbind(MD_adj,MW_adj,NE_adj,PA_adj,S_adj)
# region_seas = rbind(MD_seas,MW_seas,NE_seas,PA_seas,S_seas)
# 
# NE = c('ME','NH','MA','RI','VT','NY','PA','NJ','DE','CT')
# MW = c('ND','SD','NE','KS','MO','IA','MN','WI','IL','IN','MI','OH')
# S = c('WV','VA','MD','DC','NC','KY','TN','SC','GA','AL','MS','LA','AR','OK','TX','FL')
# MD = c('MT','ID','WY','CO','NM','AZ','UT','NV')
# PA = c('WA','OR','CA')
# cens = census %>% mutate(region = case_when(
#   state %in% PA ~ "PA",
#   state %in% MD ~ "MD",
#   state %in% MW ~ "MW",
#   state %in% NE ~ "NE",
#   state %in% S ~ "S")) %>%
#   group_by(region) %>% 
#   summarize(total_cens = mean(mean_cns))
# 
# 
# 
# 
# rm_seas = region_seas %>% group_by(region) %>%
#   summarise_by_time(.date_var = time, .by= "month", 
#                     cost = mean(cost_seas)) 
# 
# 
# region_adj = left_join(region_adj,cens, by = "region")
# rm_adj = region_adj %>% group_by(region) %>%
#   summarize_by_time(.date_var = time, .by = "month",
#                     cost = mean(cost_adj))
# 
# 
# rm_adj_yr = region_adj %>% group_by(region) %>%
#   summarize_by_time(.date_var = time, .by = "year",
#                     cost = mean(cost_adj))
# 
# 
# rm_one = rm_seas %>% group_by(region) %>%
#   filter(row_number()<=24) 
# 
# 
# 
# ggplot(inherit.aes = FALSE, data = rm_one,
#        aes(x = time, y= cost, group = region, color = region)) + 
#   theme_bw() + 
#   geom_line()+
#   facet_wrap(~region, ncol = 1) +
#   scale_x_date(date_labels = "%m", date_breaks = "2 months") +
#   labs(y = "Month",
#        x = "Number", 
#        title = "Seasonal variability of cost")
# 
# 
# 
# ggplot(inherit.aes = FALSE, data = rm_adj,
#        aes(x = time, y= cost, group = region), color = '#999999') + 
#   theme_bw() + 
#   geom_line()+
#   scale_x_date(date_labels = "%Y", date_breaks = "years") +
#   geom_line(data = rm_adj_yr, aes(x = time, y = cost, group = region, color = region),
#             size = 1.5, alpha = 0.7 ) +
#   labs(x = "Year",
#        y = "Dollar/1000K [$]", 
#        title = "Yearly variability of cost/1000K insured")
# 
# ggsave("yearly_cost_per_insured_region.jpg", width = 6, height = 4)
# 
# #### MAKE TABLE OF CHANGES
# year = as.character(2014)
# month = as.character(1)
# day = as.character(1)
# ymd = as.Date(paste(year,month,day,sep = '-'))
# p1 =data.frame(ymd)
# colnames(p1) = "time" 
# 
# year = as.character(2019)
# month = as.character(8)
# day = as.character(1)
# ymd = as.Date(paste(year,month,day,sep = '-'))
# p2 =data.frame(ymd)
# colnames(p2) = "time" 
# 
# MD_diff = predict(lm_MD,p2) - predict(lm_MD,p1)
# MW_diff = predict(lm_MW,p2) - predict(lm_MW,p1)
# NE_diff = predict(lm_NE,p2) - predict(lm_NE,p1)
# PA_diff = predict(lm_PA,p2) - predict(lm_PA,p1)
# S_diff = predict(lm_S,p2) - predict(lm_S,p1)
# 
# regions = c("MD","MW","NE","PA","S")
# cost_incr = c(MD_diff, MW_diff, NE_diff, PA_diff, S_diff)
# diff_df = data.frame(cost_incr,regions)
# 
# 
# p1 = diff_df %>% 
#   ggplot(aes(cost_incr, reorder(regions, -cost_incr),
#              group = regions, fill = regions)) + 
#   geom_col()+
#   labs(y = "Region",
#        x = "Number ", 
#        title = "Change in number of policies") +
#   geom_text(aes(x = cost_incr, 
#                 y = regions,
#                 label = round(cost_incr, 2)))
# 
# p1
# 
# ggsave("change_of_number_region.jpg", width = 4, height = 4)
# 
# 
# 
# 
# 
# p1 = ggplot(inherit.aes = FALSE, data = rm_adj,
#             aes(x = time, y= cost, group = region), alpha = 0.5) + 
#   theme_bw() + 
#   geom_line()+
#   scale_x_date(date_labels = "%Y", date_breaks = "years")
# 
# 
# p_trend = p1 + stat_smooth(inherit.aes = FALSE, data = region_adj, 
#                            aes(x = time, 
#                                y = cost_adj, 
#                                group = region,
#                                colour = region),
#                            method = "lm", se=FALSE) +
#   labs(y = "Cost [$]", x = "Year", 
#        title = "Yearly variability of cost")
# 
# p_trend
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 




## MAP with bubbles
#state_list = c('AL','AZ','AR','CA','CO','CT','DE','DC','FL','GA','ID',
#               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
#               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
#               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

#NE = c('ME','NH','MA','RI','VT','NY','PA','NJ','DE','CT')
#ne = c('maine', 'new hampshire', 'massachusetts', 'rhode island',
#       'vermont', 'new york','pennsylvania',
#       'new jersey', 'delaware','connecticut')


#MW = c('ND','SD','NE','KS','MO','IA','MN','WI','IL','IN','MI','OH')
#mw = c('north dakota','south dakota','nebraska','kansas',
#       'missouri','iowa','minnesota',
#       'wisconsin', 'illinois',
#       'indiana','michigan','ohio')

#S = c('WV','VA','MD','DC','NC','KY','TN','SC','GA','AL','MS','LA','AR','OK','TX','FL')
#s = c('west virginia','virginia','maryland','district of columbia','north carolina',
#      'kentucky','tennessee','south carolina','georgia','alabama','mississippi','louisiana',
#      'arkansas','oklahoma','texas','florida')

#MD = c('MT','ID','WY','CO','NM','AZ','UT','NV')
#md = c('montana','idaho','wyoming','colorado','new mexico','arizona','utah','nevada')

#PA = c('WA','OR','CA')
#pa = c('washington','oregon','california')

#usm = map_data('usa')
#usstates = map_data("state")
#states = unique(usstates$region)
#merg_states = data.frame(state_list,states)
#merg_states = rename(merg_states,"region" = "states")


#usstates = left_join(usstates,merg_states, by = "region")
#usstates = usstates %>% mutate(geo_region = case_when(
#      state_list %in% NE ~ "NE",
#      state_list %in% MW ~ "MW",
#      state_list %in% S ~ "S",
#      state_list %in% MD ~ "MD",
#      state_list %in% PA ~ "PA"))


#p2 = ggplot(dat = usstates, 
#            mapping = aes(x = long,y = lat, group = group, color = geo_region)) +
#      theme_bw()+
#     #geom_polygon(color = "gray90", size = 0.1) + 
#      geom_polygon()+
#      coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
#                xlim = c(-118, -75), ylim = c(50, 25)) + 
#      theme(axis.ticks.x = element_blank(),
#            axis.text.x = element_blank()) + 
#      geom_point(data = diff_df, 
#                 aes(x = re_lon, y = re_lat, 
#                     group = regions, 
#                     size = cost_incr*10e6,
#                 alpha = 0.5),
#                 color = "white", show.legend = FALSE) +
#      scale_color_manual(name="Region", 
#                         values=c("MD"= hex_codes1[1],
#                                  "MW" = hex_codes1[2],
#                                  "NE" = hex_codes1[3],
#                                  "PA" = hex_codes1[4],
#                                  "S" = hex_codes1[5]))

      
#p2      

#ggsave("change_of_cost_region_map.jpg", width = 5, height = 2.5)


