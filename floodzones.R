library(usmap)
library(ggplot2)
library(maps)
library(tidyverse) 


#plot_usmap(regions = "state") + 
#      labs(title = "US States",
#           subtitle = "Map of states in the United States.") + 
#      theme(panel.background = element_rect(color = "black", fill = "lightblue"))

###############


state_list = c('AL','AZ','AR','CA','CO','CT','DE','DC','FL','GA','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

NE = c('ME','NH','MA','RI','VT','NY','PA','NJ','DE','CT')
ne = c('maine', 'new hampshire', 'massachusetts', 'rhode island',
       'vermont', 'new york','pennsylvania',
       'new jersey', 'delaware','connecticut')


MW = c('ND','SD','NE','KS','MO','IA','MN','WI','IL','IN','MI','OH')
mw = c('north dakota','south dakota','nebraska','kansas',
       'missouri','iowa','minnesota',
       'wisconsin', 'illinois',
       'indiana','michigan','ohio')

S = c('WV','VA','MD','DC','NC','KY','TN','SC','GA','AL','MS','LA','AR','OK','TX','FL')
s = c('west virginia','virginia','maryland','district of columbia','north carolina',
      'kentucky','tennessee','south carolina','georgia','alabama','mississippi','louisiana',
      'arkansas','oklahoma','texas','florida')

MD = c('MT','ID','WY','CO','NM','AZ','UT','NV')
md = c('montana','idaho','wyoming','colorado','new mexico','arizona','utah','nevada')

PA = c('WA','OR','CA')
pa = c('washington','oregon','california')



usm = map_data('usa')
usstates = map_data("state")
states = unique(usstates$region)
merg_states = data.frame(state_list,states)
merg_states = rename(merg_states,"region" = "states")


usstates = left_join(usstates,merg_states, by = "region")
usstates = usstates %>% mutate(geo_region = case_when(
   state_list %in% NE ~ "NE",
   state_list %in% MW ~ "MW",
   state_list %in% S ~ "S",
   state_list %in% MD ~ "MD",
   state_list %in% PA ~ "PA")) 
usstates = usstates %>% rename("state" = "state_list")

############################################################
############### MEAN POLICY COST BY STATE ###############
############################################################

for (i in 1:length(state_list)){
   eval(parse(text = paste("load('data/", state_list[i], ".Rda')", sep = '')))
   eval(parse(text = paste("DD = ", state_list[i], sep = '')))
   eval(parse(text = paste("remove(", state_list[i],")", sep = "")))
   
   DD = DD %>% mutate(flood = case_when(
      startsWith(floodzone,"A") ~ 1,
      startsWith(floodzone,"V") ~ 1, 
      startsWith(floodzone, "X") ~ 0,
      startsWith(floodzone, "C") ~ 0,
      startsWith(floodzone, "D") ~ 0,
      startsWith(floodzone, "B") ~ 0))
   
   
   RATIO = DD %>%
      group_by(flood) %>% 
      summarize(number = n()) %>% 
      filter(!is.na(flood)) %>% 
      mutate(ratio = (number/sum(number))*100, state = state_list[i])
      
      

   diff_cost = DD %>% 
      group_by(flood) %>%
      summarize(mean_cost = mean(policycost),
                amt = mean(building_ins + content_ins)) %>%
      filter(!is.na(flood)) 
      
   flood = left_join(RATIO, diff_cost, by ="flood")   
   
   if (i == 1){
      df = flood
   } else {
      df = rbind(df,flood)
   }
   print(i)
} 

PERC = df %>% group_by(state) %>% summarize(perc_diff = ratio[1] - ratio[2])



flood_ratio = df %>% filter(flood == 1) %>% select(c(3,4))

ggplot(data = flood_ratio, aes(x = ratio, reorder(state, -ratio))) + 
   geom_col()+
   labs(y = "State",
        x = "Cost/1000 $ [mil $]", 
        title = "")
p3 + theme(axis.text.y = element_text(size = 12),
           axis.text.x = element_text(size = 12))

ggsave("figures/flood_ratio.jpg", width = 7, height = 10)




usstates = left_join(usstates,PERC, by = "state")

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = perc_diff))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
      coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
                xlim = c(-118, -75), ylim = c(50, 25)) + 
      labs(title = 'NO floodzoone - floodzone [%]') + labs(fill = "[%] differences") + 
      scale_fill_distiller(palette = "RdBu")
p2

ggsave("figures/floodzone_perc_diff.jpg", width = 8, height = 6)




states_uni = usstates %>% group_by(state) %>% summarize(perc_diff = mean(perc_diff)) 


p3 = states_uni %>% ggplot(aes(perc_diff, reorder(state, -perc_diff))) + geom_col()+
      labs(y = "State", x = "[%] difference", title = "")
p3 + theme(axis.text.y = element_text(size = 6))
ggsave("figures/floodzone_perc_diff_bars.jpg", width = 4, height = 6)



cost_diff = df %>% group_by(state) %>% summarize(cost_diff = mean_cost[1] - mean_cost[2])

usstates_cost_diff = left_join(usstates,cost_diff, by = "state")

xmin = min(usstates_cost_diff$cost_diff)
xmax = max(usstates_cost_diff$cost_diff)

fill_range <- seq(xmin, xmax, by=20)
p2 = ggplot(dat = usstates_cost_diff, 
            mapping = aes(x = long,y = lat, group = group, fill = cost_diff))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(title = 'NO floodzoone - floodzone cost') + labs(fill = "[$] differences") + 
   scale_fill_distiller(palette = "Blues")
p2

ggsave("figures/floodzone_cost_diff.jpg", width = 8, height = 6)


states_uni = usstates_cost_diff %>% group_by(state) %>% summarize(cost_diff = mean(cost_diff)) 

p3 = states_uni %>% ggplot(aes(cost_diff, reorder(state, -cost_diff))) + geom_col()+
   labs(y = "State", x = "[$] difference", title = "")
p3 + theme(axis.text.y = element_text(size = 6))
ggsave("figures/floodzone_cost_diff_bars.jpg", width = 4, height = 6)




cost_amt_diff = df %>% group_by(state) %>% 
   summarize(cost_amt_diff = ((mean_cost[1]/amt[1]) - (mean_cost[2]/amt[2]))*1000)


usstates_amt_diff = left_join(usstates,cost_amt_diff, by = "state")


p2 = ggplot(dat = usstates_amt_diff, 
            mapping = aes(x = long,y = lat, group = group, fill = cost_amt_diff))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(title = 'NO floodzoone - floodzone amount per $') + labs(fill = "[$] differences") + 
   scale_fill_distiller(palette = "Blues")
p2

ggsave("figures/floodzone_amt_diff.jpg", width = 8, height = 6)


states_uni = usstates_amt_diff %>% group_by(state) %>% summarize(amt_diff = mean(cost_amt_diff)) 

p3 = states_uni %>% ggplot(aes(amt_diff, reorder(state, -amt_diff))) + geom_col()+
   labs(y = "State", x = "cost $/1000$", title = "")
p3 + theme(axis.text.y = element_text(size = 6))

ggsave("figures/floodzone_amt_diff_bars.jpg", width = 4, height = 6)


###############################################################################
 ################### Change of proportion #################################
##############################################################################
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
NE_df_flood = NE_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))


NE_df_noflood = NE_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>%
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost)) 





ts_st_flood = ts(NE_df_flood$cost, start = as.yearmon(NE_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_flood_adj = df_adj

lm_NE_flood = lm(cost_adj ~ time, data = NE_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_flood_seas = df_adj



ts_st_noflood = ts(NE_df_noflood$cost, start = as.yearmon(NE_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_noflood_adj = df_adj

lm_NE_noflood = lm(cost_adj ~ time, data = NE_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_noflood_seas = df_adj







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
MW_df_flood = MW_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))

MW_df_noflood = MW_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))





ts_st_flood = ts(MW_df_flood$cost, start = as.yearmon(MW_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_flood_adj = df_adj

lm_MW_flood = lm(cost_adj ~ time, data = MW_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_flood_seas = df_adj



ts_st_noflood = ts(MW_df_noflood$cost, start = as.yearmon(MW_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_noflood_adj = df_adj

lm_MW_noflood = lm(cost_adj ~ time, data = MW_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_noflood_seas = df_adj





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
S_df_flood = S_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))

S_df_noflood = S_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))




ts_st_flood = ts(S_df_flood$cost, start = as.yearmon(S_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_flood_adj = df_adj

lm_S_flood = lm(cost_adj ~ time, data = S_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_flood_seas = df_adj



ts_st_noflood = ts(S_df_noflood$cost, start = as.yearmon(S_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_noflood_adj = df_adj

lm_S_noflood = lm(cost_adj ~ time, data = S_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_noflood_seas = df_adj





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
MD_df_flood = MD_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))

MD_df_noflood = MD_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))





ts_st_flood = ts(MD_df_flood$cost, start = as.yearmon(MD_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_flood_adj = df_adj

lm_MD_flood = lm(cost_adj ~ time, data = MD_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_flood_seas = df_adj



ts_st_noflood = ts(MD_df_noflood$cost, start = as.yearmon(MD_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_noflood_adj = df_adj

lm_MD_noflood = lm(cost_adj ~ time, data = MD_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_noflood_seas = df_adj









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
PA_df_flood = PA_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))

PA_df_noflood = PA_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost))





ts_st_flood = ts(PA_df_flood$cost, start = as.yearmon(PA_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_flood_adj = df_adj

lm_PA_flood = lm(cost_adj ~ time, data = PA_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_flood_seas = df_adj



ts_st_noflood = ts(PA_df_noflood$cost, start = as.yearmon(PA_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_noflood_adj = df_adj

lm_PA_noflood = lm(cost_adj ~ time, data = PA_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_noflood_seas = df_adj







MD_flood_adj = MD_flood_adj %>% mutate(region = "MD") 
MD_flood_adj = MD_flood_adj[c(1,7,8)]
MD_flood_seas = MD_flood_seas %>% mutate(region = "MD") 
MD_flood_seas = MD_flood_seas[c(1,7,8)]


MD_noflood_adj = MD_noflood_adj %>% mutate(region = "MD") 
MD_noflood_adj = MD_noflood_adj[c(1,7,8)]
MD_noflood_seas = MD_noflood_seas %>% mutate(region = "MD") 
MD_noflood_seas = MD_noflood_seas[c(1,7,8)]



MW_flood_adj = MW_flood_adj %>% mutate(region = "MW") 
MW_flood_adj = MW_flood_adj[c(1,7,8)]
MW_flood_seas = MW_flood_seas %>% mutate(region = "MW") 
MW_flood_seas = MW_flood_seas[c(1,7,8)]


MW_noflood_adj = MW_noflood_adj %>% mutate(region = "MW") 
MW_noflood_adj = MW_noflood_adj[c(1,7,8)]
MW_noflood_seas = MW_noflood_seas %>% mutate(region = "MW") 
MW_noflood_seas = MW_noflood_seas[c(1,7,8)]




NE_flood_adj = NE_flood_adj %>% mutate(region = "NE") 
NE_flood_adj = NE_flood_adj[c(1,7,8)]
NE_flood_seas = NE_flood_seas %>% mutate(region = "NE") 
NE_flood_seas = NE_flood_seas[c(1,7,8)]


NE_noflood_adj = NE_noflood_adj %>% mutate(region = "NE") 
NE_noflood_adj = NE_noflood_adj[c(1,7,8)]
NE_noflood_seas = NE_noflood_seas %>% mutate(region = "NE") 
NE_noflood_seas = NE_noflood_seas[c(1,7,8)]



PA_flood_adj = PA_flood_adj %>% mutate(region = "PA") 
PA_flood_adj = PA_flood_adj[c(1,7,8)]
PA_flood_seas = PA_flood_seas %>% mutate(region = "PA") 
PA_flood_seas = PA_flood_seas[c(1,7,8)]


PA_noflood_adj = PA_noflood_adj %>% mutate(region = "PA") 
PA_noflood_adj = PA_noflood_adj[c(1,7,8)]
PA_noflood_seas = PA_noflood_seas %>% mutate(region = "PA") 
PA_noflood_seas = PA_noflood_seas[c(1,7,8)]





S_flood_adj = S_flood_adj %>% mutate(region = "S") 
S_flood_adj = S_flood_adj[c(1,7,8)]
S_flood_seas = S_flood_seas %>% mutate(region = "S") 
S_flood_seas = S_flood_seas[c(1,7,8)]


S_noflood_adj = S_noflood_adj %>% mutate(region = "S") 
S_noflood_adj = S_noflood_adj[c(1,7,8)]
S_noflood_seas = S_noflood_seas %>% mutate(region = "S") 
S_noflood_seas = S_noflood_seas[c(1,7,8)]


region_flood_adj = rbind(MD_flood_adj,MW_flood_adj,NE_flood_adj,PA_flood_adj,S_flood_adj)
region_flood_seas = rbind(MD_flood_seas,MW_flood_seas,NE_flood_seas,PA_flood_seas,S_flood_seas)

region_noflood_adj = rbind(MD_noflood_adj,MW_noflood_adj,NE_noflood_adj,PA_noflood_adj,S_noflood_adj)
region_noflood_seas = rbind(MD_noflood_seas,MW_noflood_seas,NE_noflood_seas,PA_noflood_seas,S_noflood_seas)



rm_flood_seas = region_flood_seas %>% group_by(region) %>%
   summarise_by_time(.date_var = time, .by= "month", 
                     cost = mean(cost_seas)) 

rm_flood_adj = region_flood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "month",
                     cost = mean(cost_adj))


rm_flood_adj_yr = region_flood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "year",
                     cost = mean(cost_adj))

rm_flood_one = rm_flood_seas %>% group_by(region) %>%
   filter(row_number()<=24) 


rm_noflood_seas = region_noflood_seas %>% group_by(region) %>%
   summarise_by_time(.date_var = time, .by= "month", 
                     cost = mean(cost_seas)) 

rm_noflood_adj = region_noflood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "month",
                     cost = mean(cost_adj))


rm_noflood_adj_yr = region_noflood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "year",
                     cost = mean(cost_adj))

rm_noflood_one = rm_noflood_seas %>% group_by(region) %>%
   filter(row_number()<=24) 



hur_start = c("2009-06-01","2010-06-01")
hur_start = as.Date(hur_start, format = "%Y-%m-%d")

hur_end = c("2009-12-01","2010-12-01")
hur_end = as.Date(hur_end, format = "%Y-%m-%d")



P1 = ggplot(inherit.aes = FALSE, data = rm_flood_one,
       aes(x = time, y= cost, group = region, color = region)) + 
   theme_bw() + 
   geom_rect(inherit.aes = F, aes(xmin = hur_start[1], xmax = hur_end[1], ymin = -Inf, ymax = Inf),
             fill = "gray", alpha = 0.03) +
   geom_rect(inherit.aes = F, aes(xmin = hur_start[2], xmax = hur_end[2], ymin = -Inf, ymax = Inf),
             fill = "gray", alpha = 0.03) +
   geom_line()+
   facet_wrap(~region, ncol = 1) +
   scale_x_date(date_labels = "%m", date_breaks = "2 months") +
   labs(x = "Month",
        y = "Dollar [$]", 
        title = "")

P1


ggsave("figures/region_flood_seasons.jpg", width =4, height = 6)



P1 + geom_line(data = rm_noflood_one,
               aes(group = region), color = "darkgray") +
   facet_wrap(~region, ncol = 1) +
   scale_x_date(date_labels = "%m", date_breaks = "2 months") +
   ylim(-60,60)
   


ggsave("figures/region_noflood_seasons.jpg", width =4, height = 6)




p2 = ggplot(data = rm_flood_adj,
       aes(x = time, y= cost, group = region), color = "darkgray") + 
   theme_bw() + 
   geom_line()+
   scale_x_date(date_labels = "%Y", date_breaks = "years") +
   geom_line(data = rm_flood_adj_yr, aes(x = time, y = cost, group = region, color = region),
             size = 1.5, alpha = 0.7 ) +
   labs(x = "Year",
        y = "Dollar [$]", 
        title = "Yearly variability of cost")

p2
p2 + geom_line(data = rm_noflood_adj,
               aes(group = region), color = "darkgray") +
   geom_line(data = rm_noflood_adj_yr, aes(group = region, color = region),
             size = 1.5, alpha = 0.7 )



ggsave("regions_yearly_flood.jpg", width = 6, height = 5)




year = as.character(2009)
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

MD_diff_flood = predict(lm_MD_flood,p2) - predict(lm_MD_flood,p1)
MW_diff_flood = predict(lm_MW_flood,p2) - predict(lm_MW_flood,p1)
NE_diff_flood = predict(lm_NE_flood,p2) - predict(lm_NE_flood,p1)
PA_diff_flood = predict(lm_PA_flood,p2) - predict(lm_PA_flood,p1)
S_diff_flood = predict(lm_S_flood,p2) - predict(lm_S_flood,p1)

regions = c("MD","MW","NE","PA","S")
cost_incr = c(MD_diff_flood, MW_diff_flood, NE_diff_flood, PA_diff_flood, S_diff_flood)
diff_df_flood = data.frame(cost_incr,regions)


MD_diff_noflood = predict(lm_MD_noflood,p2) - predict(lm_MD_noflood,p1)
MW_diff_noflood = predict(lm_MW_noflood,p2) - predict(lm_MW_noflood,p1)
NE_diff_noflood = predict(lm_NE_noflood,p2) - predict(lm_NE_noflood,p1)
PA_diff_noflood = predict(lm_PA_noflood,p2) - predict(lm_PA_noflood,p1)
S_diff_noflood = predict(lm_S_noflood,p2) - predict(lm_S_noflood,p1)

regions = c("MD","MW","NE","PA","S")
cost_incr = c(MD_diff_noflood, MW_diff_noflood, NE_diff_noflood, PA_diff_noflood, S_diff_noflood)
diff_df_noflood = data.frame(cost_incr,regions)



diff_df_flood = diff_df_flood %>% mutate(flood = "1")
diff_df_noflood = diff_df_noflood %>% mutate(flood = "0")
diff_df = rbind(diff_df_flood,diff_df_noflood)


p1 = diff_df %>% 
   ggplot(aes(cost_incr,regions, fill = flood)) + 
   geom_bar(stat = "identity",position = 'dodge')+
   labs(y = "Region",
        x = "Dollar [$]", 
        title = "") +
   scale_fill_discrete(labels = c("Non - Floodzone", "Floodzone"))
p1

ggsave("figures/Change_cost_floodzones.jpg", width = 4, height = 4)



###############################################################################
################### Change of proportion #################################
##############################################################################
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
NE_df_flood = NE_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 


NE_df_noflood = NE_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 
   




ts_st_flood = ts(NE_df_flood$cost_per, start = as.yearmon(NE_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_flood_adj = df_adj

lm_NE_flood = lm(cost_adj ~ time, data = NE_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_flood_seas = df_adj



ts_st_noflood = ts(NE_df_noflood$cost_per, start = as.yearmon(NE_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_noflood_adj = df_adj

lm_NE_noflood = lm(cost_adj ~ time, data = NE_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
NE_noflood_seas = df_adj







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
MW_df_flood = MW_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 



MW_df_noflood = MW_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 




ts_st_flood = ts(MW_df_flood$cost_per, start = as.yearmon(MW_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_flood_adj = df_adj

lm_MW_flood = lm(cost_adj ~ time, data = MW_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_flood_seas = df_adj



ts_st_noflood = ts(MW_df_noflood$cost_per, start = as.yearmon(MW_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_noflood_adj = df_adj

lm_MW_noflood = lm(cost_adj ~ time, data = MW_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MW_noflood_seas = df_adj





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
S_df_flood = S_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 




S_df_noflood = S_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 




ts_st_flood = ts(S_df_flood$cost_per, start = as.yearmon(S_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_flood_adj = df_adj

lm_S_flood = lm(cost_adj ~ time, data = S_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_flood_seas = df_adj



ts_st_noflood = ts(S_df_noflood$cost_per, start = as.yearmon(S_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_noflood_adj = df_adj

lm_S_noflood = lm(cost_adj ~ time, data = S_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
S_noflood_seas = df_adj





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
MD_df_flood = MD_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 





MD_df_noflood = MD_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 




ts_st_flood = ts(MD_df_flood$cost_per, start = as.yearmon(MD_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_flood_adj = df_adj

lm_MD_flood = lm(cost_adj ~ time, data = MD_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_flood_seas = df_adj



ts_st_noflood = ts(MD_df_noflood$cost_per, start = as.yearmon(MD_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_noflood_adj = df_adj

lm_MD_noflood = lm(cost_adj ~ time, data = MD_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
MD_noflood_seas = df_adj









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
PA_df_flood = PA_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "1") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 

PA_df_noflood = PA_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   mutate(flood = case_when(
      startsWith(floodzone,"A") ~"1",
      startsWith(floodzone,"V") ~ "1", 
      startsWith(floodzone, "X") ~ "0",
      startsWith(floodzone, "C") ~ "0",
      startsWith(floodzone, "D") ~ "0",
      startsWith(floodzone, "B") ~ "0")) %>%
   group_by(flood) %>%
   filter(flood == "0") %>% 
   summarise_by_time(.date_var = policydate, .by= "day", 
                     cost = mean(policycost),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(cost_per = (cost/amt)*1000) 




ts_st_flood = ts(PA_df_flood$cost_per, start = as.yearmon(PA_df_flood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_flood)

st_seasAdj = ts_st_flood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_flood_adj = df_adj

lm_PA_flood = lm(cost_adj ~ time, data = PA_flood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_flood_seas = df_adj



ts_st_noflood = ts(PA_df_noflood$cost_per, start = as.yearmon(PA_df_noflood$policydate[1]), freq = 24*15)
decom_st = decompose(ts_st_noflood)

st_seasAdj = ts_st_noflood - decom_st$seasonal
df_adj = data.frame(cost_adj = as.matrix(st_seasAdj), date =time(st_seasAdj))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_noflood_adj = df_adj

lm_PA_noflood = lm(cost_adj ~ time, data = PA_noflood_adj)


df_adj = data.frame(cost_seas = as.matrix(decom_st$seasonal), date =time(decom_st$seasonal))
df_adj$year = as.numeric(trunc(df_adj$date))
df_adj$month = (df_adj$date - df_adj$year) * 12 +1
df_adj$month2 = as.numeric(trunc(df_adj$month))
df_adj$day = (df_adj$month - df_adj$month2) * 30
df_adj$day = round(as.numeric(df_adj$day + 1),0)
df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")
PA_noflood_seas = df_adj







MD_flood_adj = MD_flood_adj %>% mutate(region = "MD") 
MD_flood_adj = MD_flood_adj[c(1,7,8)]
MD_flood_seas = MD_flood_seas %>% mutate(region = "MD") 
MD_flood_seas = MD_flood_seas[c(1,7,8)]


MD_noflood_adj = MD_noflood_adj %>% mutate(region = "MD") 
MD_noflood_adj = MD_noflood_adj[c(1,7,8)]
MD_noflood_seas = MD_noflood_seas %>% mutate(region = "MD") 
MD_noflood_seas = MD_noflood_seas[c(1,7,8)]



MW_flood_adj = MW_flood_adj %>% mutate(region = "MW") 
MW_flood_adj = MW_flood_adj[c(1,7,8)]
MW_flood_seas = MW_flood_seas %>% mutate(region = "MW") 
MW_flood_seas = MW_flood_seas[c(1,7,8)]


MW_noflood_adj = MW_noflood_adj %>% mutate(region = "MW") 
MW_noflood_adj = MW_noflood_adj[c(1,7,8)]
MW_noflood_seas = MW_noflood_seas %>% mutate(region = "MW") 
MW_noflood_seas = MW_noflood_seas[c(1,7,8)]




NE_flood_adj = NE_flood_adj %>% mutate(region = "NE") 
NE_flood_adj = NE_flood_adj[c(1,7,8)]
NE_flood_seas = NE_flood_seas %>% mutate(region = "NE") 
NE_flood_seas = NE_flood_seas[c(1,7,8)]


NE_noflood_adj = NE_noflood_adj %>% mutate(region = "NE") 
NE_noflood_adj = NE_noflood_adj[c(1,7,8)]
NE_noflood_seas = NE_noflood_seas %>% mutate(region = "NE") 
NE_noflood_seas = NE_noflood_seas[c(1,7,8)]



PA_flood_adj = PA_flood_adj %>% mutate(region = "PA") 
PA_flood_adj = PA_flood_adj[c(1,7,8)]
PA_flood_seas = PA_flood_seas %>% mutate(region = "PA") 
PA_flood_seas = PA_flood_seas[c(1,7,8)]


PA_noflood_adj = PA_noflood_adj %>% mutate(region = "PA") 
PA_noflood_adj = PA_noflood_adj[c(1,7,8)]
PA_noflood_seas = PA_noflood_seas %>% mutate(region = "PA") 
PA_noflood_seas = PA_noflood_seas[c(1,7,8)]





S_flood_adj = S_flood_adj %>% mutate(region = "S") 
S_flood_adj = S_flood_adj[c(1,7,8)]
S_flood_seas = S_flood_seas %>% mutate(region = "S") 
S_flood_seas = S_flood_seas[c(1,7,8)]


S_noflood_adj = S_noflood_adj %>% mutate(region = "S") 
S_noflood_adj = S_noflood_adj[c(1,7,8)]
S_noflood_seas = S_noflood_seas %>% mutate(region = "S") 
S_noflood_seas = S_noflood_seas[c(1,7,8)]


region_flood_adj = rbind(MD_flood_adj,MW_flood_adj,NE_flood_adj,PA_flood_adj,S_flood_adj)
region_flood_seas = rbind(MD_flood_seas,MW_flood_seas,NE_flood_seas,PA_flood_seas,S_flood_seas)

region_noflood_adj = rbind(MD_noflood_adj,MW_noflood_adj,NE_noflood_adj,PA_noflood_adj,S_noflood_adj)
region_noflood_seas = rbind(MD_noflood_seas,MW_noflood_seas,NE_noflood_seas,PA_noflood_seas,S_noflood_seas)



rm_flood_seas = region_flood_seas %>% group_by(region) %>%
   summarise_by_time(.date_var = time, .by= "month", 
                     cost = mean(cost_seas)) 

rm_flood_adj = region_flood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "month",
                     cost = mean(cost_adj))


rm_flood_adj_yr = region_flood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "year",
                     cost = mean(cost_adj))

rm_flood_one = rm_flood_seas %>% group_by(region) %>%
   filter(row_number()<=24) 


rm_noflood_seas = region_noflood_seas %>% group_by(region) %>%
   summarise_by_time(.date_var = time, .by= "month", 
                     cost = mean(cost_seas)) 

rm_noflood_adj = region_noflood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "month",
                     cost = mean(cost_adj))


rm_noflood_adj_yr = region_noflood_adj %>% group_by(region) %>%
   summarize_by_time(.date_var = time, .by = "year",
                     cost = mean(cost_adj))

rm_noflood_one = rm_noflood_seas %>% group_by(region) %>%
   filter(row_number()<=24) 


year = as.character(2009)
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

MD_diff_flood = predict(lm_MD_flood,p2) - predict(lm_MD_flood,p1)
MW_diff_flood = predict(lm_MW_flood,p2) - predict(lm_MW_flood,p1)
NE_diff_flood = predict(lm_NE_flood,p2) - predict(lm_NE_flood,p1)
PA_diff_flood = predict(lm_PA_flood,p2) - predict(lm_PA_flood,p1)
S_diff_flood = predict(lm_S_flood,p2) - predict(lm_S_flood,p1)

regions = c("MD","MW","NE","PA","S")
cost_incr = c(MD_diff_flood, MW_diff_flood, NE_diff_flood, PA_diff_flood, S_diff_flood)
diff_df_flood = data.frame(cost_incr,regions)


MD_diff_noflood = predict(lm_MD_noflood,p2) - predict(lm_MD_noflood,p1)
MW_diff_noflood = predict(lm_MW_noflood,p2) - predict(lm_MW_noflood,p1)
NE_diff_noflood = predict(lm_NE_noflood,p2) - predict(lm_NE_noflood,p1)
PA_diff_noflood = predict(lm_PA_noflood,p2) - predict(lm_PA_noflood,p1)
S_diff_noflood = predict(lm_S_noflood,p2) - predict(lm_S_noflood,p1)

regions = c("MD","MW","NE","PA","S")
cost_incr = c(MD_diff_noflood, MW_diff_noflood, NE_diff_noflood, PA_diff_noflood, S_diff_noflood)
diff_df_noflood = data.frame(cost_incr,regions)



diff_df_flood = diff_df_flood %>% mutate(flood = "1")
diff_df_noflood = diff_df_noflood %>% mutate(flood = "0")
diff_df = rbind(diff_df_flood,diff_df_noflood)


p1 = diff_df %>% 
   ggplot(aes(cost_incr,regions, fill = flood)) + 
   geom_bar(stat = "identity",position = 'dodge')+
   labs(y = "Region",
        x = "Dollar [$]", 
        title = "") +
   scale_fill_discrete(labels = c("Non - Floodzone", "Floodzone"))  
p1

ggsave("figures/Change_amount_floodzones.jpg", width = 4, height = 4)




