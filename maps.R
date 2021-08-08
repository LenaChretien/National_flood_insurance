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

usm = map_data('usa')
usstates = map_data("state")
states = unique(usstates$region)

############################################################
############### MEAN POLICY COST BY STATE ###############
############################################################

for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   DD = DD %>% summarize(mean_cost = mean(DD$policycost), 
                      number = n(),
                      amt = mean(DD$building_ins + DD$content_ins))
   
   if (i == 1){
      df = data.frame(states[i], DD)
      df['state'] = state_list[i]
      colnames(df) = c('region', 'mean_cost','number','amt','state')
   } else {
      dfdf = data.frame(states[i],DD)
      dfdf['state'] = state_list[i]
      colnames(dfdf) = c('region', 'mean_cost','number','amt','state')
      df = rbind(df,dfdf)
   }
   print(i)
} 

usstates = left_join(usstates,df, by = "region")
   





p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = mean_cost))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(fill = "Mean Cost [$}", x = '', y = '') +
   theme(axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 7), 
         legend.text = element_text(size = 7),
         legend.key.size = unit(1,"cm"),
         legend.key.width = unit(0.5,"cm"))+ 
   scale_fill_gradient(low = "white", high = "#800026")
p2
ggsave("state_cost.jpg", width = 6, height = 4)



states_uni = usstates %>% group_by(state) %>% summarize(mean_cost = mean(mean_cost)) 


p3 = states_uni %>% 
   ggplot(aes(mean_cost, reorder(state, -mean_cost))) + 
   geom_col()+
   labs(y = "State",
        x = "Policy Cost [$]", 
        title = "Average cost of policy per state")
p3 + theme(axis.text.y = element_text(size = 12),
           axis.text.x = element_text(size = 12))
ggsave("state_cost_bar.jpg", width = 6, height = 10)





############################################################
############### NUMBER OF POLICIES BY STATE ###############
############################################################


census = read.csv("data/nst-est2019-01.csv", header = F, sep = ",", fill = TRUE)
census = census %>% filter(V1 !="Alaska") %>% filter(V1 != "Hawaii") 
   #mutate(state = state_list)
census = census[c(7,8,9,10,11,12,13)]
colnames(census) = c("2013","2014","2015","2016","2017","2018","2019")
census$mean_cns = rowMeans(census)
census = cbind(census,state_list)
colnames(census) = c("2013","2014","2015","2016","2017","2018","2019","mean_cns","state")



usstates_census = left_join(usstates,census, by = "state")


p2 = ggplot(dat = usstates_census, 
            mapping = aes(x = long,y = lat, group = group, fill = (number/mean_cns)*100))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(fill = "Percent [%]", x = '', y = '') +
   theme(axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 7), 
         legend.text = element_text(size = 7),
         legend.key.size = unit(1,"cm"),
         legend.key.width = unit(0.5,"cm"))+ 
   scale_fill_gradient(low = "white", high = "#800026") +
   labs(title = "Number of policies per population")
p2

ggsave("state_policy_percent.jpg", width = 6, height = 4)



states_uni = usstates_census %>% group_by(state) %>% summarize(percent_policies = mean((number/mean_cns)*100)) 


p3 = states_uni %>% 
   ggplot(aes(percent_policies, reorder(state, -percent_policies))) + 
   geom_col()+
   labs(y = "State",
        x = "Policy Cost [$]", 
        title = "Average cost of policy per state")
p3 + theme(axis.text.y = element_text(size = 12),
           axis.text.x = element_text(size = 12))
ggsave("state_percent_bar.jpg", width = 6, height = 10)








###### Total amount covered by insurance
# Add building and content together
############################################################
############### MEAN TOTAL AMOUNT COVERED BY COST BY STATE ###############
############################################################

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = amt/1e3))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(fill = "Amount [K $]", x = '', y = '') +
   theme(axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 7), 
         legend.text = element_text(size = 7),
         legend.key.size = unit(1,"cm"),
         legend.key.width = unit(0.5,"cm"))+ 
   scale_fill_gradient(low = "white", high = "#800026")
p2
ggsave("state_amt.jpg", width = 6, height = 4)



states_uni = usstates %>% group_by(state) %>% summarize(amount_ins = mean(amt)) 


p3 = states_uni %>% 
   ggplot(aes(amount_ins/1e3, reorder(state, -amount_ins))) + 
   geom_col()+
   labs(y = "State",
        x = "Amount [K $]", 
        title = "Average amount insured")
p3 + theme(axis.text.y = element_text(size = 12),
           axis.text.x = element_text(size = 12))
ggsave("state_amt_bar.jpg", width = 6, height = 10)







###### cost per $ insured 
##### cost/amt
############################################################
############### MEAN TOTAL AMOUNT COVERED BY COST BY STATE ###############
############################################################

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = (mean_cost/amt)*1000))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(fill = "Cost per insured [$/K $]", x = '', y = '') +
   theme(axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 7), 
         legend.text = element_text(size = 7),
         legend.key.size = unit(1,"cm"),
         legend.key.width = unit(0.5,"cm"))+ 
   scale_fill_gradient(low = "white", high = "#800026")
p2
ggsave("state_cost_per_amt.jpg", width = 6, height = 4)



states_uni = usstates %>% group_by(state) %>% summarize(cost_amt = mean((mean_cost/amt)*1000)) 


p3 = states_uni %>% 
   ggplot(aes(cost_amt, reorder(state, -cost_amt))) + 
   geom_col()+
   labs(y = "State",
        x = "Cost/1000 $ [mil $]", 
        title = "Cost per 1000$")
p3 + theme(axis.text.y = element_text(size = 12),
           axis.text.x = element_text(size = 12))
ggsave("state_cost_per_amt.jpg", width = 6, height = 10)










###### Insurance amount by county
#library(zipcodeR)

############################################################
############### MEAN POLICYCOST BY COUNTY ###############
############################################################

 

#for (i in 1:length(state_list)){
#   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   
#   eval(parse(text = paste("zip_county = search_state('", state_list[i], "')", sep = '')))
   
#   zip_county = zip_county %>% 
#      select("zip" = "zipcode","county", "lat", "lng") %>% 
#      mutate(zip = as.integer(zip)) %>%
#      mutate(county = tolower(county)) %>% mutate(county = gsub(' county','',county))
   
#   DD_new = left_join(DD,zip_county, by = "zip")
   
#   avg_cost = DD_new %>% group_by(county) %>% 
#      summarise(mean_cost = mean(policycost)) %>% 
#      mutate(region = states[i])
   
#   avg_cost['state'] = state_list[i]
   
#   if (i == 1){
#      df = avg_cost
#   } else {
#      df = rbind(df,avg_cost)
#   }
#   print(i)
#} 


#df = df %>% rename(subregion = county) 


#uscounty = map_data("county")
#uscounty = left_join(uscounty,df, by = c("subregion","region"))
#uscounty = uscounty %>%  
#   filter(!is.na(mean_cost)) %>% 
#   select("long", "lat", "group","subregion","mean_cost")
                               
                               
                               

#p2 = ggplot(dat = uscounty, 
#            mapping = aes(x = long,y = lat, group = group, fill = mean_cost)) 

#p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
#   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
#             xlim = c(-118, -75), ylim = c(50, 25)) + 
#   labs(title = 'Average policy cost by county') + labs(fill = "mean_cost") + 
#   labs(x = 'Longitude', y = 'Latitude') + 
#   scale_fill_gradient(low = "#ffffd9", high = "#081d58")
#p2


############################################################
######## TOTAL AMOUNT OF MONEY PAYED FOR INSURANCE  ###############
###### sum of policy cost  ##################
############################################################

usm = map_data('usa')
usstates = map_data("state")
states = unique(usstates$region)


for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   DD = sum(DD$policycost)
   if (i == 1){
      df = data.frame(states[i], DD)
      df['state'] = state_list[i]
      colnames(df) = c('region', 'sum_cost', 'state')
   } else {
      dfdf = data.frame(states[i],DD)
      dfdf['state'] = state_list[i]
      colnames(dfdf) = c('region', 'sum_cost', 'state')
      df = rbind(df,dfdf)
   }
   print(i)
} 

usstates = left_join(usstates,df, by = "region")
usstates$sum_cost = usstates$sum_cost/10e+6

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = sum_cost))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(title = 'Total policy cost') + labs(fill = "$ Mil.") + 
   scale_fill_gradient(low = "white", high = "#800026")
p2



states_uni = usstates %>% group_by(state) %>% summarize(sum_cost = mean(sum_cost)) 


p3 = states_uni %>% ggplot(aes(sum_cost, reorder(state, -sum_cost))) + geom_col()+
   labs(y = "State", x = "Total Policy Cost [Mil $]", title = "Total cost of policy per state")
p3 + theme(axis.text.y = element_text(size = 6))
