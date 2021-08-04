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

for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   DD = mean(DD$policycost)
   if (i == 1){
      df = data.frame(states[i], DD)
      df['state'] = state_list[i]
      colnames(df) = c('region', 'mean_cost', 'state')
   } else {
      dfdf = data.frame(states[i],DD)
      dfdf['state'] = state_list[i]
      colnames(dfdf) = c('region', 'mean_cost', 'state')
      df = rbind(df,dfdf)
   }
   print(i)
} 

usstates = left_join(usstates,df, by = "region")
      

#p = ggplot(dat = usstates, aes(x = long,y = lat, group = group) )
#p = p + geom_polygon(fill = "white", color = 'black') 
#p



p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = mean_cost))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(title = 'Mean policy cost') + labs(fill = "Policy cost") + 
   scale_fill_gradient(low = "white", high = "#800026")
p2





states_uni = usstates %>% group_by(state) %>% summarize(mean_cost = mean(mean_cost)) 


p3 = states_uni %>% ggplot(aes(mean_cost, reorder(state, -mean_cost))) + geom_col()+
   labs(y = "State", x = "Policy Cost", title = "Average cost of policy per state")
p3 + theme(axis.text.y = element_text(size = 6))




#### number of policy holders. 
usstates = map_data("state")
states = unique(usstates$region)

for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   DD = dim(DD)[1]
   if (i == 1){
      df = data.frame(states[i], DD)
      df['state'] = state_list[i]
      colnames(df) = c('region', 'num_policies', 'state')
   } else {
      dfdf = data.frame(states[i], DD)
      dfdf['state'] = state_list[i]
      colnames(dfdf) = c('region', 'num_policies', 'state')
      df = rbind(df,dfdf)
   }
   print(i)
} 

usstates = left_join(usstates,df, by = "region")
usstates$num_policies = log(usstates$num_policies)

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = num_policies))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(title = 'Number of policyholders') + labs(fill = "Log Number") + 
   labs(x = 'Longitude', y = 'Latitude') + 
   scale_fill_gradient(low = "#ffffd9", high = "#081d58")
p2





states_uni = usstates %>% group_by(state) %>% summarize(num_policies = mean(num_policies)) 


p3 = states_uni %>% ggplot(aes(num_policies, reorder(state, -num_policies))) + geom_col()+
   labs(y = "State", x = "Number of policies", title = "(Log) Number of policies between 2015 - 2019 per state")
p3 + theme(axis.text.y = element_text(size = 6))






###### Total amount covered by insurance
# Add building and content together

usstates = map_data("state")
states = unique(usstates$region)

for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   DD = mean(DD$building_ins + DD$content_ins)
   if (i == 1){
      df = data.frame(states[i], DD)
      df['state'] = state_list[i]
      colnames(df) = c('region', 'amount_ins', 'state')
   } else {
      dfdf = data.frame(states[i], DD)
      dfdf['state'] = state_list[i]
      colnames(dfdf) = c('region', 'amount_ins', 'state')
      df = rbind(df,dfdf)
   }
   print(i)
} 

usstates = left_join(usstates,df, by = "region")
usstates$amount_ins = usstates$amount_ins/1000

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = amount_ins))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45, 
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(title = 'Dollar amount of insured property', subtitle = '(building and content)') + labs(fill = "$$ insured [K]") + 
   labs(x = 'Longitude', y = 'Latitude') + 
   scale_fill_gradient(low = "#fff7fb", high = "#014636")
p2





states_uni = usstates %>% group_by(state) %>% summarize(amount_ins = mean(amount_ins)) 


p3 = states_uni %>% ggplot(aes(amount_ins, reorder(state, -amount_ins))) + geom_col()+
   labs(y = "State", x = "Number of policies", title = "(Log) Number of policies between 2015 - 2019 per state")
p3 + theme(axis.text.y = element_text(size = 6))



###### Insurance amount by county
library(zipcodeR)


 

for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   
   eval(parse(text = paste("zip_county = search_state('", state_list[i], "')", sep = '')))
   
   zip_county = zip_county %>% 
      select("zip" = "zipcode","county", "lat", "lng") %>% 
      mutate(zip = as.integer(zip)) %>%
      mutate(county = tolower(county)) %>% mutate(county = gsub(' county','',county))
   
   DD_new = left_join(DD,zip_county, by = "zip")
   
   avg_cost = DD_new %>% group_by(county) %>% 
      summarise(mean_cost = mean(policycost)) %>% 
      mutate(region = states[i])
   
   avg_cost['state'] = state_list[i]
   
   if (i == 1){
      df = avg_cost
   } else {
      df = rbind(df,avg_cost)
   }
   print(i)
} 


df = df %>% rename(subregion = county) 


uscounty = map_data("county")
uscounty = left_join(uscounty,df, by = c("subregion","region"))
uscounty = uscounty %>%  
   filter(!is.na(mean_cost)) %>% 
   select("long", "lat", "group","subregion","mean_cost")
                               
                               
                               

p2 = ggplot(dat = uscounty, 
            mapping = aes(x = long,y = lat, group = group, fill = mean_cost)) 

p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(title = 'Average policy cost by county') + labs(fill = "mean_cost") + 
   labs(x = 'Longitude', y = 'Latitude') + 
   scale_fill_gradient(low = "#ffffd9", high = "#081d58")
p2


