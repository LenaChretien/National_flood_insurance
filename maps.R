library(usmap)
library(ggplot2)
library(maps)
library(tidyverse) 


plot_usmap(regions = "state") + 
      labs(title = "US States",
           subtitle = "Map of states in the United States.") + 
      theme(panel.background = element_rect(color = "black", fill = "lightblue"))

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
      colnames(df) = c('region', 'mean_cost')
   } else {
      dfdf = data.frame(states[i], DD)
      colnames(dfdf) = c('region', 'mean_cost')
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
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45) + 
   labs(title = 'Mean policy cost') + labs(fill = "Policy cost") + 
   scale_fill_gradient(low = "white", high = "#800026")
p2



#### number of policy holders. 
usstates = map_data("state")
states = unique(usstates$region)

for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   DD = dim(DD)[1]
   if (i == 1){
      df = data.frame(states[i], DD)
      colnames(df) = c('region', 'num_policies')
   } else {
      dfdf = data.frame(states[i], DD)
      colnames(dfdf) = c('region', 'num_policies')
      df = rbind(df,dfdf)
   }
   print(i)
} 

usstates = left_join(usstates,df, by = "region")
usstates$num_policies = log(usstates$num_policies)

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = num_policies))
p2
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45) + 
   labs(title = 'Number of policyholders') + labs(fill = "Log Number") + 
   labs(x = 'Longitude', y = 'Latitude') + 
   scale_fill_gradient(low = "#ffffd9", high = "#081d58")
p2


###### Total amount covered by insurance
# Add building and content together

usstates = map_data("state")
states = unique(usstates$region)

for (i in 1:length(state_list)){
   eval(parse(text = paste("DD = read.csv('data/", state_list[i], ".csv')", sep = '')))
   DD = mean(DD$building_ins + DD$content_ins)
   if (i == 1){
      df = data.frame(states[i], DD)
      colnames(df) = c('region', 'amount_ins')
   } else {
      dfdf = data.frame(states[i], DD)
      colnames(dfdf) = c('region', 'amount_ins')
      df = rbind(df,dfdf)
   }
   print(i)
} 

usstates = left_join(usstates,df, by = "region")
usstates$amount_ins = usstates$amount_ins/1000

p2 = ggplot(dat = usstates, 
            mapping = aes(x = long,y = lat, group = group, fill = amount_ins))
p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45) + 
   labs(title = 'Dollar amount of insured property (building and content') + labs(fill = "$$ insured [K]") + 
   labs(x = 'Longitude', y = 'Latitude') + 
   scale_fill_gradient(low = "#fff7fb", high = "#014636")
p2


