library(ggplot2)
library(dplyr)
library(usmap)
library(maps)
library(timetk)
library(tidyverse) 
library(plotly)

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


ggplot(data = usstates, aes(x = long, y = lat, group = group, fill = geo_region)) +
   geom_polygon(color = "black") +
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(fill = "Regions", x = '', y = '') +
   theme(axis.ticks.x = element_blank(),
         axis.text.x = element_blank())


   ggsave("regions.jpg", width = 4, height = 4)



# Make a graph of the yearly insurance cost by region
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
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'NE')

remove(st)


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
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'MW')

remove(st)


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
remove(st)
S_df = S_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'S')




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
remove(st)
MD_df = MD_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'MD')



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
remove(st)
PA_df = PA_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'PA')


region_df = rbind(MD_df,MW_df,NE_df,PA_df,S_df)




p1 = ggplot(data = region_df, aes(x = policydate, y = cost, group = state, color = state)) +
   theme_bw() + 
   geom_line() + 
   geom_point() + 
   labs(subtitle = 'Insurance cost', x = "Year", y = "Policy cost [$]") +
   theme(panel.grid.minor.x = element_blank()) + 
   scale_y_continuous(breaks = seq(600,1400, by = 100)) +
   scale_x_date(date_labels = "%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_discrete(name = "Region")
p1

ggsave("regions_yearly.jpg", width = 4, height = 7)

#p2 = ggplot(data = region_df, aes(x = policydate, y = num/1e6, group = state, color = state)) + theme_bw() + 
#   geom_line() + 
#   geom_point() + 
#   labs(subtitle = 'Policy numbers', x = "Year", y = "Number [mil]") +
#   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months")# +
#p2



#p3 = ggplot(data = region_df, aes(x = policydate, y = amt/1e6, group = state, color = state)) + theme_bw() + 
#   geom_line() + 
#   geom_point() + 
#   labs(subtitle = 'Insured amount', x = "Year", y = "Amount [mil $]") +
#   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months")# +
#p3



# MAPS 

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
   filter(policydate < as.Date("2019-08-01")) %>% summarize(cost = mean(policycost), 
          num = n(),
          amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'NE')

remove(st)



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
   summarise(cost = mean(policycost), 
             num = n(),
             amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'MW')
remove(st)



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
remove(st)
S_df = S_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise(cost = mean(policycost), 
             num = n(),
             amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'S')




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
remove(st)
MD_df = MD_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise(cost = mean(policycost), 
             num = n(),
             amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'MD')



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
remove(st)
PA_df = PA_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise(cost = mean(policycost), 
             num = n(),
             amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'PA')

region_df = rbind(MD_df,MW_df,NE_df,PA_df,S_df)
region_df = rename(region_df,"geo_region" = "state")


usm = map_data('usa')
usstates = map_data("state")
states = unique(usstates$region)
merg_states = data.frame(state_list,states)
merg_states = rename(merg_states,"region" = "states")


NE = c('ME','NH','MA','RI','VT','NY','PA','NJ','DE','CT')
usstates = left_join(usstates,merg_states, by = "region")
usstates = usstates %>% mutate(geo_region = case_when(
   state_list %in% NE ~ "NE",
   state_list %in% MW ~ "MW",
   state_list %in% S ~ "S",
   state_list %in% MD ~ "MD",
   state_list %in% PA ~ "PA"))
usstates = left_join(usstates,region_df, by = 'geo_region')


p1 = ggplot(dat = usstates, 
            aes(x = long,y = lat, group = group, fill = cost))
p1 = p1 + geom_polygon(color = "gray90", size = 0.1) + 
   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
             xlim = c(-118, -75), ylim = c(50, 25)) + 
   labs(fill = "Regions", x = '', y = '') +
   theme(axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         legend.title = element_text(size = 7), 
         legend.text = element_text(size = 7),
         legend.key.size = unit(1,"cm"),
         legend.key.width = unit(0.5,"cm")) + 
   scale_fill_gradient(low = "white", high = "#800026")
p1


ggsave("regions_cost.jpg", width = 6, height = 4)


#p2 = ggplot(dat = usstates, 
#            aes(x = long,y = lat, group = group, fill = num/1e6))
#p2 = p2 + geom_polygon(color = "gray90", size = 0.1) + 
#   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
#             xlim = c(-118, -75), ylim = c(50, 25)) + 
#   labs(title = 'Number of Policies') + labs(fill = "Number [mil]") + 
#   scale_fill_gradient(low = "white", high = "#800026")
#p2




#p3 = ggplot(dat = usstates, 
#            aes(x = long,y = lat, group = group, fill = amt/1e6))
#p3 = p3 + geom_polygon(color = "gray90", size = 0.1) + 
#   coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
#             xlim = c(-118, -75), ylim = c(50, 25)) + 
#   labs(title = 'Amount insured') + labs(fill = "Amount [mil $]") + 
#   scale_fill_gradient(low = "white", high = "#800026")
#p3




#### MAKE GRAPHS FOR MONTHLY COST
# Make a graph of the monthly insurance cost by region
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
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'NE')

remove(st)


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
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'MW')

remove(st)


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
remove(st)
S_df = S_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'S')




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
remove(st)
MD_df = MD_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'MD')



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
remove(st)
PA_df = PA_df %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   mutate(state = 'PA')


region_df = rbind(MD_df,MW_df,NE_df,PA_df,S_df)




v.lines = c("2013-01-15", "2014-01-15", "2015-01-15", "2016-01-15",
            "2017-01-15", "2018-01-15", "2019-01-15")

hur_start = c("2013-06-01", "2014-06-01", "2015-06-01", "2016-06-01", "2017-06-01", "2018-06-01", "2019-06-01")
hur_start = as.Date(hur_start, format = "%Y-%m-%d")

hur_end = c("2013-12-01", "2014-12-01", "2015-12-01", "2016-12-01", "2017-12-01", "2018-12-01", "2019-12-01")
hur_end = as.Date(hur_end, format = "%Y-%m-%d")

p1 = ggplot(data = region_df, aes(x = policydate, y = cost, group = state, color = state)) +
   theme_bw() + 
   geom_rect(inherit.aes = F, aes(xmin = hur_start[1], xmax = hur_end[1], ymin = -Inf, ymax = Inf),
             fill = "pink", alpha = 0.03) +
   geom_rect(inherit.aes = F, aes(xmin = hur_start[2], xmax = hur_end[2], ymin = -Inf, ymax = Inf),
             fill = "pink", alpha = 0.03) +
   geom_rect(inherit.aes = F, aes(xmin = hur_start[3], xmax = hur_end[3], ymin = -Inf, ymax = Inf),
             fill = "pink", alpha = 0.03) +
   geom_rect(inherit.aes = F, aes(xmin = hur_start[4], xmax = hur_end[4], ymin = -Inf, ymax = Inf),
             fill = "pink", alpha = 0.03) +
   geom_rect(inherit.aes = F, aes(xmin = hur_start[5], xmax = hur_end[5], ymin = -Inf, ymax = Inf),
             fill = "pink", alpha = 0.03) +
   geom_rect(inherit.aes = F, aes(xmin = hur_start[6], xmax = hur_end[6], ymin = -Inf, ymax = Inf),
             fill = "pink", alpha = 0.03) +
   geom_rect(inherit.aes = F, aes(xmin = hur_start[7], xmax = hur_end[7], ymin = -Inf, ymax = Inf),
             fill = "pink", alpha = 0.03) +
   geom_line() + 
   geom_point() + 
   geom_vline(xintercept = as.Date(v.lines)) + 
   labs(subtitle = 'Insurance cost', x = "Year", y = "Policy cost [$]") +
   theme(panel.grid.minor.x = element_blank()) + 
   scale_y_continuous(breaks = seq(600,1400, by = 100)) +
   scale_x_date(date_labels = "%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_discrete(name = "Region")
p1

ggsave("regions_monthly.jpg", width = 8, height = 4)



## Make interactive chart for readability
p3 =ggplot(data = region_df, aes(x = policydate, y = cost, group = state, color = state,
                                  text = paste0(state, "<br>", "Cost: $", round(amt/1e3,0)))) +
   xlab("Year") + 
   ylab("Amount insured [K $]") + 
   theme_minimal(base_size = 14) + 
   geom_point(aes(color=state), size = 1.5, alpha = 0.5) +
   geom_line(aes(color=state)) +
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months")


p3_interactive <- ggplotly(p3, tooltip="text") %>% 
   config(displayModeBar = FALSE)

# plot the chart
print(p3_interactive)

htmlwidgets::saveWidget(as_widget(p3), "plotly.html")
