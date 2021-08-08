claims = read.csv('data/claim_number_per_year.csv', skip = 1, header = F, sep = ',', fill = TRUE )

colnames(claims) = c("State","2012","2013","2014","2015","2016","2017","2018","2019")
claims = claims %>% filter(State !="Hawaii") %>% filter(State != "Alaska") %>%
      select(c(1,3,4,5,6,7,8,9)) 


state_list = c('AL','AZ','AR','CA','CO','CT','DE','FL','GA','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY',NA)

claims = claims %>% mutate(states = state_list) 

total = claims[FALSE,]
total = claims %>% filter(State == "Total") %>% select(c(2,3,4,5,6,7,8))# %>%
      

claims = claims %>% filter(State != "Total")
year = c(2013,2014,2015,2016,2017,2018,2019)
total = t(total)
total = cbind(total,year)
total = data.frame(total)

ggplot(total, aes(x = year, y = V1/1e3)) +
      theme_bw() +
      geom_line() + geom_point() + 
      scale_x_continuous(name = "Year", breaks = year) +
      scale_y_continuous(name = "Number of claims [K]", breaks = seq(20,160,20)) + 
      labs(title = "")

ggsave("figures/total_claims_yr.jpg", width = 5, height= 3)

claims$sum_ = rowSums(claims[c(2,3,4,5,6,7,8)])



p3 = claims %>% 
      ggplot(aes(sum_/1e3, reorder(states, -sum_))) + 
      theme_bw() +
      geom_col()+
      labs(y = "State",
           x = "Number of Claims [K]", 
           title = "") + 
      theme(axis.text.y = element_text(size = 12),
           axis.text.x = element_text(size = 12))

p3
ggsave("figures/state_claims_bar.jpg", width = 7, height= 10)


### Only show claims larger than 1% of total claims
claims_large = claims %>% filter(sum_ > sum(sum_)*0.01) %>%
      ggplot(aes(sum_/1e3, reorder(states, -sum_))) + 
      theme_bw() +
      geom_col()+
      labs(y = "State",
           x = "Number of Claims [K]", 
           title = "") + 
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12))

claims_large

ggsave("figures/state_claims_large_bar.jpg", width = 4, height= 6)



top = claims %>% filter(sum_ > 17000) %>% select(c(2,3,4,5,6,7,8))
top = t(top)
top = cbind(top,year)
top = data.frame(top)




ggplot(inherit.aes = FALSE) +
   theme_bw() +
   geom_line(data = top, aes(x= year, y =V1/1e3, color =  "#6a51a3"),color =  "#6a51a3", size = 1.1) +
   geom_point(data = top, aes(x= year,y =V1/1e3, color =  "#6a51a3"),color =  "#6a51a3") + 
   geom_line(data = top, aes(x= year,y= V2/1e3, color = "#225ea8"),color =  "#225ea8", size = 1.1) + 
   geom_point(data = top, aes(x= year,y= V2/1e3, color = "#225ea8"),color =  "#225ea8") + 
   geom_line(data = top, aes(x= year,y = V3/1e3, color = "#238443"),color = "#238443", size = 1.1) +
   geom_point(data = top, aes(x= year,y = V3/1e3, color = "#238443"),color = "#238443") +
   geom_line(data = top, aes(x= year,y = V4/1e3, color = "#cc4c02"),color = "#cc4c02", size = 1.1) +
   geom_point(data = top, aes(x= year,y = V4/1e3, color = "#cc4c02"),color = "#cc4c02") + 
   geom_line(data = top, aes(x= year,y = V5/1e3, color = "#fec44f"), color = "#fec44f", size = 1.1) + 
   geom_point(data = top, aes(x= year,y = V5/1e3, color = "#fec44f")) + 
   scale_x_continuous(name = "Year", breaks = year) +
   scale_y_continuous(name = "Number of claims [K]", breaks = seq(20,160,20)) + 
   labs(title = "") + 
   scale_color_manual(name="States",
                       values = c("FL" = "#6a51a3","LA"= "#225ea8","NC" =  "#238443","SC"="#cc4c02","TX"="#fec44f"))


ggsave("figures/top_state_claims.jpg", width =5, height= 3)

### Color the bars of the five states shown here
st = c('FL','LA','NC','SC','TX')
'%ni%' <- Negate('%in%')

claims_large = claims %>% filter(sum_ > sum(sum_)*0.01) %>% mutate(col_group = case_when(
   states =="FL" ~ "1",
   states == "LA" ~ "2",
   states == "NC" ~ "3",
   states == "SC" ~ "4",
   states == "TX" ~ "5",
   states %ni% st ~ "0"))

ggplot(data = claims_large, aes(sum_/1e3, reorder(states, -sum_), fill = col_group)) + 
   theme_bw() +
   geom_col()+
   labs(y = "State",
        x = "Number of Claims [K]", 
        title = "") + 
   theme(axis.text.y = element_text(size = 12),
         axis.text.x = element_text(size = 12)) +
   scale_fill_manual(values = c("1" = "#6a51a3", "2" = "#225ea8", "3" =  "#238443",
                                "4" = "#cc4c02", "5" = "#fec44f", "0" = "darkgray"), guide = "none")


claims_large

ggsave("figures/state_claims_large_bar_color.jpg", width = 4, height= 6)


####################################
####################################
####################################
state_list = c('AL','AZ','AR','CA','CO','CT','DE','DC','FL','GA','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

usstates = map_data("state")
states = unique(usstates$region)
merg_states = data.frame(state_list,states)
merg_states = rename(merg_states,"region" = "states")


usstates = left_join(usstates,merg_states, by = "region") 
usstates = usstates %>% rename("states" = "state_list")
claims_large = claims %>% filter(sum_ > sum(sum_)*0.01) %>% select(c(9,10))

usstates = left_join(usstates,claims_large, by = 'states')


ggplot(data = usstates, aes(x = long, y = lat, group = group, fill = sum_/1e3)) +
      geom_polygon(color = "black") +
      coord_map(projection = 'albers', lat0 = 39, lat1 = 45,
                xlim = c(-118, -75), ylim = c(50, 25)) + 
      labs(fill = "Number of claims [K]", x = '', y = '') +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
      scale_fill_distiller(palette = "YlGnBu", direction = 1)


ggsave("figures/high_claim_number.jpg", width = 6, height = 5)
