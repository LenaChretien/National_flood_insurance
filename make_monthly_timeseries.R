year_list = c('2013','2014','2015','2016','2017','2018','2019')


for (i in 1:length(year_list)){
   eval(parse(text = paste("DD = read.csv('data/yr_", year_list[i], ".csv')", sep = '')))
   
   year = DD %>% 
      group_by(month) %>% summarise(mean_cost = mean(policycost)) %>% 
      mutate(year = year_list[i])
   
   
   number = DD %>% group_by(month) %>% summarize(cnt = n())
   number = number %>% mutate(year = year_list[i])
   
   
   
   amt = DD %>% group_by(month) %>% summarize(amount = mean(building_ins + content_ins)) %>%
      mutate(year = year_list[i]) 
   
   
   fst_half = left_join(year,number, by = c('month', 'year')) 
   fst_half = left_join(fst_half, amt, by = c('month','year'))
   
   
   
   ## Second half of years data
   eval(parse(text = paste("DD = read.csv('data/yr_", year_list[i], "_2.csv')", sep = '')))
   year2 = DD %>% 
      group_by(month) %>% 
      summarise(mean_cost = mean(policycost)) %>% 
      mutate(year = year_list[i])
   

   number2 = DD%>% group_by(month) %>% summarize(cnt = n()) %>% mutate(year = year_list[i]) 
   
      
   amt2 = DD %>% group_by(month)%>% 
      summarize(amount = mean(building_ins + content_ins)) %>% 
      mutate(year = year_list[i])
   
   scd_half = left_join(year2,number2, by = c('month', 'year')) 
   scd_half = left_join(scd_half, amt2, by = c('month','year'))
   
   year_month = rbind(fst_half,scd_half)
   
   
   if (i == 1){
      df = year_month
   } else {
      df = rbind(df,year_month)
   }

   remove(DD)
   print(i)
} 



year_month_mean = df

write.csv(year_month_mean, 'data/year_month_mean.csv')
