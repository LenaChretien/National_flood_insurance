year_list = c('2013','2014','2015','2016','2017','2018','2019')


for (i in 1:length(year_list)){
      eval(parse(text = paste("DD = read.csv('data/yr_", year_list[i], ".csv')", sep = '')))
      
      year = DD %>% filter(policydate < as.Date("2019-08-01")) %>%
            summarise(mean_cost = mean(policycost)) %>% 
            mutate(year = year_list[i])

      
      number = dim(DD)[1]

      number = data.frame(number)
      number = number %>% mutate(year = year_list[i])
      
      
      amt = mean(DD$building_ins + DD$content_ins)
      
      amt = data.frame(amt,year_list[i])
      colnames(amt) = c('amount_ins', 'year')
      
      
      ## Second half of years data
      
      eval(parse(text = paste("DD = read.csv('data/yr_", year_list[i], "_2.csv')", sep = '')))
      year2 = DD %>% filter(policydate < as.Date("2019-08-01")) %>%
            summarise(mean_cost = mean(policycost)) %>% 
            mutate(year = year_list[i])

      yr = rbind(year,year2)
      yr = yr %>% 
            summarise(mean_cost = mean(mean_cost)) %>% mutate(year = year_list[i])
      
      
      
      
      number2 = dim(DD)[1]
      number2 = data.frame(number2)
      number2 = number2 %>% mutate(year = year_list[i]) %>% rename("number" = "number2")
      
      
      
      num = rbind(number,number2)
      num = num %>% 
            summarise(number = sum(number)) %>% mutate(year = year_list[i])
     
      
      

      amt2 = mean(DD$building_ins + DD$content_ins)
      
      amt2 = data.frame(amt2,year_list[i])
      colnames(amt2) = c('amount_ins', 'year')
      
      amt = rbind(amt,amt2)
      amt = amt %>% 
            summarise(amount_ins = sum(amount_ins)) %>% mutate(year = year_list[i])
      
       
      
      if (i == 1){
            dfdf = data.frame(yr$mean_cost, num$number, amt)
            df = dfdf
      } else {
            dfdf = data.frame(yr$mean_cost, num$number, amt)
            df = rbind(df,dfdf)
      }
      remove(year)
      remove(year2)
      print(i)
} 



year_mean = df

write_csv(year_mean, 'data/year_mean.csv')
