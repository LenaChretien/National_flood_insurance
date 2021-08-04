library(tidyverse)
library(ggplot2)

#Load file, subset for dates 2016 - 2021 and average national prices and dollar amounts into month. 
cnames = c(6,9,15,16,19,21,25,27,28,29,30,32,33,36,37,38,41,43,44,45)

year_list = c('2013','2014','2015','2016','2017','2018','2019')

data <- read.table('Data/flood5.csv', skip = 1, nrows = 1, header=F, sep=",", fill=TRUE)


data %>% select(cnames) %>%
      rename(condo = V6, discount = V9, 
             fedfee = V15,  floodzone = V16,
             lat = V19, lon = V21,
             floors = V25,
             type = V27, date_build = V28,
             date_nb = V29, policycost = V30, policydate = V32,
             policy_enddata = V33,
             residence =V36, state = V37,
             zip = V38, city = V41, 
             building_ins = V43,
             content_ins = V44, 
             premium = V45) %>%
      mutate(date_build = as.Date(date_build, format = "%Y-%m-%d"), 
             date_nb = as.Date(date_nb, format = "%Y-%m-%d"),
             policydate = as.Date(policydate, format = "%Y-%m-%d"),
             policy_enddata = as.Date(policy_enddata, format = "%Y-%m-%d")) #%>%


for (i in 1:length(year_list)) {
      eval(parse(text = paste('yr_',year_list[i], '=', data[FALSE,], sep = '')))
}

for (i in 1:length(year_list)) {
   eval(parse(text = paste('yr_',year_list[i], '_2 =', data[FALSE,], sep = '')))
}
remove(data)




skipSize = 1
for (i in 1:51){
      data <- read.table(paste('Data/flood',i,'.csv',sep = ''), skip=skipSize,
                         header=F, fill=TRUE, sep=",")
      
      data =  data %>% select(cnames) %>%
         rename(condo = V6, discount = V9, 
                fedfee = V15,  floodzone = V16,
                lat = V19, lon = V21,
                floors = V25,
                type = V27, date_build = V28,
                date_nb = V29, policycost = V30, policydate = V32,
                policy_enddata = V33,
                residence =V36, state = V37,
                zip = V38, city = V41, 
                building_ins = V43,
                content_ins = V44, 
                premium = V45) %>%
         mutate(date_build = as.Date(date_build, format = "%Y-%m-%d"),
                date_nb = as.Date(date_nb, format = "%Y-%m-%d")) %>% 
         mutate(policydate = as.Date(policydate, format = "%Y-%m-%d"),
                policy_enddata = as.Date(policy_enddata, format = "%Y-%m-%d")) %>%
         filter(!is.na(policycost)) %>% filter(!is.na(policydate)) %>% 
         filter(!is.na(zip)) %>% filter(!is.na(city)) %>% filter(!is.na(building_ins)) %>%
         filter(!is.na(content_ins)) %>% filter(!is.na(policycost)) %>% filter(!is.na(floodzone)) %>%
         filter(policydate > as.Date("2012-12-31")) %>% filter(policycost <= 5000)
      
      data_year = data %>% mutate(year = as.numeric(format(policydate,"%Y")))
      uni = unique(data_year$year)
      
      data_year = data_year %>% mutate(month = as.numeric(format(policydate, "%m")))
      
      
      if (dim(data)[1] != 0){
         for (k in 1:length(uni)){
            yr = uni[k]
            dd = data_year %>% filter(year == yr, month < 7) %>% arrange(month)
            eval(parse(text = paste('yr_',uni[k], ' = rbind(yr_',uni[k], ',dd)', sep = '')))
            
            dd = data_year %>% filter(year == yr, month > 6) %>% arrange(month)
            eval(parse(text = paste('yr_',uni[k], '_2 = rbind(yr_',uni[k], '_2 ,dd)', sep = '')))
         }
            
      }
      print(i)
}

## Save the state data

for (i in 1:length(year_list)) {
   eval(parse(text = paste("write.csv(yr_", year_list[i], ",'Data/yr_", year_list[i], ".csv')", sep = '')))
   eval(parse(text = paste("write.csv(yr_", year_list[i], "_2,'Data/yr_", year_list[i], "_2.csv')", sep = '')))
   print(i)
}

