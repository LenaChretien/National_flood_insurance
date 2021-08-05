library(tidyverse)
library(ggplot2)

#Load file, subset for dates 2016 - 2021 and split into states to help with memory. 



cnames = c(6,9,15,16,19,21,25,27,28,29,30,32,33,36,37,38,41,43,44,45)

state_list = c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID',
              'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
              'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
              'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')



data <- read.table('data/flood5.csv', skip = 1, nrows = 1, header=F, sep=",", fill=TRUE)


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
  

for (i in 1:length(state_list)) {
  eval(parse(text = paste(state_list[i], '=', data[FALSE,], sep = '')))
}
remove(data)




skipSize = 1
for (i in 46:51){
  data <- read.table(paste('data/flood',i,'.csv',sep = ''), skip=skipSize,
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
  
    
  
  if (dim(data)[1] != 0){
    for (k in 1:51){
      dd = filter(data, data$state == state_list[k])
      eval(parse(text = paste(state_list[k], '= rbind(',state_list[k], ',dd)', sep = '')))
    }
  }
  
  
  print(i)
}

## Save the state data
remove(data)
for (i in 1:length(state_list)) {
  eval(parse(text = paste("save(", state_list[i], ",file = 'data/", state_list[i], ".Rda')", sep = '')))
}



#start.time <- Sys.time()
#save(FL,file="data/FL.Rda")
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken



#start.time <- Sys.time()
#write.csv(FL,="data/FL.Rda")
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken





len = 0
for (i in 1:length(state_list)) {
  state = eval(parse(text = paste("read.csv('Data/", state_list[i], ".csv')", sep = '')))
  len = len+dim(state)[1]
  print(i)
}
## 15,986,508 data points (of 50,000,000)
