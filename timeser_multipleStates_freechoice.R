library(ggplot2)
library(dplyr)
library(gridExtra)
library(timetk)

state_list = c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
print(state_list)

my.states <- readline(prompt="Enter up to three states: ")
my.states = unlist(strsplit(my.states, ' '))
my.states = toupper(my.states)
instates = my.states %in% state_list

my.length = length(my.states)
if (my.length == 0){
   stop("No states were input")
} else if (my.length > 3){
   print('More than 3 states were input.')
} else if (sum(instates) == 0){
   stop('No states were matched to list. Please make sure to input as two letters')
} else if (sum(instates) <=3 & sum(instates) > 0) {
   print(paste(sum(instates), 'states were found in the list.', sep = " "))
   
   
   ### Plot time series for variables
   for (i in (1:length(my.states))){
      load(paste('data/', my.states[i], '.Rda', sep = ''))
      eval(parse(text = paste('st= ' , my.states[i], sep = '')))
      
      st = st %>%
         filter(policydate < as.Date("2019-08-01")) %>%
         summarise_by_time(.date_var = policydate, .by= "year", 
                           cost = mean(policycost), 
                           num = n(),
                           amt = mean(building_ins + content_ins)) 
      eval(parse(text = paste('st', i, '= st' , sep = '')))
      
   }
   if (length(my.states)) == 1{
      
      p1 = ggplot() + theme_bw() + 
         geom_line(data = st1, aes(x = policydate, y = cost), color = "darkred") + 
         geom_point(data = st1, aes(x = policydate, y = cost), color = "darkred") + 
         labs(title = paste('Average insurance cost in', my.states[1]), x = "Year", y = "Policy cost [$]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p1
      
      
      
      p2 = ggplot() + theme_bw() + 
         geom_line(data = st1, aes(x = policydate, y =  num/1e6), color = "darkred") + 
         geom_point(data = st1, aes(x = policydate, y =  num/1e6), color = "darkred") + 
         labs(title = paste('Number of policies in ', my.states[1]), x = "Year", y = "Number [mil]")  +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months")
      p2
      
      
      p3 = ggplot() + theme_bw() + 
         geom_line(data = st1, aes(x = policydate, y =  amt/1e6), color = "darkred") + 
         geom_point(data = st1, aes(x = policydate, y =  amt/1e6), color = "darkred") + 
         labs(title = 'Amount property insured', x = "Year", y = "Ampunt [mil $] ")  +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months")
      p3
   } else if (length(my.state)) == 2{
      st1['state'] = paste("'",my.states[1], "'", sep = "")
      st2['state'] = paste("'",my.states[2], "'", sep = "")
      
      st = rbind(st1,st2)
      st$state = gsub("'", "",st$state)
      
      
      p1 = ggplot(data = st, aes(x = policydate, y = cost, group = state, color = state)) + theme_bw() + 
         geom_line() + 
         geom_point() + 
         scale_color_manual(name="States", values = c('darkred','darkblue')) + 
         labs(subtitle = 'Insurance cost', x = "Year", y = "Policy cost [$]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p1
      
      
      p2 = ggplot(data = st, aes(x = policydate, y = num/1e6, group = state, color = state)) + theme_bw() + 
         geom_line() + 
         geom_point() + 
         scale_color_manual(name="States", values = c('darkred','darkblue')) + 
         labs(subtitle = 'Number of policies', x = "Year", y = "Number [mil]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p2
      
      
      
      p3 = ggplot(data = st, aes(x = policydate, y = amt/1e6, group = state, color = state)) + theme_bw() + 
         geom_line() + 
         geom_point() + 
         scale_color_manual(name="States", values = c('darkred','darkblue')) + 
         labs(subtitle = 'property insured', x = "Year", y = "Amount [mil $]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p3
      
   }
   else if (length(my.state)) == 3{
      st1['state'] = paste("'",my.states[1], "'", sep = "")
      st2['state'] = paste("'",my.states[2], "'", sep = "")
      st3['state'] = paste("'",my.states[3], "'", sep = "")
      
      
      st = rbind(st1,st2,st3)
      st$state = gsub("'", "",st$state)
      
      
      p1 = ggplot(data = st, aes(x = policydate, y = cost, group = state, color = state)) + theme_bw() + 
         geom_line() + 
         geom_point() + 
         scale_color_manual(name="States", values = c('darkred','darkblue','darkgreen')) + 
         labs(subtitle = 'Insurance cost', x = "Year", y = "Policy cost [$]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p1
      
      
      p2 = ggplot(data = st, aes(x = policydate, y = num/1e6, group = state, color = state)) + theme_bw() + 
         geom_line() + 
         geom_point() + 
         scale_color_manual(name="States", values =  c('darkred','darkblue','darkgreen')) + 
         labs(subtitle = 'Number of policies', x = "Year", y = "Number [mil]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p2
      
      
      
      p3 = ggplot(data = st, aes(x = policydate, y = amt/1e6, group = state, color = state)) + theme_bw() + 
         geom_line() + 
         geom_point() + 
         scale_color_manual(name="States", values =  c('darkred','darkblue','darkgreen')) + 
         labs(subtitle = 'property insured', x = "Year", y = "Amount [mil $]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p3
      
   }
}


###### Make monthly plots

v.lines = c("2013-01-15", "2014-01-15", "2015-01-15", "2016-01-15",
            "2017-01-15", "2018-01-15", "2019-01-15")

hur_start = c("2013-06-01", "2014-06-01", "2015-06-01", "2016-06-01", "2017-06-01", "2018-06-01", "2019-06-01")
hur_start = as.Date(hur_start, format = "%Y-%m-%d")

hur_end = c("2013-12-01", "2014-12-01", "2015-12-01", "2016-12-01", "2017-12-01", "2018-12-01", "2019-12-01")
hur_end = as.Date(hur_end, format = "%Y-%m-%d")


if (my.length == 0){
   stop("No states were input")
} else if (my.length > 3){
   print('More than 3 states were input.')
} else if (sum(instates) == 0){
   stop('No states were matched to list. Please make sure to input as two letters')
} else if (sum(instates) <=3 & sum(instates) > 0) {
   print(paste(sum(instates), 'states were found in the list.', sep = " "))
   
   
   ### Plot time series for variables
   for i in (1:length(my.states)){
      load(paste('data/', my.states[i], '.Rda', sep = ''))
      eval(parse(text = paste('st= ' , my.states[i], sep = '')))
      
      st = st %>%
         filter(policydate < as.Date("2019-08-01")) %>%
         summarise_by_time(.date_var = policydate, .by= "month", 
                           cost = mean(policycost), 
                           num = n(),
                           amt = mean(building_ins + content_ins)) 
      eval(parse(text = paste('st', i, '= st' , sep = '')))
      
   }
   ### Plot time series for variables
   for (i in (1:length(my.states))){
      load(paste('data/', my.states[i], '.Rda', sep = ''))
      eval(parse(text = paste('st= ' , my.states[i], sep = '')))
      
      st = st %>%
         filter(policydate < as.Date("2019-08-01")) %>%
         summarise_by_time(.date_var = policydate, .by= "month", 
                           cost = mean(policycost), 
                           num = n(),
                           amt = mean(building_ins + content_ins)) 
      eval(parse(text = paste('st', i, '= st' , sep = '')))
   }
   
   
   if (length(my.states) == 1){
      st = st1
      values = 'darkred'
   } else if (length(my.states) == 2){
      st1['state'] = paste("'",my.states[1], "'", sep = "")
      st2['state'] = paste("'",my.states[2], "'", sep = "")
      
      st = rbind(st1,st2)
      st$state = gsub("'", "",st$state)
      
      values = c('darkred','darkblue')
   } else if (length(my.states) == 3){
      st1['state'] = paste("'",my.states[1], "'", sep = "")
      st2['state'] = paste("'",my.states[2], "'", sep = "")
      st3['state'] = paste("'", my.states[3], "'", sep = "")
      
      st = rbind(st1,st2,st3)
      st$state = gsub("'", "",st$state)
      values = c('darkred','darkblue','darkgreen')
   }
   
      
      p1 = ggplot(data = st, aes(x = policydate, y = cost, group = state, color = state)) + 
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
         scale_color_manual(name="States", values = values) + 
         labs(subtitle = 'Insurance cost', x = "Year", y = "Policy cost [$]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p1
      
      p2 = ggplot(data = st, aes(x = policydate, y = num/1e6, group = state, color = state)) + 
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
         scale_color_manual(name="States", values = values) + 
         labs(subtitle = 'Number of policies', x = "Year", y = "Policy cost [$]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p2
      
      p3 = ggplot(data = st, aes(x = policydate, y = cost, group = state, color = state)) + 
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
         scale_color_manual(name="States", values = values) + 
         labs(subtitle = 'Amount insured', x = "Year", y = "Policy cost [$]") +
         scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p3


