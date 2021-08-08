#predict the cost
library(ggplot2)
library(dplyr)
library(gridExtra)
library(timetk)

v.lines = c("2013-01-15", "2014-01-15", "2015-01-15", "2016-01-15",
            "2017-01-15", "2018-01-15", "2019-01-15")

hur_start = c("2013-06-01", "2014-06-01", "2015-06-01", "2016-06-01", "2017-06-01", "2018-06-01", "2019-06-01")
hur_start = as.Date(hur_start, format = "%Y-%m-%d")

hur_end = c("2013-12-01", "2014-12-01", "2015-12-01", "2016-12-01", "2017-12-01", "2018-12-01", "2019-12-01")
hur_end = as.Date(hur_end, format = "%Y-%m-%d")


state_list = c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

print(state_list)

my.states <- readline(prompt="Enter a state: ")
my.states = unlist(strsplit(my.states, ' '))
my.states = toupper(my.states)
instates = my.states %in% state_list

my.length = length(my.states)
if (my.length == 0){
      stop("No states were input")
} else if (my.length > 1){
      print('More than 1 state was entered.')
} else if (sum(instates) == 0){
      stop('No states were matched to list. Please make sure to input as two letters')
} else if (my.length ==1) {
          
      
      ### Plot time series for variables
      load(paste('data/', my.states, '.Rda', sep = ''))
      eval(parse(text = paste('st= ' , my.states, sep = '')))
            
      st1 = st %>%
            filter(policydate < as.Date("2019-08-01")) %>%
            summarise_by_time(.date_var = policydate, .by= "month", 
                              cost = mean(policycost), 
                              num = n(),
                              amt = mean(building_ins + content_ins)) %>%
            mutate(state = my.states)
      

      p1 = ggplot(data = st1, aes(x = policydate, y = cost, group = state, color = state)) + 
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
            scale_color_manual(name="States", values = "darkred") + 
            labs(subtitle = 'Insurance cost', x = "Year", y = "Policy cost [$]") +
            scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
      p1
      
      
      # decompose signal
      st2 = st %>% 
            filter(policydate < as.Date("2019-08-01")) %>%
            mutate(state = my.states) %>%
            arrange(policydate) %>% 
            summarise_by_time(.date_var = policydate, .by= " day",
                              cost = mean(policycost)) 
                              #num = n(),
                              #amt = mean(building_ins + content_ins))
      
      #ts_st = ts(st2, frequency = 4)
      #decom_st = decompose(ts_st,"additive")

      #plot(as.ts(decom_st$seasonal))
      #plot(as.ts(decom_st$trend))
      #plot(as.ts(decom_st$random))
      #plot(decom_st)
      library(lubridate)
      library(forecast)
      library(reshape2)      
      
      ts_st = ts(st2$cost, start = as.yearmon(st2$policydate[1]), freq = 24*15)
      plot.ts(ts_st)
      decom_st = decompose(ts_st)
      
      plot(decom_st)
      
      
      st_seasAdj = ts_st - decom_st$seasonal
      plot.ts(st_seasAdj)
      
      st_season = decom_st$seasonal
      plot(st_season)
      
      
      df_adj = data.frame(as.matrix(st_seasAdj), date =time(st_seasAdj))
      df_adj$year = as.numeric(trunc(df_adj$date))
      df_adj$month = (df_adj$date - df_adj$year) * 12 +1
      df_adj$month2 = as.numeric(trunc(df_adj$month))
      df_adj$day = (df_adj$month - df_adj$month2) * 30
      df_adj$day = round(as.numeric(df_adj$day + 1),0)
      df_adj = df_adj %>% mutate(time = paste(year,month2,day,sep = '-')) 
      df_adj$time = as.Date(df_adj$time, format = "%Y-%m-%d")

      
      
      
      #df_sea = data.frame(as.matrix(st_season), date =time(st_season))
      #df_sea$year = trunc(df_sea$date)
      #df_sea$month = (df_sea$date - df_sea$year) * 12 +1
      #df_sea$month2 = trunc(df_sea$month)
      #df_sea = df_sea %>% group_by(month2) %>% summarise(monthly_cost = mean(as.matrix.st_season.))
      #plot(df_sea)
      
      
      lm_st = lm(as.matrix.st_seasAdj. ~ time, data = df_adj)
      
      ggplot(df_adj, aes(x = time, y = as.matrix.st_seasAdj.)) + 
            geom_point() +
            stat_smooth(method = "lm", col = "red")
      
      

      year = 2022
      year = as.character(year)
      month = 5
      month = as.character(month)
      day = as.character(15)
      ymd = paste(year,month,day,sep = '-')
      ymd = as.Date(ymd)
     
      p =data.frame(ymd)
      colnames(p) = "time" 
      
      
      
      predict(lm_st,p)
      
      
      
      
      }

