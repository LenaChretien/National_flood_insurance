library(ggplot2)
library(dplyr)
library(gridExtra)
library(timetk)

### Plot time series for variables
load('data/FL.Rda')


fl = FL %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) 
   



p1 = ggplot(fl, aes(x = policydate, y = cost)) +
      geom_line(color = "red") + geom_point(color = "red") + 
      labs(title = 'Average US flood insurance cost', x = "Year", y = "Policy cost [$]") +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
p1




p2 = ggplot(fl, aes(x = policydate, y = num/1e6)) +
      geom_line(color = "blue") + geom_point(color = "blue") + 
      labs(title = 'Total number of policies', subtitle = 'in the US', x = "Year", y = "Number [mil]")  +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
p2



p3 = ggplot(fl, aes(x = policydate, y = amt/1e6)) +
      geom_line(color = "green") + geom_point(color = "green") + 
      labs(title = 'Amount of property insured', x = "Year", y = "Ampunt [mil $] ")  +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
p3

grid.arrange(p1,p2,p3, nrow = 3)


###### Make monthly plots

fl = FL %>%
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) %>%
   filter(policydate < as.Date("2019-08-01"))




v.lines = c("2013-01-15", "2014-01-15", "2015-01-15", "2016-01-15",
            "2017-01-15", "2018-01-15", "2019-01-15")

hur_start = c("2013-06-01", "2014-06-01", "2015-06-01", "2016-06-01", "2017-06-01", "2018-06-01", "2019-06-01")
hur_start = as.Date(hur_start, format = "%Y-%m-%d")

hur_end = c("2013-12-01", "2014-12-01", "2015-12-01", "2016-12-01", "2017-12-01", "2018-12-01", "2019-12-01")
hur_end = as.Date(hur_end, format = "%Y-%m-%d")

p1 = ggplot(fl, aes(x = policydate, y = cost)) +
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
      geom_line(color = "red") + geom_point(color = "red") + 
      labs(title = 'Average US flood insurance cost', x = "Year", y = "Policy cost [$]") +
      geom_vline(xintercept = as.Date(v.lines)) + 
      scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
p1


p2 = ggplot(fl, aes(x = policydate, y = num/1e6)) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[1], xmax = hur_end[1], ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[2], xmax = hur_end[2], ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[3], xmax = hur_end[3], ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[4], xmax = hur_end[4], ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[5], xmax = hur_end[5], ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[6], xmax = hur_end[6], ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[7], xmax = hur_end[7], ymin = -Inf, ymax = Inf),
                fill = "lightblue", alpha = 0.03) +
      geom_line(color = "blue") + geom_point(color = "blue") + 
      labs(title = 'Total number of policies', subtitle = 'in the US', x = "Year", y = "Number [mil]")  +
      geom_vline(xintercept = as.Date(v.lines)) + 
      scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
p2



p3 = ggplot(fl, aes(x = policydate, y = amt/1e6)) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[1], xmax = hur_end[1], ymin = -Inf, ymax = Inf),
                fill = "lightgreen", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[2], xmax = hur_end[2], ymin = -Inf, ymax = Inf),
                fill =  "lightgreen", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[3], xmax = hur_end[3], ymin = -Inf, ymax = Inf),
                fill =  "lightgreen", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[4], xmax = hur_end[4], ymin = -Inf, ymax = Inf),
                fill =  "lightgreen", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[5], xmax = hur_end[5], ymin = -Inf, ymax = Inf),
                fill =  "lightgreen", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[6], xmax = hur_end[6], ymin = -Inf, ymax = Inf),
                fill =  "lightgreen", alpha = 0.03) +
      geom_rect(inherit.aes = F, aes(xmin = hur_start[7], xmax = hur_end[7], ymin = -Inf, ymax = Inf),
                fill =  "lightgreen", alpha = 0.03) +
      geom_line(color = "darkgreen") + geom_point(color = "darkgreen") + 
      labs(title = 'Amount of property insured', x = "Year", y = "Amount [mil $] ")  +
      geom_vline(xintercept = as.Date(v.lines)) + 
      scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") 
p3


grid.arrange(p1,p2,p3, nrow = 3)
