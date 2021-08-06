library(ggplot2)
library(dplyr)
library(gridExtra)
library(timetk)

### Plot time series for variables
load('data/FL.Rda')
load('data/TX.Rda')
load('data/LA.Rda')

st1 = FL %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) 

st2 = TX %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) 

st3 = LA %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "year", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) 







p1 = ggplot() + theme_bw() + 
   geom_line(data = st1, aes(x = policydate, y = cost, color = "FL"), color = "darkred") + 
   geom_point(data = st1, aes(x = policydate, y = cost, color = "FL")) + 
   geom_line(data = st2, aes(x = policydate, y = cost, color = "TX")) + 
   geom_point(data = st2, aes(x = policydate, y = cost, color = "TX"), color = "darkblue") + 
   geom_line(data = st3, aes(x = policydate, y = cost, color = "LA"), color = "darkgreen") + 
   geom_point(data = st3, aes(x = policydate, y = cost, color = "LA"), color = "darkgreen") + 
   labs(title = 'Average US flood insurance cost', x = "Year", y = "Policy cost [$]") +
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_manual(name="States", values=c("FL"="darkred", "TX"="darkblue", "LA" = "darkgreen"))
#scale_color_hue(name="States") +
p1



p2 = ggplot() + theme_bw() + 
   geom_line(data = st1, aes(x = policydate, y =  num/1e6, color = "FL"), color = "darkred") + 
   geom_point(data = st1, aes(x = policydate, y =  num/1e6, color = "FL")) + 
   geom_line(data = st2, aes(x = policydate, y =  num/1e6, color = "TX")) + 
   geom_point(data = st2, aes(x = policydate, y =  num/1e6, color = "TX"), color = "darkblue") + 
   geom_line(data = st3, aes(x = policydate, y =  num/1e6, color = "LA"), color = "darkgreen") + 
   geom_point(data = st3, aes(x = policydate, y =  num/1e6, color = "LA"), color = "darkgreen") + 
   labs(title = 'Total number of policies', subtitle = 'in the US', x = "Year", y = "Number [mil]")  +
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_manual(name="States", values=c("FL"="darkred", "TX"="darkblue", "LA" = "darkgreen"))
p2



p3 = ggplot() + theme_bw() + 
   geom_line(data = st1, aes(x = policydate, y =  amt/1e6, color = "FL"), color = "darkred") + 
   geom_point(data = st1, aes(x = policydate, y =  amt/1e6, color = "FL")) + 
   geom_line(data = st2, aes(x = policydate, y =  amt/1e6, color = "TX")) + 
   geom_point(data = st2, aes(x = policydate, y =  amt/1e6, color = "TX"), color = "darkblue") + 
   geom_line(data = st3, aes(x = policydate, y =  amt/1e6, color = "LA"), color = "darkgreen") + 
   geom_point(data = st3, aes(x = policydate, y =  amt/1e6, color = "LA"), color = "darkgreen") + 
   labs(title = 'Amount of property insured', x = "Year", y = "Ampunt [mil $] ")  +
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_manual(name="States", values=c("FL"="darkred", "TX"="darkblue", "LA" = "darkgreen"))
p3

grid.arrange(p1,p2,p3, nrow = 3)


###### Make monthly plots

st1 = FL %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) 

st2 = TX %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) 

st3 = LA %>%
   filter(policydate < as.Date("2019-08-01")) %>%
   summarise_by_time(.date_var = policydate, .by= "month", 
                     cost = mean(policycost), 
                     num = n(),
                     amt = mean(building_ins + content_ins)) 





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
   geom_line(data = st1, aes(x = policydate, y =  cost, color = "FL"), color = "darkred") + 
   geom_point(data = st1, aes(x = policydate, y =  cost, color = "FL")) + 
   geom_line(data = st2, aes(x = policydate, y =  cost, color = "TX")) + 
   geom_point(data = st2, aes(x = policydate, y = cost, color = "TX"), color = "darkblue") + 
   geom_line(data = st3, aes(x = policydate, y =  cost, color = "LA"), color = "darkgreen") + 
   geom_point(data = st3, aes(x = policydate, y =  cost, color = "LA"), color = "darkgreen") + 
   labs(title = 'Average US flood insurance cost', x = "Year", y = "Policy cost [$]") +
   geom_vline(xintercept = as.Date(v.lines)) + 
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_manual(name="States", values=c("FL"="darkred", "TX"="darkblue", "LA" = "darkgreen"))

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
   geom_line(data = st1, aes(x = policydate, y =  num/1e6, color = "FL"), color = "darkred") + 
   geom_point(data = st1, aes(x = policydate, y =  num/1e6, color = "FL")) + 
   geom_line(data = st2, aes(x = policydate, y =  num/1e6, color = "TX")) + 
   geom_point(data = st2, aes(x = policydate, y =  num/1e6, color = "TX"), color = "darkblue") + 
   geom_line(data = st3, aes(x = policydate, y =  num/1e6, color = "LA"), color = "darkgreen") + 
   geom_point(data = st3, aes(x = policydate, y =  num/1e6, color = "LA"), color = "darkgreen") + 
   labs(title = 'Total number of policies', subtitle = 'in the US', x = "Year", y = "Number [mil]")  +
   geom_vline(xintercept = as.Date(v.lines)) + 
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_manual(name="States", values=c("FL"="darkred", "TX"="darkblue", "LA" = "darkgreen"))

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
   geom_line(data = st1, aes(x = policydate, y =  amt/1e6, color = "FL"), color = "darkred") + 
   geom_point(data = st1, aes(x = policydate, y =  amt/1e6, color = "FL")) + 
   geom_line(data = st2, aes(x = policydate, y =  amt/1e6, color = "TX")) + 
   geom_point(data = st2, aes(x = policydate, y =  amt/1e6, color = "TX"), color = "darkblue") + 
   geom_line(data = st3, aes(x = policydate, y =  amt/1e6, color = "LA"), color = "darkgreen") + 
   geom_point(data = st3, aes(x = policydate, y =  amt/1e6, color = "LA"), color = "darkgreen") + 
   labs(title = 'Amount of property insured', x = "Year", y = "Amount [mil $] ")  +
   geom_vline(xintercept = as.Date(v.lines)) + 
   scale_color_manual(name="States", values=c("FL"="darkred", "TX"="darkblue", "LA" = "darkgreen"))
p3


grid.arrange(p1,p2,p3, nrow = 3)
