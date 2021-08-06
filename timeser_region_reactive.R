library(ggplot2)
library(dplyr)
library(readr)


state_list = c('AL','AZ','AR','CA','CO','CT','DE','DC','FL','GA','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

load("data/region_yearly.Rda")

p1 = ggplot(data = region_df, aes(x = policydate, y = cost, 
                                  group = state, color = state,
                                  text = paste0(state, "Cost: $", round(cost,0)))) +
   xlab("Year") + 
   ylab("Policy cost") + 
   theme_minimal(base_size = 14) + 
   geom_point(aes(color=state), size = 3, alpha = 0.5) +
   geom_line(aes(color=state)) +
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_brewer(palette = "Set1",
                      name = "Region")
                      
p1

p1_interactive <- ggplotly(p1, tooltip="text") %>% 
   config(displayModeBar = FALSE)

# plot the chart
print(p1_interactive)



p2 = ggplot(data = region_df, aes(x = policydate, y = num/1e6, 
                                  group = state, color = state,
                                  text = paste0(state, "Cost: $", num/1e6))) +
   xlab("Year") + 
   ylab("Number [mil]") + 
   theme_minimal(base_size = 14) + 
   geom_point(aes(color=state), size = 3, alpha = 0.5) +
   geom_line(aes(color=state)) +
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_brewer(palette = "Set1",
                      name = "Region")

p2

p2_interactive <- ggplotly(p2, tooltip="text") %>% 
   config(displayModeBar = FALSE)

# plot the chart
print(p2_interactive)


p3 = ggplot(data = region_df, aes(x = policydate, y = amt/1e3, 
                                  group = state, color = state, 
                                  text = paste0(state, "<br>", "Cost: $", round(amt/1e3,0)))) +
   xlab("Year") + 
   ylab("Amount insured [K $]") + 
   theme_minimal(base_size = 14) + 
   geom_point(aes(color=state), size = 3, alpha = 0.5) +
   geom_line(aes(color=state)) +
   scale_x_date(date_labels = "%m/%Y", date_breaks = "years", date_minor_breaks = "months") +
   scale_color_brewer(palette = "Set1",
                      name = "Region")


p3_interactive <- ggplotly(p3, tooltip="text") %>% 
   config(displayModeBar = FALSE)

# plot the chart
print(p3_interactive)



