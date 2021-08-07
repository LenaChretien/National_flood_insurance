library(ggplot2)
library(dplyr)
#library(gridExtra)
library(usmap)
library(maps)


state_list = c('AL','AZ','AR','CA','CO','CT','DE','DC','FL','GA','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

usm = map_data('usa')
usstates = map_data("state")
states = unique(usstates$region)

############################################################
############### MEAN POLICY COST BY STATE ###############
############################################################

for (i in 1:length(state_list)){
      eval(parse(text = paste("load('data/", state_list[i], ".Rda')", sep = '')))
      eval(parse(text = paste("DD = ", state_list[i], sep = '')))
      eval(parse(text = paste("remove(", state_list[i],")", sep = "")))
      DD = mean(DD$policycost)
      if (i == 1){
            df = data.frame(states[i], DD)
            df['state'] = state_list[i]
            colnames(df) = c('region', 'mean_cost', 'state')
      } else {
            dfdf = data.frame(states[i],DD)
            dfdf['state'] = state_list[i]
            colnames(dfdf) = c('region', 'mean_cost', 'state')
            df = rbind(df,dfdf)
      }
      print(i)
} 



### MAP 
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(leaflet.minicharts)
library(dplyr)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(geojsonio)
library(tigris)

remove(states)
states = states(cb = T)
class(states)

'WI','WV','VT','TX','SD','RI',
'OR','NY','NH','NE','KS','MS',
'IL','DE','CT','AR','IN','MO',
'FL','NV','ME','MI','GA','TN',
'VA','NJ','KY','ND','MN','OK'


sdf_st = c('WI','WV','VT','TX','SD','RI',
           'OR','NY','NH','NE','KS','MS',
           'IL','DE','CT','AR','IN','MO',
           'FL','NV','ME','MI','GA','TN',
           'VA','NJ','KY','ND','MN','OK',
           'MT','WA','UT','CO','OH','AL',
           'IA','NM','SC','PA','AZ','MD',
           'MA','CA','ID','WY','NC','LA')

sdf_lat = c(44.500000,39.000000, 44.000000,31.00000,
            44.500000,41.700001,44.000000,43.000000,
            44.000000,41.500000,38.500000,33.000000,
            40.000000,39.000000,41.599998,34.799999,
            40.273502,38.573936,27.994402,39.876019,
            45.367584,44.182205,33.247875,35.860119,
            37.926868,39.833851,37.839333,47.650589,
            46.392410, 36.084621,46.965260,47.751076,
            39.419220, 39.113014, 40.367474,32.318230,
            42.032974, 34.307144, 33.836082, 41.203323,
            34.048927, 39.045753, 42.407211,36.778259,
            44.068203, 43.075970,35.782169,30.391830)




sdf_lon = c(-89.500000, -80.500000,	-72.699997,	-100.000000,
            -100.000000,-71.500000, -120.500000, -75.000000,
            -71.500000, -100.000000, -98.000000,-90.000000,
            -89.000000,-75.500000,-72.699997,-92.199997,
            -86.126976, -92.603760,-81.760254,-117.224121,
            -68.972168, -84.506836,	-83.441162,-86.660156,
            -78.024902,-74.871826,	-84.270020,-100.437012,
            -94.636230,	-96.921387, -109.533691,-120.740135,
            -111.950684, -105.358887,-82.996216,-86.902298,
            -93.581543,	-106.018066,-81.163727,-77.194527,
            -111.093735,-76.641273,	-71.382439,-119.417931,
            -114.742043,-107.290283,-80.793457,-92.329102)
sdf = data.frame(sdf_st,sdf_lon,sdf_lat)



states %>% 
   leaflet() %>%
   addTiles() %>%
   addPolygons(popup= ~NAME)


states_merged_sb = geo_join(states, df, "STUSPS", "state")
#states_merged_sb <- subset(states_merged_sb, !is.na(total))

# Color pallet 
cost_pal <- colorNumeric(palette="YlOrRd", domain= states_merged_sb$mean_cost)

# setting up pop-up text
popup_sb = paste0("State: ", states_merged_sb$STUSPS, "<br>","<b>","Cost: $", round(states_merged_sb$mean_cost,0))

map1 = leaflet() %>%
   #addProviderTiles(providers$CartonDB.Positron) %>%
   addTiles() %>%
   setView(-98.483330, 38.712046, zoom = 3.5) %>%
   addPolygons(data = states_merged_sb, 
               fillColor = ~cost_pal(states_merged_sb$mean_cost),
               fillOpacity = 0.7,
               weight = 0.2,
               smoothFactor = 0.2,
               highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
               popup = ~popup_sb) %>%
   #addLabelOnlyMarkers(lng = sdf, lat = 44.5,
   #                   label = "WI",
   #                   labelOptions = labelOptions(noHide = T)) %>%
   addLegend(cost_pal, values = states_merged_sb$mean_cost,
             position = "bottomright",
             title = "Mean policy cost") 
   
map1
   #addMarkers(
   #   lng = -89.5, lat = 44.5,
   #   label = "Static label",
   #   labelOptions = labelOptions(
   #      noHide = T, textOnly = TRUE,
   #      style = list(
   #         "color" = "red",
   #         "font-family" = "serif",
   #         "font-style" = "italic",
   #         "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
   #         "font-size" = "12px",
   #         "border-color" = "rgba(0,0,0,0.5)"
   #      )))

map1


year_list = c('2013','2014','2015','2016','2017','2018','2019')

state_list = c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID',
               'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
               'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
               'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

load('data/states_meancost.Rda')

st = "AL"

p = df %>% mutate(group = ifelse(state == "AL", "1","0")) %>%
   ggplot(aes(mean_cost, reorder(state, -mean_cost), fill = group)) + geom_col()+
   labs(y = "State", x = "Policy Cost", title = "Average cost of policy per state")
p = p + theme(axis.text.y = element_text(size = 6)) + 
   scale_fill_manual(values = c("1" = "red", "0" = "darkgray"), guide = FALSE)
p



