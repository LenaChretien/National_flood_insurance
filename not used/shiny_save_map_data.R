# load and save Shiny map data

library(ggplot2)
library(dplyr)
library(usmap)
library(maps)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(geojsonio)
library(tigris)


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


remove(states)
states = states(cb = T)
states = states %>% filter(STUSPS %in% state_list)

states_merged_sb = geo_join(states, df, "STUSPS", "state")

save(states_merged_sb, file = 'data/states_merged_sb.Rda')
