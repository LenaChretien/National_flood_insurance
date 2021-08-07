#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)


load('data/states_merged_sb.Rda')
load('data/states_meancost.Rda')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Flood insurance Cost"),
    
    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
    #    sidebarPanel(
    #sliderInput("bins",
    #            "Number of bins:",
    #            min = 1,
    #           max = 50,
    #           value = 30)
    #   ),
    
    # Show a plot of the generated distribution
    fluidRow(
        column(10,leafletOutput(output = "mymap")),
        column(2,plotOutput("graph"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Make colorpallete
    cost_pal <- colorNumeric(palette="YlOrRd", domain= states_merged_sb$mean_cost)
    popup_sb = paste0("State: ", states_merged_sb$STUSPS, "<br>","<b>","Cost: $", round(states_merged_sb$mean_cost,0))
    
    st = reactive({
        event = input$map_shape_click
        if (!is.null(event)){
            return(states_merged_sp$STUSPS)
        }
    })
    
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
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
            addLegend(cost_pal, values = states_merged_sb$mean_cost,
                      position = "bottomright",
                      title = "Mean policy cost")
    })
    
    # Default bar graph
    output$graph = renderPlot({
        p = df %>% 
            ggplot(aes(mean_cost, reorder(state, -mean_cost))) + geom_col()+
            labs(y = "State", x = "Policy Cost", title = "Average cost of policy per state")
        
        p + theme(axis.text.y = element_text(size = 6)) + 
            scale_fill_manual(values = c("1" = "red", "0" = "darkgray"), guide = FALSE)
    })
    
    
    observeEvent(input$map_click,
                 {output$graph = renderPlot({
                     df %>%  mutate(group = ifelse(state == st(), "1","0")) %>%
                         ggplot(aes(mean_cost, reorder(state, -mean_cost), fill = group)) + 
                         geom_col()+
                         labs(y = "State", x = "Policy Cost", title = "Average cost of policy per state") + 
                         theme(axis.text.y = element_text(size = 6)) + 
                         scale_fill_manual(values = c("1" = "red", "0" = "darkgray"), guide = FALSE)
                 })
                 }
                 
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
