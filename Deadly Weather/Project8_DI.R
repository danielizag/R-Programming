library("dplyr")
library("shiny")
library("ggplot2")
library("markdown")
library("plotly")

weather <- read.csv("WeatherEvents.csv")

### Economic Impact
e_impact <- weather %>% 
  group_by(EVENT, STATE) %>% 
  summarise(Economic = sum(PROPDMG + CROPDMG)) %>% 
  arrange(desc(Economic)) %>%
  arrange(desc(EVENT)) %>% 
  filter(Economic >= 4000000000) %>% 
  group_by(EVENT, STATE) %>% 
  arrange(desc(Economic))

View(e_impact)

e_impact$Id <- c(1,2,3)

### Health and Safety Impact
hs_impact <- weather %>% 
  group_by(EVENT, STATE) %>% 
  summarise(Health_Safety = sum(FATALITIES + INJURIES)) %>%
  arrange(desc(Health_Safety)) %>% 
  arrange(desc(EVENT)) %>%
  filter(Health_Safety >= 7000) %>% 
  group_by(EVENT, STATE) %>% 
  arrange(desc(Health_Safety))
hs_impact$Id <- c(1,2,3)

View(e_impact)

ui <- fluidPage(    
  titlePanel("Project 8"),
  tabsetPanel(
    tabPanel("Economic",
      tabPanel("Economic Damage Data",
        DT::dataTableOutput("e_table")
      ),
      tabPanel("Economic Damage Plot",
        plotlyOutput("e")
      )
    ),
    tabPanel("Health and Safety",
      tabPanel("Health and Safety Data",
        DT::dataTableOutput("hs_table")
      ),
      tabPanel("Health and Safety Plot",
        plotlyOutput("hs")
      )
    )
  )
)

server <- function(input, output) {
  output$e <- renderPlotly({
    plot_ly(x=e_impact$Id,y=e_impact$Economic,name="chart",type="bar") %>% 
    layout(title = "Property and Crop Damage Due to Events",
            xaxis = list(title = "Identification"),
            yaxis = list(title = "Damages (Dollars)"))
  })
  output$e_table <- DT::renderDataTable({
    DT::datatable(e_impact)
  })
  output$hs <- renderPlotly({
    plot_ly(x=hs_impact$Id,y=hs_impact$Health_Safety,name="chart",type="bar") %>% 
      layout(title = "Health and Safety Impact Due to Events",
             xaxis = list(title = "Identification"),
             yaxis = list(title = "Aggregate Deaths and Injuries"))
  })
  output$hs_table <- DT::renderDataTable({
    DT::datatable(hs_impact)
  })
}
?plot_ly
shinyApp(ui = ui, server = server)



