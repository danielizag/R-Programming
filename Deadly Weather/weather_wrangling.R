library("dplyr")

weather <- read.csv("WeatherEvents.csv")

View(weather)

### Economic Impact
e_impact <- weather %>% 
  group_by(EVENT, STATE) %>% 
  summarise(Total_COST = sum(PROPDMG + CROPDMG)) %>% 
  arrange(desc(Total_COST)) %>%
  arrange(desc(EVENT)) %>% 
  filter(Total_COST >= 1) %>% 
  top_n(3, Total_COST) %>% 
  group_by(EVENT, STATE)
View(e_impact)

### Health and Safety Impact
# Health
hs_impact <- weather %>% 
  group_by(EVENT, STATE) %>% 
  summarise(Total_FATAL = sum(FATALITIES + INJURIES)) %>%
  arrange(desc(Total_FATAL)) %>% 
  arrange(desc(EVENT)) %>%
  filter(Total_FATAL >= 1 ) %>% 
  top_n(3, Total_FATAL) %>% 
  group_by(EVENT, STATE)
View(hs_impact)

