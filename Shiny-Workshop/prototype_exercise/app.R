pacman::p_load(shiny, tidyverse, tmap, sf, DT)


aspatial<-read_csv("data/aspatial/aspatial.csv") 

aspatial.1 <- aspatial %>%
  filter(Time == 2019) %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = rowSums(.[3:6])
         +rowSums(.[12])) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[7:11])+
           rowSums(.[13:15]))%>%
  mutate(`AGED`=rowSums(.[16:21])) %>%
  mutate(`TOTAL`=rowSums(.[3:21])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`)

aspatial.2 <- aspatial.1 %>%
  mutate_at(.vars = vars(PA, SZ), 
            .funs = funs(toupper)) %>% #converting column inputs to uppercase
  filter(`ECONOMY ACTIVE` > 0)

geoSG<-st_read(dsn="data/geospatial",
               layer="MP14_SUBZONE_WEB_PL") 

mpsz_pop2020 <- left_join(geoSG, aspatial.2,
                          by = c("SUBZONE_N" = "SZ"))

ui <- fluidPage(
  titlePanel("Choropleth Mapping System"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Mapping Variable",
                  choices = c("Dependency"="DEPENDENCY",
                              "Economy Active"="ECONOMY ACTIVE",
                              "Young"="YOUNG",
                              "Aged"="AGED",
                              "Total"="TOTAL"),
                  selected="DEPENDENCY",
                  multiple=FALSE),
      selectInput(inputId = "classification",
                  label = "Classification Method:",
                  choices = c("pretty"="pretty",
                              "quantile"="quantile",
                              "sd"="sd",
                              "equal"="equal",
                              "kmeans"="kmeans",
                              "hclust"="hclust",
                              "fisher"="fisher",
                              "jenks"="jenks"),
                  selected="pretty",
                  multiple=FALSE),
      sliderInput(inputId = "classes",
                  label = "number of classes",
                  min=6,
                  max=12,
                  value=8),
      selectInput(inputId = "color",
                  label = "Color Scheme",
                  choices = c("Reds"="Reds",
                              "Blues"="Blues",
                              "Yellow-Orange-Red"="YIOrRd"
                              
                  ),
                  selected="Reds",
                  multiple=FALSE)),
    mainPanel(
      tmapOutput("mapPlot")#for interactive plot, change it to tmap output
    )
  )
)

server <- function(input, output) {
  #dataset<-reactive({
    #mpsz_pop2020 |> 
     # filter(`OUTLET TYPE` %in% input$type) |> 
     # filter(`Gp1Gp2 Winnings` >= input$winnings)
  #})
  
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE)+
    tm_shape(shp = mpsz_pop2020) +
     tm_fill(input$variable,
             n=input$classes,
             style=input$classification,
             palette=input$color
             ) +
      tm_view(set.zoom.limits = c(11,16))
  })
}

shinyApp(ui, server)