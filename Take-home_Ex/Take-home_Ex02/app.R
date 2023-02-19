pacman::p_load(sf, raster, spatstat, tmap, tidyverse, sfdep, maptools,readxl)
library(dplyr)

geoJAR <- st_read(dsn = "data/geospatial/",
                  layer = "BATAS_DESA_DESEMBER_2019_DUKCAPIL_DKI_JAKARTA") %>%
  st_transform(crs=23878)
# filtering out the island
geoJAR <- filter(geoJAR, KAB_KOTA != "KEPULAUAN SERIBU")

# Set the working directory to the folder containing the Excel files
setwd("data/aspatial/") 

# Get a list of all Excel files in the directory
files <- list.files(pattern = ".xlsx")

# Loop through the files and read each one into a data frame
for (file in files) {
  assign(gsub(".xlsx", "", file), read_excel(file))
}

April_2022 <- vac_apr2022 %>%
  mutate(date = 'April 2022') %>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  dplyr::select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

December_2021 <- vac_dec2021 %>%
  mutate(date = 'December 2021')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

February_2022 <- vac_feb2022 %>%
  mutate(date = 'February 2022')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

January_2022 <- vac_jan2022 %>%
  mutate(date = 'January 2022')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

July_2021 <-  vac_jul2021 %>%
  mutate(date = 'July 2021')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

June_2022 <- vac_jan2022 %>%
  mutate(date = 'June 2022') %>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

May_2022 <- vac_may2022 %>%
  mutate(date = 'May 2022')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

November_2021 <- vac_nov2021 %>%
  mutate(date = 'November 2021')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

October_2021 <- vac_oct2021 %>%
  mutate(date = 'October 2021')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

September_2021 <- vac_sep2021 %>%
  mutate(date = 'September 2021')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

March_2022 <- vac_mar2022 %>%
  mutate(date = 'March 2022')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

August_2021 <-vac_aug2021  %>%
  mutate(date = 'August 2021')%>% rename(village_code=`KODE KELURAHAN`, city_region =`WILAYAH KOTA`, subdistrict=`KECAMATAN`, ward=`KELURAHAN`, total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, not_vaccinated =`BELUM VAKSIN`) %>% 
  select(date,village_code, city_region, subdistrict, not_vaccinated ,total_vaccination)

df <- rbind(July_2021, August_2021, September_2021,October_2021,November_2021,December_2021,January_2022,February_2022,March_2022,April_2022,May_2022, June_2022) %>%   
  na.exclude() %>%
  filter(city_region != "KAB.ADM.KEP.SERIBU") %>%
  mutate(total_population = total_vaccination + not_vaccinated, vaccination_rate = total_vaccination/total_population)


df <- filter(df, city_region != "KEPULAUAN SERIBU")
# Left join geospatial to aspatial
combined_df <- left_join(df, geoJAR, 
                         by = c("village_code" = "KODE_DESA"))

cdf <- filter(combined_df, KAB_KOTA != c("KEPULAUAN SERIBU", "KAB.ADM.KEP.SERIBU"))

df_sf <- st_as_sf(combined_df) %>%
  mutate(date = as.factor(date))

library(shiny)
library(tmap)

date <- c("July 2021", "August 2021", "September 2021", "October 2021", "November 2021", "December 2021", "January 2022", "February 2022", "March 2022", "April 2022", "May 2022", "June 2022")
# Define the UI
ui <- fluidPage(
  selectInput("dates", "Pick a month",
              date, selected = "July 2021",
              multiple = FALSE),
  
  tmapOutput("my_map")
)

# Define the server
server <- function(input, output) {
  # Render the tmap in the output element
  output$my_map <- renderTmap({
    a <- df_sf |>
      filter(date == input$dates)
    
    tm_shape(a) +
      tm_fill("vaccination_rate",
              style = "quantile",
              palette ="Blues")
  })
}

# Run the app
shinyApp(ui, server)