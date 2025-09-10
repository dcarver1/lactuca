pacman::p_load(plotly, readr,dplyr,htmlwidgets)



# read in data 
d1 <- readr::read_csv("outputs/ecoGeoData.csv")
# alter the first species names 
d1$species[1] <- "Lvir"

# add some specific values for box plot visualizaiton 
d2 <- d1 |>
  dplyr::mutate(
    name = case_when(
      species == "Lvir" ~ "L. virosa",
      species == "Lser" ~ "L. serroila",
      species == "Lser, Lvir" ~ "L. serroila & L. virosa"
    )
  )

# format data for visualizations 
d3 <- d2 |>
  dplyr::select( 
    "name", "species","altitude"  ,"Annual mean temperature"                           
    ,"Mean diurnal temperature range"                       ,"Isothermality"                                       
    ,"Temperature seasonality (standard deviation)"         ,"Maximum temperature of warmest month"                
    ,"Minimum temperature of coldest month"                 ,"Temperature annual range"                            
    ,"Mean temperature of wettest quarter"                  ,"Mean temperature of driest quarter"                  
    ,"Mean temperature of warmest quarter"                  ,"Mean temperature of coldest quarter"                 
    ,"Annual precipitation"                                 ,"Precipitation of wettest month"                      
    ,"Precipitation of driest month"                        ,"Precipitation seasonality (coefficient of variation)"
    ,"Precipitation of wettest quarter"                     ,"Precipitation of driest quarter"                     
    ,"Precipitation of warmest quarter"                     ,"Precipitation of coldest quarter"                    
    ,"srad"                                                 ,"vapr"                                                
    ,"wind"                                                                                            
    ,"Aspect (North-South)"                                 ,"Aspect (East-West)"                                  
    ,"Slope"                                                ,"name" 
    )
desired_order <- c("L. virosa", "L. serroila","L. serroila & L. virosa")

# 3. Convert the 'Species' column to a factor with the specified level order
# This is the key step! We are modifying the dataframe.
d3$name <- factor(d3$name, levels = desired_order)

titles <- c("Altitude","Annual mean temperature"
            ,"Mean diurnal temperature range"                       ,"Isothermality"                                       
            ,"Temperature seasonality (standard deviation)"         ,"Maximum temperature of warmest month"                
            ,"Minimum temperature of coldest month"                 ,"Temperature annual range"                            
            ,"Mean temperature of wettest quarter"                  ,"Mean temperature of driest quarter"                  
            ,"Mean temperature of warmest quarter"                  ,"Mean temperature of coldest quarter"                 
            ,"Annual precipitation"                                 ,"Precipitation of wettest month"                      
            ,"Precipitation of driest month"                        ,"Precipitation seasonality (coefficient of variation)"
            ,"Precipitation of wettest quarter"                     ,"Precipitation of driest quarter"                     
            ,"Precipitation of warmest quarter"                     ,"Precipitation of coldest quarter"                    
            ,"Solar Raditation"                                     ,"Water Vapor Pressure"                                                
            ,"Average Wind Speed"                                                                           
            ,"Aspect (North-South)"                                 ,"Aspect (East-West)"                                  
            ,"Slope")

genBoxPlot <- function(param, title, data){
  fig_horizontal <- plot_ly(
    data = data,
    x = ~data[[param]],      # Swapped: Numeric variable on the x-axis
    y = ~name ,           # Swapped: Categorical variable on the y-axis
    type = "box",
    color = ~name ,
    colors = c("#4390cb","#00a553","#ec7371" )
  )
  
  # 3. (Optional) Update the axis titles in the layout
  fig_horizontal <- fig_horizontal %>% layout(
    # paper_bgcolor = '#1f2937', # Outer background color
    plot_bgcolor = '#d0d3d3',  # Inner plotting area color
    showlegend = FALSE,
    title = paste0("<b>", title, "</b>"),
    xaxis = list(title = ""),
    yaxis = list(title = "",
                 tickfont = list(family ='Georgia, italic', size = 12)
                 )            # Updated y-axis title
  )
  fig_horizontal
  # saveWidget(fig_horizontal, paste0(title,".html"))
}



genBoxPlot(data = d3, param = "altitude", title = "Altitude")


plots <- purrr::map2(.x = names(d3)[3:28],
                     .y = titles , 
                     .f = genBoxPlot,
                     data = d3)

