###
# format the lat lon data to DD 
### 

dms_dd <- function(df, colname){

  d2 <- df %>%
    dplyr::mutate(
      lat = case_when(
        grepl("S", latitude) ~ -1,
        TRUE ~ 1 
      ),
      lon = case_when(
        grepl("W", longitude) ~ -1,
        TRUE ~ 1 
      ),
      degree = case_when(
        colname == "latitude" ~ lat, 
        colname == "longitude" ~ lon
      )
    ) |>
    tidyr::separate(col = colname, into = c("deg","v2"), sep ="°")%>%
    tidyr::separate(col = "v2", into = c("min","sec"), sep ="´")|>
    dplyr::mutate(sec = as.numeric(str_replace(sec, ",", "."))) |>
    dplyr::mutate(
      deg = as.numeric(deg),
      min = as.numeric(min)/60,
      sec = as.numeric(sec)/3600
      ) |>
    rowwise() |>
    dplyr::mutate({{colname}} := (deg + min + sec)*degree) |>  
    dplyr::select(names(df))

  return(d2)
}
