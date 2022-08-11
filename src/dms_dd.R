###
# format the lat lon data to DD 
### 

dms_dd <- function(df, colname){

  d2 <- df %>%
    tidyr::separate(col = colname, into = c("deg","v2"), sep ="°")%>%
    tidyr::separate(col = "v2", into = c("min","sec"), sep ="´")%>%
    dplyr::mutate(
      deg = as.numeric(deg),
      min = as.numeric(min)/60,
      sec = as.numeric(str_replace(sec, ",", "."))/3600
      )%>%
    rowwise()%>%
    dplyr::mutate({{colname}} := deg + min + sec)%>%  
    dplyr::select(names(df))

  return(d2)
}