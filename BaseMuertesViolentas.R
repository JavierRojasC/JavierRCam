Base <- function(){ 
  Base <- read_delim("https://www.dropbox.com/s/9dgh5lbs9momyfv/BASE%20MUERTES_VIOLENTAS_y%20CTG%20F.csv?dl=1", 
                                                    ";", escape_double = FALSE, trim_ws = TRUE)
  return(Base)}
