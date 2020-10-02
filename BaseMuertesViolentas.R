Base <- function(){ 
  Base <- read_delim("https://github.com/JavierRojasC/JavierRCam/raw/master/BASE%20MUERTES_VIOLENTAS.csv", 
                                                    ";", escape_double = FALSE, trim_ws = TRUE)
  return(Base)}
