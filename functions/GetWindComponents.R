GetWindComponentsFromWindSpeedDirection <- function(speed = NULL,
                                                    dir = NULL,
                                                    data = NULL){
  # west-east
  v_e = -speed*sin(2*pi*(dir/360))
  # south - north
  v_n = -speed*cos(2*pi*(dir/360))
  return(data.frame("v_e" = v_e, 
                    "v_n" = v_n))
}

GetWindSpeedDirectionFromComponents <- function(v_e = NULL,
                                                v_n = NULL,
                                                data = NULL,
                                                mean = FALSE){
  if (is.null(data)){
    # use the inputs
  } else
  {
    v_e <- data$v_e
    v_n <- data$v_n
  }
  if (mean){
    dnu <- (is.na(v_e) | is.na(v_n))
    v_e = mean(v_e[!dnu])
    v_n = mean(v_n[!dnu])
  }
  
  speed = (v_e^2 + v_n^2)^(1/2)
  dir.temp = (atan2(v_e,v_n) / (2*pi)) * 360
  # get the signs right
  dir <- dir.temp
  dir[dir.temp > 180] <- dir[dir.temp > 180] - 180
  dir[dir.temp <= 180] <- dir[dir.temp <= 180] + 180
  
  return(data.frame("speed" = speed, 
                    "dir" = dir))
}