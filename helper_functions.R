

### functions to run Wiegetisch preprocessing data app
#library("haven")     # read_sav
#library("R.matlab")  # read .mat files
#library("stringr")   # work with strings

# .txt Dateien einlesen und initiale Parameter berechnen
load_and_save_raw_data <- function(file_path_txt, name_dat_raw, snack_names){

  files <- dir(file_path_txt, pattern = ".txt", full.names = TRUE)

  dat_raw <- data.frame()
  for(file in files){
    temp <- read.table(file, header = TRUE)
    temp <- temp[, c("vp", "time", trimws(scan(text = snack_names, what = "char", sep = ",")))]      
    dat_raw <- rbind(dat_raw, temp)
  }

  dat_raw$vp <- factor(dat_raw$vp)
  saveRDS(dat_raw, paste0("./data_temp/", name_dat_raw))
  return(dat_raw)
}

# initiale Parameter der Datenaufbereitung berechnen
calc_cleaning_prameters <- function(name_dat_raw){

    # Aufbau: vp snack range_min range_max delete_first

    dat_raw <- readRDS(paste0("./data_temp/", name_dat_raw))

    snacks <- colnames(dat_raw)[3:ncol(dat_raw)]
    vps    <- unique(dat_raw$vp)

    paras <- data.frame()
    for(vp in vps){
      for(snack in snacks){
          
         data <- dat_raw[dat_raw$vp == vp, snack]  
         
         temp <- NULL
         temp$vp <- vp
         temp$snack <- snack

         num_points <- 10 # num. of first and last points to calculate meaningful range
         temp$range_max <- round(min(mean(data[5:(5+num_points)], na.rm = TRUE) + 3 * sd(data[5:(5+num_points)], na.rm = TRUE) + .5, max(data)), 1)
         temp$range_min <- round(max(mean(data[(length(data) - num_points):length(data)],  na.rm = TRUE) - 3 * sd(data[(length(data) - num_points):length(data)],  na.rm = TRUE) - .5, min(data)), 1)

         temp$delete_first <- 0
         temp$first_lag    <- 20
         temp$first_error  <- 20
         temp$second_lag   <- 10
         temp$second_error <- 10

         temp$add_points <- ""

         paras <- rbind(paras, temp)
      
      }
    }
    saveRDS(paras, paste0("./data_temp/",  paste0("paras_", name_dat_raw)))

}


##### Bereinigungs Funktionen

# Daten interpolieren
interpolate_na <- function(x, cut = 0.5){
  #print(x)
  # NA am anfang durch ersten nicht NA wert ersetzen
  if(is.na(x[1])) x[1:(which(!is.na(x))[1] - 1)] <- x[which(!is.na(x))[1]]
  if(any(is.na(x))){

    start <- which(!is.na(x) & is.na(c(x,0)[-1])) # start NA sequenz
    end <- which(!is.na(c(x,0)) & is.na(c(0, x))) # ende NA sequenz
    laenge <- (which(!is.na(c(x,0)) & is.na(c(0, x))) - which(!is.na(x) & is.na(c(x,0)[-1])) - 1) # laenge NA sequenz

    for(idx in 1:length(start)){
      if(end[idx] <= length(x)){
        x[(start[idx] + 1):(start[idx] + ceiling(laenge[idx] * cut))] <- x[start[idx]]

        if(laenge[idx] != 1){
          x[(start[idx] + ceiling(laenge[idx] * cut) + 1):(start[idx] + laenge[idx])] <- x[end[idx]]
        }
      } else{
        x[(start[idx] + 1):(start[idx] + laenge[idx])] <- x[start[idx]]

      }
    }
  }

  if(any(is.na(x))) warning("Interpolation did not finish without NAs")
  return(x)
}

# gleitendes Mittel "lag" breit, Wertea, die danach kommen und
# groesser als gM + sdV*fehler (sdV*fehler = Schwelle) sind auf NA setzen

running_mean_forward <- function(daten, lag, propSD = TRUE, fehler = 5){

  result <- daten

  for(idx in 2:length(result)){
    if(!is.na(result[idx])){  
      temp <- result[-idx]
      # gleitendes Mittel VOR Wert (ohne Wert selbst)
      if(idx <= lag){
        gmV <- mean(temp[1:lag], na.rm = TRUE)
        if(propSD) sdV <- sd(temp[1:lag], na.rm = TRUE) else sdV <- 1   
      } else {
        gmV <- mean(temp[(idx - 1 - lag):(idx - 1)], na.rm = TRUE)
        if(propSD) sdV <- sd(temp[(idx - 1 - lag):(idx - 1)], na.rm = TRUE) else sdV <- 1  
      } 

      # alle Werte die ueber gmV+Schwelle und nach idx kommen auf NA setze
      result[which(result > (gmV + (sdV * fehler)))[which(result > (gmV + (sdV * fehler))) >= idx]] <- NA

    }
  }

  return(result)
}


# wie running_mean_forward nur von hinten und entsprechend kleineren Werten als gmV-Schwelle 
running_mean_backward <- function(daten, lag, propSD = TRUE, fehler = 5){

  result <- daten

  for(idx in length(result):2){
    if(!is.na(result[idx])){  
      temp <- result[-idx]
      # gleitendes Mittel VOR Wert (ohne Wert selbst)
      if(idx >= length(result) - lag){
        gmV <- mean(temp[length(result):(length(result) - lag)], na.rm = TRUE)
        if(propSD) sdV <- sd(temp[length(result):(length(result) - lag)], na.rm = TRUE) else sdV <- 1  

      } else {
        gmV <- mean(temp[(idx + 1):(idx + 1 + lag)], na.rm = TRUE)
        if(propSD) sdV <- sd(temp[(idx + 1):(idx + 1 + lag)], na.rm = TRUE) else sdV <- 1  
      } 

      # alle Werte die unter gmV-Schwelle und nach idx kommen auf NA setze
      result[which(result < (gmV - (sdV * fehler)))[which(result < (gmV - (sdV * fehler))) <= idx]] <- NA

    }
  }

  return(result)
}


# logische Regel: Werte im zeitlichen Verlauf duerfen nur kleiner Werten
only_decreasing <- function(x){
  for(idx in 2:length(x)){
      if(x[idx] > x[idx - 1]) x[idx] <-  x[idx - 1]
  }
  if(any(diff(x) > 0)) warning("only_decreasing not working correctly")
  return(x)
}

