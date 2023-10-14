### Year : 2022
### Authors : Marie Joigneau, Amedee Roy, Sophie Bertrand
### Organism : IRD Sete
### Project : to clean and adapt the tropical bird dataset (Functions)

library(datetime)
library(plyr)
library(lubridate)
library(M3)
library(dplyr)
library(stringr)
library(devtools)
library(suncalc)
library(gridGraphics)
library(tidyverse)
library(sf)
library(lutz)
library(filesstrings)
library(ggpubr)
library(cowplot)
library(stringr)
library(crayon)

### ---------- I. SETTING PARAMETER FOR ALL CSV/TEXT IN THE SAME FILE------

## permit to set parameters for the following code 
set_parameter <- function(){
  
  print("SET_PARAMETER FUNCTION")
  
  # we propose vectors already done, as the files are often alike, if not, the user does it manually
  df_par_chosen <- read.csv("parameters - already_chosen.csv",header=TRUE,sep=";",dec=',')
  print(df_par_chosen)
  
  already_done <- as.character(readline("If your parameters are the same as one of those, write the vector_name, if not, write 0 "))
  
  if (already_done!="0"){
    
    # if we have already entered the parameters in the dataframe, we just call them by the name given to the group
    
    cat(green("vector name chosen ",already_done,"\n"))
    nb <- which(already_done==df_par_chosen$vector_name)
    print(paste("number chosen ",nb))
    
    format <- as.character(df_par_chosen$format[nb])
    sep <- as.character(df_par_chosen$sep[nb])
    if (sep=="space"){
      sep <- ""
    }
    dec <- as.character(df_par_chosen$dec[nb])
    reading_pb <- as.character(df_par_chosen$reading_pb[nb])
    spot_extra_lines <- as.character(df_par_chosen$spot_extra_lines[nb])
    num_line <- as.character(df_par_chosen$num_line[nb])
    column_date <- as.character(df_par_chosen$column_date[nb])
    column_time <- as.character(df_par_chosen$column_time[nb])
    column_latitude <- as.character(df_par_chosen$column_latitude[nb])
    lat_N_or_S <- as.character(df_par_chosen$lat_N_or_S[nb])
    column_longitude <- as.character(df_par_chosen$column_longitude[nb])
    long_E_or_W <- as.character(df_par_chosen$long_E_or_W[nb])
    format_date <- as.character(df_par_chosen$format_date[nb])
    hour <- as.character(df_par_chosen$hour[nb])
    location_col <- as.character(df_par_chosen$location_col[nb])
    column_ID <- as.character(df_par_chosen$column_ID[nb])
    header <- as.character(df_par_chosen$header[nb])
    datetime_or_not <- as.character(df_par_chosen$datetime_or_not[nb])
    
    param_vec <- c(format,sep,dec,reading_pb,spot_extra_lines,num_line,column_date,column_time,column_latitude,
                   lat_N_or_S,column_longitude,long_E_or_W,format_date,hour,location_col,column_ID,header,datetime_or_not)
    cat(green("param_vec \n"))
    print(param_vec)
    
    return(param_vec)
  }
  
  cat(green("In this function you will write the parameters needed to format all the files in this folder
  CAUTIOUS: All the files in the folder need to have EXACTLY the same presentation
  If not, separate them in different sub folders
  Let's begin
  ---------------------------------------
  1) Question to read the data"))
  format <- as.character(readline("Which type is your dataset? (text or csv) "))
  sep <- as.character(readline("How are your data separated? (for example space or ;)"))
  if (sep=="space"){
    sep <- ""
  }
  dec <- as.character(readline("What is your dec? (usually . or ,)"))
  header <- as.character(readline("Do you have header=TRUE (columns named) or header=FALSE (columns no named) (write TRUE/FALSE) "))
  
  reading_pb <- as.character(readline("Do you have line with text so a problem for the reading? If yes, write the problem (EVENT/extra_lines/no)"))
  
  spot_extra_lines <- as.character(readline("How do you spot your extra lines? (like with the word EVENT)"))
  num_line <- as.numeric(readline("Seeing you txt file, enter the number of the first line you want to skip "))
  
  cat(green("
  ---------------------------------------
  2) Questions to get the right columns"))
  
  column_date <- as.numeric(readline("Which column is date? (1,2,...) "))
  column_time <- as.numeric(readline("Which column is time? (1,2,...) "))
  
  column_latitude <- as.numeric(readline("Which column is latitude? (1,2,...) warning : S,N,E,W can be columns too "))
  lat_N_or_S <- as.character(readline("Do you have N or S or nothing? "))
  
  column_longitude <- as.numeric(readline("Which column is longitude? (1,2,...) warning : S,N,E,W can be columns too "))
  long_E_or_W <- as.character(readline("Do you have E or W or nothing? "))
  
  column_ID <- as.numeric(readline("Which column is ID? (put no if no separation needed) "))
  
  cat(green("
  ---------------------------------------
  3) Questiont to format the dates and help for time. Here a few examples to help you.
  If you have 27.11.2008, you write the format %d.%m.%Y
  If you have 2020/10/21, you write the format %Y/%m/%d"))
  
  format_date <- as.character(readline("Write here the format "))
  hour <- as.character(readline("Is it HLOC or UTC? "))
  datetime_or_not <- as.character(readline("Do you have datetime (if yes write the nb of the column) or date | time (no)? "))
  
  cat(green("
  ---------------------------------------
  4) Here the different colonies you can have in your dataset (longitude,latitude)
  automatic = colony guessed by median
  manually = you entered yourself the coordinates
  and below the dataframe with names of colony and positions
  "))
  df_colony_par <- read.csv("metadata_colony_format.csv",header=TRUE,sep=";",dec=',')
  print(df_colony_par)
  
  location_col <- as.numeric(readline("Write here the number of the colony position by the dataframe (or automatic/manually) "))
  
  # LAST STEP
  # Here the final vector we obtain :
  param_vec <- c(format,sep,dec,reading_pb,spot_extra_lines,num_line,column_date,column_time,column_latitude,
                 lat_N_or_S,column_longitude,long_E_or_W,format_date,hour,location_col,column_ID,header,datetime_or_not)
  cat(green("param_vec \n"))
  cat(green("format,sep,dec,reading_pb,spot_extra_lines,num_line,column_date,column_time,column_latitude,lat_N_or_S,
        column_longitude,long_E_or_W,format_date,location_col,column_ID,datetime_or_not \n"))
  print(param_vec)
  
  like_or_not <- as.character(readline("Do you agree with what you have written? (yes/no) "))
  if (like_or_not == "no"){
    set_parameter()
  }
  
  return(param_vec)
}

### ---------- II. HAVING A GOOD DATABASE -----------------------------
### --- II.1. OPENING THE FILE ----------------------------------------

## permit to open the file even if exception 
open_file <- function(full_path,param){

  print("OPEN_FILE FUNCTION")
  
  # depending of the categories defined by parameters, we open the file
  
  # CATEGORY 1 : CSV
  if (param[1] == "csv"){
    print("We have a csv")
    
    # SUBCATEGORY 1 : CSV WITH 1 EXTRA ROW
    if (param[4]=="extra_lines"){
      print("csv with wrong header/extra line")
      df <- read.csv(full_path, header=as.logical(param[17]), sep=param[2], dec=param[3],skip=as.numeric(param[6]))
      return(df)
    }else{
      # SUBCATEGORY 2 : A NORMAL CSV
      print("normal csv")
      df <- read.csv(full_path, header=as.logical(param[17]), sep=param[2], dec=param[3],stringsAsFactors = TRUE)
      return(df)
    }
    
  }else{
    
    # CATEGORY 2 : TXT
    print("We have a txt")
    
    # SUBCATEGORY 1: we have text lines with EVENT
    if (param[4]=="EVENT"){
      print("We have an EVENT problem")
      
      # STEP 1: find lines with EVENT
      # we open the file as a csv with a single column...
      df <- read.table(full_path, sep =';')
      # ... and then we we find the lines with the sentence with particular word like EVENT (param[5])
      idx_EVENT <- which((str_detect(df$V1,as.character(param[5])))==TRUE)
      
      # STEP 2: skip theses lines and transform in a df that can be used for after
      # I read again the txt but without the target lines
      df <- read.table(full_path, header =TRUE, sep =';')[-(idx_EVENT-1), ]
      # I split the one column vector into nine
      df <- str_split_fixed(df, "\t", 9)
      # Then I create a df by a matrix first (in 2 steps)
      df <- matrix(df,ncol=9)
      df <- data.frame(df)
      df$X1 <- as.factor(df$X1)
      df$X2 <- as.factor(df$X2)
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      
      return(df)
    }
    
    # SUBCATEGORY 2: we have extra lines at the beginning
    if (param[4]=="extra_lines"){
      print("We have an extra lines problem")
      df <- read.table(full_path, sep=param[2], dec=param[3],stringsAsFactors = TRUE,skip=as.numeric(param[6]))
      return(df)
    }
    
    # SUBCATEGORY 3: we have a rather normal txt
    if(param[4]=="no"){
      print("we have a rather normal txt")
      df <- read.table(full_path, header=as.logical(param[17]),sep=param[2], dec=param[3],stringsAsFactors = TRUE)
      return(df)
    }
  }
}

### --- II.2. RENAME AND ORDER FILE ------

## Permit to keep only date, time, longitude, and latitude columns from the original dataframe, and eventually id column for the dataframe
rename_and_order_file <- function(df,param,separ){
  
  print("RENAME AND ORDER FILE")
  
  # If dataset doesn't need to be separated :
  if(separ!="separate"){
    
    print("data no separation")
    
    # if we have date, time, lon and lat columns :
    if (param[18]=="no"){
      
      # The new dataframe takes only the date, time, longitude and latitude from the original dataframe by the column number informed by user in the vector param
      # param[7] is the number of the date column in the original file 
      # param[8] for time
      # param[11] for longitude
      # param[9] for latitude
      print(paste("date column ",param[7]))
      print(paste("time column ",param[8]))
      print(paste("lon ",param[11]))
      print(paste("lat ",param[9]))
      df <- data.frame(date=df[,as.numeric(param[7])],time=df[,as.numeric(param[8])],lon=df[,as.numeric(param[11])],lat=df[,as.numeric(param[9])])
      
      # If the latitude and longitude are written with N/S/E/W and not with +/-, it needs to be corrected
      # param[10] = S,N or nothing if it's correct. If it's S, it needs a minus. 
      if (param[10] == "S"){
        df$lat <- -df$lat
      }
      # param[12] = W,N or nothing if it's correct. If it's W, it needs a minus.
      if (param[12] == "W"){
        df$lon <- -df$lon
      }
      
    }else{
      # if we have a  column and not the usual date and time columns :
      print("we have datetime and not date | time")
      
      # we retrieve and convert the datetime column
      df$datetime <- df[,as.numeric(param[18])]
      # if we have a datetime with day-month-year time: (27-Nov-09 11:40:00)
      print(paste("param datetime ",param[13]))
      if (param[13]=="dmy"){
        print("param[13]==dmy")
        df$datetime <- dmy_hms(df$datetime)
      }
      # if we have a datetime with year-month-day time:
      if (param[13]=="ymd"){
        print("param[13]==ymd")
        df$datetime <- ymd_hms(df$datetime)
      }
      # if we have a normal datetime: (11/28/2010 20:01:00)
      if ((param[13]!="ymd")&&(param[13]!="dmy")){
        print("param[13]!=dmy and ymd")
        df$datetime <- as.POSIXct(df$datetime,format = paste(as.character(param[13]),"%H:%M:%S",sep=" "))
      }
      print("df$datetime[2]")
      print(df$datetime[2])
      # we extract time and date
      df$time <- format(df$datetime, format = "%H:%M:%S")
      df$date <- date(df$datetime)
      # we finally obtain a normal dataframe
      df <- data.frame(date=df$date,time=df$time,lon=df[,as.numeric(param[11])],lat=df[,as.numeric(param[9])])
      
      
    }
    
  }else{
    
    # If dataset needs to be separated : we just renamed the ID column and don't change the dataset
    # param[16] : number of the ID column
    print("data with separation")
    names(df)[as.numeric(param[16])] <- "BirdID"
  }
  
  # we convert lon lat in numeric classes
  df$lon <- as.character(df$lon)
  df$lat <- as.character(df$lat)
  df$lon <- as.numeric(df$lon)
  df$lat <- as.numeric(df$lat)

  return(df)
}

### --- II.3. SPLIT BY ID AND SAVE ------------------------------------

## Split the dataframe by ID and save each one of them
split_by_ID_and_save <- function(df,path_save_ID){
  
  print("SPLIT BY ID AND SAVE")
  
  # We split the dataframe by ID in a list
  df_split = split(df, df$BirdID)
  
  # Each sub dataframe is saved in the same folder as the mother dataframe
  for (i in 1:length(df_split)){
    write.csv(df_split[[i]], file=paste(path_save_ID,df_split[[i]]$BirdID[1],"by_Marie.csv", sep="_"),row.names = FALSE)
  }
}

### --- II.4. FORMAT DATE ---------------------------------------------

## Format the date
format_date <- function(df,param){
  
  print("FORMAT DATE")
  
  # param[13] can be "%Y/%m/%d", "%d/%m/%Y" for example
  # %Y = full year (2022), %m = month (08), %d = day (03)
  df$date=as.Date(df$date, format=param[13])
  
  return(df)
}

### --- II.5. REAL LINE ERROR -----------------------------------------

## Gives the real line of an error
real_line_error <- function(list_all_error){
  
  print("REAL LINE ERROR")
  
  #err_previous : the precedent error index
  err_previous <- list_all_error[[1]]
  # err_new : the new error index
  err_new <- list_all_error[[2]]
  
  # If err_previous or err_new are empty, the function is not necessary
  if ((length(err_previous)==0)||(length(err_new)==0)){
    print("no enough length for real_line_error")
    return(err_new)
  }
  
  # the idea is to know how many line errors inferiors of new lines errors were deleted to know the real values
  # we use it for each i error
  for (i in 1:length(err_new)){ 
    print(paste("i ",i))
    print(paste("len new error ",length(err_new)))
    j=0
    # for each error, if it's superior to the element j of the previous error vector, j=j+1, with a limit of the length of previous error vector
    while ((err_new[i] >= err_previous[j+1]) && (j+1<=length(err_previous))){ 
      print("while loop")
      print(paste("j ",j))
      j <- j+1
    }
    # at the end we know how many previous errors are inferiors to the new errors and we add it to the new error vector
    err_new[i] <- err_new[i] + j
  }
  
  return(err_new)
}

### --- II.6. CHECKING AFTER MIDNIGHT TIME ABERRATIONS... -------------

## Correct the time after midnight error (25:10:03 instead of 01:10:03)
time_after_midnight <- function(df,choice_save){
  
  print("TIME AFTER MIDNIGHT FUNCTION")
  
  # So first I detect the index with the error, here the steps :
  #   substr(bird$time, 1, 2) -> I take the first two characters
  #   str_detect(X,"25") -> I detect if it's 25
  #   which(X == TRUE) -> I show the index where it's the case
  #   sort(unique(X)) -> I sort the index and filter if I don't have them twice
  idx_error_after_midnight <- sort(unique(c(which((str_detect(substr(df$time, 1, 2),"25"))==TRUE),which((str_detect(substr(df$time, 1, 2),"26"))==TRUE),which((str_detect(substr(df$time, 1, 2),"27"))==TRUE),which((str_detect(substr(df$time, 1, 2),"28"))==TRUE),which((str_detect(substr(df$time, 1, 2),"29"))==TRUE))))
  print(paste("index midnight error ",idx_error_after_midnight))
  
  # CHOICE 1: needed for saving the error index = need to be done first
  if (choice_save == "index"){
    print(paste("choice save ",2))
    return(idx_error_after_midnight)
  }
  else{
    # CHOICE 2: needed to correct the df
    
    # If I have at least 1 midnight error, I take care of the errors :
    if (length(idx_error_after_midnight)!=0){
      
      # Need to go from factor to character to change the value
      df$time <- as.character(df$time)
      print(paste("choice save ",3))
      
      for (i in 1:length(idx_error_after_midnight)){
        k = idx_error_after_midnight[i]
        print(paste("idx",k))
        # Then I replace them :
        #   I take the time of the error k
        err_mid2 <- df$time[k]
        # I take the hours in this time
        hour_err2 <- substr(err_mid2, 1, 2)
        # and I replace in the time (err_mid2) the hour (hour_err2) by substract 24h
        a = str_replace(err_mid2,hour_err2, as.character(as.numeric(hour_err2)-24))
        # then I do the same as in the format_time() function (I format it as a time data) to be able to replace the data (if not, not same factor level and not accepted, so NA)
        Test=strptime(a, format="%H:%M:%S") # work also for a 12:30:04.00 format
        Time_bird_last=format(Test, "%H:%M:%S")
        df$time[k] <- Time_bird_last
      }
      # I put it back into factor
      df$time <- as.factor(df$time)
    }
    return(df)
  }
}

### --- ... ADDING LINES TO ERROR FILE... ------------------------------

## Notes midnight errors in the error file
error_after_midnight <- function(df_error,idx){
  
  print("MIDNIGHT ERROR")
  
  # We add a title to separate the different types of error
  df_error <- rbind(df_error,c("_______","_______"))
  df_error <- rbind(df_error,c("MIDNIGHT ERROR","_______"))
  df_error <- rbind(df_error,c("_______","_______"))
  
  # If we have errors:
  if (length(idx)!=0){
    # I note on the txt error the lines with the error
    for (i in 1:length(idx)){
      print("we have midnight error")
      k = idx[i]
      df_error <- rbind(df_error,c("midnight error",k))
    }
    return(df_error)
  }
  
  # If we don't have errors, I indicate it:
  df_error <- rbind(df_error,c("No midnight error",0))
  return(df_error)
}

### --- ... AND FORMAT TIME --------------------------------------------

# Convert time in correct format
format_time <- function(df){
  
  print("FORMAT TIME FUNCTION")
  
  # We convert the time column (work also for 12:30:04.00 or 21:57:15.75)
  # We need 3 steps ("%H:%M:%S" = hour:minute:second)
  Time_bird=strptime(df$time, format="%H:%M:%S") 
  Time_bird_last=format(Time_bird, "%H:%M:%S")
  df$time <- Time_bird_last
  
  return(df)
}

### ---------- III. FUNCTION REPLACE DELETE DATA ----------------------------

## For each error, permit to delete it, replace it manually or replace it automatically
replace_or_delete_data <- function(df,idx,category,choice_repl_del){
  
  print("REPLACE DELETE DATA FUNCTION")
  
  # We create a delete list
  list_delete <- c()
  
  cat(green("here your errors \n"))
  print(idx)
  
  # I have finally chosen that all the errors are deleted in the programm
  # In effect, when I have datetime errors and then lon/lat error, I adjust the lont/lat error index as the datetime errors were deleted
  # Nevertheless I let the second part if needed for ulterior use. Just be cautious to change the code as warned above.
  # I will just use the 1st if
  if (choice_repl_del == "delete"){
    
    cat(green("The errors are deleted \n"))
    list_delete <- idx
    
    # 2nd choice not used :
  }else{
    # For each index, the problem is resolved
    print(paste("For each problem in each index, you will be asked if you want to delete it or replace it"))
    for (i in 1:length(idx)){
      
      print(paste("i ",i))
      k = idx[i]
      print(paste("k ",k))
      
      # We show the row with the error i (line k of dataframe)
      cat(green("Here the index ",k, " with a ",category," problem \n"))
      print(df[k,])
      
      # We propose to the user to make a choice : delete - replace manually - replace automatically
      delete_or_not <- as.character(readline("Do you want to delete, replace or replace automatically (not possible if first or last)? "))
      print(paste("choice : ",delete_or_not))
      
      # CHOICE 1 : we delete
      if (delete_or_not == "delete"){
        print("we delete")
        list_delete <- c(list_delete,k)
      }
      
      # CHOICE 2 : we replace manually
      # Here we replace manually
      if(delete_or_not == "replace"){
        print("we replace")
        if (category == "lon_lat"){
          # we note it is a bad quality (=1 for lon lat, useful because the error is kept)
          df$quality[k] <- 1
          new_lon <- as.numeric(readline("Write the new longitude value "))
          new_lat <- as.numeric(readline("Write the new latitude value "))
          df$lon[k] <- new_lon
          df$lat[k] <- new_lat
          if (k!=1){
            distance_bird <- dist_ortho(df$lon[k],df$lat[k],df$lon[k-1],df$lat[k-1])
            df$step_speed_corrected[k] <- distance_bird / abs(as.numeric(difftime(df$datetime[k], df$datetime[k-1], units = "secs")))
          }
        }
        if (category == "datetime"){
          # we note it is a bad quality (=2 for lon lat, useful because the error is kept)
          df$quality[k] <- 2
          cat(green("Here a exemple for you to re write datetime the right way \n"))
          cat(green("2010-04-14-04-35-59 -> date then time \n"))
          new_datetime <- as.character(readline("Write the new datetime value "))
          df$datetime[k] <- ymd_hms(new_datetime)
        }
      }
      
      # CHOICE 3 : we replace automatically when possible by the mean of the 2 values below and above (not possible if 1st or last data for longitude or latitude)
      if (delete_or_not == "replace automatically"){
        print("we automatically replace")
        if ( (category == "lon_lat") && ((k!=1)&&(k!=length(df$lon))) ){
          df$quality[k] <- 1
          df$lon[k] <- mean(c(df$lon[k-1],df$lon[k+1]))
          df$lat[k] <- mean(c(df$lat[k-1],df$lat[k+1]))
          distance_bird <- dist_ortho(df$lon[k],df$lat[k],df$lon[k-1],df$lat[k-1])
          df$step_speed_corrected[k] <- distance_bird / abs(as.numeric(difftime(df$datetime[k], df$datetime[k-1], units = "secs")))
        }
        if( (category == "datetime")&& ((k!=1)&&(k!=length(df$datetime))) ){
          df$quality[k] <- 2
          df$datetime[k] <- mean(c(df$datetime[k-1],df$datetime[k+1]))
          print(df$datetime[k])
          print(df$datetime[k-1])
          print(df$datetime[k+1])
        }
        else{
          cat(green("We recall that an error on first or last value of the data frame need to be done manually, try again \n"))
          replace_or_delete_data(df,idx,category,"delete")
        }
      }
    }
  }
  
  # Then we delete all errors at the same time 
  if (length(list_delete)>=1){
    df <- df[-(list_delete),]
    print("I have deleted")
  }
  
  print("I have finished!")
  return(df)
}

### ---------- IV. HAVING NEW VARIABLES / COLUMNS ---------------------
### --- IV.1. DATETIME...  --------------------------------------------

## References: 
# https://statisticsglobe.com/difftime-r-function/
# https://cran.r-project.org/web/packages/lutz/lutz.pdf 

## if we are in a UTC (Universal Time Coordinated) format, permit to obtain the jet lag necessary to correct the datetime into HLOC (Hour LOCal) in the new_datetime_column_function
jet_lag <- function(df){
  
  print("JET LAG FUNCTION")
  
  print(df$lon[2])
  print(df$lat[2])
  
  # 1st step: we obtain the time zone with lon / lat of the 2nd value (the 1st value has a probability greater to be a NA than the 2nd one)
  time_zone <- tz_lookup_coords(df$lat[2], df$lon[2], method = "accurate", warn = TRUE)
  print(paste("time zone ",time_zone))
  
  #2nd: we find the offset from GMT at a particular date/time in a particular time zone
  offset <- tz_offset(df$date[2], tz = as.character(time_zone))
  
  print("offset")
  print(offset)
  print(paste("jet lag ",offset$utc_offset_h[1]))
  
  return(offset$utc_offset_h[1])
}

## combine date and hour to obtain the datetime column
new_datetime_columns <- function(df,param){
  
  print("NEW DATETIME COLUMN FUNCTION")
  
  ### We create a combination of the 2 columns with HLOC or UTC as param[14]
  df$datetime <- combine.date.and.time(date = df$date, time = df$time)
  print("Datetime before")
  print(df$datetime[1])
  
  # HLOC or UTC are as param[14]
  # if it's not in HLOC, we need to modify the date, time and datetime columns
  if (param[14] != "HLOC"){
    print("no HLOC")
    # we put datetime into a datetime format and add the jet_lag (can be - or +)
    df$datetime <- ymd_hms(df$datetime)+hours(as.integer(jet_lag(df)))
    # consequently, we need to adapt the time and date columns
    print("we have correct date and time")
    df$time <- format(df$datetime, format = "%H:%M:%S")
    df$date <- date(df$datetime)
    print("We check")
    print(df$datetime[1])
    print(df$time[1])
    print(df$date[1])
    
  }else{
    # if it's in HLOC, we just put it correctly into a datetime format
    df$datetime <- ymd_hms(df$datetime)
  }
  
  return(df)
  
}

### --- ... AND CHECKING DATETIME ABBERATION ---------------------------

## obtain a list of datetime error
find_error_datetime <- function(choice,df){
  
  print("FIND ERROR DATETIME FUNCTION")
  
  # we have 4 types of datetime errors : NA errors, logical errors, same datetime errors and errors seen by the user
  # we find the lines with NA
  pb_NA <- which(is.na(df$datetime==TRUE))
  
  # here the problem of logic between 2 datetime
  pb_logic <- c()
  
  # here if there is same datetime in 2 consecutive lines
  v1 <- df$datetime[1:(length(df$datetime)-1)]
  v2 <- df$datetime[2:length(df$datetime)]
  pb_same <- which(v1==v2)
  
  # and finally if there is a problem seen by the user
  pb_user <- c()
  
  # CHOICE AUTOMATIC : permit to have pb_logic and pb_user
  if (choice != "0"){
    
    # then we try to find logic problem in datetime like an aberrant one
    first_set <- which((diff(df$datetime)<0))
    print("first set")
    print(first_set)
    
    # if there are logical errors, we ask the user to point the right error line
    # it's not possible to automatically find the right line with which((diff(df$datetime)<0))
    # example 1: 1,2,4,3,5 give +,+,-,+
    # example 2: 1,2,1,3,5 give +,-,+,+
    if(length(first_set>0)){
      for (i in 1:length(first_set)){
        cat(green("cautious : middle line = ",first_set[i]," \n"))
        print(df[c(first_set[i]-2,first_set[i]-1,first_set[i],first_set[i]+1,first_set[i]+2),])
        right_line <- as.character(readline("Write here the line in the 3 proposed with a problem, or 'no' if there is no problem "))
        if (right_line!="no"){
          pb_logic <- c(pb_logic,as.numeric(right_line))
        }
      }
    }
    
    # here we manually check for the first and last datetime, not possible to catch with the method above
    pb_first_last <- c()
    print(df[c(1,2),])
    cat(green("cautious : len-1 = ",length(df$datetime)-1," len= ",length(df$datetime)," \n"))
    print(df[c((length(df$datetime)-1),length(df$datetime)),])
    pb_first_last <- (readline("Write here 1 or length(df$datetime) if there is a problem, else write 'nothing'"))
    if (pb_first_last != "nothing"){
      print(pb_first_last)
      pb_first_last <- as.numeric(pb_first_last)
      if (length(pb_first_last>0)){
        pb_logic <- unique(c(pb_logic,pb_first_last))
      }
    }
    pb_logic <- sort(pb_logic)
    
    # we ask if user want to delete some lines
    error_by_user <- as.character(readline("Write here if you want to delete specific lines (yes/no) "))
    if (error_by_user == "yes"){
      idx_error_b <- as.numeric(readline("Write here 1st index to delete"))
      idx_error_e <- as.numeric(readline("Write here last index to delete"))
      pb_user <- c(idx_error_b:idx_error_e)
      print(paste("pb_user ",pb_user))
    }
  }
  
  # we change from 0 to 2 for these datetime error in the quality column
  df$quality[unique(c(pb_NA,pb_logic,pb_same,pb_user))] <- 2
  
  return(list(pb_NA,pb_logic,pb_same,pb_user))
}

### --- ... ADDING LINE TO ERROR FILE ---------------------------

## notes datetime errors in the error file
error_datetime <- function(df,df_error,idx_error_1st_set){
  
  print("ERROR DATETIME")
  
  # We add a title to separate the different types of error
  df_error <- rbind(df_error,c("_______","_______"))
  df_error <- rbind(df_error,c("DATETIME ERROR","_______"))
  df_error <- rbind(df_error,c("_______","_______"))
  
  # we retrieve the the different error index
  idx_error_NA_datetime <- idx_error_1st_set[[1]]
  print(paste("idx_error_NA_datetime ",idx_error_NA_datetime))
  idx_error_logic_datetime <- idx_error_1st_set[[2]]
  print(paste("idx_error_logic_datetime ",idx_error_logic_datetime))
  idx_error_same_datetime <- idx_error_1st_set[[3]]
  print(paste("idx_error_same_datetime ",idx_error_same_datetime))
  idx_error_user_datetime <- idx_error_1st_set[[4]]
  print(paste("idx_error_user_datetime ",idx_error_user_datetime))
  
  # if we have datetime errors,I note on the txt error the lines with the error :
  if (length(c(idx_error_NA_datetime,idx_error_logic_datetime,idx_error_same_datetime,idx_error_user_datetime))!=0){
    
    # NA error
    if (length(idx_error_NA_datetime)!=0){
      print("we have datetime error (NA)")
      for (i in 1:length(idx_error_NA_datetime)){
        m = idx_error_NA_datetime[i]
        df_error <- rbind(df_error,c("datetime error (NA)",m))
      }
    }
    # logic error
    if (length(idx_error_logic_datetime)!=0){
      print("we have datetime error (logic)")
      for (i in 1:length(idx_error_logic_datetime)){
        j = idx_error_logic_datetime[i]
        df_error <- rbind(df_error,c("datetime error (logic)",j))
      }
    }
    # same datetime error
    if (length(idx_error_same_datetime)!=0){
      print("we have datetime error (same)")
      for (i in 1:length(idx_error_same_datetime)){
        k = idx_error_same_datetime[i]
        df_error <- rbind(df_error,c("datetime error (same)",k))
      }
    }
    # NA error
    if (length(idx_error_user_datetime)!=0){
      print("we have datetime error (user)")
      for (i in 1:length(idx_error_user_datetime)){
        l = idx_error_user_datetime[i]
        df_error <- rbind(df_error,c("datetime error (user)",l))
      }
    }

    return(list(df_error,(sort(unique(c(idx_error_NA_datetime,idx_error_logic_datetime,idx_error_same_datetime,idx_error_user_datetime))))))
  }
  
  # if we don't have datetime error, we note it also
  df_error <- rbind(df_error,c("No datetime error",0))
  return(list(df_error,(sort(unique(c(idx_error_NA_datetime,idx_error_logic_datetime,idx_error_same_datetime,idx_error_user_datetime))))))
}

### --- ... AND TAKING CARE OF THESES ERRORS ---------------------------

## take care of datetime problem thanks to the previous functions
check_replace_time <- function(df,choice,idx_error_1st_set){
  
  print("FUNCTION CHECK REPLACE TIME")
  
  # We retrieve the idx errors to delete
  idx_error_NA_datetime <- idx_error_1st_set[[1]]
  print("idx error NA datetime")
  print(idx_error_NA_datetime)
  idx_error_same_datetime <- idx_error_1st_set[[3]]
  print("idx error same datetime")
  print(idx_error_same_datetime)
  idx_error_user_datetime <- idx_error_1st_set[[4]]
  print("idx error user datetime")
  print(idx_error_user_datetime)
  
  # We automatically delete all the NA + same + user errors
  print("error to delete")
  error_to_delete <- unique(c(idx_error_NA_datetime,idx_error_same_datetime,idx_error_user_datetime))
  print(error_to_delete)
  if (length(error_to_delete)>0){
    df <- df[-(error_to_delete),]
  }
  
  if (choice != "0"){
    
    cat(green("here you do it a second time after lines deleted to be sure to have the right index \n"))
    
    # We check logic pb, we do it again after having deleted the NA errors to have the right index
    idx_error_logic_datetime <- find_error_datetime(choice,df)[[2]]
    
    # We show the error index to the user
    print("Final error logic index: ")
    print(idx_error_logic_datetime)
    # and we add 2 to the quality value to show it's a bad one
    df$quality[idx_error_logic_datetime] <- 2
    
    # Finally we see with him if he want to delete / replace / keep the wrong data if there are errors
    if (length(idx_error_logic_datetime)>0){
      keep_or_not <- as.character(readline("Do you want to keep the errors or not? (yes/no) "))
      if (keep_or_not == "no"){
        df <- replace_or_delete_data(df,idx_error_logic_datetime,"datetime","delete")
      }
    }
  }
  
  return(df)
}

### --- IV.2. GREAT CIRCLE DISTANCE ----------------------------------

# https://gis.stackexchange.com/questions/242188/calculating-the-earth-radius-at-latitude 

## calcul of the Earth radius depending of the latitude
radius_earth <- function(lat){
  
  # re radius at equator (UGGI)
  # rp radius at pole (in meter)
  re <- 6378137.0
  rp <- 6356752.314
  
  a <- ((re^2)*cos(lat))^2 + ((rp^2)*sin(lat))^2
  b <- (re*cos(lat))^2 + (rp*sin(lat))^2
  R <- sqrt(a/b)
  
  return(R)
}

## give the orthodromic distance between 2 points
dist_ortho <- function(lon1,lat1,lon2,lat2){
  
  # radius calculated by lat1 taken by default
  R <- radius_earth(lat1) 
  
  # calcul of the orthodromic distance
  a = (sin ( (lat1 - lat2)/2 *pi/180 ) )^2
  b = cos (lat1* pi/180) * cos (lat2* pi/180)
  c = (sin ((lon1- lon2)/2 * pi/180) )^2
  distance = R * 2* asin( sqrt(a + b*c))
  
  return(distance)
}

### --- IV.3. STEP SPEED (M/S)... ----------------------------------------

## References: 
# https://statisticsglobe.com/difftime-r-function/

## give the speed between 2 points in different times and positions
step_speed_column <- function(df){
  
  print("STEP SPEED FUNCTION")
  
  # for 1st column, it is an NA because it's a difference between 2 points and there is nothing before 1
  df$step_speed[1] = NA
  df$diff_time[1] = NA

  for (i in 2:length(df$datetime)){
    # distance
    distance_bird = dist_ortho(df$lon[i],df$lat[i],df$lon[i-1],df$lat[i-1])
    # time
    df$diff_time[i] = as.numeric(difftime(df$datetime[i], df$datetime[i-1], units = "secs"))
    # speed = distance / time
    df$step_speed[i] = distance_bird / abs(df$diff_time[i])
  }
  return(df)
}

### --- ... AND CHECKING LON LAT ABBERATIONS --------------------------

## We check any longitude latitude aberrant point
check_replace_lat_lon <- function(df,choice){
  
  print("CHECK REPLACE LAT LON")
  
  # I create empty vector for the 3 types of errors in longitude and latitude
  # NA errors
  idx_NA_lon_lat <- c()
  # errors seen by the user
  idx_error_lon_lat <- c()
  # automatic errors spotted by step_speed 
  idx_error_speed <- c()
  
  # I detect the NA error
  idx_NA_lon_lat_before <- sort(unique(c(which(is.na(df$lat)==TRUE),which(is.na(df$lon)==TRUE))))
  
  # I detect the speed error (only if automatic choice)
  # I consider if it's manual, you choose to delete or not it with the following code
  # (cautious, you have 2 lines spotted in the dataframe, 2 lines permitting to have the step speed aberration)
  # in the example b is the aberrant longitude / latitude, line 2 and 3 will be spotted
  # a -> step_speed = NA (line 1)
  # b -> step_speed depend of b/a (line 2)
  # c -> step_speed depend of c/b (line 3)
  # possible to have only 1 line spotted if they are closed to 50
  if (choice ==0){
    idx_error_speed <- which(df$step_speed>50)
    # quality of 3 for speed errors kept to show the errors in automatic choice
    df$quality[idx_error_speed] <- 3
  }
  
  # CHOICE MANUAL
  # If the choice is not automatic, I will propose the user to find more error by plotting
  if (choice != 0){
    
    # we first show the trajectory and the user will decide if he sees any errors
    plot(df$lon,df$lat)
    go_next <- as.character(readline("Here the lat/lon plot, write something to see the plot -> good way to see where the error comes from "))
    check_error_lat_lon_or_no <- as.character(readline("Do you have errors in lat lon? (yes/no) "))
    
    # if he sees errors :
    if (check_error_lat_lon_or_no=="yes"){
      
      # errors seen by latitude
      plot(df$lat)
      min_lat <- as.numeric(readline("Here the latitude you have, which minimum do you want to put? "))
      max_lat <- as.numeric(readline("Which maximum do you want to put? "))
      idx_error_lat <- c(which(ifelse(df$lat<min_lat,1,0)==1),which(ifelse(df$lat>max_lat,1,0)==1))
      cat(green("Here the index with a lat problem seen by you \n"))
      print(idx_error_lat)
      
      # errors seen by longitude
      plot(df$lon)
      min_lon <- as.numeric(readline("Here the longitude you have, which minimum do you want to put? "))
      max_lon <- as.numeric(readline("Which maximum do you want to put? "))
      #idx_error_lon <- which((ifelse((df$lon<min_lon)||(df$lon>max_lon),1,0))==1)
      idx_error_lon <- c(which(ifelse(df$lon<min_lon,1,0)==1),which(ifelse(df$lon>max_lon,1,0)==1))
      cat(green("Here the index with a lon problem seen by you \n"))
      print(idx_error_lon)
      
      # errors seen by step_speed
      idx_error_lon_lat_s <- c()
      plot(df$step_speed)
      limit_speed <- as.numeric(readline("Here the step_speed you have, which limit do you want to put? "))
      idx_error_lon_lat_s_first <- which((ifelse(df$step_speed>limit_speed,1,0))==1)
      print(paste("Here the index with a step speed error seen by you ",idx_error_lon_lat_s_first))
      # we recall the example
      # in the example b is the aberrant longitude / latitude, line 2 and 3 will be spotted
      # a -> step_speed = NA (line 1)
      # b -> step_speed depend of b/a (line 2)
      # c -> step_speed depend of c/b (line 3)
      # The idea is that the 1st line spotted is the line with a latitude longitude error. That's why we take only the 1st of each group
      # if you have idx_error_lon_lat_s <- c(3,4,8,9), you have idx_error_lon_lat_s <- c(3,8)
      # if you have idx_error_lon_lat_s <- c(3,4,6,8,9), you have idx_error_lon_lat_s <- c(3,6,8)
      # However it's better to use the longitude and latitude limits instead of speed limit
      
      i <- 1
      # while we have errors in the index:
      while (i <= length(idx_error_lon_lat_s_first)){
        
        print(paste("------ i :",i," ------"))
        
        # we study 2 by 2 : 
        k1 = idx_error_lon_lat_s_first[i]
        k2 = idx_error_lon_lat_s_first[i+1]
        print(paste("k1 ",k1))
        print(paste("k2 ",k2))
        
        # by default the index error is the 1st one
        idx_error_lon_lat_s <- c(idx_error_lon_lat_s,k1)
        
        #if the 2 index are closed, it means they indicate the same error, so I keep just the 1st
        if (k2-k1==1){
          print("k2==k1")
          i <- i+2
        }else{
          print("k2!=k1")
          i <- i+1
        }
      }
      cat(green("Here the index with a step speed problem seen by you \n"))
      print(idx_error_lon_lat_s)
      
      # We combine the error vectors by avoiding same index values
      idx_error_lon_lat <- sort(unique(c(idx_error_lat,idx_error_lon,idx_error_lon_lat_s)))
      
      # We check if the user is ok with the error index
      cat(green("Final error index: \n"))
      print(idx_error_lon_lat)
      like_or_not <- as.character(readline("Here the error index you have, do you agree with it? (yes/no) "))
      if (like_or_not == "no"){
        check_replace_lat_lon(df,choice)
      }
      
      # We note that there are errors in these index
      df$quality[idx_error_lon_lat] <- 1
      
      # Finally we see with him if he want to delete / replace / keep the wrong data
      keep_or_not <- as.character(readline("Do you want to keep the errors or not? (yes/no) "))
      if (keep_or_not == "no"){
        df <- replace_or_delete_data(df,idx_error_lon_lat,"lon_lat","delete")
      }
    }
    
  }
  
  # I detect again the NA error (idx different because they were posibly just deleted before)
  idx_NA_lon_lat_after <- sort(unique(c(which(is.na(df$lat)==TRUE),which(is.na(df$lon)==TRUE))))
  print(paste("index error NA lon lat ",idx_NA_lon_lat))
  # Then I delete the NA
  if (length(idx_NA_lon_lat_after>0)){
    df <- df[-(idx_NA_lon_lat_after),]
  }
  
  return(list(df,idx_NA_lon_lat_before,idx_error_lon_lat,idx_error_speed))
}

### --- ... ADDING LINES TO ERROR FILE... -----------------------------

## notes longitude latitude errors in the error file
error_lon_lat <- function(idx_NA,idx_error,df_error,idx_speed,choice,error_before){
  
  print("ERROR LON LAT/SPEED")
  
  # We add a title to separate the different types of error
  df_error <- rbind(df_error,c("_______","_______"))
  df_error <- rbind(df_error,c("ERROR LON LAT/SPEED","_______"))
  df_error <- rbind(df_error,c("_______","_______"))
  
  # we retrieve the the different error index
  idx_NA <- real_line_error(list(error_before,idx_NA))
  print(paste("idx_error_NA_lon_lat real line ",idx_NA))
  idx_error <- real_line_error(list(error_before,idx_error))
  print(paste("idx_error_lon_lat real line ",idx_error))
  idx_speed <- real_line_error(list(error_before,idx_speed))
  print(paste("idx_speed real line ",idx_speed))
  
  # NA errors
  if (length(idx_NA)!=0){
    for (i in 1:length(idx_NA)){
      print("we have NA lon lat error")
      k = idx_NA[i]
      df_error <- rbind(df_error,c("NA lon lat error",k))
    }
  }else{
    df_error <- rbind(df_error,c("No NA lon lat error",0))
  }
  
  # errors seen by the user
  if (length(idx_error)!=0){
    for (i in 1:length(idx_error)){
      print("we have NA lon lat error")
      j = idx_error[i]
      df_error <- rbind(df_error,c("lon lat/speed error",j))
    }
  }else{
    df_error <- rbind(df_error,c("No lon lat/speed error found",0))
  }
  
  return(list(df_error,(sort(unique(c(error_before,idx_NA,idx_error))))))
}

### --- ... AND THE ONES KEPT -----------------------------

# notes errors kept in the error file
error_kept <- function(df,df_error,choice,index_raw_error_after_datetime_lon_lat){
  
  print("ERROR KEPT")
  
  # We add a title to separate the different types of error
  df_error <- rbind(df_error,c("_______","_______"))
  df_error <- rbind(df_error,c("ERROR KEPT","_______"))
  df_error <- rbind(df_error,c("_______","_______"))
  
  # CATEGORY 1 : datetime (2) and longitude latitude (1) errors recorded
  # if manual choice we have datetime and lon lat errors kept
  if (choice != 0){
    
    print("no automatic")
    
    # SUBCATEGORY 1 : datetime error kept
    # we find datetime errors kept by quality columns values of 2
    index_error_datetime_this_df <- which(df$quality==2)
    print(index_error_datetime_this_df)
    
    if (length(index_error_datetime_this_df)!=0){
      
      index_error_datetime <- real_line_error(list(index_raw_error_after_datetime_lon_lat,index_error_datetime_this_df))
      
      for (i in 1:length(index_error_datetime)){
        print("we have datetime error to save")
        k = index_error_datetime[i]
        if (df$quality[k]==2){
          df_error <- rbind(df_error,c("datetime error kept",k))
        }
      }
    }else{
      print("we don't have datetime error to save")
      df_error <- rbind(df_error,c("no datetime error kept",0))
    }
    
    # SUBCATEGORY 2 : longitude latitude error kept
    # we find lon lat errors kept by quality columns values of 1
    index_error_lon_lat_this_df <- which(df$quality==1)
    print(index_error_lon_lat_this_df)
    
    if (length(index_error_lon_lat_this_df)!=0){
      
      index_error_lon_lat <- real_line_error(list(index_raw_error_after_datetime_lon_lat,index_error_lon_lat_this_df))
      
      for (i in 1:length(index_error_lon_lat)){
        print("we have datetime error to save")
        k = index_error_lon_lat[i]
        if (df$quality[k]==1){
          df_error <- rbind(df_error,c("lon lat error",k))
        }
      }
    }else{
      print("we don't have lon lat error to save")
      df_error <- rbind(df_error,c("no lon lat error kept",0))
    }
    

  }else{
    # automatic format so these errors are recorded
    
    print("automatic")
    df_error <- rbind(df_error,c("no datetime/lon lat error kept because automatic choice",0))
  }
  
  # CATEGORY 2 : speed error kept (3)
  
  index_error_speed_this_df <- which(df$quality==3)
  print(paste("index_error_speed_this_df ",index_error_speed_this_df))
  
  if (length(index_error_speed_this_df)!=0){
    
    print("length(index_error_speed_this_df)!=0")
    
    index_error_speed <- real_line_error(list(index_raw_error_after_datetime_lon_lat,index_error_speed_this_df))
    print(paste("index_error_speed ",index_error_speed))
    
    for (i in 1:length(index_error_speed)){
      print("we have speed error to save")
      k = index_error_speed[i]
      df_error <- rbind(df_error,c("speed error",k))
    }
    
  }else{
    print("we don't have speed error to save")
    df_error <- rbind(df_error,c("no speed error kept",0))
  }
  
  return(df_error)
}

### --- IV.4. STEP DIRECTION ----------------------------------------

## give the cap (direction) between 2 points
cap <- function(lon1, lat1, lon2, lat2){
  
  print("CAP FUNCTION")
  
  # convert the latititude / longitude to radians
  lat1 = lat1*pi/180
  lat2 = lat2*pi/180
  lon1 = lon1*pi/180
  lon2 = lon2*pi/180
  
  delta_lon = lon2-lon1
  
  a = cos(lat1) * sin(lat2) - sin(lat1)*cos(lat2)*cos(delta_lon)
  b = sin(delta_lon) * cos(lat2)
  
  cap = atan2(b,a)
  # convert to modulo (%%) to have between -180 and 180
  cap = cap%%(2*pi) 
  
  return (cap*180/pi)
}

## give the difference of caps
step_direction_bird <- function(df){
  
  print("DIRECTION BIRD FUNCTION")
  
  c = cap( df$lon[1:(length(df$lon)-1)], df$lat[1:(length(df$lon)-1)],
           df$lon[2:length(df$lon)], df$lat[2:length(df$lon)] )
  # difference part by part of the vector, % modulo (between -180 and 180°)
  d = ifelse(diff(c)%%360>180,diff(c)%%360-360,diff(c)%%360)  
  df$step_direction <- c(NA,NA,d)
  
  hist(df$step_direction)
  
  return(df)
}

### ----IV.4. GAPS ----------------------------------------

## permit to quantify when a time difference is normal comparing to the others or unusual
gaps_column <- function(df){
  
  print("GAPS FUNCTION")
  
  # Calcul of the resolution by the median of all the time differences:
  time_difference <- c()
  for (i in 2:length(df$datetime)){
    time_difference <- c(time_difference,difftime(df$datetime[i], df$datetime[i-1], units = "secs"))
  }
  resolution <- median(time_difference)
  print(paste("resolution ",resolution))
  
  # Calcul of the gaps for each time difference:
  gaps <- (time_difference-resolution)/resolution
  df$gaps <- c(NA,gaps)
  
  plot(df$gaps)
  
  return(df)
}

### ----IV.5. DISTANCE COLONY (IN M)  ---------------------------------

## permit to obtain the position of the colony automatically, manually or by choosing in a list
position_colony <- function(df,param){
  
  print("POSITION COLONY FUNCTION")
  
  # all turns around the param[15]
  # choice 1 : param[15] = automatic
  # choice 2 : param[15] = manually
  # chocie 3 : param[15] = the number of the colony
  
  # CATEGORY 1 : we don't have a colony position so automatic
  if (param[15] == "automatic"){
    
    print("automatic position")
    
    colony_latitude <- c()
    colony_longitude <- c()
    # for all the points ...
    for (i in 2:length(df$step_speed)){
      # ... where the bird as a step_speed < 1 (so not moving) ...
      if(as.numeric(df$step_speed[i])<1){
        # ... we obtain the vector with all the positions
        colony_latitude <- c(colony_latitude,as.numeric(df$lat[i]))
        colony_longitude <- c(colony_longitude,as.numeric(df$lon[i]))
      }
    }
    # we then calculate the median as the position of the colony
    position <- c(median(colony_longitude),median(colony_latitude))
    print(paste("position",position))
    
    return(position)
  }
  
  # CATEGORY 2 : we know the colony position but it's not in the list so it's done manually
  if (param[15] == "manually"){
    
    print("manual position")
    
    # we ask the coordinates to the user
    colony_longitude <- as.character(readline("Where is your colony (in longitude) "))
    colony_latitude <- as.character(readline("Where is your colony (in latitude) "))
    
    # and then save it
    position <- c(colony_longitude,colony_latitude)
    print(paste("position",position))
    
    return(position)
    
  }
  
  #CATEGORY 3 : we know the colony position and it's in the dataframe 
  
  print("chosen position")
  
  # we read the metadata with colony position
  df_colony <- read.csv("metadata_colony_format.csv",header=TRUE,sep=";",dec=',')
  
  print(paste("param 15 ",param[15]))
  lon_colony <- as.numeric(df_colony$lon[as.numeric(param[15])])
  print(paste("lon colony",lon_colony))
  lat_colony <- as.numeric(df_colony$lat[as.numeric(param[15])])
  print(paste("lat colony",lat_colony))

  position <- c(lon_colony,lat_colony)

  print(paste("position",position))
  
  return(position)
  
}

## permit to obtain the distance between the bird and the colony
distance_colony <- function(df,param){
  
  print("DISTANCE COLONY FUNCTION")
  
  # we retrieve the colony position
  colony <- position_colony(df,param)
  # then we calculate the distance between the colony and the position of the bird
  for (i in 1:length(df$lon)){
    df$dist_colony[i] <- dist_ortho(colony[1],colony[2],df$lon[i],df$lat[i])
  }
  
  # we add a plot to visualize
  plot(df$dist_colony)
  
  return(df)
}

### ----IV.6. TRIP ----------------------------------------

## permit to detect when a bird is on a trip
detect_trip <- function(choice,df){
  
  print("DETECT TRIP FUNCTION")
  
  # we put automatic value ...
  threshold_dist = 500
  threshold_speed = 5
  # ... that can be changed manually if needed
  if (choice != 0){
    threshold_dist <- as.numeric(readline("In which minimum distance do you consider the bird is not anymore in the colony? "))
    threshold_speed <- as.numeric(readline("In which minimum speed do you consider the bird has finished its trip? "))
  }
  
  # SPECIAL CASE : NO TRIP
  if (length(which(df$dist_colony>500))==0){
    df$trip <- 0
    return(df)
  }
  
  # We find a try of beginning (>500) and end (<500) by threshold distance
  distance_colony <- df$dist_colony
  
  # IDX BEGIN by threshold distance
  idx_trip_start = which(diff(1*(distance_colony > threshold_dist))==1)+1
  print(paste("idx start first ",idx_trip_start))
  # exception : if first value distance > 500, 1 is the first index (not spotted by the code above)
  if (df$dist_colony[1]>threshold_dist){
    print("dist_colony[1]>threshold_dist")
    idx_trip_start <- unique(sort((c(1,idx_trip_start))))
    print("new idx trip start")
    print(idx_trip_start)
  }

  # IDX END by threshold distance
  idx_trip_end = which(diff(1*(distance_colony > threshold_dist))==-1)
  print(paste("idx end first ",idx_trip_end))
  # exception 1 : if no value found for the idx end, it's the last one
  if (length(idx_trip_end)==0){
    print("length(idx_trip_end)==0")
    idx_trip_end = length(df$dist_colony)
  }
  # exception 2 : if there is not enough idx trip founded, it's also the last one
  if (length(idx_trip_end)<length(idx_trip_start)){
    print("length(idx_trip_end)<length(idx_trip_start)")
    idx_trip_end <- c(idx_trip_end,length(df$dist_colony))
  }
  
  print(paste("idx start ",idx_trip_start,"idx end",idx_trip_end))
  
  # we find the number of trip we will have
  nb_trip = length(idx_trip_start)
  # and create a trip vector with only 0
  trip <- vector(mode="integer",length=length(df$dist_colony))
  # nb_sub is the number we will subtract if 2 trips are merged in 1 trip
  # example : trip 3 = trip 2 -> trip 3 will have 2 in value (nb_sub=1)
  #           then all the following trip have their value - 1 (nb_sub=1) and it can be additive
  nb_sub=0 
  
  for (i in 1:nb_trip){
    
    print(paste("NUM TRIP ",i,"-------"))
    print("K START")
    
    # STEP 1: WE LOOK FOR KSTART
    kstart = as.integer(idx_trip_start[i])
    print(paste("kstart ",kstart))
    speed = df$step_speed[kstart]
    print(paste("1st speed ",speed))
    # if kstart = 1, the first speed is NA so we can't use the while, anyways if it's 1, we know the beginning 
    if (kstart != 1){
      print("we use speed to find kstart")
      while((speed > threshold_speed)&&(kstart!=1)){ # we find the beginning of the trip by speed
        kstart <- kstart - 1
        speed <- df$step_speed[kstart]
      }
    }else{
      kstart <- 1
    }
    print(paste("nearly final kstart ",kstart))
    
    
    print("K END")
    
    # STEP 2: WE LOOK FOR KEND
    kend = as.integer(idx_trip_end[i])
    print(paste("kend ",kend))
    # if kend is not the last k:
    if ((kend < length(df$dist_colony))&&(kend!=1)){ # we are looking for the end by speed
      speed = df$step_speed[kend]
      print("we use speed to find kend")
      while ((speed > threshold_speed)&&(kend!=length(df$dist_colony))){
        kend <- kend + 1
        #print(paste("kend while ",kend))
        speed <- df$step_speed[kend]
        #print(paste("speed while ",speed))
      }
    }else{
      kend <- length(df$dist_colony)
    }
    print(paste("nearly final kend ",kend))
    
    # STEP 3: we look for if we can merge 2 trips in 1 with the nb_sub value
    # if the last trip finish in the next trip
    # if the end of the last trip is the beginning of the new trip
    if (i!=1){
      print(paste("kend_before ",kend_before))
      print(paste("kstart_before ",kstart_before))
    }
    if ((i!=1)&& ((kend_before>=kstart) || ((kstart-kend_before==1)) )){
      print("kstart<kend")
      nb_sub <- nb_sub + 1
      print(paste("nb_sub ",nb_sub))
    }
    print(paste("nb_sub ",nb_sub))
    
    # Different delimitations depending of kend and kstart
    # if we have a normal trip
    if ((kend-kstart>=3) && (kstart!=1) && (kend!=length(df$dist_colony)) ){
      print("Normal ones")
      kstart_before <- kstart+1
      kend_before <- kend-1
      trip[(kstart+1):(kend-1)] = i - nb_sub
    }else{
      # if the trip is really tiny (length <3)
      print("Special ones")
      kstart_before <- kstart
      kend_before <- kend
      trip[kstart:kend] = i - nb_sub
    }
  }
  
  # we fill the column with the final vector
  df$trip <- trip
  # and show a plot for the user
  plot(df$trip)
  
  return(df)
}

### ---------- V. HAVING GRAPHS TO HAVE A GENERAL CHECK -------------
### ----V.1. WHEN IS NIGHT -------------------------------------------

## References : 
# https://github.com/SWotherspoon/SGAT
# https://gallery.htmlwidgets.org/
# https://cran.r-project.org/web/packages/suncalc/suncalc.pdf
# https://upload.wikimedia.org/wikipedia/commons/8/88/World_Time_Zones_Map.png (Check time zones with map)

## indicates when it's night
when_is_night <- function(df){
  
  print("WHEN_IS_NIGHT FUNCTION")
  
  print(paste("date ",df$date[2]))
  print(paste("latitude ",df$lat[2]))
  print(paste("longitude ",df$lon[2]))
  
  # we get the time zone
  time_zone <- tz_lookup_coords(df$lat[2], df$lon[2], method = "accurate", warn = TRUE)
  print(paste("time_zone ",time_zone))
  
  # dataframe showing the sun light time
  # "night" : night starts (dark enough for astronomical observations)
  # "nightEnd" : night ends (morning astronomical twilight starts)
  sun_all <- getSunlightTimes(date=df$date[2],lat=df$lat[2],lon=df$lon[2],keep = c( "night","nightEnd"), tz = time_zone)
  print(sun_all)
  
  # end of night
  end_night_calc = format(as.POSIXct(sun_all$nightEnd[1]), format = "%H:%M:%S")
  # beginning of night
  begin_night_calc = format(as.POSIXct(sun_all$night[1]), format = "%H:%M:%S")
  
  return(c(end_night_calc,begin_night_calc))
}

### ----V.2. GRAPH 1: LON LAT ----------------------------------

## References:
# http://www.sthda.com/french/wiki/ggplot2-types-de-points-logiciel-r-et-visualisation-de-donnees
# https://stackoverflow.com/questions/22915337/if-else-condition-in-ggplot-to-add-an-extra-layer
# https://stackoverflow.com/questions/45505388/add-condition-geom-point-ggplot2-r

## plot a trajectory graph 
graph1 <- function(df,title_ID,param){
  
  print("GRAPH 1")
  
  # Colony position
  pos <- position_colony(df,param)
  
  # CASE 1 : there are trips
  
  if (length(which(df$dist_colony>500))!=0){
    
    print("there is a trip")
    
    # I chose only the dataset when bird is on trip named df_trip
    df_trip = df[which(df$trip!=0),]
    idx_trip <- seq(from=df_trip$trip[1],to=length(df_trip$trip))
    
    g1 <- ggplot(data = df_trip, aes(lon, lat)) +
      
      # the points are colored by the number of the index, following a gradient
      geom_point(aes(col=idx_trip),shape = 16)+
      # if the gap>1, the point is surrounded by a black circle
      geom_point(data = df[unique(which(df$gaps>1),which(df$trip!=0)),], aes(lon,lat), shape=1,colour = "black")+ 
      # then the colony is showed by a big cross (+)
      geom_point(aes(x=pos[1],y=pos[2]),colour="brown",size=10,shape=3)+
      # here the gradient
      scale_color_gradient(low = "yellow", high = "red")+
      
      # and then the details of presentation for the graph
      xlab("Longitude") +
      ylab("Latitude") + 
      labs(title=paste(title_ID,": trajectory"))+
      scale_fill_identity()+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(hjust = 0.5,size=9))
    
    return(g1)
    
  }else{
    
    # CASE 2 : no trip
    
    print("no trip")
    
    # we take all the lines as there is no trip (exception)
    idx_no_trip <- seq(from=df$trip[1],to=length(df$trip))[-1]
    
    g1 <- ggplot(data = df, aes(lon, lat)) +

      # the points are colored by the number of the index, following a gradient
      geom_point(aes(col=idx_no_trip),shape = 16)+
      # if the gap>1, the point is surrounded by a black circle
      geom_point(data = df[unique(which(df$gaps>1),which(df$trip!=0)),], aes(lon,lat), shape=1,colour = "black")+ 
      # then the colony is showed by a big cross (+)
      geom_point(aes(x=pos[1],y=pos[2]),colour="brown",size=10,shape=3)+
      # here the gradient
      scale_color_gradient(low = "yellow", high = "red")+
      
      # and then the details of presentation for the graph
      xlab("Longitude") +
      ylab("Latitude") + 
      labs(title=paste(title_ID,": trajectory"))+
      scale_fill_identity()+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(hjust = 0.5,size=9))
    
    return(g1)
  }
}

### ----V.3. GRAPH 2: DIST_COL BY TIME/LIGHT--------------------

## References :
# https://www.faqcode4u.com/faq/46141/add-legend-to-geom-vline

graph2 <- function(df,title_ID){
  
  print("GRAPH 2")
  
  # NIGHT / DAY STUFF : we create a new column df$night which indicate if the point is in day or night, creating a background different
  
  # We retrieve the hours of the beginning of day and night
  time_day_night <- when_is_night(df)
  begin_day <- time_day_night[1]
  print(paste("here the beginning of the day ",begin_day))
  begin_night <- time_day_night[2]
  print(paste("here the beginning of the night ",begin_night ))
  
  # Then we create a column df$night = "gray90" (day) or "gray60" (night)
  df$night <- 0
  # category 1 : begin_day and begin_night in the same day (like 6:00am to 10:00pm)
  if (begin_day<begin_night){
    df$night =ifelse(df$time >= begin_day & df$time <= begin_night, "gray90", "gray60")
  }else{
    # category 2 :begin_day and begin_night are not in the same day (like 6:00am to 00:30am)
    df$night =ifelse(df$time >= begin_day | df$time <= begin_night, "gray90", "gray60")
  }
  print("night day stuff ok")
  
  
  
  # LINE FOR TRIP : we take care of x lines of different colors showing beginning an end of trips
  # Orange is the beginning, red the end, and yellow indicates a one line trip
  
  # 3 index for 3 situations
  idx_start <- c() # index for 1st line of a trip
  idx_end <- c() # index for last line of a trip
  idx_middle <- c() # index if you have only 1 line for the trip
  
  # if we don't have only 1 trip :
  if (length(unique(df$trip)) > 1){
    # for each trip (we don't count 0) :
    for (i in 1:(length(unique(df$trip))-1) ){ 
      
      print(paste("length(unique(df$trip))-1 ",length(unique(df$trip))-1))
      # we have a vector of all the index of the trip i
      vec <- which(df$trip==i)
      # idx_start is the first index
      idx_start <- c(idx_start,vec[1])
      # idx_end is the last one
      idx_end <- c(idx_end,vec[length(vec)])
      print(paste("i ",i))
      print(paste("idx_start ",idx_start))
      print(paste("idx_end ",idx_end))
      
      # if we have a one line trip :
      if (idx_start[i] == idx_end[i]){
        print("we have a middle idx")
        idx_middle <- c(idx_middle,idx_start[i])
        print(paste("idx middle ",idx_middle))
      }
    }
  }else{
    # if we have 1 trip
    idx_start <- 1
    idx_end <- length(df$trip)
  }
  
  print(paste("idx start final ",idx_start))
  print(paste("idx end final ",idx_end))

  # we retrieve the datetime paired with the index found before
  datetime_start <- df$datetime[idx_start]
  datetime_end <- df$datetime[idx_end]
  datetime_middle <- df$datetime[idx_middle]
  # color for the index 
  colors <- c("trip start" = "orange", "trip end" = "red", "1 day trip" = "yellow")

  print("trip stuff ok")
  
  # DENSITY OF DATETIME ON X AXIS
  # Based on a good visual example of this graph (datetime on x axis every 5 hours, for a lenght of 6525 lines)
  # x_hour = (5/6525)*length(df$datetime)
  
  # GRAPH
  
  # CASE 1 : there are trips
  
  if (length(which(df$dist_colony>500))!=0){
    
    print("there are trips")
    g2 <- ggplot(df, aes(datetime, dist_colony)) +
      
      # we put a background indicating day or night
      geom_rect(aes(xmin = datetime, xmax = lead(datetime), ymin = -Inf, ymax = Inf,
                    fill = night))+
      
      # then add the points with green colors depending of the speed
      geom_point(aes(col=step_speed),shape=16)+
      # if the gap>1, the point is surrounded by a black circle
      geom_point(data = df[which(df$gaps>1),], aes(datetime, dist_colony), shape=1,colour = "black")+ 
      # gradient of the points is in green
      scale_color_gradient(low = "springgreen", high = "springgreen4")+
      
      # then we add the x lines indicating the trips
      geom_vline(xintercept = datetime_start,col="orange")+
      geom_vline(xintercept = datetime_end,col="red")+
      geom_vline(xintercept = datetime_middle,col="yellow")+
      
      scale_x_datetime(date_breaks = "4 hours",date_labels = "%Y/%m/%d %H:%M")+ # (every hour)
      #scale_x_datetime(date_breaks = paste(x_hour,"hours"), date_labels = "%Y/%m/%d %H:%M")+
      
      # then details for presentation
      xlab("Datetime") +
      ylab("Bird distance colony (in m)") +
      labs(title=paste(title_ID,": Distance to the colony"))+
      scale_fill_identity()+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(hjust = 0.5,size=9))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(g2)
    
  }else{
    
    # CASE 2 : there is no trip
    
    print("there is no trip")
    
    g2 <- ggplot(df, aes(datetime, dist_colony)) +
      
      # we put a background indicating day or night
      geom_rect(aes(xmin = datetime, xmax = lead(datetime), ymin = -Inf, ymax = Inf,
                    fill = night))+
      
      # then add the points with green colors depending of the speed
      geom_point(aes(col=step_speed),shape=16)+
      # if the gap>1, the point is surrounded by a black circle
      geom_point(data = df[which(df$gaps>1),], aes(datetime, dist_colony), shape=1,colour = "black")+ 
      # gradient of the points is in green
      scale_color_gradient(low = "springgreen", high = "springgreen4")+
      
      scale_x_datetime(date_breaks = "4 hours", date_labels = "%Y/%m/%d %H:%M")+
      
      # then details for presentation
      xlab("Datetime") +
      ylab("Bird distance colony (in m)") +
      labs(title=paste(title_ID,": Distance to the colony (no trip)"))+
      scale_fill_identity()+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(hjust = 0.5,size=9))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(g2)
  }
  
  
}

### ----V.4. GRAPH 3: BIRD DIRECTION / SPEED --------------------------

## plots a graph showing step speed depending on step direction
graph3 <- function(df,title_ID){
  
  print("GRAPH 3")
  
  g3 <- ggplot(df, aes(step_direction, step_speed)) +
    
    # we plot the points
    geom_point(col="springgreen4",shape=16)+
    # the ones with a gap>1 are surrounded by a black circle
    geom_point(data = df[which(df$gaps>1),], aes(step_direction, step_speed), shape=1,colour = "red")+ 
    
    # then there are details for presentation
    xlab("Step direction of the bird (in °)") +
    ylab("Step speed of the bird (in m/s)") + 
    labs(title=paste(title_ID,": speed depending of direction"))+
    scale_fill_identity()+
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    theme(plot.title = element_text(hjust = 0.5,size=9))
  
  return(g3)
}

### ----V.5. GRAPH 4: DIFF TIME --------------------------

## plots a graph showing a difference of time histogramm
graph4 <- function(df,title_ID){
  
  print("GRAPH 4G")
  
  # new column for transforming gaps into factor
  # "sup" is if gap>1, "inf" is if gap<1
  gaps_factor <- as.factor(ifelse(df$gaps>1,"sup","inf"))
  df$gaps_factor <- gaps_factor
  
  # calcul of the resolution:
  resolution <- median(df$diff_time[2:length(df$diff_time)])
  
  # calcul of the mean
  mu <- ddply(df, "gaps_factor", summarise, grp.mean=mean(diff_time))
  
  g4 <- ggplot(df, aes(x=diff_time,color=gaps_factor)) + 
    
    # the histogramm is filled with white, and colored by the gap factor
    geom_histogram(fill="white")+
    # the resolution (median) is a x line in black
    geom_vline(xintercept = resolution,col="black")+
    # there are 2 more dotted lines, showing the mean for each factor
    geom_vline(data=mu, aes(xintercept=grp.mean, color=gaps_factor),
               linetype="dashed")+
    
    # then there are details for presentation
    xlab("Time Difference") +
    ylab("Count") + 
    labs(title=paste(title_ID,": time difference"))+
    scale_fill_identity()+
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    theme(plot.title = element_text(hjust = 0.5,size=9))
  
  return(g4)
}

### ---------- VI. ID_BIRD -----------------------------------------------

## create 2 news columns for the unique ID of bird trajectory
ID_bird <- function(df,name){
  
  print("ID BIRD")
  
  # we add the 2 last columns : 
  # IDbird (the unique name of the bird trajectory)
  # trip_ID (IDbird with number of trip)
  df$IDbird <- as.character(name)
  df$trip_ID <- paste(as.character(name),df$trip,sep="_")
  
  return(df)
}

### ---------- VII. MAIN FUNCTION -------------------------------------

format_data_in_a_folder <- function(choice,the_path,to_separate_or_no){
  
  # ----------- IN A SPECIFIC FOLDER -----------
  
  # We take the length of all the file in this folder
  len_file <- length(list.files(the_path))
  print(paste("you have ",len_file," csv/txt in your folder"))
  
  # we set same parameters for all the file in a same folder
  param_chosen <- set_parameter()
  
  # we check if it's not an empty file
  if (len_file==0){
    return("error, no file in this folder")
  }
  
  # then we format all the data in the file : 
  for (i in 1:len_file){
    
    # ------------- IN A SPECIFIC FILE -------
    
    # if it's not the 1st file of the folder, we add 3 because of the new csv, png and txt created thanks to the programm
    if (i!=1){
      k = (i-1)*4+1 #k1 = 1 ; k2 = 5 = (2-1)*4+1 ; k3 = 9 = (3-1)*4+1 ; ...
    }else{
      k=1
    }
    
    print(paste("k ",k))
    print(paste("i ",i))
    cat(green("the name of your file ",i," is ",list.files(the_path)[k]," \n"))
    
    # we find the specific path, so the name and the path, a mix between :
    # - "FDN_2022_..._HLOC.csv" for example
    # - "D:/OneDrive/Stage.../test/dossier1" for example
    specific_path <- as.character(list.files(the_path,full.names = TRUE)[k])
    print(paste("specific path ",specific_path))
    # we delete the .csv or .txt part for just after (path_save = the entire path without the last 4 letters (.csv or .txt))
    path_save <- substr(specific_path,1,nchar(specific_path)-4)
    # we take only the ID or title of the csv (by removing .csv/.txt)
    name_data <- as.character(list.files(the_path)[k])
    name_save <- substr(name_data,1,nchar(name_data)-4)
    print(paste("name save ",name_save))

    # OPEN FILE
    data <- open_file(specific_path,param_chosen)
    print("df opened")
    print(data[1,])
    
    # SPLIT BY ID AND SAVE (OR NO)
    if (to_separate_or_no=="separate"){
      split_by_ID_and_save(data,path_save)
      return(data)
    }

    # ERROR FILE for this file
    error <- data.frame(name=c(name_save),line_error=c(0))
    error <- rbind(error,c("_______","_______"))

    # RENAME AND ORDER FILE
    data <- rename_and_order_file(data,param_chosen,to_separate_or_no)

    # FORMAT DATE
    data <- format_date(data,param_chosen)
    print(paste("date ",data$date[1]))

    # CHECK TIME
    idx_midnight <- time_after_midnight(data,"index") 
    data <- time_after_midnight(data,"change_df")
    
    # WRITE ERROR AFTER MIDNIGHT
    error <- error_after_midnight(error,idx_midnight)
    
    # FORMAT TIME
    data <- format_time(data)
    print(paste("time ",data$time[1]))
    
    # DATETIME
    data <- new_datetime_columns(data,param_chosen)
    print(data$datetime[1])
    
    # CHECK DATETIME
    
    data$quality <- 0
    data$step_speed_corrected <- c(0)
    # General visualization of datetime for manual choice
    if (choice!="0"){
      cat(green("here a general view of your datetime variable","\n"))
      plot(data$datetime)
      go_next <- as.character(readline("Write something to see the date plot -> good way to see where the error comes from "))
      plot(data$date)
    }
    # we find the datetime errors
    idx_error_datetime <- find_error_datetime(choice,data)
    
    # function to save the different errors in the error df + save the error index:
    error_d <- error_datetime(data,error,idx_error_datetime)
    #     the errors are written
    error <- error_d[[1]]
    #     the error index is saved
    idx_raw_previous_error2 <- error_d[[2]]
    
    # and we take care of the errors in the dataframe
    data <- check_replace_time(data,choice,idx_error_datetime)
    
    # STEP SPEED
    data <- step_speed_column(data)

    
    # CHECK LON LAT
    
    # we obtain the new df and lon lat errors (nothing change now)
    df_and_lon_lat_error <- check_replace_lat_lon(data,choice)

    # function to save the different errors in the error df + save the error index + new df :
    error_ll <- error_lon_lat(df_and_lon_lat_error[[2]],df_and_lon_lat_error[[3]],error,df_and_lon_lat_error[[4]],choice,idx_raw_previous_error2)
    #     new df error
    error <- error_ll[[1]]
    #     new error index
    idx_raw_previous_error3 <- error_ll[[2]]
    #     new df saved
    data <- df_and_lon_lat_error[[1]]
    
    
    # STEP SPEED (to do again if necessary)
    if (length(c(df_and_lon_lat_error[[2]],df_and_lon_lat_error[[3]]))>0){
      data <- step_speed_column(data)
      plot(data$step_speed)
    }

    # ERROR IN THAT DATASET KEPT
    error <- error_kept(data,error,choice,idx_raw_previous_error3)

    # SAVE ERROR INTO TXT
    # we save the error txt which will show all errors in this folder
    if (choice==0){
      write.table(error, file=paste(path_save,"error_auto.txt", sep="_"), sep=' ')
    }else{
      write.table(error, file=paste(path_save,"error_manual.txt", sep="_"), sep=' ')
    }

    # DIRECTION
    data <- step_direction_bird(data)

    # GAPS
    data <- gaps_column(data)

    # DIST COLONY
    data <- distance_colony(data,param_chosen)
    
    # DETECT TRIP
    data <- detect_trip(choice,data)

    # ID
    data <- ID_bird(data,name_save)

    # SAVE DATASET INTO CSV
    # we save the new csv/txt
    if (choice==0){
      write.csv(data, file=paste(path_save,"formatted_auto.csv", sep="_"),row.names = FALSE)
    }else{
      write.csv(data, file=paste(path_save,"formatted_manual.csv", sep="_"),row.names = FALSE)
    }
    
    # GRAPH
    gr1 <- graph1(data,name_save,param_chosen)
    gr2 <- graph2(data,name_save)
    gr3 <- graph3(data,name_save)
    gr4 <- graph4(data,name_save)
    # we arrange the 4 graphs into 1 file to better visualize
    ggarrange(gr1,gr2,gr3,gr4,ncol=2,nrow=2)
    if (choice==0){
      ggsave(paste(path_save,"graphs_auto.png",sep="_"), width = 11, height = 8)
    }else{
      ggsave(paste(path_save,"graphs_manual.png",sep="_"), width = 11, height = 8)
    }

    print("--------------------------------------------------------------------------")
    print("--------------------------------------------------------------------------")
    print("--------------------------------------------------------------------------")
    print("--------------------------------------------------------------------------")

  }
  
  return(data)
}
