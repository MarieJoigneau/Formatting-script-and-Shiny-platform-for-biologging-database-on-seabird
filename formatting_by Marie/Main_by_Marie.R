### Year : 2022
### Authors : Marie Joigneau, Amedee Roy, Sophie Bertrand
### Organism : IRD Sete
### Project : to clean and adapt the tropical bird dataset (Main)


setwd("D:/OneDrive/Stage CESURE - Sete oiseaux marins/Formatage/formatting_by Marie")
source("Functions_by_Marie.R")

### ---------- I. FIND SUB FOLDERS ----------------------------------

# We first find all the sub folders in the mother folder 'folders to format'
list_of_all_folders <- list.dirs(path="./folders to format",full.names=TRUE, recursive = TRUE)[-1]
cat(green("here you folder list: ",list_of_all_folders,"\n"))

### ---------- II. SEPARATE ID (IF NEEDED) ---------------------------

## For some files with different ID, we need to separate them into different csv
for (i in 1:length(list_of_all_folders)){
  print(paste("FOLDER",list_of_all_folders[i]))
  bird <- format_data_in_a_folder(0,list_of_all_folders[i],"separate")
}

### ---------- III. RENAME FILES -----------------------------

setwd("D:/OneDrive/Stage CESURE - Sete oiseaux marins/Formatage/test2/to format")

### ---- III.1. WE FILL THE METADATA WITH FILE NAMES --------------------

## We open  a metadata file empty with just the column names
name_df <- read.csv("./metadata to format/Perou 2015 Pescadores s_by_Marie.csv", header=TRUE, sep=";", dec=".")

# We retrieve the names of the files
# CAUTIOUS : metadata file need to not be in the same folder as trajectory files, or else it will be considered as a file and noted in the column
list_f_to_name <- list.files("./folders to format/files to format")
print(list_f_to_name)
for (i in 1:length(list_f_to_name)){
  name_df$file_original[i] = list_f_to_name[i]
}

# And save it
write.csv2(name_df, "./metadata to format/name_df_by_Marie.csv",row.names = FALSE)

## We need to fill all the other columns before going to the next step

### ---- III.2. WE FILL THE METADATA WITH 2 LAST COLUMNS --------------------

## We open again a metadata file
name_df <- read.csv("./metadata to format/Perou 2012 Pescadores s_by_Marie.csv", header=TRUE, sep=";", dec=".")

# we fill the "file_renamed_by_Marie" column
# CAUTIOUS : change txt or csv (at the end) depending of the format of the original file
type_files <- as.character(readline("Write here if you original files are csv or txt "))
last_part_renamed <- paste("by_Marie",type_files,sep=".")
print(last_part_renamed)
for (k in 1:length(name_df$archipelago)){
  name_df$file_renamed_by_Marie[k] = paste(name_df$archipelago_abb[k],as.Date(name_df$date[k], "%d/%m/%Y"),name_df$island_abb[k],name_df$species_abb[k],
                                           name_df$bird_id[k],name_df$band[k],name_df$sensor_name_abb[k],name_df$gps_id[k],
                                           name_df$local_hour[k],last_part_renamed,sep="_")
}

# we fill the "file_formatted_by_Marie" column
for (k in 1:length(name_df$archipelago)){
  name_df$file_formatted_by_Marie[k] = paste(substr(name_df$file_renamed_by_Marie[k],1,nchar(name_df$file_renamed_by_Marie[k])-4),"formatted_auto.csv",sep="_")
}

# And save it again
write.csv2(name_df, "./metadata to format/name_df_by_Marie.csv",row.names = FALSE)


### ---- III.3. WE RENAME THE FILES ---------------------------------

## we rename the raw files, going from "file_original" to "file_renamed_by_Marie" columns

# STEP i : in all the folders
for (i in 1:length(list_of_all_folders)){ 
  
  print(paste("i ",i))
  
  print(paste("folder name ",list_of_all_folders[i]))
  print(paste("all files in this folder ",list.files(list_of_all_folders[i])))
  
  # length of folder = number of files
  # Here list of all raw files
  list_f <- list.files(list_of_all_folders[i])
  list_f_full_path <- list.files(list_of_all_folders[i],full.names = TRUE)
  
  # SUB STEPS j  : in all the files of the folder
  for (j in 1:length(list_f)){ 
    
    print(paste("j ",j))
    
    file.rename(from = paste("./folders to format/files to format",name_df$file_original[j],sep="/"), to = paste("./folders to format/files to format",name_df$file_renamed_by_Marie[j],sep="/"))
  }
}

### ---------- IV. PROBLEM AXY-TREK ----------------------------
### ---- IV.1. FILTER ----------------------------------------------
# --- CASE 1: SPACE SO LOST FOR COLUMN ----

## First case is to have space between columns and not always value for some columns

for (i in 1:length(list_of_all_folders)){
  
  # the folders
  the_path <- list_of_all_folders[i]
  print(paste("the path ",the_path))
  
  # the files
  len_file <- length(list.files(the_path))
  list_file_axy <- list.files(the_path,full.names=TRUE)
  list_file_axy_short <- list.files(the_path)
  print(list_file_axy)
  
  # I usually put "for (j in 1:2){" because they are huge files for the computer
  for (j in 1:len_file){
    
    print(paste("j: ",j," ---------------"))
    
    if (j!=1){
      j=(j-1)*2+1
    }
    
    print(paste("new j: ",j))
    print(list_file_axy[j])
    
    print("STEP 1")
    # STEP 1: find lines with Active/Dry
    # we open the file as a csv with a single column...
    test2 <- read.csv(list_file_axy[j], header =FALSE, sep =';')
    # ... and then we we find the lines with the sentence with particular word like EVENT (param[5])
    print("idx_active_dry")
    idx_active_dry <- which((str_detect(test2$V1,as.character("Active/Dry")))==TRUE)
    
    print("STEP 2")
    # STEP 2: skip theses lines and transform in a df that can be used for after : with Active/Dry
    # I read again the txt but without the target lines
    test <- test2[idx_active_dry,]
    # I split the one column vector into nine
    print("we split")
    test <- str_split_fixed(test, "\t", 17)
    # Then I create a df by a matrix first (in 2 steps)
    test <- matrix(test,ncol=17)
    test <- data.frame(test)
    
    print("STEP 3")
    # STEP 3: we keep all the lines with a latitude / longitude
    # X10 = latitude
    # X11 = longitude
    test$X10 <- as.numeric(test$X10)
    test$X11 <- as.numeric(test$X11)
    
    idx_lat_lon <- which(is.na(test$X10)==FALSE)
    idx_lat_lon
    test <- test[idx_lat_lon,]
    
    # the new file is named with a "_filtered" added to differentiate it and then saved
    new_name <- paste(substr(list_file_axy_short[j],1,nchar(list.files(the_path)[j])-4),"filtered.csv",sep="_")
    
    write.csv(test, new_name ,row.names = FALSE)
  }
  
}

# --- CASE 2: ACTIVE/DRY NOT OK, WE HAVE TO MANY GPS LINES ----

# In this case we have gps data in all lines of the Axy-trek and so too many lines
# In this case I filter one by one
list_file_axy <- list.files("./folders to format/files to format",full.names=TRUE)
testtest2 <- read.csv(list_file_axy[1], header =FALSE, sep =',')

### We convert the time column
Time_bird=strptime(testtest2$V3, format="%H:%M:%S") # work also for a 12:30:04.00 format
Time_bird_last=format(Time_bird, "%H:%M:%S")
testtest2$V3 <- Time_bird_last

# same datetime in 2 lines next to each other !...
v1 <- testtest2$V3[1:(length(testtest2$V3)-1)]
v2 <- testtest2$V3[2:length(testtest2$V3)]
pb_same <- which(v1==v2)
testtest2 <- testtest2[-pb_same,]

write.csv(testtest2, "axy_filtered.csv" ,row.names = FALSE)

# --- CASE 3 : , and normal columns ----

## In this case we have sep="," and normal columns. It's the ideal csv file.
# In this case I filter one by one
list_file_axy <- list.files("./folders to format/files to format",full.names=TRUE)
axyt <- read.csv(list_file_axy[1], header =TRUE, sep =',')

# We find the line with lat lon
idx_lat_lon <- which((axyt$location.lat)!=0)
print(idx_lat_lon)
axyt2 <- axyt[idx_lat_lon,]

### IF ALSO SEVERAL GPS POINTS IN 1 SEC :
### We convert the time column
Time_bird=strptime(axyt2$Time, format="%H:%M:%S") # work also for a 12:30:04.00 format
Time_bird_last=format(Time_bird, "%H:%M:%S")
axyt2$Time <- Time_bird_last
# same datetime in 2 lines next to each other:
v1 <- axyt2$Time[1:(length(axyt2$Time)-1)]
v2 <- axyt2$Time[2:length(axyt2$Time)]
pb_same <- which(v1==v2)
axyt2 <- axyt2[-pb_same,]

# Then we save it
new_name <- paste(substr(list.files(the_path)[1],1,nchar(list.files(the_path)[1])-4),"filtered.csv",sep="_")
write.csv(axyt2, new_name ,row.names = FALSE)

### ---- IV.2. PB AXY-TREK : MERGE THE FILTERED ONES ----------------------

list_file_separ <- list.files("./folders to format/files to format",full.names=TRUE)
print(list_file_separ)

# When we want to filter axy-trek files, we need first to separate the csv file in several tiny csv files for the computer
# here is the step of merging 3 filter files, you can adapt it by adding one file (like the one with #)
print(list_file_separ[1])
test2 <- read.csv(list_file_separ[1], header =TRUE, sep =',',dec=".")

print(list_file_separ[2])
test3 <- read.csv(list_file_separ[2], header =TRUE, sep =',',dec=".")

#print(list_file_separ[3])
#test3 <- read.csv(list_file_separ[3], header =TRUE, sep =',',dec=".")

#print(list_file_separ[4])
#test3 <- read.csv(list_file_separ[4], header =TRUE, sep =',',dec=".")

total <- rbind(test2,test3)
#total <- rbind(test2,test3,test4)
#total <- rbind(test2,test3,test4,test5)

write.csv(total,"./folders to format/files to format/merged.csv",row.names = FALSE)

### ---------- V. FORMAT FILES ---------------------------

## We format the files well-named automatically
for (i in 1:length(list_of_all_folders)){
  cat(green("FOLDER",list_of_all_folders[i],"\n"))
  bird <- format_data_in_a_folder(0,list_of_all_folders[i],"no_separate")
}

## We format the files well-named manually
for (i in 1:length(list_of_all_folders)){
  cat(green("FOLDER",list_of_all_folders[i],"\n"))
  bird <- format_data_in_a_folder(1,list_of_all_folders[i],"no_separate")
}
