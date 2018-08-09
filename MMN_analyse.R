library(mlmRev)
library(reshape)
library(lattice)
library(car)
library(lme4)
library(leaps)
library(MASS)
library(gdata)
library(Rmisc)
library(ggplot2)


######################################################################################################################################
######################################  EEG ANALYSIS MMN  ############################################################################
######################################################################################################################################

#### IMPORT DATA ---------------------------------------------------------------------------------------------------------------------
subject_list <- c("ppn1", "ppn3", "ppn5", "ppn7", "ppn50", "ppn52", "ppn54", "ppn56", "ppn60", "ppn62", "ppn64", "ppn66")
for (k in 1:length(subject_list)){
  assign(paste0(subject_list[k], "_1_fast_30_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_fast_30_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_1_fast_30_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_fast_30_standard.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_1_fast_60_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_fast_60_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_1_fast_60_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_fast_60_standard.txt")), header = TRUE, check.names = FALSE))
  
  assign(paste0(subject_list[k], "_1_slow_30_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_slow_30_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_1_slow_30_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_slow_30_standard.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_1_slow_60_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_slow_60_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_1_slow_60_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_1_slow_60_standard.txt")), header = TRUE, check.names = FALSE))
  
  assign(paste0(subject_list[k], "_2_fast_30_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_fast_30_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_2_fast_30_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_fast_30_standard.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_2_fast_60_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_fast_60_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_2_fast_60_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_fast_60_standard.txt")), header = TRUE, check.names = FALSE))
  
  assign(paste0(subject_list[k], "_2_slow_30_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_slow_30_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_2_slow_30_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_slow_30_standard.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_2_slow_60_deviant"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_slow_60_deviant.txt")), header = TRUE, check.names = FALSE))
  assign(paste0(subject_list[k], "_2_slow_60_standard"), read.delim(sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", paste0(subject_list[k], "_2_slow_60_standard.txt")), header = TRUE, check.names = FALSE))
}

# new subject list
pp <- c(1, 3, 5, 7, 50, 52, 54,56, 60, 62, 64, 66)
# aanpassen bij ppn 5 en 7

#######################################################################################
## define timescale for mean amplitude (mean of matrices and looking for min value)  ##
#######################################################################################

# all files in directory
filelist = list.files(path = "/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/" ,pattern = "*.txt")

# ------------------------------------ mean 30 deviant ----------------------#

# make a list of all filenames ending with _30_deviant
deviant30 = grepl("_30_deviant", filelist, fixed = TRUE)
table(deviant30)

# make sum matrix
# kolom 1 en 172 verwijderen want die zijn niet nodig 
sum_matrix_deviant30 = matrix(0, nrow = 64, ncol = 170)

for (i in 1:length(deviant30)){
  if (deviant30[i] == TRUE){
    print(filelist[i])
    path = sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", filelist[i])
    dataset = read.delim(path, header = TRUE, check.names = FALSE)
    dataset[172] = NULL
    dataset[1] = NULL
    dataset = as.matrix(dataset)
    sum_matrix_deviant30 = sum_matrix_deviant30 + dataset
  }
}

# make mean matrix
mean_deviant30 = as.matrix(sum_matrix_deviant30 / 40)
mean_deviant30

# ----------------------------- mean 30 standard ------------------------- #

# make a list of all filenames ending with _30_standard
standard30 = grepl("_30_standard", filelist, fixed = TRUE)
table(standard30)

for (i in 1:length(standard30)){
  if (standard30[i] == TRUE){
    print(filelist[i])
  }
}

# make sum matrix
# kolom 1 en 172 verwijderen want die zijn niet nodig 
sum_matrix_standard30 = matrix(0, nrow = 64, ncol = 170)

for (i in 1:length(standard30)){
  if (standard30[i] == TRUE){
    print(filelist[i])
    path = sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", filelist[i])
    dataset = read.delim(path, header = TRUE, check.names = FALSE)
    dataset[172] = NULL
    dataset[1] = NULL
    dataset = as.matrix(dataset)
    sum_matrix_standard30 = sum_matrix_standard30 + dataset
  }
}

#make mean matrix
mean_standard30 = as.matrix(sum_matrix_standard30 / 40)
mean_standard30

#----------------------------)) MMN 30 ----------------------------------#
MMN30 = as.matrix(mean_deviant30 - mean_standard30)

MMN30[38, 118:128]
# peak MMN ligt op 194: -1.4568

#----------------------------- mean 60 deviant ---------------------------#

# make a list of all filenames ending with _30_deviant
deviant60 = grepl("_60_deviant", filelist, fixed = TRUE)
table(deviant60)

for (i in 1:length(deviant60)){
  if (deviant60[i] == TRUE){
    print(filelist[i])
  }
}

# make sum matrix
# kolom 1 en 172 verwijderen want die zijn niet nodig 
sum_matrix_deviant60 = matrix(0, nrow = 64, ncol = 170)

for (i in 1:length(deviant60)){
  if (deviant60[i] == TRUE){
    print(filelist[i])
    path = sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", filelist[i])
    dataset = read.delim(path, header = TRUE, check.names = FALSE)
    dataset[172] = NULL
    dataset[1] = NULL
    dataset = as.matrix(dataset)
    sum_matrix_deviant60 = sum_matrix_deviant60 + dataset
  }
}

# make mean matrix
mean_deviant60 = as.matrix(sum_matrix_deviant60 / 40)
mean_deviant60

#----------------------------- mean 60 standard --------------------------#

# make a list of all filenames ending with _30_standard
standard60 = grepl("_60_standard", filelist, fixed = TRUE)
table(standard60)

for (i in 1:length(standard60)){
  if (standard60[i] == TRUE){
    print(filelist[i])
  }
}

# make sum matrix
# kolom 1 en 172 verwijderen want die zijn niet nodig 
sum_matrix_standard60 = matrix(0, nrow = 64, ncol = 170)

for (i in 1:length(standard60)){
  if (standard60[i] == TRUE){
    print(filelist[i])
    path = sprintf("/Volumes/DRIVE-N-GO/Analyse EEG/Condities R/%s", filelist[i])
    dataset = read.delim(path, header = TRUE, check.names = FALSE)
    dataset[172] = NULL
    dataset[1] = NULL
    dataset = as.matrix(dataset)
    sum_matrix_standard60 = sum_matrix_standard60 + dataset
  }
}

#make mean matrix
mean_standard60 = as.matrix(sum_matrix_standard60 / 40)
mean_standard60

## MMN 60 ##
MMN60 = as.matrix(mean_deviant60 - mean_standard60)

MMN60[38, 103:113]
# peak MMN ligt op 164: -3.1363

########################
## design data matrix ##
########################
D = data.frame(matrix(0, nrow = 8 * length(pp), ncol = 6))
colnames(D) <- c("PPN", "Group", "Order", "Speed", "Tone_type", "Mean_amplitude")
D

D$PPN <- rep(c(1, 3, 5, 7, 50, 52, 54, 56, 60, 62, 64, 66), each = 8) 
# aanpassen bij ppn 5 en 7
for(i in 1:length(D$PPN)){
  if(D$PPN[i] < 50){
    D$Group[i] = 1
  } else {
    D$Group[i] = 2
  }
}
D$Order <- rep(c(1, 1, 1, 1, 2, 2, 2,2), times = length(pp))
D$Speed <- rep(c(1, 1, 2, 2, 1, 1, 2, 2), times = length(pp)) # 1 = slow
D$Tone_type <- rep(c(1,2), times = 4*length(pp)) # 1 = 30 

# loop om MMN te berekenen voor alle condities
############# if D$PPN = 5 en if D$PPN = 7 erbij zetten bij ppn 5 en 7
for (i in 1:length(D$PPN)){
  if(D$PPN[i] == 1){
     if(D$Order[i] == 1){
    
      if(D$Speed[i] == 1){
      
        if(D$Tone_type[i] == 1){
        
          ppn1_1_slow_30_deviant[172] = NULL
          ppn1_1_slow_30_deviant[1] = NULL
          ppn1_1_slow_30_deviant = as.matrix(ppn1_1_slow_30_deviant)
          
          ppn1_1_slow_30_standard[172] = NULL
          ppn1_1_slow_30_standard[1] = NULL
          ppn1_1_slow_30_standard = as.matrix(ppn1_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn1_1_slow_30_deviant[38, 118:128]) - mean(ppn1_1_slow_30_standard[38, 118:128])
        }
        
       if(D$Tone_type[i] == 2){
        
         ppn1_1_slow_60_deviant[172] = NULL
         ppn1_1_slow_60_deviant[1] = NULL
         ppn1_1_slow_60_deviant = as.matrix(ppn1_1_slow_60_deviant)
         
         ppn1_1_slow_60_standard[172] = NULL
         ppn1_1_slow_60_standard[1] = NULL
         ppn1_1_slow_60_standard = as.matrix(ppn1_1_slow_60_standard)
         
         D$Mean_amplitude[i] = mean(ppn1_1_slow_60_deviant[38, 103:113]) - mean(ppn1_1_slow_60_standard[38, 103:113])
       }
      }
     if(D$Speed[i] == 2){
      
        if(D$Tone_type[i] == 1){
          
          ppn1_1_fast_30_deviant[172] = NULL
          ppn1_1_fast_30_deviant[1] = NULL
          ppn1_1_fast_30_deviant = as.matrix(ppn1_1_fast_30_deviant)
          
          ppn1_1_fast_30_standard[172] = NULL
          ppn1_1_fast_30_standard[1] = NULL
          ppn1_1_fast_30_standard = as.matrix(ppn1_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn1_1_fast_30_deviant[38, 118:128]) - mean(ppn1_1_fast_30_standard[38, 118:128])
       }
       if(D$Tone_type[i] == 2){
        
         ppn1_1_fast_60_deviant[172] = NULL
         ppn1_1_fast_60_deviant[1] = NULL
         ppn1_1_fast_60_deviant = as.matrix(ppn1_1_fast_60_deviant)
         
         ppn1_1_fast_60_standard[172] = NULL
         ppn1_1_fast_60_standard[1] = NULL
         ppn1_1_fast_60_standard = as.matrix(ppn1_1_fast_60_standard)
         
         D$Mean_amplitude[i] = mean(ppn1_1_fast_60_deviant[38, 103:113]) - mean(ppn1_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
      
       if(D$Tone_type[i] == 1){
        
         ppn1_2_slow_30_deviant[172] = NULL
         ppn1_2_slow_30_deviant[1] = NULL
         ppn1_2_slow_30_deviant = as.matrix(ppn1_2_slow_30_deviant)
         
         ppn1_2_slow_30_standard[172] = NULL
         ppn1_2_slow_30_standard[1] = NULL
         ppn1_2_slow_30_standard = as.matrix(ppn1_2_slow_30_standard)
         
         D$Mean_amplitude[i] = mean(ppn1_2_slow_30_deviant[38, 118:128]) - mean(ppn1_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
        
          ppn1_2_slow_60_deviant[172] = NULL
          ppn1_2_slow_60_deviant[1] = NULL
          ppn1_2_slow_60_deviant = as.matrix(ppn1_2_slow_60_deviant)
          
          ppn1_2_slow_60_standard[172] = NULL
          ppn1_2_slow_60_standard[1] = NULL
          ppn1_2_slow_60_standard = as.matrix(ppn1_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn1_2_slow_60_deviant[38, 103:113]) - mean(ppn1_2_slow_60_standard[38, 103:113])
        }
      }
     if(D$Speed[i] == 2){
      
        if(D$Tone_type[i] == 1){
        
          ppn1_2_fast_30_deviant[172] = NULL
          ppn1_2_fast_30_deviant[1] = NULL
          ppn1_2_fast_30_deviant = as.matrix(ppn1_2_fast_30_deviant)
          
          ppn1_2_fast_30_standard[172] = NULL
          ppn1_2_fast_30_standard[1] = NULL
          ppn1_2_fast_30_standard = as.matrix(ppn1_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn1_2_fast_30_deviant[38, 118:128]) - mean(ppn1_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
        
          ppn1_2_fast_60_deviant[172] = NULL
          ppn1_2_fast_60_deviant[1] = NULL
          ppn1_2_fast_60_deviant = as.matrix(ppn1_2_fast_60_deviant)
          
          ppn1_2_fast_60_standard[172] = NULL
          ppn1_2_fast_60_standard[1] = NULL
          ppn1_2_fast_60_standard = as.matrix(ppn1_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn1_2_fast_60_deviant[38, 103:113]) - mean(ppn1_2_fast_60_standard[38, 103:113])
        }
      }
    }
  }
  if(D$PPN[i] == 3){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn3_1_slow_30_deviant[172] = NULL
          ppn3_1_slow_30_deviant[1] = NULL
          ppn3_1_slow_30_deviant = as.matrix(ppn3_1_slow_30_deviant)
          
          ppn3_1_slow_30_standard[172] = NULL
          ppn3_1_slow_30_standard[1] = NULL
          ppn3_1_slow_30_standard = as.matrix(ppn3_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_1_slow_30_deviant[38, 118:128]) - mean(ppn3_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn3_1_slow_60_deviant[172] = NULL
          ppn3_1_slow_60_deviant[1] = NULL
          ppn3_1_slow_60_deviant = as.matrix(ppn3_1_slow_60_deviant)
          
          ppn3_1_slow_60_standard[172] = NULL
          ppn3_1_slow_60_standard[1] = NULL
          ppn3_1_slow_60_standard = as.matrix(ppn3_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_1_slow_60_deviant[38, 103:113]) - mean(ppn3_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn3_1_fast_30_deviant[172] = NULL
          ppn3_1_fast_30_deviant[1] = NULL
          ppn3_1_fast_30_deviant = as.matrix(ppn3_1_fast_30_deviant)
          
          ppn3_1_fast_30_standard[172] = NULL
          ppn3_1_fast_30_standard[1] = NULL
          ppn3_1_fast_30_standard = as.matrix(ppn3_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_1_fast_30_deviant[38, 118:128]) - mean(ppn3_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn3_1_fast_60_deviant[172] = NULL
          ppn3_1_fast_60_deviant[1] = NULL
          ppn3_1_fast_60_deviant = as.matrix(ppn3_1_fast_60_deviant)
          
          ppn3_1_fast_60_standard[172] = NULL
          ppn3_1_fast_60_standard[1] = NULL
          ppn3_1_fast_60_standard = as.matrix(ppn3_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_1_fast_60_deviant[38, 103:113]) - mean(ppn3_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn3_2_slow_30_deviant[172] = NULL
          ppn3_2_slow_30_deviant[1] = NULL
          ppn3_2_slow_30_deviant = as.matrix(ppn3_2_slow_30_deviant)
          
          ppn3_2_slow_30_standard[172] = NULL
          ppn3_2_slow_30_standard[1] = NULL
          ppn3_2_slow_30_standard = as.matrix(ppn3_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_2_slow_30_deviant[38, 118:128]) - mean(ppn3_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn3_2_slow_60_deviant[172] = NULL
          ppn3_2_slow_60_deviant[1] = NULL
          ppn3_2_slow_60_deviant = as.matrix(ppn3_2_slow_60_deviant)
          
          ppn3_2_slow_60_standard[172] = NULL
          ppn3_2_slow_60_standard[1] = NULL
          ppn3_2_slow_60_standard = as.matrix(ppn3_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_2_slow_60_deviant[38, 103:113]) - mean(ppn3_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn3_2_fast_30_deviant[172] = NULL
          ppn3_2_fast_30_deviant[1] = NULL
          ppn3_2_fast_30_deviant = as.matrix(ppn3_2_fast_30_deviant)
          
          ppn3_2_fast_30_standard[172] = NULL
          ppn3_2_fast_30_standard[1] = NULL
          ppn3_2_fast_30_standard = as.matrix(ppn3_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_2_fast_30_deviant[38, 118:128]) - mean(ppn3_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn3_2_fast_60_deviant[172] = NULL
          ppn3_2_fast_60_deviant[1] = NULL
          ppn3_2_fast_60_deviant = as.matrix(ppn3_2_fast_60_deviant)
          
          ppn3_2_fast_60_standard[172] = NULL
          ppn3_2_fast_60_standard[1] = NULL
          ppn3_2_fast_60_standard = as.matrix(ppn3_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn3_2_fast_60_deviant[38, 103:113]) - mean(ppn3_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 5){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn5_1_slow_30_deviant[172] = NULL
          ppn5_1_slow_30_deviant[1] = NULL
          ppn5_1_slow_30_deviant = as.matrix(ppn5_1_slow_30_deviant)
          
          ppn5_1_slow_30_standard[172] = NULL
          ppn5_1_slow_30_standard[1] = NULL
          ppn5_1_slow_30_standard = as.matrix(ppn5_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_1_slow_30_deviant[38, 118:128]) - mean(ppn5_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn5_1_slow_60_deviant[172] = NULL
          ppn5_1_slow_60_deviant[1] = NULL
          ppn5_1_slow_60_deviant = as.matrix(ppn5_1_slow_60_deviant)
          
          ppn5_1_slow_60_standard[172] = NULL
          ppn5_1_slow_60_standard[1] = NULL
          ppn5_1_slow_60_standard = as.matrix(ppn5_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_1_slow_60_deviant[38, 103:113]) - mean(ppn5_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn5_1_fast_30_deviant[172] = NULL
          ppn5_1_fast_30_deviant[1] = NULL
          ppn5_1_fast_30_deviant = as.matrix(ppn5_1_fast_30_deviant)
          
          ppn5_1_fast_30_standard[172] = NULL
          ppn5_1_fast_30_standard[1] = NULL
          ppn5_1_fast_30_standard = as.matrix(ppn5_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_1_fast_30_deviant[38, 118:128]) - mean(ppn5_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn5_1_fast_60_deviant[172] = NULL
          ppn5_1_fast_60_deviant[1] = NULL
          ppn5_1_fast_60_deviant = as.matrix(ppn5_1_fast_60_deviant)
          
          ppn5_1_fast_60_standard[172] = NULL
          ppn5_1_fast_60_standard[1] = NULL
          ppn5_1_fast_60_standard = as.matrix(ppn5_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_1_fast_60_deviant[38, 103:113]) - mean(ppn5_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn5_2_slow_30_deviant[172] = NULL
          ppn5_2_slow_30_deviant[1] = NULL
          ppn5_2_slow_30_deviant = as.matrix(ppn5_2_slow_30_deviant)
        
          ppn5_2_slow_30_standard[172] = NULL
          ppn5_2_slow_30_standard[1] = NULL
          ppn5_2_slow_30_standard = as.matrix(ppn5_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_2_slow_30_deviant[38, 118:128]) - mean(ppn5_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn5_2_slow_60_deviant[172] = NULL
          ppn5_2_slow_60_deviant[1] = NULL
          ppn5_2_slow_60_deviant = as.matrix(ppn5_2_slow_60_deviant)
          
          ppn5_2_slow_60_standard[172] = NULL
          ppn5_2_slow_60_standard[1] = NULL
          ppn5_2_slow_60_standard = as.matrix(ppn5_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_2_slow_60_deviant[38, 103:113]) - mean(ppn5_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn5_2_fast_30_deviant[172] = NULL
          ppn5_2_fast_30_deviant[1] = NULL
          ppn5_2_fast_30_deviant = as.matrix(ppn5_2_fast_30_deviant)
          
          ppn5_2_fast_30_standard[172] = NULL
          ppn5_2_fast_30_standard[1] = NULL
          ppn5_2_fast_30_standard = as.matrix(ppn5_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_2_fast_30_deviant[38, 118:128]) - mean(ppn5_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn5_2_fast_60_deviant[172] = NULL
          ppn5_2_fast_60_deviant[1] = NULL
          ppn5_2_fast_60_deviant = as.matrix(ppn5_2_fast_60_deviant)
          
          ppn5_2_fast_60_standard[172] = NULL
          ppn5_2_fast_60_standard[1] = NULL
          ppn5_2_fast_60_standard = as.matrix(ppn5_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn5_2_fast_60_deviant[38, 103:113]) - mean(ppn5_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 7){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn7_1_slow_30_deviant[172] = NULL
          ppn7_1_slow_30_deviant[1] = NULL
          ppn7_1_slow_30_deviant = as.matrix(ppn7_1_slow_30_deviant)
          
          ppn7_1_slow_30_standard[172] = NULL
          ppn7_1_slow_30_standard[1] = NULL
          ppn7_1_slow_30_standard = as.matrix(ppn7_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_1_slow_30_deviant[38, 118:128]) - mean(ppn7_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn7_1_slow_60_deviant[172] = NULL
          ppn7_1_slow_60_deviant[1] = NULL
          ppn7_1_slow_60_deviant = as.matrix(ppn7_1_slow_60_deviant)
          
          ppn7_1_slow_60_standard[172] = NULL
          ppn7_1_slow_60_standard[1] = NULL
          ppn7_1_slow_60_standard = as.matrix(ppn7_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_1_slow_60_deviant[38, 103:113]) - mean(ppn7_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn7_1_fast_30_deviant[172] = NULL
          ppn7_1_fast_30_deviant[1] = NULL
          ppn7_1_fast_30_deviant = as.matrix(ppn7_1_fast_30_deviant)
          
          ppn7_1_fast_30_standard[172] = NULL
          ppn7_1_fast_30_standard[1] = NULL
          ppn7_1_fast_30_standard = as.matrix(ppn7_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_1_fast_30_deviant[38, 118:128]) - mean(ppn7_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn7_1_fast_60_deviant[172] = NULL
          ppn7_1_fast_60_deviant[1] = NULL
          ppn7_1_fast_60_deviant = as.matrix(ppn7_1_fast_60_deviant)
          
          ppn7_1_fast_60_standard[172] = NULL
          ppn7_1_fast_60_standard[1] = NULL
          ppn7_1_fast_60_standard = as.matrix(ppn7_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_1_fast_60_deviant[38, 103:113]) - mean(ppn7_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn7_2_slow_30_deviant[172] = NULL
          ppn7_2_slow_30_deviant[1] = NULL
          ppn7_2_slow_30_deviant = as.matrix(ppn7_2_slow_30_deviant)
          
          ppn7_2_slow_30_standard[172] = NULL
          ppn7_2_slow_30_standard[1] = NULL
          ppn7_2_slow_30_standard = as.matrix(ppn7_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_2_slow_30_deviant[38, 118:128]) - mean(ppn7_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn7_2_slow_60_deviant[172] = NULL
          ppn7_2_slow_60_deviant[1] = NULL
          ppn7_2_slow_60_deviant = as.matrix(ppn7_2_slow_60_deviant)
          
          ppn7_2_slow_60_standard[172] = NULL
          ppn7_2_slow_60_standard[1] = NULL
          ppn7_2_slow_60_standard = as.matrix(ppn7_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_2_slow_60_deviant[38, 103:113]) - mean(ppn7_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn7_2_fast_30_deviant[172] = NULL
          ppn7_2_fast_30_deviant[1] = NULL
          ppn7_2_fast_30_deviant = as.matrix(ppn7_2_fast_30_deviant)
          
          ppn7_2_fast_30_standard[172] = NULL
          ppn7_2_fast_30_standard[1] = NULL
          ppn7_2_fast_30_standard = as.matrix(ppn7_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_2_fast_30_deviant[38, 118:128]) - mean(ppn7_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn7_2_fast_60_deviant[172] = NULL
          ppn7_2_fast_60_deviant[1] = NULL
          ppn7_2_fast_60_deviant = as.matrix(ppn7_2_fast_60_deviant)
          
          ppn7_2_fast_60_standard[172] = NULL
          ppn7_2_fast_60_standard[1] = NULL
          ppn7_2_fast_60_standard = as.matrix(ppn7_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn7_2_fast_60_deviant[38, 103:113]) - mean(ppn7_2_fast_60_standard[38, 103:113])
        }
      }
    }
  }
  if(D$PPN[i] == 50){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn50_1_slow_30_deviant[172] = NULL
          ppn50_1_slow_30_deviant[1] = NULL
          ppn50_1_slow_30_deviant = as.matrix(ppn50_1_slow_30_deviant)
          
          ppn50_1_slow_30_standard[172] = NULL
          ppn50_1_slow_30_standard[1] = NULL
          ppn50_1_slow_30_standard = as.matrix(ppn50_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_1_slow_30_deviant[38, 118:128]) - mean(ppn50_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn50_1_slow_60_deviant[172] = NULL
          ppn50_1_slow_60_deviant[1] = NULL
          ppn50_1_slow_60_deviant = as.matrix(ppn50_1_slow_60_deviant)
          
          ppn50_1_slow_60_standard[172] = NULL
          ppn50_1_slow_60_standard[1] = NULL
          ppn50_1_slow_60_standard = as.matrix(ppn50_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_1_slow_60_deviant[38, 103:113]) - mean(ppn50_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn50_1_fast_30_deviant[172] = NULL
          ppn50_1_fast_30_deviant[1] = NULL
          ppn50_1_fast_30_deviant = as.matrix(ppn50_1_fast_30_deviant)
          
          ppn50_1_fast_30_standard[172] = NULL
          ppn50_1_fast_30_standard[1] = NULL
          ppn50_1_fast_30_standard = as.matrix(ppn50_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_1_fast_30_deviant[38, 118:128]) - mean(ppn50_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn50_1_fast_60_deviant[172] = NULL
          ppn50_1_fast_60_deviant[1] = NULL
          ppn50_1_fast_60_deviant = as.matrix(ppn50_1_fast_60_deviant)
          
          ppn50_1_fast_60_standard[172] = NULL
          ppn50_1_fast_60_standard[1] = NULL
          ppn50_1_fast_60_standard = as.matrix(ppn50_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_1_fast_60_deviant[38, 103:113]) - mean(ppn50_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn50_2_slow_30_deviant[172] = NULL
          ppn50_2_slow_30_deviant[1] = NULL
          ppn50_2_slow_30_deviant = as.matrix(ppn50_2_slow_30_deviant)
          
          ppn50_2_slow_30_standard[172] = NULL
          ppn50_2_slow_30_standard[1] = NULL
          ppn50_2_slow_30_standard = as.matrix(ppn50_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_2_slow_30_deviant[38, 118:128]) - mean(ppn50_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn50_2_slow_60_deviant[172] = NULL
          ppn50_2_slow_60_deviant[1] = NULL
          ppn50_2_slow_60_deviant = as.matrix(ppn50_2_slow_60_deviant)
          
          ppn50_2_slow_60_standard[172] = NULL
          ppn50_2_slow_60_standard[1] = NULL
          ppn50_2_slow_60_standard = as.matrix(ppn50_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_2_slow_60_deviant[38, 103:113]) - mean(ppn50_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn50_2_fast_30_deviant[172] = NULL
          ppn50_2_fast_30_deviant[1] = NULL
          ppn50_2_fast_30_deviant = as.matrix(ppn50_2_fast_30_deviant)
          
          ppn50_2_fast_30_standard[172] = NULL
          ppn50_2_fast_30_standard[1] = NULL
          ppn50_2_fast_30_standard = as.matrix(ppn50_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_2_fast_30_deviant[38, 118:128]) - mean(ppn50_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn50_2_fast_60_deviant[172] = NULL
          ppn50_2_fast_60_deviant[1] = NULL
          ppn50_2_fast_60_deviant = as.matrix(ppn50_2_fast_60_deviant)
          
          ppn50_2_fast_60_standard[172] = NULL
          ppn50_2_fast_60_standard[1] = NULL
          ppn50_2_fast_60_standard = as.matrix(ppn50_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn50_2_fast_60_deviant[38, 103:113]) - mean(ppn50_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 52){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn52_1_slow_30_deviant[172] = NULL
          ppn52_1_slow_30_deviant[1] = NULL
          ppn52_1_slow_30_deviant = as.matrix(ppn52_1_slow_30_deviant)
          
          ppn52_1_slow_30_standard[172] = NULL
          ppn52_1_slow_30_standard[1] = NULL
          ppn52_1_slow_30_standard = as.matrix(ppn52_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_1_slow_30_deviant[38, 118:128]) - mean(ppn52_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn52_1_slow_60_deviant[172] = NULL
          ppn52_1_slow_60_deviant[1] = NULL
          ppn52_1_slow_60_deviant = as.matrix(ppn52_1_slow_60_deviant)
          
          ppn52_1_slow_60_standard[172] = NULL
          ppn52_1_slow_60_standard[1] = NULL
          ppn52_1_slow_60_standard = as.matrix(ppn52_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_1_slow_60_deviant[38, 103:113]) - mean(ppn52_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn52_1_fast_30_deviant[172] = NULL
          ppn52_1_fast_30_deviant[1] = NULL
          ppn52_1_fast_30_deviant = as.matrix(ppn52_1_fast_30_deviant)
          
          ppn52_1_fast_30_standard[172] = NULL
          ppn52_1_fast_30_standard[1] = NULL
          ppn52_1_fast_30_standard = as.matrix(ppn52_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_1_fast_30_deviant[38, 118:128]) - mean(ppn52_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn52_1_fast_60_deviant[172] = NULL
          ppn52_1_fast_60_deviant[1] = NULL
          ppn52_1_fast_60_deviant = as.matrix(ppn52_1_fast_60_deviant)
          
          ppn52_1_fast_60_standard[172] = NULL
          ppn52_1_fast_60_standard[1] = NULL
          ppn52_1_fast_60_standard = as.matrix(ppn52_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_1_fast_60_deviant[38, 103:113]) - mean(ppn52_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn52_2_slow_30_deviant[172] = NULL
          ppn52_2_slow_30_deviant[1] = NULL
          ppn52_2_slow_30_deviant = as.matrix(ppn52_2_slow_30_deviant)
          
          ppn52_2_slow_30_standard[172] = NULL
          ppn52_2_slow_30_standard[1] = NULL
          ppn52_2_slow_30_standard = as.matrix(ppn52_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_2_slow_30_deviant[38, 118:128]) - mean(ppn52_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn52_2_slow_60_deviant[172] = NULL
          ppn52_2_slow_60_deviant[1] = NULL
          ppn52_2_slow_60_deviant = as.matrix(ppn52_2_slow_60_deviant)
          
          ppn52_2_slow_60_standard[172] = NULL
          ppn52_2_slow_60_standard[1] = NULL
          ppn52_2_slow_60_standard = as.matrix(ppn52_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_2_slow_60_deviant[38, 103:113]) - mean(ppn52_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn52_2_fast_30_deviant[172] = NULL
          ppn52_2_fast_30_deviant[1] = NULL
          ppn52_2_fast_30_deviant = as.matrix(ppn52_2_fast_30_deviant)
          
          ppn52_2_fast_30_standard[172] = NULL
          ppn52_2_fast_30_standard[1] = NULL
          ppn52_2_fast_30_standard = as.matrix(ppn52_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_2_fast_30_deviant[38, 118:128]) - mean(ppn52_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn52_2_fast_60_deviant[172] = NULL
          ppn52_2_fast_60_deviant[1] = NULL
          ppn52_2_fast_60_deviant = as.matrix(ppn52_2_fast_60_deviant)
          
          ppn52_2_fast_60_standard[172] = NULL
          ppn52_2_fast_60_standard[1] = NULL
          ppn52_2_fast_60_standard = as.matrix(ppn52_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn52_2_fast_60_deviant[38, 103:113]) - mean(ppn52_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 54){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn54_1_slow_30_deviant[172] = NULL
          ppn54_1_slow_30_deviant[1] = NULL
          ppn54_1_slow_30_deviant = as.matrix(ppn54_1_slow_30_deviant)
          
          ppn54_1_slow_30_standard[172] = NULL
          ppn54_1_slow_30_standard[1] = NULL
          ppn54_1_slow_30_standard = as.matrix(ppn54_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_1_slow_30_deviant[38, 118:128]) - mean(ppn54_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn54_1_slow_60_deviant[172] = NULL
          ppn54_1_slow_60_deviant[1] = NULL
          ppn54_1_slow_60_deviant = as.matrix(ppn54_1_slow_60_deviant)
          
          ppn54_1_slow_60_standard[172] = NULL
          ppn54_1_slow_60_standard[1] = NULL
          ppn54_1_slow_60_standard = as.matrix(ppn54_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_1_slow_60_deviant[38, 103:113]) - mean(ppn54_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn54_1_fast_30_deviant[172] = NULL
          ppn54_1_fast_30_deviant[1] = NULL
          ppn54_1_fast_30_deviant = as.matrix(ppn54_1_fast_30_deviant)
          
          ppn54_1_fast_30_standard[172] = NULL
          ppn54_1_fast_30_standard[1] = NULL
          ppn54_1_fast_30_standard = as.matrix(ppn54_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_1_fast_30_deviant[38, 118:128]) - mean(ppn54_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn54_1_fast_60_deviant[172] = NULL
          ppn54_1_fast_60_deviant[1] = NULL
          ppn54_1_fast_60_deviant = as.matrix(ppn54_1_fast_60_deviant)
          
          ppn54_1_fast_60_standard[172] = NULL
          ppn54_1_fast_60_standard[1] = NULL
          ppn54_1_fast_60_standard = as.matrix(ppn54_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_1_fast_60_deviant[38, 103:113]) - mean(ppn54_1_fast_60_standard[38, 103:103])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn54_2_slow_30_deviant[172] = NULL
          ppn54_2_slow_30_deviant[1] = NULL
          ppn54_2_slow_30_deviant = as.matrix(ppn54_2_slow_30_deviant)
          
          ppn54_2_slow_30_standard[172] = NULL
          ppn54_2_slow_30_standard[1] = NULL
          ppn54_2_slow_30_standard = as.matrix(ppn54_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_2_slow_30_deviant[38, 118:128]) - mean(ppn54_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn54_2_slow_60_deviant[172] = NULL
          ppn54_2_slow_60_deviant[1] = NULL
          ppn54_2_slow_60_deviant = as.matrix(ppn54_2_slow_60_deviant)
          
          ppn54_2_slow_60_standard[172] = NULL
          ppn54_2_slow_60_standard[1] = NULL
          ppn54_2_slow_60_standard = as.matrix(ppn54_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_2_slow_60_deviant[38, 103:113]) - mean(ppn54_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn54_2_fast_30_deviant[172] = NULL
          ppn54_2_fast_30_deviant[1] = NULL
          ppn54_2_fast_30_deviant = as.matrix(ppn54_2_fast_30_deviant)
          
          ppn54_2_fast_30_standard[172] = NULL
          ppn54_2_fast_30_standard[1] = NULL
          ppn54_2_fast_30_standard = as.matrix(ppn54_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_2_fast_30_deviant[38, 118:128]) - mean(ppn54_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn54_2_fast_60_deviant[172] = NULL
          ppn54_2_fast_60_deviant[1] = NULL
          ppn54_2_fast_60_deviant = as.matrix(ppn54_2_fast_60_deviant)
          
          ppn54_2_fast_60_standard[172] = NULL
          ppn54_2_fast_60_standard[1] = NULL
          ppn54_2_fast_60_standard = as.matrix(ppn54_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn54_2_fast_60_deviant[38, 103:113]) - mean(ppn54_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 56){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn56_1_slow_30_deviant[172] = NULL
          ppn56_1_slow_30_deviant[1] = NULL
          ppn56_1_slow_30_deviant = as.matrix(ppn56_1_slow_30_deviant)
          
          ppn56_1_slow_30_standard[172] = NULL
          ppn56_1_slow_30_standard[1] = NULL
          ppn56_1_slow_30_standard = as.matrix(ppn56_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_1_slow_30_deviant[38, 118:128]) - mean(ppn56_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn56_1_slow_60_deviant[172] = NULL
          ppn56_1_slow_60_deviant[1] = NULL
          ppn56_1_slow_60_deviant = as.matrix(ppn56_1_slow_60_deviant)
          
          ppn56_1_slow_60_standard[172] = NULL
          ppn56_1_slow_60_standard[1] = NULL
          ppn56_1_slow_60_standard = as.matrix(ppn56_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_1_slow_60_deviant[38, 103:113]) - mean(ppn56_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn56_1_fast_30_deviant[172] = NULL
          ppn56_1_fast_30_deviant[1] = NULL
          ppn56_1_fast_30_deviant = as.matrix(ppn56_1_fast_30_deviant)
          
          ppn56_1_fast_30_standard[172] = NULL
          ppn56_1_fast_30_standard[1] = NULL
          ppn56_1_fast_30_standard = as.matrix(ppn56_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_1_fast_30_deviant[38, 118:128]) - mean(ppn56_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn56_1_fast_60_deviant[172] = NULL
          ppn56_1_fast_60_deviant[1] = NULL
          ppn56_1_fast_60_deviant = as.matrix(ppn56_1_fast_60_deviant)
          
          ppn56_1_fast_60_standard[172] = NULL
          ppn56_1_fast_60_standard[1] = NULL
          ppn56_1_fast_60_standard = as.matrix(ppn56_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_1_fast_60_deviant[38, 103:113]) - mean(ppn56_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn56_2_slow_30_deviant[172] = NULL
          ppn56_2_slow_30_deviant[1] = NULL
          ppn56_2_slow_30_deviant = as.matrix(ppn56_2_slow_30_deviant)
          
          ppn56_2_slow_30_standard[172] = NULL
          ppn56_2_slow_30_standard[1] = NULL
          ppn56_2_slow_30_standard = as.matrix(ppn56_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_2_slow_30_deviant[38, 118:128]) - mean(ppn56_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn56_2_slow_60_deviant[172] = NULL
          ppn56_2_slow_60_deviant[1] = NULL
          ppn56_2_slow_60_deviant = as.matrix(ppn56_2_slow_60_deviant)
          
          ppn56_2_slow_60_standard[172] = NULL
          ppn56_2_slow_60_standard[1] = NULL
          ppn56_2_slow_60_standard = as.matrix(ppn56_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_2_slow_60_deviant[38, 103:113]) - mean(ppn56_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn56_2_fast_30_deviant[172] = NULL
          ppn56_2_fast_30_deviant[1] = NULL
          ppn56_2_fast_30_deviant = as.matrix(ppn56_2_fast_30_deviant)
          
          ppn56_2_fast_30_standard[172] = NULL
          ppn56_2_fast_30_standard[1] = NULL
          ppn56_2_fast_30_standard = as.matrix(ppn56_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_2_fast_30_deviant[38, 118:128]) - mean(ppn56_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn56_2_fast_60_deviant[172] = NULL
          ppn56_2_fast_60_deviant[1] = NULL
          ppn56_2_fast_60_deviant = as.matrix(ppn56_2_fast_60_deviant)
          
          ppn56_2_fast_60_standard[172] = NULL
          ppn56_2_fast_60_standard[1] = NULL
          ppn56_2_fast_60_standard = as.matrix(ppn56_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn56_2_fast_60_deviant[38, 103:113]) - mean(ppn56_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 60){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn60_1_slow_30_deviant[172] = NULL
          ppn60_1_slow_30_deviant[1] = NULL
          ppn60_1_slow_30_deviant = as.matrix(ppn60_1_slow_30_deviant)
          
          ppn60_1_slow_30_standard[172] = NULL
          ppn60_1_slow_30_standard[1] = NULL
          ppn60_1_slow_30_standard = as.matrix(ppn60_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_1_slow_30_deviant[38, 118:128]) - mean(ppn60_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn60_1_slow_60_deviant[172] = NULL
          ppn60_1_slow_60_deviant[1] = NULL
          ppn60_1_slow_60_deviant = as.matrix(ppn60_1_slow_60_deviant)
          
          ppn60_1_slow_60_standard[172] = NULL
          ppn60_1_slow_60_standard[1] = NULL
          ppn60_1_slow_60_standard = as.matrix(ppn60_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_1_slow_60_deviant[38, 103:113]) - mean(ppn60_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn60_1_fast_30_deviant[172] = NULL
          ppn60_1_fast_30_deviant[1] = NULL
          ppn60_1_fast_30_deviant = as.matrix(ppn60_1_fast_30_deviant)
          
          ppn60_1_fast_30_standard[172] = NULL
          ppn60_1_fast_30_standard[1] = NULL
          ppn60_1_fast_30_standard = as.matrix(ppn60_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_1_fast_30_deviant[38, 118:128]) - mean(ppn60_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn60_1_fast_60_deviant[172] = NULL
          ppn60_1_fast_60_deviant[1] = NULL
          ppn60_1_fast_60_deviant = as.matrix(ppn60_1_fast_60_deviant)
          
          ppn60_1_fast_60_standard[172] = NULL
          ppn60_1_fast_60_standard[1] = NULL
          ppn60_1_fast_60_standard = as.matrix(ppn60_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_1_fast_60_deviant[38, 103:113]) - mean(ppn60_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn60_2_slow_30_deviant[172] = NULL
          ppn60_2_slow_30_deviant[1] = NULL
          ppn60_2_slow_30_deviant = as.matrix(ppn60_2_slow_30_deviant)
          
          ppn60_2_slow_30_standard[172] = NULL
          ppn60_2_slow_30_standard[1] = NULL
          ppn60_2_slow_30_standard = as.matrix(ppn60_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_2_slow_30_deviant[38, 118:128]) - mean(ppn60_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn60_2_slow_60_deviant[172] = NULL
          ppn60_2_slow_60_deviant[1] = NULL
          ppn60_2_slow_60_deviant = as.matrix(ppn60_2_slow_60_deviant)
          
          ppn60_2_slow_60_standard[172] = NULL
          ppn60_2_slow_60_standard[1] = NULL
          ppn60_2_slow_60_standard = as.matrix(ppn60_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_2_slow_60_deviant[38, 103:113]) - mean(ppn60_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn60_2_fast_30_deviant[172] = NULL
          ppn60_2_fast_30_deviant[1] = NULL
          ppn60_2_fast_30_deviant = as.matrix(ppn60_2_fast_30_deviant)
          
          ppn60_2_fast_30_standard[172] = NULL
          ppn60_2_fast_30_standard[1] = NULL
          ppn60_2_fast_30_standard = as.matrix(ppn60_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_2_fast_30_deviant[38, 118:128]) - mean(ppn60_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn60_2_fast_60_deviant[172] = NULL
          ppn60_2_fast_60_deviant[1] = NULL
          ppn60_2_fast_60_deviant = as.matrix(ppn60_2_fast_60_deviant)
          
          ppn60_2_fast_60_standard[172] = NULL
          ppn60_2_fast_60_standard[1] = NULL
          ppn60_2_fast_60_standard = as.matrix(ppn60_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn60_2_fast_60_deviant[38, 103:113]) - mean(ppn60_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 62){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn62_1_slow_30_deviant[172] = NULL
          ppn62_1_slow_30_deviant[1] = NULL
          ppn62_1_slow_30_deviant = as.matrix(ppn62_1_slow_30_deviant)
          
          ppn62_1_slow_30_standard[172] = NULL
          ppn62_1_slow_30_standard[1] = NULL
          ppn62_1_slow_30_standard = as.matrix(ppn62_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_1_slow_30_deviant[38, 118:128]) - mean(ppn62_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn62_1_slow_60_deviant[172] = NULL
          ppn62_1_slow_60_deviant[1] = NULL
          ppn62_1_slow_60_deviant = as.matrix(ppn62_1_slow_60_deviant)
          
          ppn62_1_slow_60_standard[172] = NULL
          ppn62_1_slow_60_standard[1] = NULL
          ppn62_1_slow_60_standard = as.matrix(ppn62_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_1_slow_60_deviant[38, 103:113]) - mean(ppn62_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn62_1_fast_30_deviant[172] = NULL
          ppn62_1_fast_30_deviant[1] = NULL
          ppn62_1_fast_30_deviant = as.matrix(ppn62_1_fast_30_deviant)
          
          ppn62_1_fast_30_standard[172] = NULL
          ppn62_1_fast_30_standard[1] = NULL
          ppn62_1_fast_30_standard = as.matrix(ppn62_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_1_fast_30_deviant[38, 118:128]) - mean(ppn62_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn62_1_fast_60_deviant[172] = NULL
          ppn62_1_fast_60_deviant[1] = NULL
          ppn62_1_fast_60_deviant = as.matrix(ppn62_1_fast_60_deviant)
          
          ppn62_1_fast_60_standard[172] = NULL
          ppn62_1_fast_60_standard[1] = NULL
          ppn62_1_fast_60_standard = as.matrix(ppn62_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_1_fast_60_deviant[38, 103:113]) - mean(ppn62_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn62_2_slow_30_deviant[172] = NULL
          ppn62_2_slow_30_deviant[1] = NULL
          ppn62_2_slow_30_deviant = as.matrix(ppn62_2_slow_30_deviant)
          
          ppn62_2_slow_30_standard[172] = NULL
          ppn62_2_slow_30_standard[1] = NULL
          ppn62_2_slow_30_standard = as.matrix(ppn62_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_2_slow_30_deviant[38, 118:128]) - mean(ppn62_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn62_2_slow_60_deviant[172] = NULL
          ppn62_2_slow_60_deviant[1] = NULL
          ppn62_2_slow_60_deviant = as.matrix(ppn62_2_slow_60_deviant)
          
          ppn62_2_slow_60_standard[172] = NULL
          ppn62_2_slow_60_standard[1] = NULL
          ppn62_2_slow_60_standard = as.matrix(ppn62_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_2_slow_60_deviant[38, 103:113]) - mean(ppn62_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn62_2_fast_30_deviant[172] = NULL
          ppn62_2_fast_30_deviant[1] = NULL
          ppn62_2_fast_30_deviant = as.matrix(ppn62_2_fast_30_deviant)
          
          ppn62_2_fast_30_standard[172] = NULL
          ppn62_2_fast_30_standard[1] = NULL
          ppn62_2_fast_30_standard = as.matrix(ppn62_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_2_fast_30_deviant[38, 118:128]) - mean(ppn62_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn62_2_fast_60_deviant[172] = NULL
          ppn62_2_fast_60_deviant[1] = NULL
          ppn62_2_fast_60_deviant = as.matrix(ppn62_2_fast_60_deviant)
          
          ppn62_2_fast_60_standard[172] = NULL
          ppn62_2_fast_60_standard[1] = NULL
          ppn62_2_fast_60_standard = as.matrix(ppn62_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn62_2_fast_60_deviant[38, 103:113]) - mean(ppn62_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 64){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn64_1_slow_30_deviant[172] = NULL
          ppn64_1_slow_30_deviant[1] = NULL
          ppn64_1_slow_30_deviant = as.matrix(ppn64_1_slow_30_deviant)
          
          ppn64_1_slow_30_standard[172] = NULL
          ppn64_1_slow_30_standard[1] = NULL
          ppn64_1_slow_30_standard = as.matrix(ppn64_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_1_slow_30_deviant[38, 118:128]) - mean(ppn64_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn64_1_slow_60_deviant[172] = NULL
          ppn64_1_slow_60_deviant[1] = NULL
          ppn64_1_slow_60_deviant = as.matrix(ppn64_1_slow_60_deviant)
          
          ppn64_1_slow_60_standard[172] = NULL
          ppn64_1_slow_60_standard[1] = NULL
          ppn64_1_slow_60_standard = as.matrix(ppn64_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_1_slow_60_deviant[38, 103:113]) - mean(ppn64_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn64_1_fast_30_deviant[172] = NULL
          ppn64_1_fast_30_deviant[1] = NULL
          ppn64_1_fast_30_deviant = as.matrix(ppn64_1_fast_30_deviant)
          
          ppn64_1_fast_30_standard[172] = NULL
          ppn64_1_fast_30_standard[1] = NULL
          ppn64_1_fast_30_standard = as.matrix(ppn64_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_1_fast_30_deviant[38, 118:128]) - mean(ppn64_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn64_1_fast_60_deviant[172] = NULL
          ppn64_1_fast_60_deviant[1] = NULL
          ppn64_1_fast_60_deviant = as.matrix(ppn64_1_fast_60_deviant)
          
          ppn64_1_fast_60_standard[172] = NULL
          ppn64_1_fast_60_standard[1] = NULL
          ppn64_1_fast_60_standard = as.matrix(ppn64_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_1_fast_60_deviant[38, 103:113]) - mean(ppn64_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn64_2_slow_30_deviant[172] = NULL
          ppn64_2_slow_30_deviant[1] = NULL
          ppn64_2_slow_30_deviant = as.matrix(ppn64_2_slow_30_deviant)
          
          ppn64_2_slow_30_standard[172] = NULL
          ppn64_2_slow_30_standard[1] = NULL
          ppn64_2_slow_30_standard = as.matrix(ppn64_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_2_slow_30_deviant[38, 118:128]) - mean(ppn64_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn64_2_slow_60_deviant[172] = NULL
          ppn64_2_slow_60_deviant[1] = NULL
          ppn64_2_slow_60_deviant = as.matrix(ppn64_2_slow_60_deviant)
          
          ppn64_2_slow_60_standard[172] = NULL
          ppn64_2_slow_60_standard[1] = NULL
          ppn64_2_slow_60_standard = as.matrix(ppn64_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_2_slow_60_deviant[38, 103:113]) - mean(ppn64_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn64_2_fast_30_deviant[172] = NULL
          ppn64_2_fast_30_deviant[1] = NULL
          ppn64_2_fast_30_deviant = as.matrix(ppn64_2_fast_30_deviant)
          
          ppn64_2_fast_30_standard[172] = NULL
          ppn64_2_fast_30_standard[1] = NULL
          ppn64_2_fast_30_standard = as.matrix(ppn64_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_2_fast_30_deviant[38, 118:128]) - mean(ppn64_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn64_2_fast_60_deviant[172] = NULL
          ppn64_2_fast_60_deviant[1] = NULL
          ppn64_2_fast_60_deviant = as.matrix(ppn64_2_fast_60_deviant)
        
          ppn64_2_fast_60_standard[172] = NULL
          ppn64_2_fast_60_standard[1] = NULL
          ppn64_2_fast_60_standard = as.matrix(ppn64_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn64_2_fast_60_deviant[38, 103:113]) - mean(ppn64_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
  if(D$PPN[i] == 66){
    if(D$Order[i] == 1){
      
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn66_1_slow_30_deviant[172] = NULL
          ppn66_1_slow_30_deviant[1] = NULL
          ppn66_1_slow_30_deviant = as.matrix(ppn66_1_slow_30_deviant)
        
          ppn66_1_slow_30_standard[172] = NULL
          ppn66_1_slow_30_standard[1] = NULL
          ppn66_1_slow_30_standard = as.matrix(ppn66_1_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_1_slow_30_deviant[38, 118:128]) - mean(ppn66_1_slow_30_standard[38, 118:128])
        }
        
        if(D$Tone_type[i] == 2){
          
          ppn66_1_slow_60_deviant[172] = NULL
          ppn66_1_slow_60_deviant[1] = NULL
          ppn66_1_slow_60_deviant = as.matrix(ppn66_1_slow_60_deviant)
          
          ppn66_1_slow_60_standard[172] = NULL
          ppn66_1_slow_60_standard[1] = NULL
          ppn66_1_slow_60_standard = as.matrix(ppn66_1_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_1_slow_60_deviant[38, 103:113]) - mean(ppn66_1_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn66_1_fast_30_deviant[172] = NULL
          ppn66_1_fast_30_deviant[1] = NULL
          ppn66_1_fast_30_deviant = as.matrix(ppn66_1_fast_30_deviant)
          
          ppn66_1_fast_30_standard[172] = NULL
          ppn66_1_fast_30_standard[1] = NULL
          ppn66_1_fast_30_standard = as.matrix(ppn66_1_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_1_fast_30_deviant[38, 118:128]) - mean(ppn66_1_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn66_1_fast_60_deviant[172] = NULL
          ppn66_1_fast_60_deviant[1] = NULL
          ppn66_1_fast_60_deviant = as.matrix(ppn66_1_fast_60_deviant)
          
          ppn66_1_fast_60_standard[172] = NULL
          ppn66_1_fast_60_standard[1] = NULL
          ppn66_1_fast_60_standard = as.matrix(ppn66_1_fast_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_1_fast_60_deviant[38, 103:113]) - mean(ppn66_1_fast_60_standard[38, 103:113])
        } 
      }
    }
    if(D$Order[i] == 2){
      if(D$Speed[i] == 1){
        
        if(D$Tone_type[i] == 1){
          
          ppn66_2_slow_30_deviant[172] = NULL
          ppn66_2_slow_30_deviant[1] = NULL
          ppn66_2_slow_30_deviant = as.matrix(ppn66_2_slow_30_deviant)
          
          ppn66_2_slow_30_standard[172] = NULL
          ppn66_2_slow_30_standard[1] = NULL
          ppn66_2_slow_30_standard = as.matrix(ppn66_2_slow_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_2_slow_30_deviant[38, 118:128]) - mean(ppn66_2_slow_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn66_2_slow_60_deviant[172] = NULL
          ppn66_2_slow_60_deviant[1] = NULL
          ppn66_2_slow_60_deviant = as.matrix(ppn66_2_slow_60_deviant)
          
          ppn66_2_slow_60_standard[172] = NULL
          ppn66_2_slow_60_standard[1] = NULL
          ppn66_2_slow_60_standard = as.matrix(ppn66_2_slow_60_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_2_slow_60_deviant[38, 103:113]) - mean(ppn66_2_slow_60_standard[38, 103:113])
        }
      }
      if(D$Speed[i] == 2){
        
        if(D$Tone_type[i] == 1){
          
          ppn66_2_fast_30_deviant[172] = NULL
          ppn66_2_fast_30_deviant[1] = NULL
          ppn66_2_fast_30_deviant = as.matrix(ppn66_2_fast_30_deviant)
          
          ppn66_2_fast_30_standard[172] = NULL
          ppn66_2_fast_30_standard[1] = NULL
          ppn66_2_fast_30_standard = as.matrix(ppn66_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_2_fast_30_deviant[38, 118:128]) - mean(ppn66_2_fast_30_standard[38, 118:128])
        }
        if(D$Tone_type[i] == 2){
          
          ppn66_2_fast_60_deviant[172] = NULL
          ppn66_2_fast_60_deviant[1] = NULL
          ppn66_2_fast_60_deviant = as.matrix(ppn66_2_fast_60_deviant)
          
          ppn66_2_fast_60_standard[172] = NULL
          ppn66_2_fast_60_standard[1] = NULL
          ppn66_2_fast_60_standard = as.matrix(ppn66_2_fast_30_standard)
          
          D$Mean_amplitude[i] = mean(ppn66_2_fast_60_deviant[38, 103:113]) - mean(ppn66_2_fast_60_standard[38, 103:113])
        } 
      }
    }
  }
}




##############################################################################################
###########################                   ANOVA                 ##########################
##############################################################################################

D$fsubject  <- paste("S",factor(D$PPN), sep="")
D$fgroup <- factor(D$Group, levels = c(1,2), labels = c("TD", "ASD"))
D$forder <- factor(D$Order)
D$fspeed <- factor(D$Speed, levels = c(1,2), labels = c("slow", "fast"))
D$ftonetype <- factor(D$Tone_type, levels = c(1,2), labels = c("30", "60"))
names(D)[6] <- "MeanAmplitude"

## BESTE MANIER
data.melt <- melt(D, id.var=c("fsubject","fgroup","forder","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt)
D.wide <- cast(fsubject + fgroup ~ forder + fspeed + ftonetype, data=data.melt)
head(D.wide)

names(D.wide)[3] <- "slow1_30"
names(D.wide)[4] <- "slow1_60"
names(D.wide)[5] <- "fast1_30"
names(D.wide)[6] <- "fast1_60"
names(D.wide)[7] <- "slow2_30"
names(D.wide)[8] <- "slow2_60"
names(D.wide)[9] <- "fast2_30"
names(D.wide)[10] <- "fast2_60"
head(D.wide)

order <- factor(rep(1:2, each=4))
speed <- factor(c(1, 1, 2, 2, 1, 1, 2, 2))
tonetype <- factor(c(1, 2, 1, 2, 1, 2, 1, 2))
idata <- data.frame(order,speed,tonetype)
idata

options(contrasts=c("contr.sum", "contr.poly"))
fit <- lm( cbind(slow1_30, slow1_60, fast1_30, fast1_60, slow2_30, slow2_60, fast2_30, fast2_60) ~ fgroup, data=D.wide)
out <- Anova(fit, type="III", test="Wilks", idata=idata, idesign=~order*speed*tonetype)
summary(out, multivariate=FALSE, univariate=TRUE)

######## overall effect
head(D)

fit_overall <- aov(MeanAmplitude ~ fgroup*forder*fspeed*ftonetype + Error(fsubject/(forder*fspeed*ftonetype)) + fgroup, data=D) 
fit_overall <- aov(MeanAmplitude ~ fgroup*forder*fspeed*ftonetype + Error(fgroup/fsubject/(forder*fspeed*ftonetype)), data=D)
# volgens Judith beter maar geeft warning
summary(fit_overall)

library(effects)
print(plot( effect("fgroup*forder*fspeed*ftonetype", fit_overall)))

printCoefmat(summary(fit_overall)[["coefficients"]])

######### TD
D_TD <- subset(D, Group == 1)
head(D_TD)

fitt_TD <- aov(MeanAmplitude ~ forder*fspeed*ftonetype + Error(fsubject/(forder*fspeed*ftonetype)), data=D_TD) 
summary(fitt_TD)

data.melt_TD <- melt(D_TD, id.var=c("fsubject", "forder","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt_TD)
D.wide_TD <- cast(fsubject ~ forder + fspeed + ftonetype, data=data.melt_TD)
D.wide_TD

names(D.wide_TD)[2] <- "slow1_30"
names(D.wide_TD)[3] <- "slow1_60"
names(D.wide_TD)[4] <- "fast1_30"
names(D.wide_TD)[5] <- "fast1_60"
names(D.wide_TD)[6] <- "slow2_30"
names(D.wide_TD)[7] <- "slow2_60"
names(D.wide_TD)[8] <- "fast2_30"
names(D.wide_TD)[9] <- "fast2_60"
head(D.wide_TD)

order <- factor(rep(1:2, each=4))
speed <- factor(c(1, 1, 2, 2, 1, 1, 2, 2))
tonetype <- factor(c(1, 2, 1, 2, 1, 2, 1, 2))
idata <- data.frame(order,speed,tonetype)
idata

options(contrasts=c("contr.sum", "contr.poly"))
fit_TD <- lm( cbind(slow1_30, slow1_60, fast1_30, fast1_60, slow2_30, slow2_60, fast2_30, fast2_60) ~ 1, data=D.wide_TD)
out_TD <- Anova(fit_TD, type="III", test="Wilks", idata=idata, idesign=~order*speed*tonetype)
summary(out_TD, multivariate=FALSE, univariate=TRUE)

######### D_TD order 1
D_TD_order1 <- subset(D_TD, Order == 1)
head(D_TD_order1)

data.melt_TD_order1 <- melt(D_TD_order1, id.var=c("fsubject","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt_TD_order1)
D.wide_TD_order1 <- cast(fsubject ~ fspeed + ftonetype, data=data.melt_TD_order1)
head(D.wide_TD_order1)

names(D.wide_TD_order1)[2] <- "slow_30"
names(D.wide_TD_order1)[3] <- "slow_60"
names(D.wide_TD_order1)[4] <- "fast_30"
names(D.wide_TD_order1)[5] <- "fast_60"
head(D.wide_TD_order1)

speed <- factor(c(1, 1, 2, 2))
tonetype <- factor(c(1, 2, 1, 2))
idata_TD_order1 <- data.frame(speed,tonetype)
idata_TD_order1

options(contrasts=c("contr.sum", "contr.poly"))
fit_TD_order1 <- lm( cbind(slow_30, slow_60, fast_30, fast_60) ~ 1, data=D.wide_TD_order1)
out_TD_order1 <- Anova(fit_TD_order1, type="III", test="Wilks", idata=idata_TD_order1, idesign=~speed*tonetype)
summary(out_TD_order1, multivariate=FALSE, univariate=TRUE)

######### D_TD order 2
D_TD_order2 <- subset(D_TD, Order == 2)
head(D_TD_order2)

data.melt_TD_order2 <- melt(D_TD_order2, id.var=c("fsubject","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt_TD_order2)
D.wide_TD_order2 <- cast(fsubject ~ fspeed + ftonetype, data=data.melt_TD_order2)
head(D.wide_TD_order2)

names(D.wide_TD_order2)[2] <- "slow_30"
names(D.wide_TD_order2)[3] <- "slow_60"
names(D.wide_TD_order2)[4] <- "fast_30"
names(D.wide_TD_order2)[5] <- "fast_60"
head(D.wide_TD_order2)

speed <- factor(c(1, 1, 2, 2))
tonetype <- factor(c(1, 2, 1, 2))
idata_TD_order2 <- data.frame(speed,tonetype)
idata_TD_order2

options(contrasts=c("contr.sum", "contr.poly"))
fit_TD_order2 <- lm( cbind(slow_30, slow_60, fast_30, fast_60) ~ 1, data=D.wide_TD_order2)
out_TD_order2 <- Anova(fit_TD_order2, type="III", test="Wilks", idata=idata_TD_order2, idesign=~speed*tonetype)
summary(out_TD_order2, multivariate=FALSE, univariate=TRUE)


######## ASD
D_ASD <- subset(D, Group == 2)
head(D_ASD)

fitt_ASD <- aov(MeanAmplitude ~ forder*fspeed*ftonetype + Error(fsubject/(forder*fspeed*ftonetype)), data=D_ASD) 
summary(fitt_ASD)

data.melt_ASD <- melt(D_ASD, id.var=c("fsubject", "forder","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt_ASD)
D.wide_ASD <- cast(fsubject ~ forder + fspeed + ftonetype, data=data.melt_ASD)
head(D.wide_ASD)

names(D.wide_ASD)[2] <- "slow1_30"
names(D.wide_ASD)[3] <- "slow1_60"
names(D.wide_ASD)[4] <- "fast1_30"
names(D.wide_ASD)[5] <- "fast1_60"
names(D.wide_ASD)[6] <- "slow2_30"
names(D.wide_ASD)[7] <- "slow2_60"
names(D.wide_ASD)[8] <- "fast2_30"
names(D.wide_ASD)[9] <- "fast2_60"
head(D.wide_ASD)

order <- factor(rep(1:2, each=4))
speed <- factor(c(1, 1, 2, 2, 1, 1, 2, 2))
tonetype <- factor(c(1, 2, 1, 2, 1, 2, 1, 2))
idata <- data.frame(order,speed,tonetype)
idata

options(contrasts=c("contr.sum", "contr.poly"))
fit_ASD <- lm( cbind(slow1_30, slow1_60, fast1_30, fast1_60, slow2_30, slow2_60, fast2_30, fast2_60) ~ 1, data=D.wide_ASD)
out_ASD <- Anova(fit_ASD, type="III", test="Wilks", idata=idata, idesign=~order*speed*tonetype)
summary(out_ASD, multivariate=FALSE, univariate=TRUE)


###########
## Plots ##
###########

data.melt <- melt(na.omit(D), id.var=c("fsubject","fgroup","forder","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt)

SEplot <- summarySE(D, measurevar="MeanAmplitude", groupvars=c("forder","ftonetype", "fspeed"))
SEplot
names(SEplot)[1] <- "order"
names(SEplot)[2] <- "tone"
names(SEplot)[3] <- "speed"
SEplot

SEplot_TD <- summarySE(D_TD, measurevar="MeanAmplitude", groupvars=c("forder","ftonetype", "fspeed"))
SEplot_TD
names(SEplot_TD)[1] <- "order"
names(SEplot_TD)[2] <- "tone"
names(SEplot_TD)[3] <- "speed"
SEplot_TD

###--------------------- main effect group (niet significant) --------------------###
data.plot.fgroup <- cast(data.melt, fgroup ~ ., mean)
names(data.plot.fgroup)[2] <- "MeanAmplitude"
data.plot.fgroup

print(xyplot(MeanAmplitude~fgroup, data.plot.fgroup, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Group", ylab = "Mean Amplitude", ylim = c(0, -5 )))
print(barchart(MeanAmplitude~fgroup, data.plot.fgroup, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Group", ylab = "Mean Amplitude", ylim = c(0, -5) ))

numbers = data.plot.fgroup$MeanAmplitude
numbers = matrix(numbers, nrow = 1, ncol = 2)
colnames(numbers) = c("TD", "ASD")

x = barplot(numbers, xlab = "Group", ylab = "Mean Amplitude", ylim = c(0,-5))

data.plot.fgroup.subject <- cast(data.melt, fgroup + fsubject ~ ., mean)
names(data.plot.fgroup.subject)[2] <- "MeanAmplitude"
names(data.plot.fgroup.subject)[3] <- "Subject"
data.plot.fgroup.subject
print(barchart(Subject~MeanAmplitude, groups = fgroup, data.plot.fgroup.subject, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T), xlab = "Subject", ylab = "Mean Amplitude" ))
# Grotere MMN bij TD

###-------------------- main effect order (marginaal significant) -------------------------#
data.plot.forder <- cast(data.melt, forder ~ ., mean)
names(data.plot.forder)[2] <- "MeanAmplitude"
data.plot.forder

print(xyplot(MeanAmplitude~forder, data.plot.forder, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Order", ylab = "Mean Amplitude", ylim = c(0, -5 )))
print(barchart(MeanAmplitude~forder, data.plot.forder, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Order", ylab = "Mean Amplitude", ylim = c(0, -5) ))

fill <- "cadetblue3"
ggplot(SEplot, aes(x = order, y = MeanAmplitude)) + geom_boxplot(fill = fill) + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("Main effect order") + scale_y_reverse(name= "Mean MMN amplitude")


# Grotere MMN in order 1

###--------------------- main effect speed --------------------###
data.plot.fspeed <- cast(data.melt, fspeed ~ ., mean)
names(data.plot.fspeed)[2] <- "MeanAmplitude"
data.plot.fspeed

print(xyplot(MeanAmplitude~fspeed, data.plot.fspeed, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Speed", ylab = "Mean Amplitude", ylim = c(0, -5 )))
print(barchart(MeanAmplitude~fspeed, data.plot.fspeed, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Speed", ylab = "Mean Amplitude", ylim = c(0, -5) ))

ggplot(SEplot, aes(x = speed, y = MeanAmplitude)) + geom_boxplot(fill = fill) + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("Main effect speed") + scale_y_reverse(name= "Mean MMN amplitude")


###--------------------- main effect tonetype -----------------###
data.plot.ftonetype <- cast(data.melt, ftonetype ~ ., mean)
names(data.plot.ftonetype)[2] <- "MeanAmplitude"
data.plot.ftonetype

data.plot.ftonetype_TD <- cast(data.melt_TD, ftonetype ~., mean)
names(data.plot.ftonetype_TD)[2] <- "MeanAmplitude"

print(xyplot(MeanAmplitude~ftonetype, data.plot.ftonetype, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Tone type", ylab = "Mean Amplitude", ylim = c(0, -5 )))

print(xyplot(MeanAmplitude~ftonetype, data.plot.ftonetype_TD, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Tone type", ylab = "Mean Amplitude", ylim = c(0, -5 )))


print(barchart(MeanAmplitude~ftonetype, data.plot.ftonetype, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Tone type", ylab = "Mean Amplitude", ylim = c(0, -5) ))

ggplot(SEplot, aes(x = tone, y = MeanAmplitude)) + geom_boxplot(fill = fill) + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("Main effect tone type") + scale_y_reverse(name = "Mean MMN amplitude")

ggplot(SEplot_TD, aes(x = tone, y = MeanAmplitude)) + geom_boxplot(fill = fill) + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("Main effect tone type TD") + scale_y_reverse(name = "Mean MMN amplitude")



###--------------------- interaction order:tonetype ------------------###
# interaction effect category - proficiency 
data.plot.forderftonetype    <- cast(data.melt, forder*ftonetype ~ ., mean)
names(data.plot.forderftonetype)[3] <- "MeanAmplitude"
data.plot.forderftonetype
#xyplot(MeanAmplitude ~ ftonetype , groups=fgroup, type=c("p", "r"),auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroupftonetype)
print(xyplot(MeanAmplitude ~ forder, groups = ftonetype, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.forderftonetype, ylim = c(0, -5), main = "order x tonetype", xlab = "order", ylab = "Mean MMN amplitude"))

ggplot(SEplot, aes(x=order ,y=MeanAmplitude,color=tone,group=tone)) + ylim(1, -9) + geom_errorbar(aes(ymin=MeanAmplitude-se, ymax=MeanAmplitude+se), width=.1) + geom_line() + geom_point() + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("Group x order x tone type")


# MMN op 60 is veel groter bij TD dan ASD. MMN op 30 lijkt ietsje kleinr bij TD dan ASD.

###------------------- interaction group:speed:tonetype --------------###
data.plot.fgroupfspeedftonetype         <- cast(data.melt, fgroup*fspeed*ftonetype ~ ., mean)
names(data.plot.fgroupfspeedftonetype)[4] <- "MeanAmplitude"
data.plot.fgroupfspeedftonetype

print(xyplot(MeanAmplitude ~ fspeed|fgroup, groups=ftonetype, type=c("g", "b"),auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroupfspeedftonetype, ylim = c(0, -6), main = "Group x speed x tone type", xlab = "speed", ylab = "Mean MMN amplitude"))

SEplot_group <- summarySE(D, measurevar="MeanAmplitude", groupvars=c("fgroup", "forder","ftonetype", "fspeed"))
SEplot_group
names(SEplot_group)[1] <- "group"
names(SEplot_group)[2] <- "order"
names(SEplot_group)[3] <- "tone"
names(SEplot_group)[4] <- "speed"
SEplot_group

ggplot(SEplot_group, aes(x=order ,y=MeanAmplitude,color=tone,group=tone)) + ylim(1, -9) + geom_errorbar(aes(ymin=MeanAmplitude-se, ymax=MeanAmplitude+se), width=.1) + geom_line() + geom_point() + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("group x speed x tonetype") + facet_grid(~group,labeller = "label_both")


###------------------- interaction order:tonetype:speed (not significant, maar hadden we wel gehoopt) --------------###
data.plot.forderftonetypefspeed        <- cast(data.melt, forder*ftonetype*fspeed ~ ., mean)
names(data.plot.forderftonetypefspeed)[4] <- "MeanAmplitude"
data.plot.forderftonetypefspeed

print(xyplot(MeanAmplitude ~ fspeed|forder, groups=ftonetype, type=c("g", "b"),auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.forderftonetypefspeed, ylim = c(0, -5)))

############ verschil interactie order:speed:tonetype tussen TD en ASD #############
data.melt_TD <- melt(na.omit(D_TD), id.var=c("fsubject","fgroup","forder","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt_TD)

data.melt_ASD <- melt(na.omit(D_ASD), id.var=c("fsubject","fgroup","forder","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt_ASD)

#------- TD --------#
data.plot.forderftonetypefspeed_TD        <- cast(data.melt_TD, forder*ftonetype*fspeed ~ ., mean)
names(data.plot.forderftonetypefspeed_TD)[4] <- "MeanAmplitude"
data.plot.forderftonetypefspeed_TD

print(xyplot(MeanAmplitude ~ fspeed|forder, groups=ftonetype, type=c("g", "b"),auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.forderftonetypefspeed_TD, ylim = c(0, -10), xlab = "speed"))


library(Rmisc)
library(ggplot2)
SEplot_TD <- summarySE(D_TD, measurevar="MeanAmplitude", groupvars=c("forder","ftonetype", "fspeed"))
SEplot_TD
names(SEplot_TD)[1] <- "order"
names(SEplot_TD)[2] <- "tone"
names(SEplot_TD)[3] <- "speed"
SEplot_TD

ggplot(SEplot_TD, aes(x=speed ,y=MeanAmplitude,color=tone,group=tone)) + ylim(1, -9) + geom_errorbar(aes(ymin=MeanAmplitude-se, ymax=MeanAmplitude+se), width=.1) + geom_line() + geom_point() +facet_grid(~order,labeller = "label_both") + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("TD") + ylab("Mean MMN amplitude")


#-------- ASD --------#
data.plot.forderftonetypefspeed_ASD        <- cast(data.melt_ASD, forder*ftonetype*fspeed ~ ., mean)
names(data.plot.forderftonetypefspeed_ASD)[4] <- "MeanAmplitude"
data.plot.forderftonetypefspeed_ASD

print(xyplot(MeanAmplitude ~ fspeed|forder, groups=ftonetype, type=c("g", "b"),auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.forderftonetypefspeed_ASD, ylim = c(0, -5), xlab = "speed"))

library(Rmisc)
library(ggplot2)
SEplot_ASD <- summarySE(D_ASD, measurevar="MeanAmplitude", groupvars=c("forder","ftonetype", "fspeed"))
SEplot_ASD
names(SEplot_ASD)[1] <- "order"
names(SEplot_ASD)[2] <- "tone"
names(SEplot_ASD)[3] <- "speed"
SEplot_ASD

ggplot(SEplot_ASD,aes(x=speed ,y=MeanAmplitude,color=tone,group=tone))  + ylim(1, -9) + geom_errorbar(aes(ymin=MeanAmplitude-se, ymax=MeanAmplitude+se), width=.1) + geom_line() + geom_point() + facet_grid(~order,labeller = "label_both") + theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + ggtitle("ASD") + ylab("Mean MMN amplitude")

  # main order ASD #
  data.plot.forder_ASD <- cast(data.melt_ASD, forder ~ ., mean)
  names(data.plot.forder_ASD)[2] <- "MeanAmplitude"
  data.plot.forder_ASD

  print(xyplot(MeanAmplitude~forder, data.plot.forder_ASD, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Order", ylab = "Mean Amplitude", ylim = c(0, -5 )))

  # main speed ASD #
  data.plot.fspeed_ASD <- cast(data.melt_ASD, fspeed ~ ., mean)
  names(data.plot.fspeed_ASD)[2] <- "MeanAmplitude"
  data.plot.fspeed_ASD

  print(xyplot(MeanAmplitude~fspeed, data.plot.fspeed_ASD, type=c("l","g"), auto.key=list(corner=c(0.2,0.2),lines=T),  xlab = "Speed", ylab = "Mean Amplitude", ylim = c(0, -5 )))

  # interaction order:speed ASD #
  data.plot.forderfspeed_ASD    <- cast(data.melt_ASD, forder*fspeed ~ ., mean)
  names(data.plot.forderfspeed_ASD)[3] <- "MeanAmplitude"
  data.plot.forderfspeed_ASD
  print(xyplot(MeanAmplitude ~ forder, groups = fspeed, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.forderfspeed_ASD, ylim = c(0, -5)))

  # interaction order:tonetype ASD #
  data.plot.forderftonetype_ASD    <- cast(data.melt_ASD, forder*ftonetype ~ ., mean)
  names(data.plot.forderftonetype_ASD)[3] <- "MeanAmplitude"
  data.plot.forderftonetype_ASD
  print(xyplot(MeanAmplitude ~ forder, groups = ftonetype, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.forderftonetype_ASD, ylim = c(0, -5))) 

  # interaction speed:tonetype ASD #
  data.plot.fspeedftonetype_ASD    <- cast(data.melt_ASD, fspeed*ftonetype ~ ., mean)
  names(data.plot.fspeedftonetype_ASD)[3] <- "MeanAmplitude"
  data.plot.fspeedftonetype_ASD
  print(xyplot(MeanAmplitude ~ fspeed, groups = ftonetype, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fspeedftonetype_ASD, ylim = c(0, -5))) 

#-------- order 1 TD --------#
data.melt_TDorder1 <- melt(na.omit(D_TD_order1), id.var=c("fsubject","fgroup","fspeed", "ftonetype"),measure.var=c("MeanAmplitude"))
head(data.melt_TDorder1)

data.plot.fspeedftonetype_TDorder1 <- cast(data.melt_TDorder1, fspeed*ftonetype ~., mean)
names(data.plot.fspeedftonetype_TDorder1)[3] <- "MeanAmplitude"
data.plot.fspeedftonetype_TDorder1

xyplot(MeanAmplitude ~ fspeed, groups=ftonetype, type=c("g", "b"),auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fspeedftonetype_TDorder1, ylim = c(0, -10))
