#-----------------------------------------------------------------------#
#              This R script was written by Tam Nguyen                  #
# The function of this script is to detect land use change at           #
# at HRU levels at the subbasin                                         #
# For comments and suggestions, please contact tamnva@gmail.com         #
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
# USER DEFINE PARAMETER - PLEASE CHANGE ONLY WITHIN THIS PART           # 	

# INPUT FILE ABOUT HRU INFORMATION
	Lu <- 0                            # don't change this line
	Lu[1] <- "D:/landuse/wipperau_1/Scenarios/Default/TxtInOut"    # e.g. HRU map 2000
	Lu[2] <- "D:/landuse/wipperau_2/Scenarios/Default/TxtInOut"    # e.g. HRU map 2010  
	Lu[3] <- "D:/landuse/wipperau_3/Scenarios/Default/TxtInOut"    # e.g. HRU map 2020

	ofolder <- "D:/landuse/luc"   # Diretory to store new TxtInOut
	ifolder <- "D:/landuse"
	luc_all <- read.csv("D:/landuse/luc_all.txt", header = TRUE, sep = "")					   

#         END USER DEFINE PARAMETER                                     #
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
#Function to convert subbasin or HRUs number to character
sub_character <- function(sub) {
	if (sub < 10){
		sub_str <- paste("0000", as.character(sub), sep = "")
	} else if (sub < 100) {
		 sub_str <- paste("000", as.character(sub), sep = "")
	} else if (sub < 1000) {
	 	sub_str <- paste("00", as.character(sub), sep = "")
	} else if (sub < 10000) {
		sub_str <- paste("0", as.character(sub), sep = "")
	} else if (sub < 100000) {
		sub_str <- paste(as.character(sub), sep = "")
	}
  return(sub_str)
}	

hru_character <- function(hru) {
	if (hru< 10){
		hru_str <- paste("000", as.character(hru), sep = "")
	} else if (hru< 100) {
		 hru_str <- paste("00", as.character(hru), sep = "")
	} else if (hru< 1000) {
	 	hru_str <- paste("0", as.character(hru), sep = "")
	} else if (hru< 10000) {
		hru_str <- paste(as.character(hru), sep = "")
	}
  return(hru_str)
}
#-----------------------------------------------------------------------#


#-----------------------------------------------------------------------#
# Copy the common file for watershed and subbasin
	
	lfiles <- list.files(Lu[1], pattern=NULL, all.files=TRUE, full.names = TRUE)
	file.copy(lfiles, ofolder)
	#lfiles <- lfiles[-grep(".hru", lfiles)]
	#lfiles <- lfiles[-grep(".mgt", lfiles)]
#-----------------------------------------------------------------------#

#-------------------------------------------------------------------.chm#
# Copy and edit HRU related files
	lfiles <- list.files(Lu[2], pattern=".chm", all.files=TRUE, full.names = FALSE)
	
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){	

	nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub) 
	nhrus <- max(nluc_all$HRU[nluc_all$SUBBASIN == sub]) + 1
	nhrus_updated <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub

	#Find the new .chm in which folder
	
	for (i in nhrus:nhrus_updated){
		k <- 0
		for (j in 1:length(Lu)){
			loc <- 6+j+k
			if (as.vector(nluc_all[6+j+k])[i,1] > 0.0){
				loc <- j + 1

				file <- paste(Lu[loc], "/", sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".chm",  sep = "")
				file.copy(file, ifolder)				
				
				setwd(ifolder)
				file.rename(paste(sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".chm",  sep = ""), 
						paste(sub_character(sub), hru_character(i), ".chm",  sep = ""))

				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".chm",  sep = ""), ofolder)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".chm",  sep = ""))
				break  		
			} else {
				k = k + 1
			}
		}
	}
	}
#-------------------------------------------------------------------.chm#

#-------------------------------------------------------------------.gw#
# Copy and edit HRU related files
	lfiles <- list.files(Lu[2], pattern=".gw", all.files=TRUE, full.names = FALSE)
	
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){	

	nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub) 
	nhrus <- max(nluc_all$HRU[nluc_all$SUBBASIN == sub]) + 1
	nhrus_updated <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub

	#Find the new .gw in which folder
	
	for (i in nhrus:nhrus_updated){
		k <- 0
		for (j in 1:length(Lu)){
			loc <- 6+j+k
			if (as.vector(nluc_all[6+j+k])[i,1] > 0.0){
				loc <- j + 1

				file <- paste(Lu[loc], "/", sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".gw",  sep = "")
				file.copy(file, ifolder)				
				
				setwd(ifolder)
				file.rename(paste(sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".gw",  sep = ""), 
						paste(sub_character(sub), hru_character(i), ".gw",  sep = ""))

				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".gw",  sep = ""), ofolder)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".gw",  sep = ""))
				break  		
			} else {
				k = k + 1
			}
		}
	}
	}
#-------------------------------------------------------------------.gw#

#-------------------------------------------------------------------.sol#
# Copy and edit HRU related files
	lfiles <- list.files(Lu[2], pattern=".sol", all.files=TRUE, full.names = FALSE)
	
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){	

	nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub) 
	nhrus <- max(nluc_all$HRU[nluc_all$SUBBASIN == sub]) + 1
	nhrus_updated <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub

	#Find the new .sol in which folder
	
	for (i in nhrus:nhrus_updated){
		k <- 0
		for (j in 1:length(Lu)){
			loc <- 6+j+k
			if (as.vector(nluc_all[6+j+k])[i,1] > 0.0){
				loc <- j + 1

				file <- paste(Lu[loc], "/", sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".sol",  sep = "")
				file.copy(file, ifolder)				
				
				setwd(ifolder)
				file.rename(paste(sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".sol",  sep = ""), 
						paste(sub_character(sub), hru_character(i), ".sol",  sep = ""))

				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".sol",  sep = ""), ofolder)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".sol",  sep = ""))
				break  		
			} else {
				k = k + 1
			}
		}
	}
	}
#-------------------------------------------------------------------.sol#

#-------------------------------------------------------------------.sep#
# Copy and edit HRU related files
	lfiles <- list.files(Lu[2], pattern=".sep", all.files=TRUE, full.names = FALSE)
	
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){	

	nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub) 
	nhrus <- max(nluc_all$HRU[nluc_all$SUBBASIN == sub]) + 1
	nhrus_updated <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub

	#Find the new .sep in which folder
	
	for (i in nhrus:nhrus_updated){
		k <- 0
		for (j in 1:length(Lu)){
			loc <- 6+j+k
			if (as.vector(nluc_all[6+j+k])[i,1] > 0.0){
				loc <- j + 1

				file <- paste(Lu[loc], "/", sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".sep",  sep = "")
				file.copy(file, ifolder)				
				
				setwd(ifolder)
				file.rename(paste(sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".sep",  sep = ""), 
						paste(sub_character(sub), hru_character(i), ".sep",  sep = ""))

				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".sep",  sep = ""), ofolder)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".sep",  sep = ""))
				break  		
			} else {
				k = k + 1
			}
		}
	}
	}
#-------------------------------------------------------------------.sep#

#-------------------------------------------------------------------.sdr#
# Copy and edit HRU related files
	lfiles <- list.files(Lu[2], pattern=".sdr", all.files=TRUE, full.names = FALSE)
	
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){	

	nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub) 
	nhrus <- max(nluc_all$HRU[nluc_all$SUBBASIN == sub]) + 1
	nhrus_updated <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub

	#Find the new .sdr in which folder
	
	for (i in nhrus:nhrus_updated){
		k <- 0
		for (j in 1:length(Lu)){
			loc <- 6+j+k
			if (as.vector(nluc_all[6+j+k])[i,1] > 0.0){
				loc <- j + 1

				file <- paste(Lu[loc], "/", sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".sdr",  sep = "")
				file.copy(file, ifolder)				
				
				setwd(ifolder)
				file.rename(paste(sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".sdr",  sep = ""), 
						paste(sub_character(sub), hru_character(i), ".sdr",  sep = ""))

				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".sdr",  sep = ""), ofolder)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".sdr",  sep = ""))
				break  		
			} else {
				k = k + 1
			}
		}
	}
	}
#-------------------------------------------------------------------.sdr#

#-------------------------------------------------------------------.mgt#
# Copy and edit HRU related files
	lfiles <- list.files(Lu[2], pattern=".mgt", all.files=TRUE, full.names = FALSE)
	
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){	

	nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub) 
	nhrus <- max(nluc_all$HRU[nluc_all$SUBBASIN == sub]) + 1
	nhrus_updated <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub

	#Find the new .mgt in which folder
	
	for (i in nhrus:nhrus_updated){
		k <- 0
		for (j in 1:length(Lu)){
			loc <- 6+j+k
			if (as.vector(nluc_all[6+j+k])[i,1] > 0.0){
				loc <- j + 1

				file <- paste(Lu[loc], "/", sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".mgt",  sep = "")
				file.copy(file, ifolder)				
				
				setwd(ifolder)
				file.rename(paste(sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".mgt",  sep = ""), 
						paste(sub_character(sub), hru_character(i), ".mgt",  sep = ""))

				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".mgt",  sep = ""), ofolder)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".mgt",  sep = ""))
				break  		
			} else {
				k = k + 1
			}
		}
	}
	}
#-------------------------------------------------------------------.mgt#

#-------------------------------------------------------------------.hru#
# Copy and edit HRU related files
	lfiles <- list.files(Lu[2], pattern=".hru", all.files=TRUE, full.names = FALSE)
	
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){	

	nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub) 
	nhrus <- max(nluc_all$HRU[nluc_all$SUBBASIN == sub]) + 1
	nhrus_updated <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub

	#Find the new .hru in which folder
	
	for (i in nhrus:nhrus_updated){
		k <- 0
		for (j in 1:length(Lu)){
			loc <- 6+j+k
			if (as.vector(nluc_all[6+j+k])[i,1] > 0.0){
				loc <- j + 1

				file <- paste(Lu[loc], "/", sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".hru",  sep = "")
				file.copy(file, ifolder)				
				
				setwd(ifolder)
				file.rename(paste(sub_character(sub), hru_character(as.vector(nluc_all[6+j+k])[i,1]), ".hru",  sep = ""), 
						paste(sub_character(sub), hru_character(i), ".hru",  sep = ""))
				
				#change content of the .hru files
				read_hru = readLines(paste(sub_character(sub), hru_character(i), ".hru",  sep = ""),-1)
				read_hru[2]="       0.0000000    | HRU_FR : Fraction of subbasin area contained in HRU"
				writeLines(read_hru,paste(sub_character(sub), hru_character(i), ".hru",  sep = ""))
				
				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".hru",  sep = ""), ofolder)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".hru",  sep = ""))
				break  		
			} else {
				k = k + 1
			}
		}
	}
	}
#-------------------------------------------------------------------.hru#



