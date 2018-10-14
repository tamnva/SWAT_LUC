#-----------------------------------------------------------------------#
# Initilize parameter 
	rm(list=ls())
	Lu <- c(0)                         # don't change this line
	inter_date <- list()               # don't change this line
#-----------------------------------------------------------------------#


#***********************************************************************#
# USER DEFINE PARAMETER - PLEASE CHANGE ONLY WITHIN THIS PART           # 	

	Lu[1] <- "D:/landuse/wipperau_1/Scenarios/Default/TxtInOut"    # e.g. TxtInout folder with land use 1990
	Lu[2] <- "D:/landuse/wipperau_2/Scenarios/Default/TxtInOut"    # e.g. TxtInout folder with land use 1995  
	Lu[3] <- "D:/landuse/wipperau_3/Scenarios/Default/TxtInOut"    # e.g. TxtInout folder with land use 2000 
                                                                     # add Lu[4] or remove Lu[3] if needed

	date <- c("01011990", "01011995", "01012000") 			   # Date (ddmmyyyy) of the three land use above
	inter_date[[1]] <- c("01011991", "01011992", "01011993")       # change to inter_date[[1]] <- c("NA") if you don't need to interpolate
	inter_date[[2]] <- c("01011996", "01011999")	               # change to inter_date[[2]] <- c("NA") if you don't need to interpolate


	ofolder <- "D:/landuse/luc"                                    # Diretory to store new TxtInOut
	hru_lu_all <- "D:/landuse/hru_lu_all.txt"			         # Location and the file name created from step 2   

# END USER DEFINE PARAMETER                                             #
#***********************************************************************#

#-----------------------------------------------------------------------#
#do.call(file.remove, list(list.files(ofolder, full.names = TRUE)))

dir.create(path2<-file.path(ofolder, "deleteme"), showWarnings = FALSE)
ifolder <- paste(ofolder, "/deleteme", sep = "")

luc_all <- read.csv(hru_lu_all, header = TRUE, sep = "")

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
#Function to write HRU related file names
hru_general <- function(sub, hru){
	name <- paste(sub_character(sub), hru_character(i), sep = "")
	file <- paste(name, ".hru", name, ".mgt", name, ".sol", name, ".chm", " ", name, ".gw", "             ", name, ".sep", sep = "")
	return(file)
}
#-----------------------------------------------------------------------#


#-----------------------------------------------------------------------#
# Copy the common file for watershed and subbasin
	
	lfiles <- list.files(Lu[1], pattern=NULL, all.files=TRUE, full.names = TRUE)
	file.copy(lfiles, ofolder, overwrite = TRUE)

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
	if (nhrus_updated >= nhrus) {
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

				file.copy(paste(ifolder, "/", sub_character(sub), hru_character(i), ".chm",  sep = ""), ofolder, overwrite = TRUE)
				file.remove(paste(ifolder, "/", sub_character(sub), hru_character(i), ".chm",  sep = ""))

				break  
						
			} else {
				k = k + 1
			}
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
	if (nhrus_updated >= nhrus) {
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
	if (nhrus_updated >= nhrus) {
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
	if (nhrus_updated >= nhrus) {
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
	if (nhrus_updated >= nhrus) {
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
	if (nhrus_updated >= nhrus) {
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
	if (nhrus_updated >= nhrus) {
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
				read_hru <- readLines(paste(sub_character(sub), hru_character(i), ".hru",  sep = ""),-1)
				read_hru[2] <- "       0.0000000    | HRU_FR : Fraction of subbasin area contained in HRU"
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
	}
#-------------------------------------------------------------------.hru#


#-------------------------------------------------------------------.sub#
	nsub = max(luc_all$SUBBASIN)
	for (sub in 1:nsub){

		setwd(Lu[1])
		nluc_all <- subset(luc_all, luc_all$SUBBASIN == sub)
		read_sub <- readLines(paste(sub_character(sub), "0000.sub",  sep = ""),-1)
		nhrus_sub <- sum(nluc_all$SUBBASIN[nluc_all$SUBBASIN == sub])/sub
		read_sub[53] <- paste("            ", nhrus_sub,"     | HRUTOT : Total number of HRUs modeled in subbasin", sep = "")
		
		for (i in 1:nhrus_sub){
			read_sub[61+i] <- hru_general(sub, i)
		}

		setwd(ofolder)
		writeLines(read_sub,paste(sub_character(sub), "0000.sub",  sep = ""))
	}




#-------------------------------------------------------------------.sub#

#-----rewrite lup.dat file
	dm <- function(d){
		if(d < 10){
			char <- paste("    ", as.character(d), sep = "")
		} else {
			char <- paste("   ", as.character(d), sep = "")
		}
		return(char)
	}

	setwd(ofolder)

	for (i in 2:length(Lu)){

		#dd <- as.numeric(substr(date[i], 1, 2))
		#mm <- as.numeric(substr(date[ii], 3, 4))
		#yyyy <- as.numeric(substr(date[ii], 5, 8))

		lup_dat <- paste(date[i],".dat", sep ="")

		#read_lup[i-1+nn] <- paste(dm(i-1+nn), dm(dd), dm(mm), " ", yyyy, "  ", lup_dat, sep = "")


		cat(NULL,file = lup_dat)
		read_hru <- readLines("lup.dat",-1)
		
		read_hru[1] <- "HRU_number   HRU_FR"
		for (k in 1:length(luc_all[[1]])) {
			read_hru[k+1] <- paste(k, format(luc_all[[i*2+4]][k], digits = 5), sep = "   ")
		}

		writeLines(read_hru,lup_dat)

		# Interpolated land use between two input years
		if (inter_date[[i-1]][1] != "NA") {
			for (m in 1:length(inter_date[[i-1]])){
				lup_dat_inter <- paste(inter_date[[i-1]][m],".dat", sep ="")
				cat(NULL,file = lup_dat_inter)
				read_hru_inter <- readLines(lup_dat_inter,-1)
				read_hru_inter[1] <- "HRU_number   HRU_FR"

				for (q in 1:length(luc_all[[1]])) {
					hru_fr_inter <- luc_all[[i*2+2]][q] + (luc_all[[i*2+4]][q] - luc_all[[i*2+2]][q])*(as.numeric(as.Date(inter_date[[i-1]][m], format('%d%m%Y')) - as.Date(date[i-1], format('%d%m%Y')))/as.numeric(as.Date(date[i], format('%d%m%Y')) - as.Date(date[i-1], format('%d%m%Y'))))
					read_hru_inter[q+1] <- paste(q, format(hru_fr_inter, digits = 5), sep = "   ")   
				}
				writeLines(read_hru_inter,lup_dat_inter)	
			}
			#nn = nn + 1
			#read_lup[i-1+nn] <- paste(dm(i-1+nn), dm(as.numeric(substr(inter_date[[i-1]][m], 1, 2))), dm(as.numeric(substr(inter_date[[i-1]][m], 3, 4))), " ", as.numeric(substr(inter_date[[i-1]][m], 5, 8)), "  ", lup_dat_inter, ".dat", sep = "")
			#print(nn)
		}
		# End interpolation
	}


	read_lup <- readLines("lup.dat",-1)
	counter <- 0

	for (ii in 2:length(date)){
		if (inter_date[[ii-1]][1] != "NA") {
			for (jj in 1:length(inter_date[[ii-1]])){
				counter = counter + 1
				read_lup[counter] <- paste(dm(counter), dm(as.numeric(substr(inter_date[[ii-1]][jj], 1, 2))), dm(as.numeric(substr(inter_date[[ii-1]][jj], 3, 4))), " ", dm(as.numeric(substr(inter_date[[ii-1]][jj], 5, 8))), "  ", inter_date[[ii-1]][jj], ".dat", sep = "")
			}			
		}
		counter = counter + 1
		read_lup[counter] <- paste(dm(counter), dm(as.numeric(substr(date[ii], 1, 2))), dm(as.numeric(substr(date[ii], 3, 4))), " ", dm(as.numeric(substr(date[ii], 5, 8))), "  ", paste(date[ii],".dat", sep =""), sep = "")
	}

	writeLines(read_lup,"lup.dat")

