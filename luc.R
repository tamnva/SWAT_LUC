#-----------------------------------------------------------------------#
#              This R script was written by Tam Nguyen                  #
# The function of this script is to detect land use change at           #
# at HRU levels at the subbasin                                         #
# For comments and suggestions, please contact tamnva@gmail.com         #
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
# USER DEFINE PARAMETER - PLEASE CHANGE ONLY WITHIN THIS PART           # 
	Lu <- 0                            # don't change this line
	Lu[1] <- "D:/landuse/luc_1.txt"    # e.g. HRU map 2000
	Lu[2] <- "D:/landuse/luc_2.txt"    # e.g. HRU map 2010  
	Lu[3] <- "D:/landuse/luc_3.txt"    # e.g. HRU map 2010  
#         END USER DEFINE PARAMETER                                     #
#-----------------------------------------------------------------------#

# Set working directory
	setwd("D:/landuse")

# Number of land use change scenario
	nluc <- length(Lu)   	

# Read HRUs information for each LU
	hru <- list()		
	for (i in 1:nluc){			
		hru[[i]] <- read.csv(Lu[i], header = TRUE, sep = "")
	}

# Number of subbasins
	nsub <- max(hru[[1]]$SUBBASIN)  	

# Number of HRUs in each sub & LU change scenario 
	#nhru_sub <- matrix(NA, nsub , nluc)
	new_hru <- list()
	

	subset_hru <- list()
	m <- NA
	n <- NA
	k <- NA


	for (i in 1:nsub){ #change i in 1:nsub

		for (j in 1:nluc){
			subset_hru[[j]] <- subset(hru[[j]], hru[[j]]$SUBBASIN == i) 
		}

		# Lookup HRU in LU scenario i and i + 1
		new_hru <- subset_hru[[1]] 
		
		p <- 0
		for (n in 2:nluc){

			new_hru[[5+n+p]] <- NA
			new_hru[[6+n+p]] <- NA
			
			for (m in 1:length(subset_hru[[n]]$HRU)){
				check <- 0
				for (k in 1:length(new_hru$HRU)){
					if ((as.vector(subset_hru[[n]]$LANDUSE)[m] == as.vector(new_hru$LANDUSE)[k]) &
					    (as.vector(subset_hru[[n]]$SOIL)[m] == as.vector(new_hru$SOIL)[k]) &
					    (as.vector(subset_hru[[n]]$SLOPE_CD)[m] == as.vector(new_hru$SLOPE_CD)[k])){
					      new_hru[[5+n+p]][k] <-  as.vector(subset_hru[[n]]$HRU)[m]
						new_hru[[6+n+p]][k] <-  as.vector(subset_hru[[n]]$HRU_FR)[m]
						check <- check + 1 
					} else  {
						if ((k == length(new_hru$HRU)) & (check == 0)){
							df <- new_hru[1,]
							df[] <- NA
							df[,1] <- as.vector(subset_hru[[n]]$SUBBASIN)[m]
							df[,2] <- 0
							df[,3] <- as.vector(subset_hru[[n]]$LANDUSE)[m]
							df[,4] <- as.vector(subset_hru[[n]]$SOIL)[m]
							df[,5] <- as.vector(subset_hru[[n]]$SLOPE_CD)[m]
							df[,6] <- 0.0
							df[,5+n+p] <- as.vector(subset_hru[[n]]$HRU)[m]
							df[,6+n+p] <- as.vector(subset_hru[[n]]$HRU_FR)[m]
							new_hru <- rbind(new_hru, df)
						}
					}	
				}
			} 
			p <- p + 1	
		}
		if (i == 1){
			nnew_hru <- new_hru 
		} else {
			nnew_hru <- rbind(nnew_hru, new_hru)

		}
	}

	nnew_hru[is.na(nnew_hru)] <- 0
	write.table(nnew_hru,"luc_all.txt",sep="\t",row.names=FALSE,quote = FALSE)
 



	
