library(ncdf4)
path <- "/Users/davidenicoli/OneDrive - Università degli Studi di Milano//LABO_MISURE_FISICHE_2021_2022/DEM_EOBS_vs_GTOPO/"

# Leggo DEM GTOPO30
ele <- "CUT_G_R00_DEM0500-2000_3500-5000.DAT"; n_file_ele <- paste(path,ele,sep="")
################# COSTRUISCO LE COORDINATE DEL DEM ######################################################################
righe <- 1801; colonne <- 1801
step <- 1/120
lon_min <- 5.00416666665667; lon_max <- lon_min + (colonne-1)*step 
lat_max <-  49.99583333334933; lat_min <- lat_max - (righe-1)*step
lat <- 0:(righe-1); lon <- 0:(colonne-1); lat <- lat_max -(lat/120); lon <- lon_min + (lon/120)
############# LEGGO DEM QUOTE E MEMORIZZO IN MATRICE dem_ele (1801x1800) ############################## 
dem_ele <- matrix(nrow=righe,ncol=colonne)
xx <- file(n_file_ele,"rb")
for (i in 1:righe){
  dem_ele[i,] <- readBin(xx,integer(),size=4,n=colonne)
}

dem_ele[which(dem_ele < 0)] <- NA

# Leggo DEM EOBS
n_file_ele_EOBS <- paste(path,"elev_ens_0.1deg_reg_v23.1e.nc",sep="")
nc <- nc_open(n_file_ele_EOBS, write=FALSE, readunlim=TRUE, verbose=FALSE) #
lon_EOBS <- ncvar_get(nc,varid = "longitude"); lat_EOBS <- ncvar_get(nc,varid = "latitude") 
mat_dati <- ncvar_get(nc,varid="elevation") 

seq_lon <- seq(6.05,18.95,0.1); seq_lat <- seq(47.95,36.05,-0.1)
mat_dif <- matrix(nrow=length(seq_lat),ncol=length(seq_lon))
for (i in 1:length(seq_lat)){
  for (j in 1:length(seq_lon)){
    lon_test <- seq_lon[j]; lat_test <- seq_lat[i] 
    index_lon_EOBS <- which(abs(lon_EOBS - lon_test) == min(abs(lon_EOBS - lon_test))); index_lat_EOBS <- which(abs(lat_EOBS - lat_test) == min(abs(lat_EOBS - lat_test)))
    ELE_EOBS <- round(mat_dati[index_lon_EOBS, index_lat_EOBS], 0)
    index_lon_GTOPO <- which(lon >= lon_EOBS[index_lon_EOBS] - 0.05 & lon <= lon_EOBS[index_lon_EOBS] + 0.05); index_lat_GTOPO <- which(lat >= lat_EOBS[index_lat_EOBS] - 0.05 & lat <= lat_EOBS[index_lat_EOBS] + 0.05)
    ELE_GTOPO30 <- mean(dem_ele[index_lat_GTOPO,index_lon_GTOPO])
    print <- c(i,j,ELE_EOBS, ELE_GTOPO30)
    mat_dif[i,j] <- ELE_EOBS-ELE_GTOPO30  
  }
}

