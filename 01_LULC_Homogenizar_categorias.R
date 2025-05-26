library(terra)

modis <- rast("../Datos/MODIS/proc/MODIS_2001_2023_UTM17_Piura.tif")
cci <- rast("../Datos/CCI/ESACCI_2001_2022_UTM17_Piura.tif")

cci[cci >= 10 & cci <= 40] <- 33
cci[cci >= 50 & cci < 100] <- 11
cci[cci >= 100 & cci < 160] <- 22
cci[cci == 200] <- 66
cci[cci == 180 | cci == 210] <- 55
cci[cci == 190] <- 44
cci[cci == 0 | cci == 220] <- 0

names(cci) <- paste0("Y_",2001:2022)
writeRaster(cci,"./CCI_Homogen_0122vf.tif",filetype="GTiff", overwrite=TRUE)

modis[modis == 11 | modis == 17] <- 55
modis[modis == 12 | modis == 14] <- 33
modis[modis == 13] <- 44
modis[modis == 16] <- 66
modis[modis >= 1 & modis < 7] <- 11
modis[modis >= 7 & modis <= 10] <- 22
modis[modis == 0 | modis == 15 | modis == 255] <- 0


names(modis) <- paste0("Y_",2001:2023)
writeRaster(modis,"./MODIS_Homogen_0123vf.tif",filetype="GTiff", overwrite=TRUE)

            
