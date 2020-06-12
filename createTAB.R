library(data.table)
library(sf)
library(dplyr)


dir <- "c:/Users/hsuanyuc/Desktop/March/"
#Production directory
#dir <- "d:/ftp/"

#get file start date
configDir <- "c:/Users/hsuanyuc/Desktop/March/config.properties"
#configDir <- "c:/script/config.properties"
sourceDate <- readLines(con= configDir)
sourceDate <- sourceDate[grepl("START_DATE", sourceDate)] %>% 
  strsplit(split = "=")
sourceDate <- sourceDate[[1]][2]
sourceDate <- paste0(substr(sourceDate,1,4), substr(sourceDate,6,7))


datePattern <- paste0(sourceDate,".*.")

csv <- list.files(dir, 
                  pattern = paste0(datePattern,"csv"), 
                  full.names = T, recursive = T)

fileNameCsv <- list.files(dir, 
                          pattern = paste0(datePattern,"csv"), 
                          full.names = T, recursive = T) %>%
  basename() %>% tools::file_path_sans_ext()

fileNameTab <- list.files(dir, 
                          pattern = paste0(datePattern,"tab"), 
                          full.names = T, recursive = T) %>% 
  basename() %>% tools::file_path_sans_ext()


tabList <- csv[!(fileNameCsv %in% fileNameTab)] 


if(length(tabList) > 0){
  for(ind in 1:length(tabList)){
    temp <- fread(tabList[ind])
    tabName <- tabList[ind] %>% tools::file_path_sans_ext()
    #check whether the file is ifmdt or not
    if (grepl("ifmdt",tabName)) {
      temp[,LEGEND_NBR1RSRP := NULL]
      temp$OGR_STYLE <- case_when(temp$NBR1RSRP >= -85 ~ "SYMBOL(a:0,c:#0000FF,s:12pt,id:ogr-sym-4)",
                                  temp$NBR1RSRP >= -95 ~ "SYMBOL(a:0,c:#008000,s:12pt,id:ogr-sym-4)",
                                  temp$NBR1RSRP >= -105 ~ "SYMBOL(a:0,c:#D2D250,s:12pt,id:ogr-sym-4)",
                                  temp$NBR1RSRP >= -115 ~ "SYMBOL(a:0,c:#FFBE78,s:12pt,id:ogr-sym-4)",
                                  temp$NBR1RSRP < -115 ~ "SYMBOL(a:0,c:#FF0000,s:12pt,id:ogr-sym-4)")
      
    }else{
      temp[,LEGEND_AVGRSRP := NULL]
      temp$OGR_STYLE <- case_when(temp$AVGRSRP >= -85 ~ "SYMBOL(a:0,c:#0000FF,s:12pt,id:ogr-sym-4)",
                                  temp$AVGRSRP >= -95 ~ "SYMBOL(a:0,c:#008000,s:12pt,id:ogr-sym-4)",
                                  temp$AVGRSRP >= -105 ~ "SYMBOL(a:0,c:#D2D250,s:12pt,id:ogr-sym-4)",
                                  temp$AVGRSRP >= -115 ~ "SYMBOL(a:0,c:#FFBE78,s:12pt,id:ogr-sym-4)",
                                  temp$AVGRSRP < -115 ~ "SYMBOL(a:0,c:#FF0000,s:12pt,id:ogr-sym-4)")
      
    }

    temp.sf <- st_as_sf(temp, coords = c("LONGITUDE", "LATITUDE"), 
                        crs=4326)
    st_write(temp.sf, dsn = paste0(tabName, ".tab"), 
             driver = "MapInfo File")
  }
}

