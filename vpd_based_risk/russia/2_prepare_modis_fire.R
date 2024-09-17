
# Download MOD14A2 (fire) product from GEE and aggregate to ERA5 grid 

reticulate::use_condaenv("risk")
library(reticulate)
py_config()
library(tidyverse)
library(stars)
# library(furrr)
# 
# options(future.fork.enable = T)
# plan(multicore)


# template

source("https://raw.github.com/carlosdobler/spatial-routines/master/cell_pos_and_import_subset.R")


s_proxy <- 
  "/mnt/bucket_mine/era/monthly/mean-tasmax/era5_mon_mean-tasmax_1970.nc" %>% 
  read_ncdf(proxy = T)

xmin <- fn_get_cell_pos(s_proxy, 1, 26)
ymin <- fn_get_cell_pos(s_proxy, 2, 40)
xmax <- fn_get_cell_pos(s_proxy, 1, -169+360)
ymax <- fn_get_cell_pos(s_proxy, 2, 78)

s <- 
  s_proxy %>% 
  .[, xmin:xmax, ymax:ymin, 1] %>% 
  st_as_stars() %>% 
  adrop() 

# s %>% 
#   write_stars("/mnt/pers_disk/data_russia/fire/like.tif")
  

# loop through batches
time_batches <- seq(as_date("2000-01-01"), as_date("2020-12-31"), length.out = 8)
time_batches[-1] <- time_batches[-1] %>% as_date() %>% {.+days(1)} %>% as.character()


for (i in seq_along(time_batches) %>% head(-1)) {
  
  time_start <- time_batches[i]
  
  print(str_glue("{time_start}"))
  
  time_end <- as_date(time_batches[i+1]) - days(1)
  time_end <- as.character(time_end)
  
  str_glue(
    "geedim search -c MODIS/061/MOD14A2 
    -r /mnt/pers_disk/data_russia/fire/like.tif 
    -s {time_start} -e {time_end} 
    download -o -r /mnt/pers_disk/data_russia/fire/like.tif
    -bn FireMask -c EPSG:4326 
    -dd /mnt/pers_disk/data_russia/fire"
  ) %>%
    str_squish() %>% 
    system(ignore.stdout = T, ignore.stderr = T)
  
  
  # regrid batch
  
  "/mnt/pers_disk/data_russia/fire" %>% 
    fs::dir_ls() %>% 
    str_subset("regrid", negate = T) %>% 
    str_subset("like", negate = T) %>% 
    
    map(function(f) {
      
      print(str_glue("   {f}"))
      
      r <- 
        f %>% 
        read_stars(proxy = F) %>%
        setNames("fire") %>% 
        mutate(fire = if_else(is.na(fire) | fire < 9, 0, 1)) %>% # high confidence
        st_warp(s, method = "sum", use_gdal = T) %>% 
        suppressWarnings() 
      
      tempdir() %>% 
        fs::dir_ls() %>% 
        fs::file_delete()
      
      r %>%  
        setNames("fire") %>% 
        mutate(fire = if_else(fire == 0, NA, fire)) %>% 
        write_stars(str_replace(f, ".tif", "_regrid.tif"))
      
      fs::file_delete(f)
      
    })
  
}

