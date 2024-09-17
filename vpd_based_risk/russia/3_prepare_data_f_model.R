
# Assemble a dataset with fire occurrence, vpd and biomass to use in model

# reticulate::use_condaenv("risk")

library(tidyverse)
library(stars)
library(furrr)
# library(reticulate)
# py_config()


options(future.fork.enable = T)
plan(multicore)





# LOAD DATA --------------------------------------------------------------------

source("https://raw.github.com/carlosdobler/spatial-routines/master/cell_pos_and_import_subset.R")



## VPD ----

ff <- 
  "/mnt/pers_disk/data_russia/rh/" %>% 
  fs::dir_ls(regexp = "vpd")


yr_i <- ff %>% first() %>% str_sub(-8, -5)
yr_f <- ff %>% last() %>% str_sub(-8, -5)

time_era <- 
  seq(as_date(str_glue("{yr_i}-01-01")), as_date(str_glue("{yr_f}-12-01")), by = "1 month")
  
s_proxy <- read_stars(ff[1], proxy = T)

# same as download_n_regrid_fire.R
xmin <- fn_get_cell_pos(s_proxy, 1, 26)
ymin <- fn_get_cell_pos(s_proxy, 2, 40)
xmax <- fn_get_cell_pos(s_proxy, 1, -169+360)
ymax <- fn_get_cell_pos(s_proxy, 2, 78)


ss_vpd <- 
  ff %>% 
  map(function(f) {
    
    f %>% 
      read_stars(proxy = T) %>% 
      .[,xmin:xmax, ymax:ymin,] %>% 
      st_as_stars() %>% 
      suppressMessages() %>% 
      setNames("vpd")
      
  })

ss_vpd <- 
  do.call(c, c(ss_vpd, along = "band")) %>% 
  st_set_dimensions(3, names = "time", values = time_era)





## BIOMASS ----

# Download from GEE


# str_squish(
# "geedim search -c NASA/ORNL/biomass_carbon_density/v1
# -r /mnt/pers_disk/data_russia/fire/like.tif
# download -o --like /mnt/pers_disk/data_russia/fire/like.tif
# -rs bilinear -bn agb
# -dd /mnt/pers_disk/data_russia/") %>%
#   system()


biomass <-
  "/mnt/pers_disk/data_russia/NASA-ORNL-biomass_carbon_density-v1-2010.tif" %>%
  read_stars() %>% 
  setNames("biomass")



## FIRE OCCURRENCE ----

ff <- 
  "/mnt/pers_disk/data_russia/fire/" %>% 
  fs::dir_ls() %>% 
  str_subset(str_flatten(2000:2020, "|"))

time_fire <- 
  ff %>% 
  str_sub(-21, -12) %>% 
  as_date()

time_fire_f <-
  time_fire %>%
  str_sub(end = 7)

ss_fire <- 
  ff %>% 
  future_map(read_stars) %>% 
  {do.call(c, c(., along = "time"))} %>% 
  setNames("fire")

ss_fire_mon <- 
  ss_fire %>% 
    st_apply(c(1,2), function(x) {
      
      if (all(is.na(x))) {
        rep(0, length(unique(time_fire_f)))
      } else {
        
        aggregate(x, by = list(time_fire_f), FUN = sum, na.rm = T)$x
        
      }
    },
    .fname = "time",
    FUTURE = T) %>% 
    aperm(c(2,3,1)) %>% 
    st_set_dimensions(3, values = str_glue("{unique(time_fire_f)}-01"))
  



# ASSEMBLE DATASET ------------------------------------------------------------

ss_vpd_sub <- 
  ss_vpd %>% 
  slice(time, -1)

st_dimensions(ss_vpd_sub) <- st_dimensions(ss_fire_mon)

ss_vpd_sub[is.na(biomass)] <- NA
ss_fire_mon[is.na(biomass)] <- NA


tb_1 <- 
  c(ss_vpd_sub, ss_fire_mon) %>% 
  as_tibble() %>%
  rename(w = fire) %>% 
  mutate(w = round(w),
         fire = if_else(w > 0, 1, 0) %>% factor)

tb_fire <- 
  tb_1 %>% 
  filter(fire == 1)

set.seed(1)
tb_nofire <- 
  tb_1 %>% 
  filter(fire == 0) %>% 
  slice_sample(n = sum(tb_fire$w)) %>% 
  mutate(w = 1)


tb_f <- 
  bind_rows(tb_fire,
            tb_nofire)

tb_f <- 
  tb_f %>% 
  left_join(as_tibble(biomass), by = c("x", "y"))

write_csv(tb_f, "/mnt/pers_disk/data_russia/tb_f_model.csv")

tb_f %>% 
  slice_sample(prop = 0.1) %>% 
  ggplot(aes(y = vpd, x = fire)) + 
  geom_boxplot()




