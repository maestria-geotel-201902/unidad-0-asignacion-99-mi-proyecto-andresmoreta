zstatsf <- function(x = NULL, y = NULL, grpx = NULL, grpy = NULL) {
  #Example:
  # zstatsf(
  #   x = 'basins_parra_ocoa.gpkg',
  #   y = 'litologias.gpkg',
  #   grpx = 'value', grpy = 'DLO')
  # zstatsf(
  #   x = 'basins_order_4_ocoa.gpkg',
  #   y = 'litologias.gpkg',
  #   grpx = 'cat', grpy = 'DLO')
  suppressWarnings({
    require(sf)
    require(tidyverse)
    require(purrr)
    
    xsf <- st_read(x, quiet = T)
    ysf <- st_read(y, quiet = T)
    
    xsf <- xsf %>% rename(varx = contains(grpx))
    ysf <- ysf %>% rename(vary = matches(grpy))
    
    names(ysf)[grep('geom', names(ysf))] <- 'geom'
    st_geometry(ysf) <- 'geom'
    poly <- any(grepl('POLY', class(ysf$geom)))
    
    inters <- xsf %>%
      mutate(zarea = st_area(.)) %>% 
      st_intersection(ysf %>% select(vary)) %>% 
      mutate(area = st_area(.), len = st_length(.)) %>%
      group_by(varx) %>% 
      mutate(zs = case_when(poly ~ area/zarea*100, !poly ~ len/zarea*1000000)) %>% 
      dplyr::select(varx, vary, 'zs') %>%
      group_by(varx, vary) %>%
      summarise(zs=sum(zs)) %>%
      st_drop_geometry() %>%
      spread(vary, zs, fill = 0) %>%
      ungroup() %>% 
      inner_join(xsf %>% select(varx), by = 'varx') %>%
      rename(!!sym(grpx):=varx) %>% 
      st_as_sf
    return(inters)
  })
}