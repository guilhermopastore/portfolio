library(terra)
library(sf)
library(tidyverse)
library(lubridate)
library(geobr)
library(magick)

# Extração de arquivos
path <- "C:/Users/guilh/OneDrive/Documentos/USP/DISSERTAÇÃO/PM 2.5"

files <- list.files(
  path,
  pattern = "\\.nc$",
  full.names = TRUE
)

# Shapefile de São Paulo - SP

sp <- read_municipality(code_muni = 3550308, year = 2010)

# Coletando todos os arquivos .nc

results <- vector("list", length(files)) # Este passo cria uma lista vazia com um slot pra cada arquivo .nc

for (i in seq_along(files)) {
  
  # Esta etapa coleta a primeira variável de cada arquivo .nc (PM 2.5)
  r  <- rast(files[i])
  pm <- r[[1]]   # GWRPM25 (nome da variável nas bases)
  
  # Verificando se o shapefile de SP e os dados de PM 2.5 possuem Sistema de Referência de Coordenadas compatíveis
  if (!st_crs(sp) == crs(pm)) {
    sp <- st_transform(sp, crs(pm))
  }
  
  # Recorte de SP
  pm_sp <- crop(pm, sp) %>%  mask(sp)
  
  # Transformando raster em data frame
  df <- as.data.frame(pm_sp, xy = TRUE, na.rm = TRUE)
  
  # Extração da data
  date <- stringr::str_extract(basename(files[i]), "\\d{6}")
  df$date <- as.Date(paste0(date, "01"), "%Y%m%d")
  
  results[[i]] <- df # Guardando o respectivo mês na lista 
}

# Juntando o painel 
pm_sp_panel <- bind_rows(results)

# Ajuste do formato da data

pm_sp_panel <- pm_sp_panel %>% 
  mutate(
    date = as.Date(date),
    year  = year(date),
    month = month(date),
    year_month = sprintf("%04d-%02d", year, month)
  ) %>% 
  select(-date)

pm_sp_panel <- pm_sp_panel %>% 
  rename(
    lon = x,
    lat  = y,
    pm25      = GWRPM25
  )

# Média anual por pixel

pm_year_pixel <- pm_sp_panel %>% 
  group_by(lon, lat, year) %>% 
  summarise(
    pm25 = mean(pm25, na.rm = TRUE),
    .groups = "drop"
  )

# Transformando em shapefile

pm_year_sf <- pm_year_pixel %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

# Criação de um df por ano para o gif de mapas

pm_2003_sf <- pm_year_sf %>% 
  filter(year == 2003)

pm_2004_sf <- pm_year_sf %>% 
  filter(year == 2004)

pm_2005_sf <- pm_year_sf %>% 
  filter(year == 2005)

pm_2006_sf <- pm_year_sf %>% 
  filter(year == 2006)

pm_2007_sf <- pm_year_sf %>% 
  filter(year == 2007)

pm_2008_sf <- pm_year_sf %>% 
  filter(year == 2008)

pm_2009_sf <- pm_year_sf %>% 
  filter(year == 2009)

pm_2010_sf <- pm_year_sf %>% 
  filter(year == 2010)

pm_2011_sf <- pm_year_sf %>% 
  filter(year == 2011)

pm_2012_sf <- pm_year_sf %>% 
  filter(year == 2012)

# Mapas

mapa_2003 <- ggplot(pm_2003_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2003",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2004 <- ggplot(pm_2004_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2004",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2005 <- ggplot(pm_2005_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2005",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2006 <- ggplot(pm_2006_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2006",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2007 <- ggplot(pm_2007_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2007",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2008 <- ggplot(pm_2008_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2008",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2009 <- ggplot(pm_2009_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2009",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2010 <- ggplot(pm_2010_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2010",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2011 <- ggplot(pm_2011_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2011",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

mapa_2012 <- ggplot(pm_2012_sf) +
  geom_sf(aes(color = pm25), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PM2.5 em São Paulo — 2012",
    subtitle = "Média anual por pixel (1 km × 1 km)",
    color = "PM2.5 (µg/m³)"
  ) +
  theme_light(base_size = 12)

# GIF

ggsave("mapa_2003.png", mapa_2003, width = 6, height = 6, dpi = 300)
ggsave("mapa_2004.png", mapa_2004, width = 6, height = 6, dpi = 300)
ggsave("mapa_2005.png", mapa_2005, width = 6, height = 6, dpi = 300)
ggsave("mapa_2006.png", mapa_2006, width = 6, height = 6, dpi = 300)
ggsave("mapa_2007.png", mapa_2007, width = 6, height = 6, dpi = 300)
ggsave("mapa_2008.png", mapa_2008, width = 6, height = 6, dpi = 300)
ggsave("mapa_2009.png", mapa_2009, width = 6, height = 6, dpi = 300)
ggsave("mapa_2010.png", mapa_2010, width = 6, height = 6, dpi = 300)
ggsave("mapa_2011.png", mapa_2011, width = 6, height = 6, dpi = 300)
ggsave("mapa_2012.png", mapa_2012, width = 6, height = 6, dpi = 300)

imgs <- image_read(list.files(pattern = "mapa_\\d{4}\\.png"))
gif  <- image_animate(imgs, fps = 1)

image_write(gif, "pm25_sp_2003_2012.gif")
