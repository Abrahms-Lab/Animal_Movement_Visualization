library(moveVis)
library(move2)
library(raster)
library(sp)
library(sf)
library(glue)
library(parallel)
library(pbapply)
library(dplyr)
library(mapview)

# Data loading and cleaning 
cheetah = read.csv("cheetahdf_3h.csv")
dog = read.csv("dogdf_3h.csv")
leaopard = read.csv("leoparddf_3h.csv")
lion = read.csv("liondf_3h.csv")

# ---------------------------
# Add species labels & clean ID fields
# ---------------------------

cheetah <- cheetah %>% mutate(animal = 'cheetah') %>%
                   mutate(ID = sub("[0-9]+$", "", id))
dog <- dog %>% mutate(animal = 'dog') %>%
              mutate(ID = sub("[0-9]+$", "", id))
leopard <- leopard %>% mutate(animal = 'leopard') %>%
                    mutate(ID = sub("[0-9]+$", "", id))
lion <- lion %>% mutate(animal = 'lion') %>%
                mutate(ID = sub("[0-9]+$", "", id))
lion$ID[lion$ID=='SaF']<- 'SaF07'

# ---------------------------
# Function to process animal movement data:
# - Converts dates
# - Transforms UTM to lat/long
# - Creates an sf object with timestamps
# ---------------------------
process_animal_data <- function(data, utm_epsg_code = 32734) {
  # Convert date to POSIXct
  data$date <- as.POSIXct(data$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Convert UTM to Lat/Long
  lat.long.df <- data.frame(x = data$x, y = data$y)
  coordinates(lat.long.df) <- ~x + y
  proj4string(lat.long.df) <- CRS(paste0("+init=epsg:", utm_epsg_code))
  latlong_coords <- spTransform(lat.long.df, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
  coords <- coordinates(latlong_coords)
  
  # Build final output dataframe
  output_df <- data.frame(
    ID = data$ID,
    date = data$date,
    animal = data$animal,
    lat = coords[, 2],
    long = coords[, 1]
  )
  
  output_df <- output_df %>% 
    mutate(ID = as.factor(ID)) %>%
    st_as_sf(coords = c("long", "lat"), crs=4326) %>% 
    mutate(timestamp = as.POSIXct(date,
                                  format = "%Y-%m-%dT%H:%M:%SZ",
                                  tz = "UTC"))
  output_df$long <- coords[, 1]
  output_df$lat <- coords[, 2]
  return(output_df)
}

# ---------------------------
# Apply processing function to each species dataset
# ---------------------------
cheetah_df <- process_animal_data(cheetah)
head(cheetah_df)
dog_df <- process_animal_data(dog)
head(dog_df)
leopard_df <- process_animal_data(leopard)
head(leopard_df)
lion_df <- process_animal_data(lion)
head(lion_df)

# ---------------------------
# Combine all animals into one dataframe
# ---------------------------
all_df <- rbind(cheetah_df,dog_df,leopard_df,lion_df)
head(all_df)


# ---------------------------
# Quick interactive map check
# ---------------------------                        
earth <- st_as_sf(rnaturalearth::countries110)

bbox <- st_bbox(all_df, crs=st_crs(earth))
bbox

all_df %>% 
  mapview(zcol="ID")

# ---------------------------
# Convert to move object and align timestamps
# ---------------------------

all_move <- df2move(df = all_df, 
                          proj=CRS("+proj=longlat +ellps=WGS84"),
                          x="long",
                          y="lat",
                          time="timestamp", 
                          track_id = "ID") %>%
  align_move(res = 1800, unit = "mins")



# ---------------------------
# Create animation GIF for a single individual ("Ferrari" example)
# ---------------------------
frames <- frames_spatial(all_move[[1]],
                         path_colours = c("blue"), #number of colors needs to match number of individual tracks
                         map_service = "mapbox",
                         map_type = "hybrid",
                         map_token = "pk.eyJ1IjoicmlvLXMiLCJhIjoiY205YndmNmEwMGM1czJpb2R0d2F4YjRscSJ9.TNe2Fhp-R9TVvbFLXQBXLQ",
                         alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(cheetah_move[[1]], type = "label") %>%
  add_progress() %>%
  add_gg(ggplot2::expr(guides(linetype = "none")))

frames[[39]]

animate_frames(frames, overwrite = TRUE, out_file = "cheetah_animation.gif")

summary(coordinates(cheetah_move[[1]]))


# ---------------------------
# Attempt to create animation for all animals (may cause memory issues)
# ---------------------------
n_ids <- length(unique(cheetah_df$id))
colors <- rainbow(n_ids)
colors

use_multicore(FALSE)
use_disk(FALSE)

title_list <- as.list(timestamp_chars)

frames <- frames_spatial(cheetah_move, 
                         path_colours = colors, #number of colors needs to match number of individual tracks
                         map_service = "esri", 
                         map_type = "world_imagery",
                         alpha = 0.5
                         ) %>%
  add_labels(x = "Longitude", y = "Latitude", title = title_list) %>% 
  add_northarrow() %>%
  add_scalebar() %>%
  add_progress() %>%
  add_gg(ggplot2::expr(guides(linetype = "none"))) 


frame_times <- get_frametimes(frames)
timestamp_chars <- as.character(format(frame_times, "%Y-%m-%d %H:%M:%S"))

# ---------------------------
# Add titles with timestamps
# ---------------------------
frames <- frames %>%
  add_gg(ggplot2::expr(labs(title = timestamp_chars[frame_i])))

print(frames[[500]])

# ---------------------------
# Enable parallel & disk caching for large animation
# ---------------------------
use_multicore(n_cores = 7, verbose = TRUE)

use_disk(
  frames_to_disk = TRUE,
  dir_frames = paste0(tempdir(), "/moveVis"),
  n_memory_frames = 2,
  verbose = TRUE
)

# export final animation
animate_frames(frames, out_file = "cheetah_animation_all.gif")

