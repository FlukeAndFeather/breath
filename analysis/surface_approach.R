library(breath)
library(cowplot)
library(depmixS4)
library(ncdf4)
library(RcppRoll)
library(rootSolve)
library(tidyverse)

# load data
nc_path <- "~/Documents/GitHub/exploration/breath-rate/data/Bm181021-TDR11_prh8.nc"
prh_nc <- nc_open(nc_path)

# read variables
dn <- ncvar_get(prh_nc, "DN")
dt <- dn_to_posix(dn, "US/Pacific")
secs <- as.numeric(dt - dt[1], unit = "secs")
p <- ncvar_get(prh_nc, "P")
pitch <- ncvar_get(prh_nc, "pitch")
speed <- ncvar_get(prh_nc, "speedJJ")
fs <- ncatt_get(prh_nc, "P", "sampling_rate")$value

# Create features (smooth over 2s and calculate rate of change)
feat <- tibble(dt, secs, p, pitch, speed) %>%
  mutate(p2 = roll_mean(p, n = fs * 2, fill = NA),
         pitch2 = roll_mean(pitch, n = fs * 2, fill = NA),
         speed2 = roll_mean(speed, n = fs * 2, fill = NA),
         ddt_p = (lead(p2) - p2) * fs,
         ddt_p2 = roll_mean(ddt_p, n = fs * 2, fill = NA),
         ddt_pitch = (lead(pitch2) - pitch2) * fs,
         ddt_pitch2 = roll_mean(ddt_pitch, n = fs * 2, fill = NA))

#### Find all breaths ####
# find regions of depth<10m for >2s
depth_rle <- rle(feat$p2 < 10)
depth_rle$surface <- depth_rle$lengths > 2 * fs & depth_rle$values == TRUE
depth_rle$start <- cumsum(lag(depth_rle$lengths, default = 1))
surfaces <- tibble(
  lengths = depth_rle$lengths,
  values = depth_rle$values,
  is_surface = lengths > 10 * fs & values == TRUE,
  surface_idx = ifelse(is_surface, cumsum(is_surface), NA),
  surface_start = cumsum(lag(lengths, default = 1)),
  surface_end = lead(surface_start, default = nrow(feat) + 1) - 1
) %>%
  filter(is_surface) %>%
  select(surface_idx:surface_end)

# Find local minima in window
find_minima <- function(surface_idx, surface_start, surface_end, period) {
  df <- slice(feat, surface_start:surface_end)

  # find local depth minima in 2 x period second window at surface
  is_min <- df$p2 == roll_min(df$p2, n = 2 * fs * period, fill = Inf)
  secs_at_min <- df$secs[is_min]
  min_gaps <- secs_at_min - lag(secs_at_min, default = -Inf)
  min_p <- df$p2[is_min]
  breath_idx <- which(is_min)[min_gaps > period & min_p <= 0]

  slice(df, breath_idx) %>%
    mutate(surface_idx = surface_idx,
           row_idx = surface_start + breath_idx - 1)
}

# Find breaths 5s apart
breaths <- pmap_dfr(surfaces, find_minima, period = 5)

# Plot depth, pitch, and speed with breaths for a surface interval
plot_surface <- function(surface_idx) {
  i <- surfaces$surface_start[surface_idx]:surfaces$surface_end[surface_idx]
  df <- slice(feat, i)
  b <- filter(breaths, surface_idx == { surface_idx })
  plot_grid(
    ggplot(df, aes(dt, p2)) +
      geom_line(size = 0.2, color = "blue") +
      geom_vline(aes(xintercept = dt), b, linetype = 3) +
      scale_x_datetime(date_labels = "%b-%d %H:%M") +
      scale_y_reverse("Depth (m)") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank()),
    ggplot(df, aes(dt, pitch2 * 180 / pi)) +
      geom_line(size = 0.2, color = "red") +
      geom_vline(aes(xintercept = dt), b, linetype = 3) +
      scale_x_datetime(date_labels = "%b-%d %H:%M") +
      scale_y_continuous("Pitch (deg)", limits = c(-15, 15)) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank()),
    ggplot(df, aes(dt, speed2)) +
      geom_line(size = 0.2, color = "purple") +
      geom_vline(aes(xintercept = dt), b, linetype = 3) +
      scale_x_datetime(date_labels = "%b-%d %H:%M") +
      scale_y_continuous("Jiggle speed (m/s)") +
      theme_minimal() +
      theme(axis.title.x = element_blank()),
    ncol = 1,
    align = "v"
  )
}
