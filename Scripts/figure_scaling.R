# Buonanduci et al. 2024.
# Few large or many small fires: Using spatial scaling of severe fire
# to quantify effects of fire-size distribution shifts. Ecosphere.

# Figure illustrating quantile regression scaling relationships

require(tidyverse)
require(quantreg)
require(quantregGrowth)
require(cowplot)


# Load data --------------

# Landscape metrics data file
fire_metrics <- read_csv("Data/fire_metrics.csv")%>%
  mutate(Fire_Regime = factor(Fire_Regime, levels = c("Low", "Mixed", "High")))

# Pull out NW fires in high-severity regimes
fire_metrics_NWh <- filter(fire_metrics, NW_Cascadia == 0 & Fire_Regime == "High") 
# Pull out NWC files
fire_metrics_NWC <- filter(fire_metrics, NW_Cascadia == 1) 

# Pool data
fire_metrics_pooled <- bind_rows(fire_metrics_NWh, fire_metrics_NWC)


# Color for plotting
mycol <- "#0d3a67"


# Area-wtd mean patch size ----

# Model fitting
mod <- gcrq(log_patch_area_AW_mean ~ ps(log_fire_area, monotone = 1),
            lambda0 = 5, tau = c(0.05,0.25,0.5,0.75,0.95), data = fire_metrics_pooled)

# Predictions
pred <- fire_metrics_pooled %>% 
  drop_na(log_patch_area_AW_mean) %>% bind_cols( as_tibble(mod$fitted.values) )


# Plotting
p1 <- ggplot(pred) +
  scale_y_continuous(breaks = c(0, 2, 4),
                     labels = c("1", "100", "10,000"),
                     name = " \nPatch size:\nArea-wtd mean (ha)") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("1,000", "10,000", "100,000"),
                     name = "Fire size (ha)") +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.05`, ymax = `0.95`), fill = mycol, alpha = 0.15) +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.25`, ymax = `0.75`), fill = mycol, alpha = 0.2) +
  geom_line(aes(x = log_fire_area, y = `0.05`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.25`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.75`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.95`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.5`), color = mycol) +
  geom_point(aes(x = log_fire_area, y = log_patch_area_AW_mean), color = "gray30", size = 0.6, alpha = 0.1) +
  geom_point(data = fire_metrics_NWC, 
             mapping = aes(x = log_fire_area, y = log_patch_area_AW_mean), shape = 1, size = 0.8) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text = element_text(size = 9),
        strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# Patch distribution: beta ----

# Model fitting
mod <- gcrq(beta ~ ps(log_fire_area),
            lambda0 = 5, tau = c(0.05,0.25,0.5,0.75,0.95), data = fire_metrics_pooled)

# Predictions
pred <- fire_metrics_pooled %>% drop_na(beta) %>% 
  bind_cols( as_tibble(mod$fitted.values) ) 

# Plotting
p2 <- ggplot(pred) +
  scale_y_continuous(name = " \nPatch size:\n\u03B2 parameter") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("1,000", "10,000", "100,000"),
                     name = "Fire size (ha)") +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.05`, ymax = `0.95`), fill = mycol, alpha = 0.15) +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.25`, ymax = `0.75`), fill = mycol, alpha = 0.2) +
  geom_line(aes(x = log_fire_area, y = `0.05`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.25`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.75`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.95`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.5`), color = mycol) +
  geom_point(aes(x = log_fire_area, y = beta), color = "gray30", size = 0.6, alpha = 0.1) +
  geom_point(data = fire_metrics_NWC, 
             mapping = aes(x = log_fire_area, y = beta), shape = 1, size = 0.8) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# Patch distribution: psi ----

# Model fitting
mod <- gcrq(psi ~ ps(log_fire_area),
            lambda0 = 5, tau = c(0.05,0.25,0.5,0.75,0.95), data = fire_metrics_pooled)

# Predictions
pred <- fire_metrics_pooled %>% drop_na(psi) %>% 
  bind_cols( as_tibble(mod$fitted.values) ) 

# Plotting
p3 <- ggplot(pred) +
  scale_y_continuous(name = " \nPatch size:\n\u03C8 parameter") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("1,000", "10,000", "100,000"),
                     name = "Fire size (ha)") +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.05`, ymax = `0.95`), fill = mycol, alpha = 0.15) +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.25`, ymax = `0.75`), fill = mycol, alpha = 0.2) +
  geom_line(aes(x = log_fire_area, y = `0.05`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.25`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.75`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.95`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.5`), color = mycol) +
  geom_point(aes(x = log_fire_area, y = psi), color = "gray30", size = 0.6, alpha = 0.1) +
  geom_point(data = fire_metrics_NWC, 
             mapping = aes(x = log_fire_area, y = psi), shape = 1, size = 0.8) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



# Core area ----

# Model fitting
mod <- gcrq(log_total_core ~ ps(log_fire_area, monotone = 1),
            lambda0 = 5, tau = c(0.05,0.25,0.5,0.75,0.95), data = fire_metrics_pooled)

# Predictions
pred <- fire_metrics_pooled %>% 
  drop_na(log_total_core) %>% bind_cols( as_tibble(mod$fitted.values) )

# Plotting
p4 <- ggplot(pred) +
  scale_y_continuous(breaks = c(0, 2, 4),
                     labels = c("1", "100", "10,000"),
                     name = " \nPatch structure:\nTotal core area (ha)") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("1,000", "10,000", "100,000"),
                     name = "Fire size (ha)") +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.05`, ymax = `0.95`), fill = mycol, alpha = 0.15) +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.25`, ymax = `0.75`), fill = mycol, alpha = 0.2) +
  geom_line(aes(x = log_fire_area, y = `0.05`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.25`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.75`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.95`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.5`), color = mycol) +
  geom_point(aes(x = log_fire_area, y = log_total_core), color = "gray30", size = 0.6, alpha = 0.1) +
  geom_point(data = fire_metrics_NWC, 
             mapping = aes(x = log_fire_area, y = log_total_core), shape = 1, size = 0.8) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



# Seed decay coefficient ----

# Model fitting
mod <- gcrq(log_SDC ~ ps(log_fire_area, monotone = -1),
            lambda0 = 5, tau = c(0.05,0.25,0.5,0.75,0.95), data = fire_metrics_pooled)

# Predictions
pred <- fire_metrics_pooled %>% 
  drop_na(log_SDC) %>% bind_cols( as_tibble(mod$fitted.values) )

# Plotting
p5 <- ggplot(pred) +
  scale_y_continuous(breaks = c(-3, -2.5, -2, -1.5),
                     labels = c("0.001", "0.003", "0.01", "0.03"),
                     name = " \nPatch structure:\nSDC parameter") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("1,000", "10,000", "100,000"),
                     name = "Fire size (ha)") +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.05`, ymax = `0.95`), fill = mycol, alpha = 0.15) +
  geom_ribbon(aes(x = log_fire_area, ymin = `0.25`, ymax = `0.75`), fill = mycol, alpha = 0.2) +
  geom_line(aes(x = log_fire_area, y = `0.05`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.25`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.75`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.95`), lwd = 0.1, color = mycol) +
  geom_line(aes(x = log_fire_area, y = `0.5`), color = mycol) +
  geom_point(aes(x = log_fire_area, y = log_SDC), color = "gray30", size = 0.6, alpha = 0.1) +
  geom_point(data = fire_metrics_NWC, 
             mapping = aes(x = log_fire_area, y = log_SDC), shape = 1, size = 0.8) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# Combine plots ---------

plot_grid(p1, p2, p3, p4, p5, labels = c("(a)", "(b)", "(c)", "(d)", "(e)"), 
          label_size = 10,
          ncol = 1, align = "v", rel_heights = c(1.2, 1, 1, 1, 1.3))

ggsave("Figures/figure2_scaling.tiff", bg = "white", compression = "lzw+p",
       width = 8.5, height = 18, units = "cm", dpi = 350)

