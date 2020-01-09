library(ggplot2)
library(tidyverse)
library(viridis)
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}
county <- map_data("county")
imm_dens <- read_csv("imm_dens.csv")

temp <- imm_dens %>%
  full_join(county.fips, by = "fips") %>%
  separate(polyname, into = c("region", "subregion"), sep = ",") %>%
  full_join(county, c("region", "subregion")) %>%
  filter(origin == "Oceania")
pretty_breaks <- c(0.00000001, 0.00000002, 0.00000003, 0.02, 0.05)
minVal <- min(temp$imm_dens, na.rm = TRUE)
maxVal <- max(temp$imm_dens, na.rm = TRUE)
qtiles <- quantile(temp$imm_dens, probs = seq(0, 1, length.out = 7), na.rm = TRUE)
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
for (idx in 1:length(brks)){
  labels <- c(labels, round(brks[idx+1],2))
}
labels <- labels[1:length(labels)-1]
temp$brks <- cut(temp$imm_dens,
                       breaks = brks,
                       labels = labels,
                       include.lowest = T)
brks_scale <- levels(temp$brks)
labels_scale <- rev(brks_scale)

ggplot() +
  geom_polygon(data = temp, aes(fill = brks,
                                x = long,
                                y = lat,
                                group = group)) +
  geom_path(data = temp, aes(x = long,
                             y = lat,
                             group = group),
            color = "white", size = 0.075) +
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = NULL,
       title = "Immigrants in the United States",
       subtitle = "Number per 100 People by County",
       caption = "Source: Census") +
  scale_fill_manual(
    values = rev(c("#004c6d","#346888","#5886a5","#7aa6c2","#9dc6e0","#c1e7ff")),
    breaks = rev(brks_scale),
    name = "# of immigrants",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )
  )
