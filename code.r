library(tidyverse)
library(ggalluvial)

# Read data
rga <- data.frame(
  rga_class = c("TNL","CNL","NL","TN","CN","NBS","RLK","RLP","TM-CC"),
  Brassica_napus = c(201,137,86,42,27,116,904,327,103),
  Brassica_juncea = c(192,125,71,35,20,90,673,250,108),
  Brassica_carinata = c(160,110,89,33,26,94,674,288,90),
  Brassica_rapa = c(99,64,38,16,17,59,331,164,46),
  Brassica_oleracea = c(97,53,33,18,15,61,374,144,45),
  Brassica_nigra = c(86,66,45,18,11,62,362,137,59),
  Arabidopsis_thaliana = c(90,66,30,14,12,56,304,124,40),
  Arabidopsis_lyrata = c(77,72,44,16,14,59,333,148,61),
  Camelina_sativa = c(170,149,49,35,25,104,691,205,96),
  Capsella_rubella = c(79,65,30,13,10,53,315,112,46),
  Eutrema_salsugineum = c(86,70,32,15,12,44,354,139,49),
  Raphanus_sativus = c(101,63,41,18,13,64,438,202,61)
)

# Convert to long format
long_df <- rga %>%
  pivot_longer(
    cols = -rga_class,
    names_to = "Species",
    values_to = "Count"
  )

# Sankey/alluvial plot
ggplot(long_df,
       aes(axis1 = Species,
           axis2 = rga_class,
           y = Count)) +
  geom_alluvium(aes(fill = rga_class),
                width = 0.15,
                alpha = 0.8) +
  geom_stratum(width = 0.15,
               fill = "grey90",
               color = "black") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3) +
  scale_x_discrete(limits = c("Species", "RGA Class"),
                   expand = c(.05, .05)) +
  labs(
    title = "Distribution of RGA Classes Across Species",
    y = "Gene Count"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10)
  )






library(tidyverse)
library(ggalluvial)

# Create plot
p <- ggplot(long_df,
            aes(axis1 = Species,
                axis2 = rga_class,
                y = Count)) +
  geom_alluvium(aes(fill = rga_class),
                width = 0.15,
                alpha = 0.8) +
  geom_stratum(width = 0.15,
               fill = "grey90",
               color = "black") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3) +
  scale_x_discrete(
    limits = c("Species", "RGA Class"),
    expand = c(.05, .05)
  ) +
  labs(
    title = "Distribution of RGA Classes Across Species",
    y = "Gene Count"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10)
  )

# Display plot
print(p)

# Save as JPG
ggsave(
  filename = "RGA_Sankey.jpg",
  plot = p,
  width = 14,
  height = 8,
  units = "in",
  dpi = 600
)

