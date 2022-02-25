#PCAS unused
#2 25 22

#too few reps, statistic issues

# Open only ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(cover_type == "Open") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  
#dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$slopepos), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=4,stroke=1, aes(color = groups))+
  xlim(-4,5)+
  ylim(-3.5,5)+
  NULL+
  theme_er()+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))

# Footslope canopy vs open ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(slopepos == "footslope") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  
#dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$cover_type), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=4,stroke=1, aes(color = groups))+
  xlim(-5,5)+
  ylim(-3.5,5)+
  NULL+
  theme_er()+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))

# Low Backslope (canopy vs open) ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(slopepos == "low backslope") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  
#dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$cover_type), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=4,stroke=1, aes(color = groups))+
  xlim(-5,5)+
  ylim(-3.5,5)+
  NULL+
  theme_er()+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))

# Backslope (canopy vs open) ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(slopepos == "backslope") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  
#dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$cover_type), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=4,stroke=1, aes(color = groups))+
  xlim(-5,5)+
  ylim(-3.5,5)+
  NULL+
  theme_er()+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))

# Canopy only ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(cover_type == "Canopy") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  
#dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$slopepos), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=4,stroke=1, aes(color = groups))+
  xlim(-5,5)+
  ylim(-3.5,5)+
  NULL+
  theme_er()+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))

