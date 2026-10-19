rm(list=ls()) # clear the workspace

# Libraries ---------------------------------------------------------------
library(ggplot2) #figures, time series plots
library(ggpubr)
library(dplyr)
library(tidyr)
library(patchwork)
library(glmmTMB)
library(lme4)
library(emmeans)
library(sjPlot)
#GAM
library(purrr)
library(mgcv)

# ggplot themes and color palettes ----------------------------------------
#Set plot theme
ts_theme <- theme_classic() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.key = element_rect(fill = "transparent"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks=element_line(color = "black", linewidth=1),
        strip.text.x = element_text(size = 12, color = "black"),
        strip.background = element_rect(color="black", fill="grey92", size=1.5, linetype="solid"))
theme_set(ts_theme)

vegPal3 <- c("#1E1719FF","#6CA167FF", "#145A76FF",  "#9F5630FF")
wcPal <- c("#19798BFF", "#F3D567FF", "#EE9B43FF", "#E74B47FF", "#B80422FF", "#912534FF")


# SOC pool calculation ----------------------------------------------------
rawSoil <- read.csv("datasets/WCSoil.csv")

###Clean the soil dataset up
soil <- rawSoil %>%
  dplyr::select(-X) %>%
  rowwise() %>%
  mutate(midDepth = sum(depth0, depth1)/2,
         percC = C/10,
         percN = N/10,
         percAsh = ash/10)

#Upload to LTER repository
soilLTER <- soil |> 
  ungroup() |> 
  mutate(site = "Wigand Creek") |> 
  select(year, site, polygon, coreRep, depth = depth.cat, bulkDensity = bulk.density, moisture = moisture, ash, C, N, delta13C = d13C, delta15N = d15N)

#write.csv(soilLTER, file = paste0(manDir, "datasets/wcSoilProperties", Sys.Date(), ".csv"), row.names = FALSE)

#group soil dataset into cores and then add cumulative concentrations
core <- soil %>%
  group_by(year, polygon, coreRep) %>%
  mutate(coreNum = cur_group_id(),
         cu.soil.stock = cumsum(soil.stock),
         cu.ash.stock = cumsum(ash.stock),
         cu.C.stock = cumsum(C.stock),
         cu.N.stock = cumsum(N.stock),
         cu.ice.stock = cumsum(moisture),
         CtoN = C/N) %>% 
  ungroup() %>% 
  mutate(type = ifelse(!(polygon %in% c("G1", "G2", "G3", "G4")), "Polygon", "Gully"),
         replicateSites = ifelse(polygon %in% c("CB", "TK2B", "TK4B"), stringr::str_remove(polygon, "B$"), polygon),
         group = factor(ifelse(!(polygon %in% c("G1", "G2", "G3", "G4")), polygon, "Gully"),
                        levels = c("C", "CB", "TK1", "TK2",  "TK2B", "TK3", "TK4", "TK4B", "Gully")),
         groupCombined = factor(ifelse(!(polygon %in% c("G1", "G2", "G3", "G4")), replicateSites, "Gully"),
                                levels = c("C", "TK1", "TK2", "TK3", "TK4", "Gully")))

### Function to calculate stocks using ash content --------------------------
#published by Plaza 2019 Nature Geoscience and Lathrop 2025 Global Change Biology

sum.soil.prop <- function(depth0, ash.stock, cu.ash.stock, soil.prop.stock, cu.soil.prop.stock, ash.index){
  index <- first(which(cu.ash.stock >= ash.index)) #find which depth reaches the cumulative ash index val
  
  if(length(index)==0 | length(which(!is.na(index))) == 0){ #conditional when the sample doesn't go deep enough to accumulate enough ash
    #soil.c.stock <- NA
    #needAsh <- ash.index - cu.ash.stock
    newInd <- which(cu.ash.stock == max(cu.ash.stock, na.rm = TRUE))
    perc.layer <- abs((ash.stock[newInd] - (cu.ash.stock[newInd] - ash.index))/ash.stock[newInd])
    soil.c.stock <- cu.soil.prop.stock[newInd-1] + (soil.prop.stock[newInd]*perc.layer)
    
    if(perc.layer > 1.5){
      soil.c.stock <- NA
    } else {
      soil.c.stock <- cu.soil.prop.stock[newInd-1] + (soil.prop.stock[newInd]*perc.layer)
    }
    
  } else if(depth0[index]==0){ #if the ash index is reached in the first sample in the core
    perc.layer <- (ash.stock[index] - (cu.ash.stock[index] - ash.index))/ash.stock[index]
    soil.c.stock <- cu.soil.prop.stock[index]*perc.layer
  } else {
    perc.layer <- (ash.stock[index] - (cu.ash.stock[index] - ash.index))/ash.stock[index]
    soil.c.stock <- cu.soil.prop.stock[index-1] + (soil.prop.stock[index]*perc.layer)
  }
  
  return(soil.c.stock)
  
}

#calculate the cumulative ash content that corresponds to the minimum ash content to reach
#depths 15, 35, and 55 cm in the 2009 cores following protocol in Plaza et al. 2019
#This accounts for heterogeneity in the soils and ensures that if you see this cumulative ash
#content you have reached AT LEAST this depth.

depths <- c(15,35,55)
ash.indices <- c()
placemark <- 1
coreMins <- subset(core, (polygon == "C" | polygon == "CB") & !is.na(ash.stock))

for(i in depths){
  ash.indices[placemark] <- min(coreMins[which(coreMins$depth1 == i), "cu.ash.stock"])
  placemark <- placemark + 1
}

#calculate stocks using minimum method
stockCalcCore <- core %>%
  group_by(polygon, coreRep) %>%
  summarise(stock.so = sum.soil.prop(depth0, ash.stock, cu.ash.stock, C.stock, cu.C.stock, ash.index = ash.indices[1]),
            stock.do = sum.soil.prop(depth0, ash.stock, cu.ash.stock, C.stock, cu.C.stock, ash.index = ash.indices[2]),
            stock.m = sum.soil.prop(depth0, ash.stock, cu.ash.stock, C.stock, cu.C.stock, ash.index = ash.indices[3])) %>%
  mutate(stock.total = stock.m, #first, to find each layer's carbon stock, subtract the previous layers('s) values from the total
         stock.m = stock.m - stock.do,
         stock.do = stock.do-stock.so,
         stock.organic = stock.so + stock.do) %>%
  pivot_longer(cols = -c(polygon, coreRep), names_to = "depth") %>%
  rename(stock = value) %>%
  ungroup() %>%
  mutate(type = ifelse(!(polygon %in% c("G1", "G2", "G3", "G4")), "Polygon", "Gully"),
         replicateSites = ifelse(polygon %in% c("CB", "TK2B", "TK4B"), stringr::str_remove(polygon, "B$"), polygon),
         group = factor(ifelse(!(polygon %in% c("G1", "G2", "G3", "G4")), polygon, "Gully"),
                        levels = c("C", "CB", "TK1", "TK2",  "TK2B", "TK3", "TK4", "TK4B", "Gully")),
         groupCombined = factor(ifelse(!(polygon %in% c("G1", "G2", "G3", "G4")), replicateSites, "Gully"),
                                levels = c("C", "TK1", "TK2", "TK3", "TK4", "Gully")))

stockCalcCore$depth <-  factor(stockCalcCore$depth,
                               levels = c("stock.so", "stock.do", "stock.m", "stock.total", "stock.organic"),
                               labels = c("Surface organic (~0-15cm)", "Deep organic (~15-35cm)", "Mineral (~35-55cm)", "Total stock (to ~55cm)", "Total organic"))

#Add in distance from creek from Google Earth
polygonLocs <- data.frame(polygonID = c("TK4", "TK4B", "TK3", "TK2", "TK2B", "TK1", "C", "CB"),
                          sampleType = "Polygon",
                          #Check these distances
                          distanceFromCreek = c(93, 101, 163, 226, 229, 289, 341, 345))

stockCalcCore$distanceFromCreek <- polygonLocs$distanceFromCreek[match(stockCalcCore$polygon, polygonLocs$polygonID)]         

## Visualize stocks --------------------------------------------------------

ggplot(stockCalcCore, aes(x = group, y = stock)) +
  geom_boxplot(aes(color = groupCombined))+
  geom_point(alpha = 0.5) +
  #scale_color_manual(values = wcPal, name = "Treatment") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")")))) +
  xlab("Sampling location\n(Increasing time since disturbance)") +
  facet_wrap(~depth, nrow = 2, scales = "free") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(stockCalcCore, aes(x = groupCombined, y = stock)) +
  geom_boxplot(aes(color = groupCombined))+
  geom_point(alpha = 0.5) +
  scale_color_manual(values = wcPal, name = "Treatment") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")")))) +
  xlab("Sampling location\n(Increasing time since disturbance)") +
  facet_wrap(~depth, nrow = 2, scales = "free") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Caluclate means for bar plots
stockMeansAll <- stockCalcCore %>% 
  group_by(depth, group) %>% 
  summarise(stockMean = mean(stock, na.rm = TRUE),
            stockSe = plotrix::std.error(stock, na.rm = TRUE))

stockMeansCombined <- stockCalcCore %>% 
  group_by(depth, groupCombined) %>% 
  summarise(stockMean = mean(stock, na.rm = TRUE),
            stockSe = plotrix::std.error(stock, na.rm = TRUE))

ggplot(stockMeansAll, aes(x = group, y = stockMean, fill = group)) +
  geom_bar(stat = "identity", color = "black")+
  geom_errorbar(aes(ymin = stockMean - stockSe, ymax = stockMean + stockSe))+
  #scale_color_manual(values = wcPal, name = "Treatment") +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")")))) +
  xlab("Sampling location\n(Increasing time since disturbance)") +
  facet_wrap(~depth, nrow = 2, scales = "free") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(stockMeansCombined, aes(x = groupCombined, y = stockMean, fill = groupCombined)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7)+
  geom_errorbar(aes(ymin = stockMean - stockSe, ymax = stockMean + stockSe), width = 0.3)+
  scale_fill_manual(values = wcPal, name = "Treatment") +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")")))) +
  xlab("Sampling location\n(Increasing time since disturbance)") +
  facet_wrap(~depth, nrow = 2, scales = "free") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# SOC stats ---------------------------------------------------------------
#First, a pairwise comparison using the grouped classifications (B sites from 2025 added to equivalent 2024 sites)

## Surface organic layer -----------------------------------------------------------
df <- subset(stockCalcCore, group != "Gully" & depth == "Surface organic (~0-15cm)")

hist(df$stock, breaks = 40)
ggplot(df, aes(groupCombined, stock)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

m1 <- lm(stock ~ groupCombined, data = df)
performance::check_model(m1)
mod <- m1

performance::check_model(mod)
pairs(emmeans::emmeans(mod, ~groupCombined, data = df, adjust = "none"))

mod.so <- mod

## deep organic layer -----------------------------------------------------------
df <- subset(stockCalcCore, group != "Gully" & depth == "Deep organic (~15-35cm)")

hist(df$stock, breaks = 30)
ggplot(df, aes(groupCombined, stock)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

m1 <- lm(stock ~ groupCombined, data = df)
performance::check_model(m1)

mod <- m1
performance::check_model(mod)

pairs(emmeans::emmeans(mod, ~groupCombined, data = df, adjust = "none"))
mod.do <- mod

## Mineral layer -----------------------------------------------------------
df <- subset(stockCalcCore, group != "Gully" & depth == "Mineral (~35-55cm)")
hist(df$stock, breaks = 20)
ggplot(df, aes(groupCombined, stock)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

m1 <- lm(stock ~ groupCombined, data = df)
performance::check_model(m1)

mod <- m1
pairs(emmeans::emmeans(mod, ~groupCombined, adjust = "none"))

mod.min <- mod

## Total stocks -----------------------------------------------------------
df <- subset(stockCalcCore, depth == "Total stock (to ~55cm)")
hist(df$stock, breaks = 20)
ggplot(df, aes(groupCombined, stock)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

m1 <- lm(stock ~ groupCombined, data = df)
performance::check_model(m1)

mod <- m1
pairs(emmeans::emmeans(mod, ~groupCombined, adjust = "none"))

mod.tot <- mod

#### summary tables -----------------------------------------------------------
stockCalcCoreSum <- stockCalcCore %>%
  group_by(depth, groupCombined) %>% 
  summarise(seStock = plotrix::std.error(stock, na.rm = TRUE),
            stock = mean(stock, na.rm = TRUE)) %>% 
  mutate(type = ifelse(!(groupCombined %in% c("Gully")), "Polygon", "Gully"))

stockChange <- stockCalcCoreSum %>%
  left_join(stockCalcCoreSum %>% 
              filter(groupCombined == "C") %>%
              select(depth, stock, seStock) %>%
              rename(control_stock = stock, se_control = seStock), 
            by = "depth") %>%
  # Calculate absolute and percent change and their SEs
  mutate(abs_change = stock - control_stock,
         se_abs_change = sqrt(seStock^2 + se_control^2),
         pct_change = (abs_change / control_stock) * 100,
         se_pct_change = (se_abs_change / control_stock) * 100)

stockPairwise <- stockCalcCoreSum %>%
  rename(stock_A = stock, se_A = seStock, group_A = groupCombined) %>%
  # Self-join by depth to get all pairs
  inner_join( stockCalcCoreSum %>%
                rename(stock_B = stock, se_B = seStock, group_B = groupCombined),
              by = "depth") %>%
  # Remove self comparisons
  filter(group_A != group_B) %>%
  # Calculate absolute and percent changes
  mutate( abs_change = stock_A - stock_B,
          se_abs_change = sqrt(se_A^2 + se_B^2),
          pct_change = (abs_change / stock_B) * 100,
          se_pct_change = (se_abs_change / stock_B) * 100) %>%
  select(depth, group_A, group_B, stock_A, stock_B, abs_change, se_abs_change, pct_change, se_pct_change)

# SOC stock plots -----------------------------------------------------------

## Functions for adding CLD or sig. values ---------------------------------
#function to compare groups to control
get_sig_vs_control <- function(mod, depth_name, data_sum) {
  emm <- emmeans::emmeans(mod, ~ groupCombined)
  ctr <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = "C") %>%
    as.data.frame()
  ctr %>% mutate(depth = depth_name,
                 groupCombined = sub(" - C", "", contrast),
                 sig = p.value < 0.11) %>%
    filter(sig) %>%
    left_join(data_sum, by = c("depth", "groupCombined")) %>%
    mutate(label = dplyr::case_when(p.value < 0.001 ~ "****",
                                    p.value < 0.01  ~ "***",
                                    p.value < 0.05  ~ "**",
                                    p.value < 0.1   ~ "*",
                                    TRUE ~ NA_character_),
           label2 = dplyr::case_when(p.value < 0.0001 ~ "p<0.001",
                                     p.value < 1.5 ~ paste0("p=", round(p.value, 3))),
           y = stock + seStock) %>%
    select(depth, groupCombined, y, label, label2)
}

#function to compare all groups to one another
get_cld_all <- function(mod, depth_name, data_sum) {
  
  pval <- anova(mod)["groupCombined", "Pr(>F)"]
  
  cld_df <- emmeans::emmeans(mod, ~ groupCombined) %>%
    multcomp::cld(adjust = "none",
                  Letters = letters,
                  sort = FALSE) %>%
    as.data.frame() %>%
    mutate(depth = depth_name,
           groupCombined = as.character(groupCombined),
           cld = trimws(.group),
           p.value = pval) %>%
    left_join(data_sum, by = c("depth", "groupCombined")) %>%
    mutate(y = stock + seStock) %>%
    select(depth, groupCombined, cld, y, p.value)
  
  return(cld_df)
}

#function to compare all groups to one another and get pvalues
get_pairwise_pvals <- function(mod, depth_name) {
  
  emm <- emmeans::emmeans(mod, ~ groupCombined)
  
  pairs_df <- pairs(emm, adjust = "none") %>%
    as.data.frame() %>%
    mutate(depth = depth_name) %>%
    select(depth, contrast, estimate, SE, df, t.ratio, p.value)
  
  return(pairs_df)
}

## Plots by depth -----------------------------------------------------------
annot_df_depth <- bind_rows(get_sig_vs_control(mod.so, "Surface organic (~0-15cm)", stockCalcCoreSum),
                            get_sig_vs_control(mod.do, "Deep organic (~15-35cm)", stockCalcCoreSum),
                            get_sig_vs_control(mod.min, "Mineral (~35-55cm)", stockCalcCoreSum))

annot_df_depth$depth <- factor(annot_df_depth$depth,
                               levels = levels(stockCalcCoreSum$depth))

depth <- ggplot(subset(stockCalcCoreSum, depth %in% c("Surface organic (~0-15cm)", "Deep organic (~15-35cm)", "Mineral (~35-55cm)") & groupCombined != "Gully"),
                aes(x = groupCombined, y = stock, fill = groupCombined))+
  geom_bar(stat = "identity", color = "black", alpha = 0.7)+
  geom_errorbar(aes(ymin = (stock - seStock), ymax = (stock + seStock)), width = 0.35)+
  scale_fill_manual(values = wcPal, name = "Treatment")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)))+
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")"))))+
  xlab("Sampling location\n(Increasing time since disturbance)")+
  facet_wrap(~depth, nrow = 4, scales = "free")+
  geom_text(data = annot_df_depth, aes(x = groupCombined, y = y * 1.25, label = label),
            inherit.aes = FALSE,
            size = 9)+
  theme(legend.position = "none")
depth


#all letters for group comparisons
cld_depth <- bind_rows(
  get_cld_all(mod.so,  "Surface organic (~0-15cm)", stockCalcCoreSum),
  get_cld_all(mod.do,  "Deep organic (~15-35cm)", stockCalcCoreSum),
  get_cld_all(mod.min, "Mineral (~35-55cm)", stockCalcCoreSum))

cld_depth$depth <- factor(cld_depth$depth,
                          levels = levels(stockCalcCoreSum$depth))

pvals_depth <- bind_rows(
  get_pairwise_pvals(mod.so,  "Surface organic (~0-15cm)"),
  get_pairwise_pvals(mod.do,  "Deep organic (~15-35cm)"),
  get_pairwise_pvals(mod.min, "Mineral (~35-55cm)"))

depthPlotCLD <- ggplot(subset(stockCalcCoreSum, depth %in% c("Surface organic (~0-15cm)", "Deep organic (~15-35cm)", "Mineral (~35-55cm)") & type == "Polygon"),
                       aes(x = groupCombined, y = stock, fill = groupCombined)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = stock - seStock, ymax = stock + seStock), width = 0.35) +
  geom_text(data = cld_depth, aes(x = groupCombined, y = y * 1.15, label = cld),
            inherit.aes = FALSE, size = 6) +
  scale_fill_manual(values = wcPal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")"))))+
  xlab("Increasing time since disturbance") +
  facet_wrap(~ depth, nrow = 4, scales = "free") +
  theme(legend.position = "none")
depthPlotCLD

## Plots for total to 55cm -----------------------------------------------------------
annot_df <- bind_rows(
  get_sig_vs_control(mod.tot, "Total stock (to ~55cm)", stockCalcCoreSum)
)

total <- ggplot(subset(stockCalcCoreSum, depth == "Total stock (to ~55cm)"),
                aes(x = groupCombined, y = stock, fill = groupCombined))+
  geom_bar(stat = "identity", color = "black", alpha = 0.7)+
  geom_errorbar(aes(ymin = (stock - seStock), ymax = (stock + seStock)), width = 0.35)+
  scale_fill_manual(values = wcPal, name = "Treatment")+
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")"))))+
  xlab("Sampling location\n(Increasing time since disturbance)")+
  facet_wrap(~depth, nrow = 4, scales = "free")+
  geom_text(data = annot_df,
            aes(x = groupCombined, y = y * 1.25, label = label2),
            inherit.aes = FALSE,
            size = 4)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)))+
  theme(legend.position = "none")
total

ggarrange(total, depth, widths = c(0.85, 1), labels = c("A)", "B)"))
#ggsave(paste0(manDir, "figures/socStocks", Sys.Date(), ".png"), width = 21.5, height = 15, units = "cm", dpi = 300)

#All letters for group comparisons
cld_total <- get_cld_all(mod.tot,
                         "Total stock (to ~55cm)",
                         stockCalcCoreSum)

totalPlotCLD <- ggplot(subset(stockCalcCoreSum, depth == "Total stock (to ~55cm)"),
                       aes(x = groupCombined, y = stock, fill = groupCombined)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = stock - seStock, ymax = stock + seStock),width = 0.35) +
  geom_text(data = cld_total, aes(x = groupCombined, y = y * 1.15, label = cld),
            inherit.aes = FALSE, size = 6) +
  scale_fill_manual(values = wcPal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")"))))+
  xlab("Increasing time since disturbance") +
  facet_wrap(~ depth, nrow = 4, scales = "free") +
  theme(legend.position = "none")

totalPlotCLD

ggarrange(totalPlotCLD, depthPlotCLD, widths = c(0.85, 1), labels = c("A)", "B)"))
#ggsave(paste0(manDir, "figures/socStocksSupplCLD", Sys.Date(), ".png"), width = 21.5, height = 15, units = "cm", dpi = 300)

# Model stocks by distance from creek with GAM -------------------------------------
polygonLocs <- data.frame(polygonID = c("TK4", "TK4B", "TK3", "TK2", "TK2B", "TK1", "C", "CB"),
                          sampleType = "Polygon",
                          distanceFromCreek = c(93, 101, 163, 226, 229, 289, 341, 345))

stockCalcCore$distanceFromCreek <- polygonLocs$distanceFromCreek[match(stockCalcCore$polygon, polygonLocs$polygonID)]         

# Data used in plot
plotDat <- stockCalcCore %>%
  mutate(polygon = factor(polygon)) %>% 
  filter(type == "Polygon" & !(depth %in% c("Total organic", "Total stock (to ~55cm)")))

## GAM for SO and LM for remaining depths ----------------------------------------

model_list <- plotDat %>%
  group_by(depth) %>%
  group_split() %>%
  setNames(unique(plotDat$depth)) %>%
  imap(function(dat, depth_name) {
    
    # Linear model
    mod_lm <- lm(
      stock ~ distanceFromCreek,
      data = dat
    )
    
    # GAM (fixed k = 3)
    mod_gam <- gam(
      stock ~ s(distanceFromCreek, k = 3),
      data = dat,
      method = "REML"
    )
    
    # Compare models (LM vs GAM)
    comp <- anova(mod_lm, mod_gam, test = "Chisq")
    
    best_model <- if (comp$`Pr(>Chi)`[2] < 0.05) {
      mod_gam
    } else {
      mod_lm
    }
    
    list(
      model = best_model,
      model_type = ifelse(inherits(best_model, "gam"), "GAM", "LM")
    )
  })

#predictions
pred_dat <- imap_dfr(model_list, function(obj, depth_name) {
  
  dat <- plotDat %>% filter(depth == depth_name)
  mod <- obj$model
  
  newdat <- data.frame(
    distanceFromCreek = seq(
      min(dat$distanceFromCreek, na.rm = TRUE),
      max(dat$distanceFromCreek, na.rm = TRUE),
      length.out = 200
    )
  )
  
  if (inherits(mod, "gam")) {
    
    p <- predict(mod, newdata = newdat, se.fit = TRUE)
    
  } else {
    
    p <- predict(mod, newdata = newdat, se.fit = TRUE)
  }
  
  newdat %>%
    mutate(
      depth = depth_name,
      fit = p$fit,
      lower = p$fit - 1.96 * p$se.fit,
      upper = p$fit + 1.96 * p$se.fit
    )
})

#labels for graphs
model_stats <- imap_dfr(model_list, function(obj, depth_name) {
  
  mod <- obj$model
  
  if (inherits(mod, "gam")) {
    
    sm <- summary(mod)
    
    tibble(
      depth = depth_name,
      label = paste0(
        "GAM",
        "\nR² = ", round(sm$r.sq, 2),
        "\nEDF = ", round(sm$s.table[1, "edf"], 2),
        "\np = ", scales::pvalue(sm$s.table[1, "p-value"])
      )
    )
    
  } else {
    
    sm <- summary(mod)
    
    tibble(
      depth = depth_name,
      label = paste0(
        "LM",
        "\nR² = ", round(sm$r.squared, 2),
        "\np = ", scales::pvalue(
          pf(sm$fstatistic[1],
             sm$fstatistic[2],
             sm$fstatistic[3],
             lower.tail = FALSE)
        )
      )
    )
  }
})

#position for labels
label_pos <- plotDat %>%
  group_by(depth) %>%
  summarise(x = 50,
            y = max(stock, na.rm = TRUE),
            .groups = "drop")

model_stats <- left_join(model_stats, label_pos, by = "depth")

#plot
gam <- ggplot(plotDat, aes(distanceFromCreek, stock)) +
  geom_point(aes(color = groupCombined), alpha = 0.6) +
  geom_ribbon(data = pred_dat,
              aes(x = distanceFromCreek, ymin = lower, ymax = upper),
              inherit.aes = FALSE,
              alpha = 0.1) +
  geom_line(data = pred_dat,
            aes(x = distanceFromCreek, y = fit),
            inherit.aes = FALSE,
            linewidth = 1) +
  geom_text(data = model_stats,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            hjust = 1,
            vjust = 1) +
  facet_wrap(~~factor(depth, levels = levels(plotDat$depth)),
             scales = "free", nrow = 3) +
  scale_color_manual(values = wcPal) +
  theme(legend.position = "none") +
  scale_x_reverse()+
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")")))) +
  xlab("Distance from stream (m)")

gam

ggarrange(total, gam, widths = c(0.85, 1.1), labels = c("A)", "B)"))
#ggsave(paste0(manDir, "figures/socStocks", Sys.Date(), ".png"), width = 22.5, height = 15, units = "cm", dpi = 300)


ggplot(plotDat, aes(distanceFromCreek, stock)) +
  geom_point(aes(color = groupCombined), alpha = 0.6) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              color = "black",
              alpha = 0.25) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
                        label.x = "right",
                        label.y = "top") +
  scale_color_manual(values = wcPal) +
  theme(legend.position = "none") +
  ylab(expression(bold(paste("SOC stock (kg ", m^-2, ")")))) +
  xlab("Distance from stream (m)")
#ggsave(paste0(manDir, "figures/suppSOCGAM", Sys.Date(), ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Plot soil data by depth -------------------------------------------------

### Polygon thaw depths -------------------------------------------------

thawData <- read.csv("datasets/WCThawData.csv")

c <- ggplot(data = thawData, aes(x = polygon, y = meanThawCore, fill = polygon))+
  geom_bar(stat = "identity", color = "black", alpha = 0.7)+
  scale_fill_manual(values = wcPal)+
  geom_errorbar(aes(ymin = meanThawCore - seThawCore, ymax = meanThawCore + seThawCore), width = 0.3)+
  xlab("Sampling location") + ylab("Thaw depth")+
  theme(legend.position = "none")

c

### Ash, bulk density, soil mositure -------------------------------------------------
ggline(subset(core, type == "Polygon"), x = "midDepth", y = "percAsh",
       add = c("mean_se"),
       add.params = list(size = .75, alpha = 0.5),
       color = "groupCombined", alpha = 0.5) +
  scale_color_manual(values = wcPal, name = "Sampling location") +
  scale_x_discrete(limits = rev) + coord_flip()+
  ylim(0,100)+
  ggtitle("% Ash by depth") + xlab("Mid depth") + ylab("%Ash")

ggline(subset(core, type == "Polygon"), x = "midDepth", y = "bulk.density",
       add = c("mean_se"),
       add.params = list(size = .75, alpha = 0.5),
       color = "groupCombined", alpha = 0.5) +
  scale_color_manual(values = wcPal, name = "Sampling location") +
  scale_x_discrete(limits = rev) + coord_flip()+
  ggtitle("Bulk density by depth") + xlab("Mid depth") + ylab("Bulk density (g/cm3)")

ggline(subset(core, type == "Polygon"), x = "midDepth", y = "moisture",
       add = c("mean_se"),
       add.params = list(size = .75, alpha = 0.5),
       color = "groupCombined", alpha = 0.5) +
  scale_color_manual(values = wcPal, name = "Polygon") +
  scale_x_discrete(limits = rev) + coord_flip()+
  ggtitle("Ice content by depth") + xlab("Mid depth") + ylab("Soil moisture/ice")

soil_long <- core %>%
  group_by(groupCombined, type) %>%
  mutate(ash = ash/10,
         moisture = moisture/10,
         C = C/10,
         N = N/10,
         CtoN = C/N) %>%
  dplyr::select(depth0, moisture, bulk.density, ash, C, N, CtoN, d13C, d15N) %>%
  gather(measurement, value, c(bulk.density, moisture, ash, C, N, CtoN, d13C, d15N))

soil_long$measurement <- factor(soil_long$measurement, 
                                levels = c("bulk.density", "moisture", "ash", "C", "N", "CtoN", "d13C", "d15N"),
                                labels = c("Bulk density", "Soil moisture", "Ash", "C", "N", "C:N", "d13C", "d15N"))

sm <- ggline(subset(soil_long, type == "Polygon" & measurement %in% c("Soil moisture")), 
             x = "depth0", y = "value", 
             facet.by = "measurement", color = "groupCombined", alpha = 0.95,
             add = c("mean_se"),
             add.params = list(size = .75, alpha = 0.85)) +
  coord_flip()+
  scale_color_manual(values = wcPal, name = "Sampling location") +
  scale_x_reverse()+
  facet_wrap(~measurement, nrow = 1, scales = "free_x")+
  ts_theme+
  xlab(element_blank()) + ylab("Soil moisture / ice (%)")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 12, color = "black"))
sm

bd <- ggline(subset(soil_long, type == "Polygon" & measurement %in% c("Bulk density")), 
             x = "depth0", y = "value", 
             facet.by = "measurement", color = "groupCombined", alpha = 0.95,
             add = c("mean_se"),
             add.params = list(size = .75, alpha = 0.85)) +
  coord_flip()+
  scale_color_manual(values = wcPal, name = "Sampling location") +
  scale_x_reverse()+
  facet_wrap(~measurement, nrow = 1, scales = "free_x")+
  xlab("Depth") + ylab("Bulk density (g/cm3)")+
  ts_theme+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 12, color = "black"))
bd

combined <- ((bd + sm) & plot_layout(guides = "collect") & theme(legend.position = "none"))
fin <- combined + c
fin

#ggsave(paste0(manDir, "figures/suppSoilDepth", Sys.Date(), ".png"), plot = fin, width = 22, height = 15, units = "cm", dpi = 300)

