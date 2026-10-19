rm(list=ls()) # clear the workspace

# Libraries ---------------------------------------------------------------
library(ggplot2) #figures, time series plots
library(dplyr)
library(tidyr)
library(readxl)
library(shapefiles)
library(stringr)
library(ggpattern)
library(ggpubr)
library(patchwork)
library(glmmTMB)
library(lme4)
library(emmeans)
library(sjPlot)

# ggplot themes and color palettes ----------------------------------------
#Set plot theme
ts_theme <- theme_classic() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = "transparent"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks=element_line(color = "black", linewidth=1),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(color="black", fill="grey92", size=1.5, linetype="solid"))
theme_set(ts_theme)


threeYearPal <- c("#999999", "#000000", "#A50021FF")

twoYearPal <- c("#999999", "#000000")
tkPal <- c("#19798BFF", "#B80422FF")
tkPalAlpha <- c("#19798B70", "#B8042270")

vegPal3 <- c("#1E1719FF","#6CA167FF", "#145A76FF",  "#9F5630FF")
wcPal <- c("#19798BFF", "#F3D567FF", "#EE9B43FF", "#E74B47FF", "#B80422FF", "#912534FF")

# Directories -------------------------------------------------------------
manDir <- "/Users/emmalathrop/Documents/NAU/SchuurLab/Manuscripts/Chapter3/"

# Import transect data ----------------------------------------------------
df2013 <- read.csv("datasets/df2013.csv")
df2023 <- read.csv("datasets/df2023.csv")

## Identify thermokarst or not along transect ------------------------------

#Visually identifying based on field measurements and based on change in elevation along transect
#Using these plots to identify:
ggplot(data = df2013, aes(x=Distance, y=Elevation)) +
  geom_line(size = 1, alpha = 0.7)+
  geom_point(size = 1, alpha = 0.7)+
  facet_wrap(Site~Transect, scales = "free")+
  ylab(label = "Elevation (ft)")

ggplot(data = df2023, aes(x=Distance, y=Elevation)) +
  geom_line(size = 1, alpha = 0.7)+
  geom_point(size = 1, alpha = 0.7)+
  facet_wrap(Site~Transect, scales = "free")+
  ylab(label = "Elevation (ft)")

#Visually assessed if point was thermokarst or not depending on elevation change and in situ observations
tkClass <- rbind(df2013, df2023) %>% 
  select(Site, year, Transect, Distance) %>%
  mutate(thermokarst = NA,
         thermokarst = ifelse(Site == "Boundary" & Distance > 10, "yes", thermokarst),
         thermokarst = ifelse((year == 2013 & Site == "LEF Cabin") & Distance >= 35 , "yes", thermokarst),
         thermokarst = ifelse((year == 2023 & Site == "LEF Cabin") & Distance >= 25 , "yes", thermokarst),
         thermokarst = ifelse(((Site == "Lower East Fork" & Transect == 1)) & Distance >= 25 , "yes", thermokarst),
         thermokarst = ifelse((Site == "Lower East Fork" & Transect == 2) & Distance >= 10 , "yes", thermokarst),
         thermokarst = ifelse((Site == "Pitchfork" & Transect == 1) & (Distance >= 25 & Distance <= 45), "yes", thermokarst),
         thermokarst = ifelse((((year == 2013 & Site == "Pitchfork") & Transect == 2)) & Distance >= 20 , "yes", thermokarst),
         thermokarst = ifelse((((year == 2023 & Site == "Pitchfork") & Transect == 2)) & Distance >= 20 , "yes", thermokarst),
         thermokarst = ifelse(Site == "Wigand Creek" & Distance >= 40, "yes", thermokarst),
         thermokarst = ifelse(Site == "WWS Burned", "yes", thermokarst),
         thermokarst = ifelse(is.na(thermokarst), "no", thermokarst))

tkClassWide <- tkClass %>%
  pivot_wider(names_from  = year,
              values_from = thermokarst,
              names_prefix = "thermokarst") %>% 
  mutate(thermokarst2023 = ifelse(is.na(thermokarst2023), "no", thermokarst2023), 
         thermokarst2025 = thermokarst2023)

#Check categorization
df <- filter(select(tkClass, year, Site, Transect, Distance, thermokarst), year == 2013) %>%
  right_join(df2013) %>% 
  filter(Site != "Wigand Creek")

a <- ggplot(data = df, 
            aes(x=Distance, y= Elevation, color = thermokarst)) +
  geom_point(size = 1, alpha = 0.7)+
  scale_color_manual(values = tkPal, name = "Classification",
                     labels = c("Control", "Thermokarst"))+
  facet_wrap(Site~Transect, scales = "free", nrow = 2)+
  ylab(label = "Elevation (m)") + labs(color = "Thermokarst")+
  ggtitle("Thermokarst classification in 2013")+ xlab("Distance along transect (m)")+
  theme(legend.position = "bottom")
a

df <- filter(select(tkClass, year, Site, Transect, Distance, thermokarst), year == 2023) %>%
  right_join(df2023) %>% 
  filter(Site != "Wigand Creek")

b <- ggplot(data =df, 
            aes(x=Distance, y= Elevation, color = thermokarst)) +
  #geom_line(size = 1, alpha = 0.7)+
  geom_point(size = 1, alpha = 0.7)+
  scale_color_manual(values = tkPal, name = "Classification",
                     labels = c("Control", "Thermokarst"))+
  facet_wrap(Site~Transect, scales = "free", nrow = 2)+
  ylab(label = "Elevation (m)") + xlab("Distance along transect (m)")+
  ggtitle("Thermokarst classification in 2023")+
  theme(legend.position = "bottom")
b

plot <- (a | b + rremove("ylab")) + 
  (plot_layout(widths = c(1,1), guides = "collect") & theme(legend.position = "bottom")) + 
  plot_annotation(tag_levels = list(c('A)', 'B)', 'C)'), '1'))
plot

#ggsave(paste0(manDir, "figures/suppTKClass", Sys.Date(), ".png"), width = 34, height = 15, units = "cm", dpi = 500)


## Add in 2025 data -----------------------------------------------------
#2025 data
df2025 <- read.csv("datasets/df2025.csv")

# Transect data manipulation and calculations ----------------------------------------------
dfBothYearsWide <- left_join(df2023, df2013, by = c("Site", "Transect", "Distance")) %>% 
  select(-"Date.x", -"Date.y", -"year.x", -"year.y") %>% 
  setNames(c("Site", "Transect", "Distance", "thawDepth2023", "organicThickness2023", "tallestShrub2023", "NDVI2023", "Elevation2023", "thawDepth2013", "organicThickness2013", "tallestShrub2013", "NDVI2013", "Elevation2013")) %>% 
  mutate(deltaElevation = 100*(Elevation2023 - Elevation2013)) %>% #change to cm
  left_join(select(tkClassWide, -thermokarst2025), by = c("Site", "Transect", "Distance"))

#Calculate thaw penetration
deltaElevation <- dfBothYearsWide %>% 
  select(Site, Transect, Distance, thawDepth2023, deltaElevation) %>%   
  mutate(tp = thawDepth2023 + -1*deltaElevation) #convert so that increasing depth is positive

dfBothYearsLong <- rbind(df2013, df2023) %>%   
  left_join(select(deltaElevation, -deltaElevation, - thawDepth2023), by = c("Site", "Transect", "Distance")) %>% 
  left_join(select(tkClassWide, -thermokarst2025), by = c("Site", "Transect", "Distance")) %>% 
  mutate(year = as.factor(year), 
         Site = as.factor(Site),
         tp = ifelse(year == 2013, NA, tp))

dfAllYearsLong <- rbind(df2013, df2023, select(df2025, -tp, -thermokarst)) %>% 
  left_join(select(deltaElevation, -deltaElevation, - thawDepth2023), by = c("Site", "Transect", "Distance")) %>% 
  left_join(tkClassWide, by = c("Site", "Transect", "Distance")) %>% 
  mutate(year = as.factor(year), 
         Site = as.factor(Site),
         tp = ifelse(is.na(tp), thawDepth, tp),
         tp = ifelse(year == 2013 | year == 2025, NA, tp))

## Data for upload to LTER -------------------------------------------------
transectUpload <- dfAllYearsLong %>%
  rename(thermokarstClass = thermokarst2023,
         distance = Distance, site = Site, transect = Transect, date = Date, elevation = Elevation) %>%
  select(-thermokarst2013, -thermokarst2025)
#write.csv(transectUpload, file = paste0(manDir, "datasets/transectData", Sys.Date(), ".csv"))

# All transect data supplemental figures:-----------------------------------------
#Data for table
siteDescTable <- dfBothYearsLong %>% 
  group_by(Site, year, thermokarst2023) %>% 
  summarise(thawDepthMean = mean(thawDepth, na.rm = TRUE),
            thawDepth_se = plotrix::std.error(thawDepth, na.rm = TRUE),
            elevation = mean(Elevation, na.rm = TRUE),
            elevation_se = plotrix::std.error(Elevation, na.rm = TRUE),
            shrubHeight = mean(tallestShrub, na.rm = TRUE),
            shrubHeight_se = plotrix::std.error(tallestShrub, na.rm = TRUE),
            organic = mean(organicThickness, na.rm = TRUE),
            organic_se = plotrix::std.error(organicThickness, na.rm = TRUE))

#Thaw depth: 3 years
a <- ggplot(data = dfAllYearsLong, aes(x=Distance,y=thawDepth, group_by = year, color = year)) +
  geom_line(size = 1, alpha = 0.9)+
  scale_color_manual(values = threeYearPal)+
  scale_y_reverse()+labs(color = "Year")+
  facet_wrap(Site~Transect, scales = "free")+
  ylab(label = "Thaw depth (cm)") +
  xlab("Distance along transect (m)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))
a

#Elevation change: 2 years
b <- ggplot(data = dfBothYearsLong %>% 
              filter(!(year == 2023 & (Site == "Lower East Fork" & Transect == 1))), 
            aes(x=Distance,y=Elevation, group_by = year, color = year)) +
  geom_line(size = 1, alpha = 0.8)+
  scale_color_manual(values = twoYearPal)+
  labs(color = "Year")+
  facet_wrap(Site~Transect, scales = "free")+
  ylab(label = "Elevation (ft)") +
  xlab("Distance along transect (m)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))
b
ggarrange(a, b, labels = c("A)", "B)"))
#ggsave(paste0(manDir, "figures/suppThawElevation", Sys.Date(), ".png"), width = 35, height = 15, units = "cm", dpi = 500)

#Shrub height change
a <- ggplot(data = dfBothYearsLong, aes(x=Distance,y=tallestShrub, group_by = year, color = year)) +
  geom_line(size = 1, alpha = 0.9)+
  scale_color_manual(values = twoYearPal)+
  labs(color = "Year")+
  facet_wrap(Site~Transect, scales = "free")+
  ylab(label = "Height of tallest shrub (cm)") +
  xlab("Distance along transect (m)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))
a

#Organic layer change
b <- ggplot(data = dfBothYearsLong, aes(x=Distance,y=organicThickness, group_by = year, color = year)) +
  geom_line(size = 1, alpha = 0.9)+
  scale_color_manual(values = twoYearPal)+
  labs(color = "Year")+
  facet_wrap(Site~Transect, scales = "free")+
  ylab(label = "Thickness of organic layer (cm)") +
  xlab("Distance along transect (m)")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))
b 
ggarrange(a, b, labels = c("A)", "B)"))
#ggsave(paste0(manDir, "figures/suppVegTransect", Sys.Date(), ".png"), width = 35, height = 15, units = "cm", dpi = 500)

# Transect data: change in thaw metric analysis --------------------------------------

#change in thaw between 2013 and 2023 and 2025
thawDf <- dfAllYearsLong %>% 
  select(Site, Transect, Distance, year, thawDepth, thermokarst2023) %>%
  pivot_wider(names_from = year,
              values_from = thawDepth,
              names_prefix = "thaw_") %>% 
  mutate(delta_2013_2023 = -1*(thaw_2023 - thaw_2013),
         delta_2013_2025 = -1*(thaw_2025 - thaw_2013),
         delta_2023_2025 = -1*(thaw_2025 - thaw_2023)) %>% 
  pivot_longer(cols = starts_with("delta_"),
               names_to = "comparison",
               values_to = "deltaThaw") %>%
  mutate(comparison = recode(comparison,
                             "delta_2013_2023" = "2013–2023",
                             "delta_2013_2025" = "2013–2025",
                             "delta_2023_2025" = "2023–2025"))

#test significant changes
df <- subset(thawDf, comparison == "2013–2023") %>% 
  mutate(thawMean = mean(c(thaw_2013, thaw_2023), na.rm = TRUE))

mod <- lmer(thaw_2013 ~ thermokarst2023 + (1|Site), data = df)
sjPlot::tab_model(mod)
performance::check_model(mod)

#changes in 2025 thaw depth? 
df <- dfAllYearsLong %>% 
  mutate(year = as.factor(year))
mod <- lmer(thawDepth ~ year*thermokarst2023 + (1|Site), data = df)
sjPlot::tab_model(mod)
performance::check_model(mod)
pairs(emmeans::emmeans(mod, ~ year | thermokarst2023), adjust = "none")

#summary table of thaw depth
thawSum <- dfAllYearsLong %>% 
  select(Site, Transect, Distance, year, thawDepth, thermokarst2023) %>%
  pivot_wider(names_from = year,
              values_from = thawDepth,
              names_prefix = "thaw_") %>% 
  mutate(delta_2013_2023 = -1*(thaw_2023 - thaw_2013),
         delta_2013_2025 = -1*(thaw_2025 - thaw_2013)) %>%  
  group_by(thermokarst2023) %>% 
  summarise(meanGlobal = mean(c(thaw_2013, thaw_2023), na.rm = TRUE),
            seGlobal = plotrix::std.error(c(thaw_2013, thaw_2023), na.rm = TRUE),
            mean2013 = mean(thaw_2013, na.rm = TRUE),
            se2013 = plotrix::std.error(thaw_2013, na.rm = TRUE),
            mean2023 = mean(thaw_2023, na.rm = TRUE),
            se2023 = plotrix::std.error(thaw_2023, na.rm = TRUE),
            mean2025 = mean(thaw_2025, na.rm = TRUE),
            se2025 = plotrix::std.error(thaw_2025, na.rm = TRUE),
            mean10YrChange = mean(delta_2013_2023, na.rm = TRUE),
            se10YrChange = plotrix::std.error(delta_2013_2023, na.rm = TRUE),
            mean10YrChange = mean(delta_2013_2023, na.rm = TRUE),
            se10YrChange = plotrix::std.error(delta_2013_2023, na.rm = TRUE))

#summary table of thaw penetration
tpSum <- dfAllYearsLong %>% 
  select(Site, Transect, Distance, year, tp, thermokarst2023) %>%
  #only year with thaw penetration calculated, remove outlier site
  filter(year == 2023 & !(Site == "Lower East Fork" & Transect == 1)) %>% 
  group_by(thermokarst2023) %>% 
  summarise(meanGlobal = mean(tp, na.rm = TRUE),
            seGlobal = plotrix::std.error(tp, na.rm = TRUE))

#test differences in thaw penetrations
df <- dfAllYearsLong %>% 
  filter(!(Site == "Lower East Fork" & Transect == 1)) %>%
  select(Site, tp, thawDepth, thermokarst2023) %>% 
  pivot_longer(cols = c(tp, thawDepth),
               names_to = "measurement",
               values_to = "value") 

mod <- lmer(value ~ measurement*thermokarst2023 + (1|Site), data = df)
tab_model(mod)  

emm <- emmeans(mod, ~ measurement | thermokarst2023)

contrasts <- pairs(emm) %>%
  summary(infer = TRUE) %>%
  as.data.frame() %>%
  filter(grepl("tp", contrast) & grepl("thaw", contrast)) %>%
  mutate(sig = p.value < 0.05,
         label = ifelse(sig, "*", ""))

#summary of average thaw depth and thaw penetration by years
thawDepthYearAvg <- dfAllYearsLong %>% 
  mutate(tp = ifelse(year == 2023 & (Site == "Lower East Fork" & Transect == 1), NA, tp)) %>%    #only year with thaw penetration calculated and remove site where elevation seems wrong
  group_by(year, thermokarst2023) %>% 
  summarise(meanThaw = mean(thawDepth, na.rm = TRUE),
            seThaw = plotrix::std.error(thawDepth, na.rm = TRUE),
            meanTp = mean(tp, na.rm = TRUE),
            seTp = plotrix::std.error(tp, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(meanThaw, seThaw, meanTp, seTp),
               names_to = "measurement",
               values_to = "value") %>%
  mutate(stat = str_extract(measurement, "^(mean|se)"),
         type = str_remove(measurement, "^(mean|se)"),
         type = tolower(type)) %>%
  select(-measurement) %>%
  pivot_wider(names_from = stat,
              values_from = value) %>% 
  mutate(type = ifelse(year == 2013 & type == "thaw", "thaw 2013", type),
         type = ifelse(year == 2023 & type == "thaw", "thaw 2023", type),
         fct = ifelse(type %in% c("thaw 2023", "thaw 2013"), "thaw", ''))

#to-do: add sig values here?
# annot_df <- thawDepthYearAvg %>%
#   filter(year == 2023, type %in% c("tp", "thaw 2023")) %>%
#   group_by(thermokarst2023) %>%
#   summarise(y = min(mean, na.rm = TRUE) + 35,   # because y-axis is reversed
#             .groups = "drop") %>%
#   left_join(contrasts, by = "thermokarst2023") %>%
#   filter(sig)

ggplot(data = subset(thawDepthYearAvg, year !=2025), aes(x=type, y=mean, pattern = type,
                                                         groups = thermokarst2023, fill = thermokarst2023)) +
  facet_wrap(~thermokarst2023, scales = "free_x",
             labeller = as_labeller(c(`no` = "Control tundra",
                                      `yes` = "Inside thermokarst")))+
  geom_vline(xintercept = 2.5, color = "black", linetype = "dashed", size = 1)+
  geom_bar(position=position_dodge(0.9), stat="identity", color = "black", alpha = 0.7)+
  geom_bar_pattern(position = position_dodge(0.9),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_density = 0.1,
                   pattern_spacing = 0.02,
                   pattern_size = 0.2)+
  scale_pattern_manual(values = c("thaw 2013" = "none", "thaw 2023" = "stripe", "tp" = "crosshatch")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, groups = thermokarst2023),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  scale_fill_manual(values = tkPalAlpha)+
  ylab(label = "Depth (cm)")+
  xlab("Measurement")+
  #scale_x_discrete(labels = c("2013 \nthaw depth","2023 \nthaw depth", stringr::str_wrap("2023 thaw penetration", width = 15)))+
  scale_x_discrete(labels = stringr::str_wrap(c("Thaw depth: 2013","Thaw depth: 2023", "Thaw penetration: 2023"),
                                              width = 12))+
  scale_y_reverse()+
  theme(legend.position = "none")

#ggsave(paste0(manDir, "figures/thawVsThawPen", Sys.Date(), ".png"), width = 25, height = 15, units = "cm", dpi = 500)

thawDfSum <- thawDf %>% 
  group_by(Site, thermokarst2023, comparison) %>% 
  summarise(meanDelta = mean(deltaThaw, na.rm = TRUE),
            seDelta = plotrix::std.error(deltaThaw, na.rm = TRUE))

#plot change in thaw over all three years
ggplot(data = thawDf, aes(x=Distance,y=deltaThaw, group_by = comparison, color = comparison)) +
  geom_line(size = 1, alpha = 0.7)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = threeYearPal)+
  facet_wrap(Site~Transect, scales = "free")+
  ylab(label = "Change in thaw depth (cm)") 

#summary table of mean change in three years
thawDfSum <- thawDf %>% 
  group_by(thermokarst2023, comparison) %>% 
  summarise(meanDelta = mean(deltaThaw, na.rm = TRUE),
            seDelta = plotrix::std.error(deltaThaw, na.rm = TRUE))

#test for sig. difference
df <- thawDf %>% filter(comparison == "2013–2023") 
mod <- lmer(deltaThaw ~ thermokarst2023 + (1|Site), data = df)
sjPlot::tab_model(mod)
performance::check_model(mod)

emm_pairs <- emmeans::emmeans(mod, ~ thermokarst2023) %>%
  pairs(adjust = "none") %>%
  as.data.frame()

thawDfSum <- thawDf %>%
  filter(comparison == "2013–2023") %>% 
  group_by(thermokarst2023) %>% 
  summarise(meanDelta = mean(deltaThaw, na.rm = TRUE),
            seDelta = plotrix::std.error(deltaThaw, na.rm = TRUE))

## Change in elevation over time --------------------------------------
deltaElevSum <- deltaElevation %>%
  left_join(select(tkClassWide, -thermokarst2013, -thermokarst2025), by = c("Site", "Transect", "Distance")) %>%
  filter(Site != "Lower East Fork" & Transect == 1) %>%
  mutate(tpMinusThaw = tp - thawDepth2023) %>%
  group_by(thermokarst2023) %>%
  summarise(meanDelta = mean(deltaElevation, na.rm = TRUE),
            seDelta = plotrix::std.error(deltaElevation, na.rm = TRUE),
            meanTp = mean(tp, na.rm = TRUE),
            seTp = plotrix::std.error(tp, na.rm = TRUE),
            meanThaw = mean(thawDepth2023, na.rm = TRUE),
            seThaw = plotrix::std.error(thawDepth2023, na.rm = TRUE),
            #How much did elevation change increase thaw penetration compared to thaw depth?
            meanTpMinusThaw = mean(tpMinusThaw, na.rm = TRUE),
            seTpMinusThaw = plotrix::std.error(tpMinusThaw, na.rm = TRUE))

#test for sig. difference
df <- deltaElevation %>%
  left_join(select(tkClassWide, -thermokarst2013, -thermokarst2025), by = c("Site", "Transect", "Distance"))
mod <- lmer(deltaElevation~ thermokarst2023 + (1|Site), data = df)
sjPlot::tab_model(mod)
performance::check_model(mod)

annot_df<- emmeans::emmeans(mod, ~ thermokarst2023) %>%
  pairs(adjust = "none") %>%
  as.data.frame() %>% 
  mutate(label =  ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", format.pval(p.value, digits = 2, eps = 0.001))))

ggplot(data = deltaElevSum, aes(x=thermokarst2023, y=meanDelta, groups = thermokarst2023, fill = thermokarst2023)) +
  geom_bar(position=position_dodge(0.9), stat="identity", color = "black", alpha = 0.7)+
  geom_errorbar(aes(ymin=meanDelta-seDelta, ymax=meanDelta+seDelta),
                width=.2,                   
                position=position_dodge(.9))+
  scale_fill_manual(values = tkPalAlpha)+
  geom_text(data = annot_df,
            aes(x = 1.5, y = 10, label = label),
            inherit.aes = FALSE,
            size = 4)+
  ylab(label = "Elevation change (cm)")+
  xlab("Sample location")+
  scale_x_discrete(labels = stringr::str_wrap(c("Control tundra", "Inside thermokarst"), width = 10))+
  theme(legend.position = "none")

#ggsave(paste0(manDir, "figures/suppElevationChange", Sys.Date(), ".png"), width = 15, height = 15, units = "cm", dpi = 500)


# Transect data: change in vegetation over time -----------------------------------------

#test significant differences in shrub height
df <- dfBothYearsLong %>% 
  mutate(year = as.factor(year))

mod <- lmer(tallestShrub ~ thermokarst2023*year + (1|Site), data = df)
performance::check_model(mod)
pairs(emmeans::emmeans(mod, ~ year | thermokarst2023), adjust = "none")
pairs(emmeans::emmeans(mod, ~ thermokarst2023 | year), adjust = "none")

#change over time
changeShrub <- dfBothYearsWide %>% 
  mutate(deltaShrub = tallestShrub2023 - tallestShrub2013) %>% 
  group_by(thermokarst2023) %>% 
  summarise(meanChangeShrub = mean(deltaShrub, na.rm = TRUE),
            seChangeShrub = plotrix::std.error(deltaShrub, na.rm = TRUE))

#mean values
shrubSum <- dfBothYearsLong %>% 
  rename(Year = year) %>% 
  group_by(thermokarst2023, Year) %>% 
  summarise(meanShrub = mean(tallestShrub, na.rm = TRUE),
            seShrub = plotrix::std.error(tallestShrub, na.rm = TRUE),
            meanOrg = mean(organicThickness, na.rm = TRUE),
            seOrg = plotrix::std.error(organicThickness, na.rm = TRUE),
            meanElevation = mean(Elevation, na.rm = TRUE),
            seElevation = plotrix::std.error(Elevation, na.rm = TRUE),
            meanThaw = mean(thawDepth, na.rm = TRUE),
            seThaw = plotrix::std.error(thawDepth, na.rm = TRUE))

#test significant differences
df <- dfBothYearsLong
mod <- lmer(tallestShrub ~ thermokarst2023*year + (1|Site), data = df)
sjPlot::tab_model(mod)
performance::check_model(mod)

annot_df <- emmeans::emmeans(mod, ~ year | thermokarst2023) %>%
  pairs(adjust = "none") %>%
  as.data.frame() %>% 
  mutate(label =  ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", format.pval(p.value, digits = 2, eps = 0.001))),
         x = thermokarst2023,
         y = shrubSum %>%
           group_by(thermokarst2023) %>%
           summarise(y = max(meanShrub + seShrub, na.rm = TRUE)) %>%
           pull(y) * 1.1)

a <- ggplot(data = shrubSum, aes(x=thermokarst2023, y=meanShrub, pattern = Year, fill = thermokarst2023)) +
  geom_bar(position=position_dodge(), stat="identity", color = "black")+
  geom_bar_pattern(position = position_dodge(0.9),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_density = 0.1,
                   pattern_spacing = 0.02,
                   pattern_size = 0.2)+
  geom_errorbar(aes(ymin=meanShrub-seShrub, ymax=meanShrub+seShrub),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_text(data = annot_df,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            size =5)+
  scale_fill_manual(values = tkPalAlpha)+
  scale_pattern_manual(values = c("2013" = "none", "2023" = "stripe")) +
  ylab(label = "Height of tallest shrub (cm)")+
  xlab("Sample location")+
  scale_x_discrete(labels = stringr::str_wrap(c("Control tundra", "Inside thermokarst"), width = 10))+
  guides(fill = "none",
         pattern = guide_legend(override.aes = list(fill = "#FFFFFF"))) 
a

#test significant differences
df <- dfBothYearsLong 
mod <- lmer(organicThickness ~ thermokarst2023*year + (1|Site), data = df)
performance::check_model(mod)
pairs(emmeans::emmeans(mod, ~ year | thermokarst2023), adjust = "none")
pairs(emmeans::emmeans(mod, ~ thermokarst2023 | year), adjust = "none")
sjPlot::tab_model(mod)
performance::check_model(mod)

annot_df <- emmeans::emmeans(mod, ~ year | thermokarst2023) %>%
  pairs(adjust = "none") %>%
  as.data.frame() %>%
  mutate(label = paste0("p = ", format.pval(p.value, digits = 2, eps = 0.001)),
         x = thermokarst2023,
         y = shrubSum %>%
           group_by(thermokarst2023) %>%
           summarise(y = max(meanOrg + seOrg, na.rm = TRUE)) %>%
           pull(y) * 1.1)

b <- ggplot(data = shrubSum, aes(x=thermokarst2023, y=meanOrg, pattern = Year, fill = thermokarst2023)) +
  geom_bar(position=position_dodge(), stat="identity", color = "black")+
  geom_hline(yintercept = 20, linetype = "dashed", linewidth = 1, alpha = 0.7)+
  geom_bar_pattern(position = position_dodge(0.9),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_density = 0.1,
                   pattern_spacing = 0.02,
                   pattern_size = 0.2)+
  geom_text(data = annot_df,
            aes(x = x, y = 22, label = label),
            inherit.aes = FALSE,
            size =5)+
  geom_errorbar(aes(ymin=meanOrg-seOrg, ymax=meanOrg+seOrg),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  scale_fill_manual(values = tkPalAlpha)+
  scale_pattern_manual(values = c("2013" = "none", "2023" = "stripe")) +
  ylab(label = "Thickness of organic layer (cm)")+
  xlab("Sample location")+
  scale_x_discrete(labels = stringr::str_wrap(c("Control tundra", "Inside thermokarst"), width = 10))+
  guides(fill = "none",
         pattern = guide_legend(override.aes = list(fill = "#FFFFFF"))) 
b

plot <- (a + xlab(NULL)) + (b + xlab(NULL)) + plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = list(c('A)', 'B)', 'C)'), '1'),
                  caption = "Sample location")  &
  theme(
    plot.caption = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5,
      color = "black",
      margin = margin(t = 10)
    ),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )
plot
#ggsave(paste0(manDir, "figures/vegMetrics", Sys.Date(), ".png"), width = 20, height = 15, units = "cm", dpi = 500)

# 14C of DOC analysis -----------------------------------------------------

#Import water sampling data (2023)
c14 <- read.csv("datasets/c14.csv")

c14Transect <- c14 %>% 
  filter(!Site %in% c("WWS_burned", "WWS_unburned")) %>% #remove fire sites
  select(-Date, -`Estimated.volume`, -timestamp, -Latitude, -Longitude, -`notes.from.collection`, -Notes.from.data.compiling, 
         -`sample.label`, -ChameleonID) %>% 
  rename(Distance = nearestTransectDistance,
         SampleType = 'Surface.or.Pore', 
         Delta14 = DOC_Delta14C,
         DeltaDelta14 = DOC_DeltaDelta14C) %>% 
  mutate(Transect = as.numeric(ifelse(Transect == "1 or 2", 1, Transect)), #make the points from pitchfork that can come from 1 or 2 transect 1 (confirmed with GPS points)
         Distance = as.numeric(Distance), 
         Depth = as.numeric(Depth),
         SampleType = tolower(SampleType),
         DeltaDelta14 = Delta14 - Atmosphere) %>% 
  left_join(y = df2023, by = c("Site", "Transect", "Distance")) %>% 
  left_join(y = deltaElevation, by = c("Site", "Transect", "Distance")) %>% 
  left_join(y = select(dfBothYearsWide, Site, Transect, Distance, thermokarst2023), by = c("Site", "Transect", "Distance")) %>% 
  mutate(thermokarstDOC = ifelse(is.na(thermokarst2023), Thermokarst, thermokarst2023)) #if there is no thermokarst classification from the transect point, use a field notes

## Data for upload to LTER -------------------------------------------------
docUpload <- c14Transect %>%
  filter(SampleType == "pore" & !is.na(F14C)) %>% 
  select(c(Year:SampleType), -PolygonID, Depth, c(F14C:Delta14C.error), atm, DeltaDelta14, thermokarstClass = thermokarstDOC) %>% 
  rename(year = Year, distance = Distance, depth = Depth, site = Site, transect = Transect, sampleID = SampleID, sampleType = SampleType,
         F14CSe = F14C.error, delta14C = Delta14, delta14CSe = Delta14C.error, deltaDelta14C = DeltaDelta14) 
#write.csv(docUpload, file = paste0(manDir, "datasets/docData", Sys.Date(), ".csv"))

#Summarize radiocarbon of DOC data
d14Sum <- c14Transect %>%
  filter(SampleType == "pore") %>% 
  mutate(Thermokarst = thermokarstDOC) %>%
  group_by(Thermokarst) %>%
  summarise(mean14C = mean(DeltaDelta14, na.rm = TRUE),
            se14C = plotrix::std.error(DeltaDelta14, na.rm = TRUE))

#Statistical differences in mean
df <- c14Transect %>% 
  filter(SampleType == "pore") #select porewater not surface water samples

dfSum <- df %>% 
  group_by(Thermokarst) %>% 
  summarise(mean = mean(Delta14, na.rm = TRUE),
            se = plotrix::std.error(Delta14, na.rm = TRUE),
            n = n())

mod <- lmer(Delta14 ~ Thermokarst + (1|Site), data = df)
summary(mod)
performance::check_model(mod)
sjPlot::tab_model(mod)

annot_df <- emmeans::emmeans(mod, ~ Thermokarst) %>%
  pairs(adjust = "none") %>%
  as.data.frame() %>%
  mutate(label = paste0("p = ", format.pval(p.value, digits = 1, eps = 0.001)))

a <- ggplot(data = subset(c14Transect, SampleType == "pore"), aes(x = Thermokarst, y = Delta14, fill = Thermokarst)) +
  geom_boxplot(alpha = 0.7)+
  scale_fill_manual(values = c("#19798BFF", "#B80422FF"), name = "Thermokarst", 
                    labels = c("Control tundra", "Thermokarst gully"))+
  #ylim(-500, -100)+
  scale_x_discrete(labels = stringr::str_wrap(c("Control tundra", "Inside thermokarst"), width = 10))+
  ggtitle("Soil porewater (all sites)")+ xlab("Sample location") +
  ylab(bquote(bold(~Delta^14~C~"‰ of DOC")))+
  geom_text(data = annot_df,
            aes(x = 1.5, y = 0, label = label),
            inherit.aes = FALSE,
            size = 7)+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 19),
        axis.title.x = element_text(size = 19, margin = margin(t = 10)),
        strip.text.x = element_text(size = 12, color = "black"),
        legend.position = "none")
a


#Wigand Creek Thaw depth versus 14C
b <- ggplot(subset(c14Transect, Site == "Wigand Creek" & SampleType != "surface"))+
  geom_point(aes(x = thawDepth, y = Delta14, shape = as.factor(SampleType)),color = "black", size = 3)+
  ggtitle("Soil porewater (Wigand Creek)")+ xlab("Thaw Depth (cm)") + 
  ylab(bquote(bold(~Delta^14~C~"‰ of DOC")))+
  labs(shape = "Sample Type")+
  xlim(35, 100)+
  ggpmisc::stat_poly_line(aes(x = thawDepth, y = Delta14), color = "black", alpha = 0.25) +
  ggpmisc::stat_poly_eq(aes(x = thawDepth, y = Delta14,
                            label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
                        formula = y ~ x,
                        label.x = "right",
                        label.y = "top",
                        size = 6) +
  geom_point(aes(x = thawDepth, y = Delta14, shape = as.factor(SampleType)),color = "black", size = 3)+
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 12, color = "black"))
b

## 14C of mushrooms -----------------------------------------------------
mushroom <- read.csv("datasets/mushroom.csv")


## Data for upload to LTER -------------------------------------------------
mushroomUpload <- mushroom %>% 
  filter(!is.na(d14C)) %>% 
  rename(year = Year, atmosphere = Atmosphere, site = Site, F14C = Fm, delta14C = d14C, deltaDelta14C = dd14C)

#write.csv(mushroomUpload, file = paste0(manDir, "datasets/mushroomData", Sys.Date(), ".csv"))

#Calculate min mean and max for boxplot
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

c <- ggplot(data = mushroom, aes(x = factor(0), y = d14C)) +
  stat_summary(fun.data = min.mean.sd.max, fill = "#B8042270", alpha = 0.7, geom = "boxplot")+
  geom_jitter(width = 0.2, alpha = 0.7, size = 2.5)+
  ggtitle("Mushroom samples")+ 
  ylab(bquote(bold(~Delta^14~C~"‰")))+
  theme(axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12, color = "black"),
        legend.position = "none")
c
plot <- (a | b | c) + plot_layout(widths = c(0.75,1, 0.5)) + plot_annotation(tag_levels = list(c('A)', 'B)', 'C)'), '1'))
plot
#ggsave(paste0(manDir, "figures/14CDOC", Sys.Date(), ".png"), width = 30, height = 15, units = "cm", dpi = 500)


# Wigand Creek transect summary -------------------------------------------------------
wcPlot <- dfBothYearsWide %>%
  left_join(select(deltaElevation, Site, Transect, Distance, tp), by = c("Site","Transect", "Distance")) %>% 
  filter(Site == "Wigand Creek") %>% 
  select(-thermokarst2013) %>% 
  #calculate metrics from 0 point
  mutate(thawDepth2013 = Elevation2013 - (thawDepth2013/100),
         thawDepth2023 = Elevation2023 - (thawDepth2023/100),
         tp = Elevation2023 - (tp/100)) %>% 
  pivot_longer(cols = -c("Site", "Transect", "Distance", "thermokarst2023"),
               names_to = "measurement",
               values_to = "value")

thawMsmts <- ggplot() + 
  geom_ribbon(data = wcPlot %>% 
                filter(measurement %in% c("Elevation2013", "tp")) %>%
                tidyr::pivot_wider(names_from  = measurement,
                                   values_from = value),
              aes(x = Distance,ymin = pmin(Elevation2013, tp),ymax = pmax(Elevation2013, tp)),
              fill = "#8B775F",
              alpha = 0.25) +
  geom_ribbon(data = wcPlot %>% filter(measurement == "tp"),
              aes(x = Distance,ymin = value,ymax = -Inf),
              fill = "steelblue",
              alpha = 0.25) +
  geom_line(data = subset(wcPlot,
                          measurement %in% c("Elevation2013","tp","thawDepth2013","thawDepth2023")),
            aes(x = Distance, y = value, color = measurement, linetype = measurement),
            linewidth = 1) +
  scale_color_manual(values = c(Elevation2013 = "#8B775F",
                                tp            = "steelblue",
                                thawDepth2013 = "#999999", 
                                thawDepth2023 = "black"),
                     labels = c("Baseline elevation",
                                "Thaw depth: 2013",
                                "Thaw depth: 2023",
                                "Thaw penetration")) +
  scale_linetype_manual(values = c(Elevation2013 = "solid",
                                   tp            = "solid",
                                   thawDepth2013 = "dashed",
                                   thawDepth2023 = "dashed"),
                        labels = c("Baseline elevation",
                                   "Thaw depth: 2013",
                                   "Thaw depth: 2023",
                                   "Thaw penetration")) +
  labs(x = "Distance (m)", y = "Elevation (m)") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
thawMsmts

wcdf<- subset(dfBothYearsLong, Site == "Wigand Creek") %>%
  select(-NDVI, -Date, -Transect, - Site, -organicThickness, -thermokarst2013) %>%
  pivot_longer(cols = -c("year","Distance", "thermokarst2023"), names_to = "measurement") 

shrub_wide <- wcdf %>%
  filter(measurement == "tallestShrub") %>%
  select(Distance, year, thermokarst2023, value) %>%
  pivot_wider(names_from = year,
              values_from = value) %>%
  mutate(deltaShrub = `2023` - `2013`) %>% 
  group_by(thermokarst2023) %>%
  summarise(mean_change = mean(deltaShrub, na.rm = TRUE),
            se_change   = sd(deltaShrub, na.rm = TRUE) / sqrt(sum(!is.na(deltaShrub))),
            .groups = "drop") %>%
  mutate(thermokarst2023 = factor(
    thermokarst2023,
    levels = c("no", "yes"),
    labels = c("Control tundra", "Inside thermokarst")))

tk_rects <- wcdf %>%
  filter(measurement == "thawDepth") %>%
  arrange(Distance) %>%
  mutate(start = Distance,
         end = lead(Distance, default = max(Distance)),
         tk = thermokarst2023) %>%
  filter(tk == "yes")

thawPlot <- ggplot() +
  geom_rect(data = tk_rects, aes(xmin = start, xmax = end,ymin = -Inf,ymax = Inf),
            fill = tkPal[-1],
            alpha = 0.10) +
  geom_line(data = subset(wcdf, measurement == "thawDepth"),
            aes(x = Distance, y = value, linetype = year, color = year, group = year),
            linewidth = 1) +
  scale_linetype_manual(values = c("2013" = "solid", "2023" = "dashed"), name = "Year") +
  scale_color_manual(values = rev(twoYearPal),name = "Year") +
  scale_y_reverse(limits = c(150, 0)) +
  ylab(stringr::str_wrap("Thaw depth (cm)", width = 10)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
thawPlot

ggplot() +
  geom_rect(data = tk_rects, aes(xmin = start, xmax = end,ymin = -Inf,ymax = Inf),
            fill = tkPal[-1],
            alpha = 0.10) +
  geom_line(data = subset(wcdf, measurement == "Elevation"),
            aes(x = Distance, y = value, linetype = year, color = year, group = year),
            linewidth = 1) +
  scale_linetype_manual(values = c("2013" = "solid", "2023" = "dashed"), name = "Year") +
  scale_color_manual(values = rev(twoYearPal),name = "Year") +
  #scale_y_reverse(limits = c(150, 0)) +
  ylab(stringr::str_wrap("Thaw depth (cm)", width = 10)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))

gpsPlot <- ggplot() +
  geom_rect(data = tk_rects,aes(xmin = start,
                                xmax = end,
                                ymin = -Inf,
                                ymax = Inf),
            fill = tkPal[-1],
            alpha = 0.15) +
  geom_line(data = subset(wcdf, measurement == "Elevation"),
            aes(x = Distance, y = value, linetype = year, color = year, group = year),
            linewidth = 1) +
  scale_linetype_manual(values = c("2013" = "solid", "2023" = "dashed"), name = "Year") +
  scale_color_manual(values = rev(twoYearPal),name = "Year") +
  ylab(stringr::str_wrap("Elevation (m)", width = 10)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
gpsPlot

shrubPlot <- ggplot(data = subset(wcdf, measurement == "tallestShrub"), aes(x=Distance, y=value, group_by = year, fill = year)) +
  geom_rect(data = tk_rects,aes(xmin = start,
                                xmax = end,
                                ymin = -Inf,
                                ymax = Inf),
            fill = tkPal[-1],
            alpha = 0.15) +
  geom_bar(stat= "identity", alpha = 0.7, position= position_dodge(), color = "black")+
  scale_fill_manual(values = twoYearPal, name = "Year")+
  ylab(label=stringr::str_wrap("Height of tallest shrub (cm)", width=15))+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        #axis.line.x = element_line(colour = "#990000", size = 1.25)
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
shrubPlot

combined <- (thawMsmts /shrubPlot) & xlab(NULL)
combined

combined <- wrap_elements(panel = combined) +
  labs(tag = "Distance along transect (m)") +
  theme(plot.tag = element_text(size = 15),
        plot.tag.position = "bottom", 
        legend.text = element_text(size = 14))+
  plot_annotation(title = "Wigand Creek Transect 10-Year Resurvey ",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))
combined


# t-tests for tp and tallestShrub
tp_test <- t.test(value ~ thermokarst2023, data = subset(wcdf, measurement == "tp"))
shrub_test <- t.test(value ~ thermokarst2023, data = subset(wcdf, measurement == "tallestShrub"))

# create labels: "*" if significant, "" if not
tp_label <- ifelse(tp_test$p.value < 0.01, "p < 0.001", paste0("p =", format.pval(tp_test$p.value, digits = 1, eps = 0.001)))
shrub_label <- ifelse(shrub_test$p.value < 0.01, "p < 0.001", paste0("p =", format.pval(shrub_test$p.value, digits = 3, eps = 0.001)))

sig_labels <- tibble::tibble(
  measurement = c("tp", "tallestShrub"),
  y = c(mean(wcdf$value[wcdf$measurement == "tp" & wcdf$thermokarst2023 == "yes"], na.rm = TRUE) * 1.25,
        mean(wcdf$value[wcdf$measurement == "tallestShrub" & wcdf$thermokarst2023 == "yes"], na.rm = TRUE) * 1.25),
  label = c(tp_label, shrub_label))

wcdf_sum <- wcdf %>%
  filter(measurement %in% c("tp", "tallestShrub")) %>%
  group_by(measurement, thermokarst2023) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
            .groups = "drop") %>% 
  #left_join(tp_label) %>% 
  #left_join(shrub_label) %>% 
  mutate(thermokarst2023 = factor(
    thermokarst2023,
    levels = c("no", "yes"),
    labels = stringr::str_wrap(c("Control tundra", "Inside thermokarst"), width = 10)))

changeShrub <- dfBothYearsWide %>% 
  filter(Site == "Wigand Creek") %>% 
  mutate(deltaShrub = tallestShrub2023 - tallestShrub2013) %>% 
  group_by(thermokarst2023) %>% 
  summarise(meanChangeShrub = mean(deltaShrub, na.rm = TRUE),
            seChangeShrub = plotrix::std.error(deltaShrub, na.rm = TRUE))

summaryBarPlot <- ggplot(wcdf_sum, 
                         aes(x = thermokarst2023, y = mean, fill = thermokarst2023)) +
  geom_col(width = 0.7,color = "black",alpha = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 0.8) +
  facet_wrap(~ measurement, scales = "free_y", nrow = 1,
             labeller = as_labeller(c(
               tp    = stringr::str_wrap("Thaw penetration (cm)", width = 15),
               tallestShrub = stringr::str_wrap("Height of tallest shrub (cm)", width = 15)))) +
  scale_fill_manual(values = tkPal,name = NULL) +
  ylab(NULL) +
  xlab(NULL) +
  geom_text(data = sig_labels,
            aes(x = 1.5, y = y, label = label),
            inherit.aes = FALSE,
            size = 6)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "none")
summaryBarPlot

ggarrange(combined, summaryBarPlot, widths = c(1, 0.45), labels = c("A)", "B)"))

#ggsave(paste0(manDir, "figures/WCTransect", Sys.Date(), ".png"), width = 35, height = 15, units = "cm", dpi = 500)


# Wigand Creek Intensive survey -------------------------------------------------

## Spruce tree rings --------------------------------------------------------
treeDat <- read.csv("datasets/treeDat.csv")


### Data for upload to LTER -------------------------------------------------
treeRingUpload  <- treeDat %>%
  rename(site = Site, species = SpeciesCode, sampleID = SampleID, polygonID = PolygonID)

#write.csv(treeRingUpload, file = paste0(manDir, "datasets/treeRingData", Sys.Date(), ".csv"))


# all vertical lines
polygonLines <- data.frame(polygonID = c("TK4", "TK4B", "TK3", "TK2", "TK2B", "TK1", "C", "CB"),
                           distanceFromCreek = c(93, 111, 163, 226, 229, 289, 341, 355))

# only label main sites
polygonLabels <- data.frame(polygonID = c("TK4", "TK3", "TK2", "TK1", "C"),
                            distanceFromCreek = c(93, 163, 226, 289, 341))

ggplot(subset(treeDat, SpeciesCode == "WS"),
       aes(x = distanceFromCreek, y = treeAge)) +
  geom_segment(data = polygonLines, aes(x = distanceFromCreek, xend = distanceFromCreek,
                                        y = 10, yend = 25, color = gsub("B$", "", polygonID)),  # TK4B -> TK4, etc.
               linetype = "dashed", linewidth = 2, alpha = 0.7) +
  geom_text(data = polygonLabels, 
            aes(x = distanceFromCreek, y = 25.5, label = polygonID),
            vjust = 0,
            fontface = "bold",
            show.legend = FALSE) +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 1),
              color = "black",
              alpha = 0.25) +
  ggpmisc::stat_poly_eq(formula = y ~ poly(x, 1),
                        aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
                        label.x = "right",
                        label.y = "top") +
  scale_color_manual(values = c(TK4 = wcPal[5],
                                TK3 = wcPal[4],
                                TK2 = wcPal[3],
                                TK1 = wcPal[2],
                                C   = wcPal[1])) +
  geom_point(size = 2.5) +
  theme(legend.position = "none")

ggplot(subset(treeDat, SpeciesCode == "SA"),
       aes(x = distanceFromCreek, y = treeAge)) +
  geom_segment(data = polygonLines, aes(x = distanceFromCreek, xend = distanceFromCreek,
                                        y = 10, yend = 25, color = gsub("B$", "", polygonID)),  # TK4B -> TK4, etc.
               linetype = "dashed", linewidth = 2, alpha = 0.7) +
  geom_text(data = polygonLabels, 
            aes(x = distanceFromCreek, y = 25.5, label = polygonID),
            vjust = 0,
            fontface = "bold",
            show.legend = FALSE) +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 1),
              color = "black",
              alpha = 0.25) +
  ggpmisc::stat_poly_eq(formula = y ~ poly(x, 1),
                        aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
                        label.x = "right",
                        label.y = "top") +
  scale_color_manual(values = c(TK4 = wcPal[5],
                                TK3 = wcPal[4],
                                TK2 = wcPal[3],
                                TK1 = wcPal[2],
                                C   = wcPal[1])) +
  geom_point(size = 2.5) +
  theme(legend.position = "none")

a <- ggplot(subset(treeDat, SpeciesCode != "BN"), aes(x = distanceFromCreek, y = treeAge)) +
  geom_segment(data = polygonLines, aes(x = distanceFromCreek, xend = distanceFromCreek,
                                        y = 0, yend = 25, color = gsub("B$", "", polygonID)),  # TK4B -> TK4, etc.
               linetype = "dashed", linewidth = 2, alpha = 0.7) +
  geom_text(data = polygonLabels, 
            aes(x = distanceFromCreek, y = 25.5, label = polygonID),
            vjust = 0,
            fontface = "bold",
            size = 7,
            show.legend = FALSE) +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 1),
              color = "black",
              alpha = 0.25) +
  ggpmisc::stat_poly_eq(formula = y ~ poly(x, 1),
                        aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
                        label.x = "right",
                        label.y = "top",
                        size = 7) +
  scale_color_manual(values = c(TK4 = wcPal[5],
                                TK3 = wcPal[4],
                                TK2 = wcPal[3],
                                TK1 = wcPal[2],
                                C   = wcPal[1])) +
  xlab("Distance from Creek (m)") +
  ylab("Tree Age (years)") +
  geom_point(size = 2.5) +
  theme(legend.position = "none")
a

## Gully expansion analysis ------------------------------------------------
gully <- read.csv("datasets/gully.csv")


b <- ggplot(data = subset(gully, !is.na(year)), aes(x = year, y = headwallDistance))+
  geom_smooth(method = "loess", color = "grey33", alpha = 0.15, se = FALSE)+
  geom_point(size = 2)+
  xlab("Year of imagery")+
  ylab(stringr::str_wrap("Headwall distance from Wigand Creek (m)", width = 25))
b

ggarrange(b, a, widths = c(0.6, 1), labels = c("A)", "B)"))

#ggsave(paste0(manDir, "figures/gullyTiming", Sys.Date(), ".png"), width = 30, height = 15, units = "cm", dpi = 500)

## Polygon elevation analysis -------------------------------------------------
#Elevation collected with Trimble in field
elevation <- read.csv("datasets/elevation.csv")

elevationSum <- elevation %>% 
  group_by(polygon) %>% 
  summarize(seElev = plotrix::std.error(LocalEllip, na.rm = TRUE),
            meanElev = mean(LocalEllip, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(polygon = as.factor(polygon),
         se = as.numeric(seElev),
         mean = as.numeric(meanElev))

ggplot(elevationSum, aes(x = polygon, y = mean, fill = polygon)) +
  geom_col(width = 0.7, alpha = 0.7, color = "black") +                     # main bars
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),   # error bars
                width = 0.2, color = "black") +
  labs(x = "Polygon", y = "Elevation at poylgons (m)", fill = "Polygon") +
  scale_fill_manual(values = wcPal) +
  coord_cartesian(ylim = c(min(elevationSum$mean - elevationSum$se) - 0.1,
                           max(elevationSum$mean + elevationSum$se) + 0.1))+
  theme(legend.position = "none")

## Vegetation composition and NDVI -------------------------------------------------
veg <- read.csv("datasets/veg.csv")


### Data for upload to LTER -------------------------------------------------
wcVegUpload <- veg %>%
  select(date = Date, site = Site, polygonID = Polygon_ID,
         -PolygonComb, plotNumber = ReplicateComb, NDVI, 
         c(AND:BN), c(CX:RO), VUG, c(VVI:Spruce), AB, sumCover)

#write.csv(wcVegUpload, file = paste0(manDir, "datasets/wcVegUpload", Sys.Date(), ".csv"))

vegSpeciesClass <- veg %>% 
  mutate(Polygon_ID = PolygonComb) %>% 
  mutate(Replicate = ReplicateComb) %>%
  select("Polygon_ID", "Replicate", "AB", "AND","BN","CX","EV","EN","RC","RO","VUG","VVI","Lichen","Sphagnum","otherMoss","deadTussock","Litter","bareGround","AB") %>%
  pivot_longer(!c("Polygon_ID", "Replicate"), names_to="Species",values_to="speciesAmount") %>% 
  mutate(speciesClass = ifelse(Species %in% c("BN", "Spruce", "RO", "EN", "VUG", "VVI"), "shrubsSpruce", NA),
         speciesClass = ifelse(Species %in% c("CX","AB", "EV", "AND", "RC"), "grassesForbs", speciesClass),
         speciesClass = ifelse(Species %in% c("Lichen","Sphagnum","otherMoss"), "mosses", speciesClass),
         speciesClass = ifelse(Species %in% c("deadTussock","Litter","bareGround"), "deadBare", speciesClass))

#Mean species coverage by polygon, for supplement
specSum <- vegSpeciesClass %>% 
  group_by(Polygon_ID, Replicate, Species) %>% 
  summarise(speciesAmount = sum(speciesAmount, na.rm = TRUE)) %>% 
  group_by(Polygon_ID, Species) %>%
  summarize(meanPerc = mean(speciesAmount, na.rm = TRUE),
            sePerc = plotrix::std.error(speciesAmount, na.rm = TRUE))

ggplot(data = specSum, aes(x= Polygon_ID, y = meanPerc, fill = Species))+
  geom_bar(stat = "identity", color = "black", alpha = 0.7)+
  geom_errorbar(aes(ymin = meanPerc-sePerc, ymax = meanPerc + sePerc), width = 0.3, alpha = 0.7)+
  #scale_fill_manual(values = vegPal3)+
  # geom_text(aes(y = cld_y, label = grouping),
  #           vjust = 0, size = 5) +
  ylab("% cover of vegetation plots")+
  xlab("Sampling location\n(Increasing time since disturbance)")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)))+
  facet_wrap(~Species, scales = "free")+
  theme(legend.position = "none")
#ggsave(paste0(manDir, "figures/suppVegSpecies", Sys.Date(), ".png"), width = 25, height = 20, units = "cm", dpi = 500)

#Create plot with compact letter displays between sites
#Linear modeling
cld_df <- vegSpeciesClass %>% 
  group_by(Polygon_ID, Replicate, speciesClass) %>% 
  summarise(speciesAmount = sum(speciesAmount, na.rm = TRUE)) %>% 
  mutate(speciesAmount = ifelse(speciesAmount < 0.0001, 0.00001, speciesAmount),
         Polygon_ID = factor(Polygon_ID, levels = c("C", "TK1", "TK2", "TK3", "TK4"))) %>% 
  group_by(speciesClass) %>%
  nest() %>%
  #modeling here
  mutate(mod = purrr::map(data, ~ lm(speciesAmount ~ Polygon_ID, data = .x)),
         emm = purrr::map(mod, ~ emmeans::emmeans(.x, ~ Polygon_ID)),
         cld = purrr::map(emm, ~ multcomp::cld(.x, adjust = "none", Letters = letters)),
         pval_df = purrr::map(emm, ~ summary(pairs(.x, adjust = "none")))) %>%
  select(speciesClass, cld, pval_df) %>%
  unnest(cld) %>%
  mutate(Polygon_ID = factor(Polygon_ID,
                             levels = c("C","TK1","TK2","TK3","TK4")),
         grouping = trimws(.group)) %>%
  group_by(speciesClass) %>%
  arrange(Polygon_ID, .by_group = TRUE) %>%
  mutate(grouping = {
    old_groups <- unique(grouping)
    new_groups <- letters[seq_along(old_groups)]
    map_groups <- setNames(new_groups, old_groups)
    unname(map_groups[grouping])
  })

#Average cover of species classification by site
vegSum <-  vegSpeciesClass %>% 
  group_by(Polygon_ID, Replicate, speciesClass) %>% 
  summarise(speciesAmount = sum(speciesAmount, na.rm = TRUE)) %>% 
  group_by(Polygon_ID, speciesClass) %>%
  summarize(meanPerc = mean(speciesAmount, na.rm = TRUE),
            sePerc = plotrix::std.error(speciesAmount, na.rm = TRUE)) %>% 
  left_join(cld_df, by = c("speciesClass", "Polygon_ID")) %>%
  group_by(speciesClass) %>%
  mutate(cld_y = max(meanPerc + sePerc, na.rm = TRUE) * 1.05)

#summary of species classification differences by sites
vegDiff <- vegSum %>%
  select(speciesClass, Polygon_ID, meanPerc, sePerc) %>%
  pivot_wider(names_from = Polygon_ID,
              values_from = c(meanPerc, sePerc),
              names_sep = "_") %>%
  rowwise() %>%
  mutate(diff_TK1 = meanPerc_TK1 - meanPerc_C,
         diff_TK1_se = sqrt(sePerc_C^2 + sePerc_TK1^2),
         
         diff_TK2 = meanPerc_TK2 - meanPerc_C,
         diff_TK2_se = sqrt(sePerc_C^2 + sePerc_TK2^2),
         
         diff_TK3 = meanPerc_TK3 - meanPerc_C,
         diff_TK3_se = sqrt(sePerc_C^2 + sePerc_TK3^2),
         
         diff_TK4 = meanPerc_TK4 - meanPerc_C,
         diff_TK4_se = sqrt(sePerc_C^2 + sePerc_TK4^2)) %>%
  ungroup()

#Plot vegetation classification by site
ggplot(data = vegSum, aes(x= Polygon_ID, y = meanPerc, fill = speciesClass))+
  geom_bar(stat = "identity", color = "black", alpha = 0.7)+
  geom_errorbar(aes(ymin = meanPerc-sePerc, ymax = meanPerc + sePerc), width = 0.3, alpha = 0.7)+
  scale_fill_manual(values = vegPal3)+
  geom_text(aes(y = cld_y, label = grouping),
            vjust = 0, size = 5) +
  ylab("% cover of vegetatin plots")+
  xlab("Sampling location\n(Increasing time since disturbance)")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)))+
  facet_wrap(~speciesClass, scales = "free",
             labeller = as_labeller(c(
               deadBare    = "Dead plants & bare ground",
               grassesForbs = "Forbs & gramminoids",
               mosses = "Lichen & mosses",
               shrubsSpruce = "Shrubs")))+
  theme(legend.position = "none")
#ggsave(paste0(manDir, "figures/suppVegCoverSigVals", Sys.Date(), ".png"), width = 20, height = 20, units = "cm", dpi = 500)

#Compare NDVI and shrub coverage
ndviShrubPlot <- veg %>%
  filter(!is.na(NDVI)) %>% 
  mutate(Polygon_ID = PolygonComb) %>% 
  select("Polygon_ID", "Replicate", "AB", "AND","BN","CX","EV","EN","RC","RO","VUG","VVI","Lichen","Sphagnum","otherMoss","deadTussock","Litter","bareGround","AB") %>%
  pivot_longer(!c("Polygon_ID", "Replicate"), names_to="Species",values_to="speciesAmount") %>% 
  mutate(speciesClass = ifelse(Species %in% c("BN", "Spruce", "RO","VUG","VVI"), "shrubsSpruce", NA),
         speciesClass = ifelse(Species %in% c("CX","AB", "EV", "AND", "EN","RC"), "grassesForbs", speciesClass),
         speciesClass = ifelse(Species %in% c("Lichen","Sphagnum","otherMoss"), "mosses", speciesClass),
         speciesClass = ifelse(Species %in% c("deadTussock","Litter","bareGround"), "deadBare", speciesClass))  %>% 
  group_by(Polygon_ID, Replicate, speciesClass) %>% 
  summarise(meanCover = sum(speciesAmount, na.rm = TRUE)) %>% 
  left_join(select(veg, Polygon_ID, Replicate, NDVI)) %>% 
  filter(speciesClass == "shrubsSpruce")

a <- ggplot(data = ndviShrubPlot, aes(x = meanCover, y = NDVI))+
  ggpmisc::stat_poly_line(aes(x = meanCover, y = NDVI), color = "black", alpha = 0.20) +
  ggpmisc::stat_poly_eq(aes(x = meanCover, y = NDVI,
                            label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
                        formula = y ~ x,
                        label.x = "right",
                        label.y = "top",
                        size = 7) +
  geom_point(aes(color = Polygon_ID), size = 2.5, alpha = 0.7)+
  scale_color_manual(values = wcPal, name  = "Sampling Location")+
  ylab("NDVI (-)")+
  xlab("Percentage of plot covered with shrub species")
a

SpeciesPlot <- vegSpeciesClass %>%
  group_by(Polygon_ID, speciesClass) %>% 
  summarise(denom = if_else(first(Polygon_ID) %in% c("C","TK4"), 8, 5),
            denom = if_else(first(Polygon_ID) %in% c("TK2"), 9, denom),
            denom = if_else(first(Polygon_ID) %in% c("TK1"), 4, denom),
            meanCover = sum(speciesAmount, na.rm = TRUE) / denom,
            se = plotrix::std.error(speciesAmount, na.rm = TRUE)) %>% 
  group_by(Polygon_ID, speciesClass) %>%
  summarise(meanCover = mean(meanCover, na.rm = TRUE),
            .groups = "drop")

b <- ggplot(SpeciesPlot, aes(x = Polygon_ID, y = meanCover, fill = speciesClass)) +
  geom_col(alpha = 0.7, color = "black") +
  scale_fill_manual(values = vegPal3, name = "Vegetation type", 
                    labels = stringr::str_wrap(c("Dead plants & bare ground", "Forbs & gramminoids",
                                                 "Lichen & mosses", "Shrubs"), width = 13))+
  ylab("% cover of plots") +
  xlab("Sampling location\n(Increasing time since disturbance)")+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12),
        legend.key.size = unit(0.85, "cm"),
        legend.position = "right")
b

plot <- (b | a) + plot_layout(widths = c(1,1)) + plot_annotation(tag_levels = list(c('A)', 'B)', 'C)'), '1'))
plot

#ggsave(paste0(manDir, "figures/WCVeg", Sys.Date(), ".png"), width = 30, height = 15, units = "cm", dpi = 500)

#NDVI boxplot
NDVIPlot <- veg %>%
  select("NDVI","Polygon_ID") %>%
  na.omit() 

ggplot(NDVIPlot,aes(x=Polygon_ID,y=NDVI,fill=Polygon_ID)) +
  geom_boxplot(alpha = 0.7)+
  scale_fill_manual(values=wcPal)+
  xlab("Increasing time since disturbance") + ylab("NDVI (-)")+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13),
        legend.position = "none")

#ggsave(paste0(manDir, "figures/suppNDVI", Sys.Date(), ".png"), width = 15, height = 10, units = "cm", dpi = 500)

### Stats -------------------------------------------------------------------

#Test difference in shrub coverage across sites
vegStats <- vegSpeciesClass %>%
  group_by(Polygon_ID, Replicate, speciesClass) %>% 
  summarise(meanCover =  mean(speciesAmount, na.rm = TRUE))

df <- subset(vegStats, speciesClass == "shrubsSpruce")
mod <- lm(meanCover ~ Polygon_ID, df)
summary(mod)
performance::check_model(mod)
sjPlot::tab_model(mod)
pairs(emmeans::emmeans(mod, ~Polygon_ID), adjust = "none")
performance::check_model(mod)

#Test difference in EV proportion across sites
df <- select(veg, EV, Polygon_ID = PolygonComb) 
mod <- lm(EV ~ Polygon_ID, df)
summary(mod)
pairs(emmeans::emmeans(mod, ~Polygon_ID), adjust = "none")