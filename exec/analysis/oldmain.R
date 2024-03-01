library("dplyr")
library("ggplot2")

# This needs to be its own data manipulation thing
soil <- read.csv("data/soil salinity 2023_Noa.csv")
soil$date <- lubridate::mdy(soil$date)
soil$replicate <- as.factor(soil$replicate)

pore <- read.csv("data/Porewater_Salinity_Brownsville_2023.csv")
pore$Date <- lubridate::mdy(pore$Date)
pore <- pore %>% filter(ID != "BVL_H__C")

pore$plot <- paste(pore$Habitat, pore$Plot, sep = "")
pore$Replicate <- as.factor(pore$Replicate)



# idk what's going on here with the plots
ppt_plot <- soil %>%
  group_by(plot) %>%
  summarise(soil_ppt = mean(Salinity_ppt, na.rm = T))
ppt_plot$plot <- as.factor(ppt_plot$plot)

temp <- pore %>%
  group_by(plot) %>%
  summarise(pore_ppt = mean(Salinity_ppt, na.rm = T))
temp$plot <- as.factor(temp$plot)
ppt_plot <- merge(ppt_plot, temp, by = "plot", all.x = TRUE)
ggplot(ppt_plot, aes(soil_ppt, pore_ppt)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE) +
  ylab("Porewater salinity (ppt)") +
  xlab("Soil salinity (ppt)") +
  geom_text(label = ppt_plot$plot, nudge_x = 0.25, nudge_y = 0, check_overlap = T, size = 6) +
  ggtitle("2023 soil (1:5 soil saturation) and porewater salinity (ppt)") +
  theme_classic(base_size = 20)

# Compare 2022 and 2023
soil2022 <- read.csv("data/soil salinity 2022_Aliya.csv", stringsAsFactors = T)
soil2022 <- soil2022 %>% mutate(Salinity_ppt = 0.49 * EC_ms.cm)
ppt_plot2022 <- soil2022 %>%
  group_by(plot) %>%
  summarise(soil_ppt2022 = mean(Salinity_ppt, na.rm = T))
soil_ppt_2022_2023 <- merge(ppt_plot2022, ppt_plot, by = "plot", all.y = T)
soil_ppt_2022_2023$level <- substr(soil_ppt_2022_2023$plot, 1, 1) # orders alphabetically, i.e. H, L, M
soil_ppt_2022_2023$level <- factor(soil_ppt_2022_2023$level, levels = c("H", "M", "L"))

ggplot(soil_ppt_2022_2023, aes(soil_ppt2022, soil_ppt)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  ylab("2023 soil salinity (ppt)") +
  xlab("2022 soil salinity (ppt)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_text(label = ppt_plot$plot, nudge_x = 0.25, nudge_y = 0, check_overlap = T, size = 6) +
  ggtitle("soil salinity in 2022 and 2023") +
  theme_classic(base_size = 20)


ppt22 <- soil2022 %>%
  select(plot, Salinity_ppt) %>%
  group_by(plot) %>%
  summarise(mean = mean(Salinity_ppt, na.rm = T), se = se(Salinity_ppt)) %>%
  mutate(year = 2022)
ppt23 <- soil %>%
  select(plot, Salinity_ppt) %>%
  group_by(plot) %>%
  summarise(mean = mean(Salinity_ppt, na.rm = T), se = se(Salinity_ppt)) %>%
  mutate(year = 2023)
ppt22_23 <- rbind(ppt22, ppt23)
ppt22_23$level <- substr(ppt22_23$plot, 1, 1) # orders alphabetically, i.e. H, L, M
ppt22_23$level <- factor(ppt22_23$level, levels = c("L", "M", "H", "R"))
ppt22_23mean <- ppt22_23 %>%
  group_by(level, year) %>%
  summarise(se = se(mean), mean = mean(mean, na.rm = T))

ggplot(ppt22_23mean, aes(x = year, y = mean, color = level)) +
  geom_line(stat = "identity") +
  geom_point(stat = "identity", size = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.05) +
  scale_colour_manual(values = c("L" = "navy", "M" = "royalblue", "H" = "skyblue", "R" = "darkgray"), labels = c("Low", "Mid", "High", "Ref")) +
  scale_x_continuous(n.breaks = 2) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16), limits = c(0, 16)) +
  ylab("soil salinity (ppt)") +
  theme_classic(base_size = 19) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1))


# Brownsville Porewater salinity trend over time 2019-2023#####
BVLppt <- read.csv("data/Forest Disturbance Plot_Porewater Salinity_2019_2023.csv", stringsAsFactors = T)

BVLppt$date <- mdy(BVLppt$date_collected)
BVLppt$level <- substr(BVLppt$plot, 1, 1) # orders alphabetically, i.e. H, L, M
BVLppt$level <- factor(BVLppt$level, levels = c("H", "M", "L"))

hist(BVLppt$ppt, breaks = 40)
boxplot(BVLppt$ppt ~ BVLppt$level)
ggplot(BVLppt, aes(x = ppt, color = level)) +
  geom_density()

BVLppt$plot <- factor(BVLppt$plot, levels = c(
  "H0", "H1", "H2", "H3", "H4", "H5", "H6", "H7",
  "M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7",
  "L0", "L1", "L2", "L3", "L4", "L5", "L6", "L7"
))
boxplot(BVLppt$ppt ~ BVLppt$plot)
levels(as.factor(BVLppt$date))
# BVLppt = BVLppt %>% group_by(level) %>% filter(date_collected != "10/29/2019") %>% droplevels(BVLppt$date_collected)
# remove the very high points in October 2019 that are after a flood event?

# lt_ave: long term average
BVL_lt_ave <- BVLppt %>%
  filter(plot != "H") %>%
  group_by(plot, level) %>%
  summarise(mean_ppt = mean(ppt, na.rm = T), se_ppt = se(ppt))
ggplot(BVL_lt_ave, aes(x = level, y = mean_ppt, fill = level)) +
  geom_bar(stat = "summary", fun = "mean")

BVL_trend <- BVLppt %>%
  group_by(level, year.collected) %>%
  summarise(mean = mean(ppt, na.rm = T), se = se(ppt))

ggplot(BVL_trend, aes(x = year.collected, y = mean, color = level)) +
  geom_line(stat = "identity")

ggplot(BVLppt, aes(x = date, y = ppt, color = level)) +
  geom_line(stat = "summary", fun = "mean") +
  geom_point(stat = "summary", fun = "mean", size = 2) +
  scale_colour_manual(values = c("H" = "skyblue", "M" = "royalblue", "L" = "navy"), labels = c("High", "Mid", "Low")) +
  ylab("porewater salinity (ppt)") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%y", limits = lubridate::ymd(c("2019-01-30", "07-01-2023"))) +
  theme_classic(base_size = 19) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1))

BVLppt_mean <- BVLppt %>%
  select(plot, date_collected, ppt, level) %>%
  group_by(plot, level, date_collected) %>%
  summarise(mean = mean(ppt)) %>%
  group_by(level, date_collected) %>%
  summarise(se = se(mean), mean = mean(mean, na.rm = T))
BVLppt_mean$date_collected <- mdy(BVLppt_mean$date_collected)
ggplot(BVLppt_mean, aes(x = date_collected, y = mean, color = level)) +
  geom_line(stat = "identity") +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 30) +
  scale_colour_manual(values = c("L" = "navy", "M" = "royalblue", "H" = "skyblue", "R" = "darkgray"), labels = c("Low", "Mid", "High", "Ref")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%y", limits = lubridate::ymd(c("2019-01-30", "07-01-2023"))) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16), limits = c(0, 18)) +
  ylab("soil salinity (ppt)") +
  xlab("date") +
  theme_classic(base_size = 19) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1))


##### TREES #####
data <- read.csv(file = "data/trees_2023.csv", sep = ",", stringsAsFactors = T)

# lump all maples, lump all oaks, since we have only a few individuals in the census
data$Species <- replace(data$Species, data$Species == "Acer pensylvanicum" | data$Species == "Acer rubrum", "Acer spp.")
data$Species <- replace(data$Species, data$Species == "Quercus spp." | data$Species == "Quercus michauxii" | data$Species == "Quercus bicolor", "Quercus spp.")
data$Species <- droplevels(data$Species)
levels(data$Species)

data$Tree.Code <- as.factor(paste(data$Plot, data$Tree.Number, substr(data$Species, 0, 3), sep = "_"))
str(data)

trunk1 <- filter(data, Trunk.Number == "1")
trunk1$Tree.Code <- droplevels(trunk1$Tree.Code)
trunk1$Year <- factor(trunk1$Year, levels = c("2019", "2020", "2021", "2022", "2023"))

n_distinct(trunk1$Tree.Code)
d1 <- trunk1 %>%
  group_by(Year) %>%
  select(Year, Tree.Code, alive.1.or.dead.0) %>%
  pivot_wider(id_cols = Tree.Code, names_from = Year, values_from = alive.1.or.dead.0, names_prefix = "alive")
a <- strsplit(as.character(d1$Tree.Code), "_")
b <- data.table::transpose(a)
d1$Species <- b[[3]]
d1$Plot <- b[[1]]
d1$Level <- substr(d1$Plot, 1, 1) # orders alphabetically, i.e. H, L, M
d1$Level <- factor(d1$Level, levels = c("R", "H", "M", "L"))
d1 <- rename(d1, plot = Plot) # new name = old name syntax

# How many trees died in each study year?
d1$died2020 <- ifelse((d1$alive2019 == 1 & d1$alive2020 == 0), 1, 0)
d1$died2021 <- ifelse((d1$alive2020 == 1 & d1$alive2021 == 0), 1, 0)
d1$died2022 <- ifelse((d1$alive2021 == 1 & d1$alive2022 == 0), 1, 0)
d1$died2023 <- ifelse((d1$alive2022 == 1 & d1$alive2023 == 0), 1, 0)
d1

mortality <- d1 %>%
  pivot_longer(cols = c(died2020, died2021, died2022, died2023), names_to = "year", names_prefix = "died", values_to = "died") %>%
  select(-c(alive2021, alive2019, alive2020, alive2022, alive2023))


by_species_level_year_plot <- d1 %>%
  pivot_longer(cols = c(alive2019, alive2020, alive2021, alive2022, alive2023), names_to = "year", names_prefix = "alive", values_to = "alive") %>%
  group_by(Species, Level, year, plot) %>%
  select(-c(died2020, died2021, died2022, died2023))
dead_live <- by_species_level_year_plot %>% summarise(
  nlive = sum(alive == 1, na.rm = T),
  ndied = sum(alive == 0, na.rm = T)
)
dead_live <- mutate(dead_live, prop_dead = ifelse(ndied + nlive > 0, ndied / (ndied + nlive), 0))
dead_live$Species <- factor(dead_live$Species)
dead_live$plot <- factor(dead_live$plot)
dead_live$year <- factor(dead_live$year)
dead_live <- dead_live %>% filter(year == "2023")
## NOTE!!! FILTERED TO 2023 proportions only to compare with 2023 salinity data#
by_species_plot_prop_dead <- dead_live %>%
  group_by(Species, plot) %>%
  summarise(prop_dead = mean(prop_dead))
by_species_plot_prop_dead %>%
  group_by(Species) %>%
  count() # how many plots have records for each species?

prop_dead_ppt <- merge(by_species_plot_prop_dead, ppt_plot, by = "plot")
colnames(prop_dead_ppt) <- c("plot", "Species", "prop_dead_2023", "soil_ppt_2023", "pore_ppt_2023")

prop_dead_ppt <- prop_dead_ppt %>%
  filter(Species != "NA") %>%
  filter(Species != "Uni") %>%
  filter(Species != "Car") %>%
  filter(Species != "Cor") %>%
  filter(Species != "Vac") %>%
  filter(Species != "Que") %>%
  filter(Species != "Liq")
prop_dead_ppt$level <- substr(prop_dead_ppt$plot, 1, 1) # orders alphabetically, i.e. R, H, L, M
prop_dead_ppt$level <- factor(prop_dead_ppt$level, levels = c("R", "H", "M", "L"))

# mean proportion of stems dead in 2023 x soil ppt 2023
ggplot(prop_dead_ppt, aes(x = soil_ppt_2023, y = prop_dead_2023)) +
  geom_point(aes(color = level), stat = "identity", size = 3) +
  ylab("Proportion dead") +
  xlab("Soil salinity 2023 (ppt)") +
  facet_wrap(prop_dead_ppt$Species, nrow = 2) +
  geom_smooth(aes(x = soil_ppt_2023, y = prop_dead_2023), method = "glm", method.args = list(family = "quasibinomial"), se = TRUE) +
  scale_colour_manual(values = c("R" = "darkgray", "H" = "deepskyblue", "M" = "royalblue", "L" = "navy")) +
  geom_text(label = prop_dead_ppt$plot, nudge_x = 1, nudge_y = 0, check_overlap = T, aes(color = level), size = 5) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 28) +
  theme(legend.position = "none")

# logistic regression and LD50 for Nyssa and Pinus
# to calculate LD50 = -constant/coef
logisticNys <- glm(prop_dead_2023 ~ soil_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Nys"))
summary(logisticNys)
performance::performance_hosmer(logisticNys, n_bins = 10)
-(-2.6673 / 0.6954) # 3.83

logisticPin <- glm(prop_dead_2023 ~ soil_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Pin"))
summary(logisticPin)
-(-1.6573 / 0.4593) # 3.608
performance::performance_hosmer(logisticPin, n_bins = 10)

logisticAce <- glm(prop_dead_2023 ~ soil_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Ace"))
summary(logisticAce)
-(-5.530 / 11.805) # 0.468
performance::performance_hosmer(logisticAce, n_bins = 4)

logisticMor <- glm(prop_dead_2023 ~ soil_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Mor"))
summary(logisticMor)
-(-3.0861 / 0.3222) # 9.578
performance::performance_hosmer(logisticMor, n_bins = 4)

logisticIle <- glm(prop_dead_2023 ~ soil_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Ile"))
summary(logisticIle)
-(-4.4817 / 1.7189) # 2.607307
performance::performance_hosmer(logisticIle, n_bins = 4)



# porewater salinity instead of soil salinity
ggplot(prop_dead_ppt, aes(x = pore_ppt_2023, y = prop_dead_2023)) +
  geom_point(aes(color = level), stat = "identity", size = 3) +
  ylab("Proportion dead") +
  xlab("Porewater salinity 2023 (ppt)") +
  facet_wrap(prop_dead_ppt$Species, nrow = 2) +
  geom_smooth(aes(x = pore_ppt_2023, y = prop_dead_2023), method = "glm", method.args = list(family = "quasibinomial"), se = TRUE) +
  scale_colour_manual(values = c("R" = "darkgray", "H" = "deepskyblue", "M" = "royalblue", "L" = "navy")) +
  geom_text(label = prop_dead_ppt$plot, nudge_x = 1, nudge_y = 0, check_overlap = T, aes(color = level), size = 5) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 28) +
  theme(legend.position = "none")

# logistic regression and LD50 for Nyssa and Pinus
# to calculate LD50 = -constant/coef
logisticNys <- glm(prop_dead_2023 ~ pore_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Nys"))
summary(logisticNys)
performance::performance_hosmer(logisticNys, n_bins = 10)
-(-2.3129 / 0.3822) # 6.051544

logisticPin <- glm(prop_dead_2023 ~ pore_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Pin"))
summary(logisticPin)
-(-2.23486 / 0.45335) # 4.929657
performance::performance_hosmer(logisticPin, n_bins = 10)

logisticAce <- glm(prop_dead_2023 ~ pore_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Ace"))
summary(logisticAce)
-(-111.525 / 118.378) # 0.9421092
# not enough data to assess goodness-of-fit performance::performance_hosmer(logisticAce, n_bins = 2)

logisticMor <- glm(prop_dead_2023 ~ pore_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Mor"))
summary(logisticMor)
-(-7.3826 / 0.9908) # 7.451151
performance::performance_hosmer(logisticMor, n_bins = 4)

logisticIle <- glm(prop_dead_2023 ~ pore_ppt_2023, family = quasibinomial(link = "logit"), data = prop_dead_ppt, subset = (Species == "Ile"))
summary(logisticIle)
# looks like a very poor fit, but Hosmer says it is ok - does not have expected shape
-(-1.444 / -0.469) #-3.078891
performance::performance_hosmer(logisticIle, n_bins = 3)




### Proportion of dead stems, all species ####

all_species_prop_dead <- dead_live %>%
  group_by(plot) %>%
  summarise(
    nlive = sum(nlive),
    ndied = sum(ndied),
    prop_dead = ifelse(ndied + nlive > 0, ndied / (ndied + nlive), 0)
  )

prop_dead_ppt <- merge(all_species_prop_dead, ppt_plot, by = "plot")
colnames(prop_dead_ppt) <- c("plot", "nlive", "ndied", "prop_dead_2023", "soil_ppt_2023", "pore_ppt_2023")
prop_dead_ppt$level <- substr(prop_dead_ppt$plot, 1, 1) # orders alphabetically, i.e. R, H, L, M

# proportion of stems dead across in 2023 x soil ppt 2023
ggplot(prop_dead_ppt, aes(x = soil_ppt_2023, y = prop_dead_2023)) +
  geom_point(aes(color = level), stat = "identity", size = 3) +
  ylab("Proportion dead") +
  xlab("Soil salinity (ppt)") +
  geom_smooth(method = "glm", formula = (y ~ exp(-x))) +
  scale_colour_manual(values = c("R" = "darkgray", "H" = "deepskyblue", "M" = "royalblue", "L" = "navy")) +
  geom_text(label = prop_dead_ppt$plot, nudge_x = 0.25, nudge_y = 0, check_overlap = T, aes(color = level), size = 5) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 28) +
  theme(legend.position = "none")
# The logistic curve is a terrible fit, but of course we know that
# the proportion dead will eventually approach 1 and level off,
# not what we observe here

## REPEAT# try removing Junipers, since they are not behaving and recalculating?
# all_species_prop_dead = dead_live %>% filter(Species != "Jun") %>%
group_by(plot) %>% summarise(
  nlive = sum(nlive),
  ndied = sum(ndied),
  prop_dead = ifelse(ndied + nlive > 0, ndied / (ndied + nlive), 0)
)

# prop_dead_ppt = merge(all_species_prop_dead, ppt_plot, by = "plot")
# colnames(prop_dead_ppt) = c("plot", "nlive", "ndied", "prop_dead_2023", "soil_ppt_2023", "pore_ppt_2023")
# prop_dead_ppt$level = substr(prop_dead_ppt$plot, 1,1) #orders alphabetically, i.e. R, H, L, M

# proportion of stems dead across in 2023 x soil ppt 2023 -- WITHOUT JUNIPER
# ggplot(prop_dead_ppt, aes(x = soil_ppt_2023, y = prop_dead_2023)) +
geom_point(aes(color = level), stat = "identity", size = 3) +
  ylab("Proportion dead") +
  xlab("Soil salinity (ppt)") +
  geom_smooth(aes(x = soil_ppt_2023, y = prop_dead_2023), method = "glm", method.args = list(family = "quasibinomial"), se = FALSE) +
  scale_colour_manual(values = c("R" = "darkgray", "H" = "deepskyblue", "M" = "royalblue", "L" = "navy")) +
  geom_text(label = prop_dead_ppt$plot, nudge_x = 0.25, nudge_y = 0, check_overlap = T, aes(color = level), size = 5) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 28) +
  theme(legend.position = "none")
## DID NOT TURN OUT VERY NICE....

Ref_ppt <- data.frame(plot = c("R1", "R2", "R3", "R4"), level = rep("R", 4), mean_ppt = rep(1, 4), se_ppt = rep(0, 4))
BVL_lt_ave <- bind_rows(BVL_lt_ave, Ref_ppt)

prop_dead_lt_ppt <- merge(by_species_plot_prop_dead, BVL_lt_ave, by = "plot", all.x = T)
colnames(prop_dead_lt_ppt) <- c("plot", "Species", "prop_dead_2023", "level", "pore_ppt_mean", "pore_ppt_se")
# filter to species that are present in >3 levels
prop_dead_lt_ppt <- prop_dead_lt_ppt %>%
  filter(Species != "NA") %>%
  filter(Species != "Uni") %>%
  filter(Species != "Car") %>%
  filter(Species != "Cor") %>%
  filter(Species != "Vac") %>%
  filter(Species != "Que") %>%
  filter(Species != "Liq")

# proportion of stems dead across in 2023 x porewater ppt 2023
ggplot(prop_dead_lt_ppt, aes(x = pore_ppt_mean, y = prop_dead_2023)) +
  geom_point(aes(color = level), stat = "identity", size = 3) +
  ylab("Proportion dead") +
  xlab("Ave. porewater salinity (ppt) 2019-2023") +
  facet_wrap(prop_dead_lt_ppt$Species, nrow = 2) +
  geom_smooth(aes(x = pore_ppt_mean, y = prop_dead_2023), method = "glm", method.args = list(family = "quasibinomial"), se = TRUE, na.rm = T) +
  scale_colour_manual(values = c("R" = "darkgray", "H" = "deepskyblue", "M" = "royalblue", "L" = "navy")) +
  geom_text(label = prop_dead_lt_ppt$plot, nudge_x = 0.75, nudge_y = 0, check_overlap = T, aes(color = level), size = 5) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 28) +
  theme(legend.position = "none")

# to calculate LD50 = -constant/coef
logisticNys <- glm(prop_dead_2023 ~ pore_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_lt_ppt, subset = (Species == "Nys"))
summary(logisticNys)
performance::performance_hosmer(logisticNys, n_bins = 10)
-(-4.1319 / 0.5842) # 7.072749

logisticPin <- glm(prop_dead_2023 ~ pore_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_lt_ppt, subset = (Species == "Pin"))
summary(logisticPin)
-(-3.39868 / 0.52831) # 6.433117
performance::performance_hosmer(logisticPin, n_bins = 7)

logisticAce <- glm(prop_dead_2023 ~ pore_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_lt_ppt, subset = (Species == "Ace"))
summary(logisticAce)
-(-7.928 / 3.144) # 2.521628
# not enough data to assess goodness-of-fit performance::performance_hosmer(logisticAce, n_bins = 2)

logisticMor <- glm(prop_dead_2023 ~ pore_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_lt_ppt, subset = (Species == "Mor"))
summary(logisticMor)
-(-5.7450 / 0.5692) # 10.09311
performance::performance_hosmer(logisticMor, n_bins = 4)

logisticIle <- glm(prop_dead_2023 ~ pore_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_lt_ppt, subset = (Species == "Ile"))
summary(logisticIle)
-(-4.9540 / 0.7371) # 6.720933
performance::performance_hosmer(logisticIle, n_bins = 3)


# using an average of 2022 and 2023 soil salinity ######
ppt22_23_mean <- ppt22_23 %>%
  group_by(plot) %>%
  summarise(ppt_mean = mean(mean, na.rm = T))

prop_dead_soil_mean <- merge(by_species_plot_prop_dead, ppt22_23_mean, by = "plot", all.x = T)
colnames(prop_dead_soil_mean) <- c("plot", "Species", "prop_dead_2023", "soil_ppt_mean")
# filter to species that are present in >3 levels
prop_dead_soil_mean <- prop_dead_soil_mean %>%
  filter(Species != "NA") %>%
  filter(Species != "Uni") %>%
  filter(Species != "Car") %>%
  filter(Species != "Cor") %>%
  filter(Species != "Vac") %>%
  filter(Species != "Que") %>%
  filter(Species != "Liq")

prop_dead_soil_mean$level <- substr(prop_dead_soil_mean$plot, 1, 1) # orders alphabetically, i.e. H, L, M
prop_dead_soil_mean$level <- factor(prop_dead_soil_mean$level, levels = c("R", "H", "M", "L"))


# proportion of stems dead across in 2023 x soil ppt 2022-2023
ggplot(prop_dead_soil_mean, aes(x = soil_ppt_mean, y = prop_dead_2023)) +
  geom_point(aes(color = level), stat = "identity", size = 3) +
  ylab("Proportion dead") +
  xlab("Ave. soil salinity (ppt) 2022-2023") +
  facet_wrap(prop_dead_soil_mean$Species, nrow = 2) +
  geom_smooth(aes(x = soil_ppt_mean, y = prop_dead_2023), method = "glm", method.args = list(family = "quasibinomial"), se = TRUE, na.rm = T) +
  scale_colour_manual(values = c("R" = "darkgray", "H" = "deepskyblue", "M" = "royalblue", "L" = "navy")) +
  geom_text(label = prop_dead_soil_mean$plot, nudge_x = 0.75, nudge_y = 0, check_overlap = T, aes(color = level), size = 5) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 28) +
  theme(legend.position = "none")

# to calculate LD50 = -constant/coef
logisticNys <- glm(prop_dead_2023 ~ soil_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_soil_mean, subset = (Species == "Nys"))
summary(logisticNys)
performance::performance_hosmer(logisticNys, n_bins = 10)
-(-3.0108 / 1.0220) # 2.945988

logisticPin <- glm(prop_dead_2023 ~ soil_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_soil_mean, subset = (Species == "Pin"))
summary(logisticPin)
-(-2.0131 / 0.7465) # 2.696718
performance::performance_hosmer(logisticPin, n_bins = 7)

logisticAce <- glm(prop_dead_2023 ~ soil_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_soil_mean, subset = (Species == "Ace"))
summary(logisticAce)
-(-2.947 / 3.981) # 0.7402663
performance::performance_hosmer(logisticAce, n_bins = 4)

logisticMor <- glm(prop_dead_2023 ~ soil_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_soil_mean, subset = (Species == "Mor"))
summary(logisticMor)
-(-3.5416 / 0.5909) # 5.993569
performance::performance_hosmer(logisticMor, n_bins = 4)

logisticIle <- glm(prop_dead_2023 ~ soil_ppt_mean, family = quasibinomial(link = "logit"), data = prop_dead_soil_mean, subset = (Species == "Ile"))
summary(logisticIle)
-(-5.366 / 2.871) # 1.869035
performance::performance_hosmer(logisticIle, n_bins = 3)





# could summarize range of tolerances described by porewater vs soil salinity


# MORTALITY AS A PROPORTION OF BASAL AREA



# SURVIVAL CURVES#####
# learning to use ggsurvfit package to make survival curves; https://www.danieldsjoberg.com/ggsurvfit/
library(ggsurvfit)
head(mortality)
mortality$year <- as.numeric(mortality$year)
mortality_noR <- mortality %>% filter(Level != "R")

mortality_Jun <- mortality_noR %>% filter(Species == "Jun")
mortality_Nys <- mortality_noR %>% filter(Species == "Nys")
mortality_Pin <- mortality_noR %>% filter(Species == "Pin")

mortality_noR$Tree.Code <- droplevels(mortality_noR$Tree.Code)
temp <- mortality_noR %>%
  filter(year == 2022) %>%
  arrange(plot, Species)
temp$Tree.Code <- droplevels(temp$Tree.Code)
unique(temp$Tree.Code)
filter(mortality, Tree.Code == "H4_133_Pin")


Jun_surv <- survfit2(Surv(year, died) ~ Level, data = mortality_Jun) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  xlim(2020, 2023) +
  ylim(0.75, 1) +
  ggtitle("Juniperus virginiana survival") +
  add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75)
Jun_surv

Nys_surv <- survfit2(Surv(year, died) ~ Level, data = mortality_Nys) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  xlim(2020, 2023) +
  ylim(0.75, 1) +
  ggtitle("Nyssa sylvatica survival") +
  add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75)
Nys_surv

Pin_surv <- survfit2(Surv(year, died) ~ Level, data = mortality_Pin) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  xlim(2020, 2023) +
  ylim(0.575, 1) +
  ggtitle("Pinus taeda survival") +
  add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75)
Pin_surv
