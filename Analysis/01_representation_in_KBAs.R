library(tidyverse)
library(tidyterra)
library(terra)

options(scipen = 999)

sp_AOH <- read.csv("Data/KBA_AOH/AOH_list.csv") # 12143 AOHs
sp_IDs <- read.csv("Data/KBA_AOH/SISID_list.csv") %>% mutate(SIS.ID = as.character(SIS.ID)) # 10666 sp
KBAs <- read.csv("Data/KBA_AOH/KBA_list.csv") # 15861 KBAs

KBA_AOH <- read.csv("Data/KBA_AOH/output_all_correct_summary_AOH.csv") 

full_traded_taxonomy <- read.csv("Data/Trade/Traded_AOH_taxonomy_NOV24upd.csv") %>% 
  select(!c(multi_allNR, multi_sname, oldname_multi)) %>% mutate(AOH_SIS = as.character(AOH_SIS))#4258 sp
#Taxonomy <- read.csv("Data/Taxonomy/BL_Taxonomy_full.csv", na.strings=c("", " ","NA")) # 33212 sp + ssp (R and NR)
Trade_dat <- read.csv("Data/Trade/Hughes_et_al_trade_data.csv") # 15686 traded birds and mammals
#bird_list <- read.csv("D:/Data/Spatial/Bird_AOH_2022/Birds_list_AOH.csv")

Trade_dat %>% group_by(taxa, Traded) %>% tally() # 4265
full_traded_taxonomy %>% group_by(Traded) %>% tally() #4356 (6274 no traded)



#### Traded species richness represented in KBAs ####
KBA_AOH_all <- readRDS("Data/KBA_AOH/KBA_AOH.rds") # 15861 KBAs
trade_mini <- full_traded_taxonomy %>% filter(Traded %in% c(0,1)) %>%
  select(AOH_SIS, Traded) #10630

## Summarize the number of traded and not traded species per KBA
KBA_AOH_all_tr <- KBA_AOH_all %>% left_join(trade_mini, by = c("SIS.ID" = "AOH_SIS")) %>%
  filter(Traded %in% c(0,1))
KBA_AOH_all_tr %>% group_by(Traded) %>% summarise(length(unique(SIS.ID)))

KBA_tr_prop <- KBA_AOH_all_tr %>% filter(Area_suitable_habitat_KBA.ha. > 0) %>%
  group_by(KBA, Traded, KBA_area.ha.) %>% 
  summarise(SR = length(unique(SIS.ID))) %>% 
  pivot_wider(values_from = "SR", names_from = "Traded") %>% 
  mutate(`1` = ifelse(is.na(`1`), 0, `1`),
         `0` = ifelse(is.na(`0`), 0, `0`)) %>%
  pivot_longer(!c(KBA, KBA_area.ha.), values_to = "SR", names_to = "Traded") %>%
  mutate(Traded = ifelse(Traded == "1", "Tr", "Not.Tr"))

## Extract the KBA data (country region etc.)
KBA_spat <- vect("F:/Data/Spatial/AOH-KBA/kbas_minus_nine_invalid.shp")
KBA_info <- KBA_spat %>% as.data.frame() %>% select(SitRecID, Country, ISO3, Region) %>% 
  mutate(ISO3 = case_when(ISO3 == "MAC" ~ "CHN",
                          ISO3 == "HKG" ~ "CHN", 
                          .default = ISO3),
         Region = case_when(ISO3 == "RUS" ~ "Europe",
                            .default = Region),
         Region2 = case_when(Region == "Australasia" ~ "Oceania",
                             Region %in% c("Caribbean", "Central America") ~ "South America",
                             Region == "Central Asia" ~ "Asia",
                             Region == "Middle East" ~ "Asia",
                             .default = Region),
         SitRecID = as.character(SitRecID))

## n = 29,932
KBA_tr_prop_info <- KBA_tr_prop %>% left_join(KBA_info, by = c("KBA" = "SitRecID"))
length(unique(KBA_tr_prop_info$KBA)) ## 14966
length(unique(KBA_info$SitRecID)) ## 16003

## KBAs with no sp.
KBA_info %>% filter(!SitRecID %in% KBA_tr_prop_info$KBA)

## Log transform area and standardize
KBA_tr_prop_info <- KBA_tr_prop_info %>% ungroup() %>%
  mutate(KBA_area.ha. = as.numeric(as.character(KBA_area.ha.)),
    area.log = log10(KBA_area.ha.),
    area.log.z = (area.log - mean(area.log))/sd(area.log))

library(brms)
library(tidybayes)
library(bayestestR)
## Fit the model
## Model specifically testing the difference in traded richness and non traded
## richness in KBAs.


prop.mod <- brm(SR ~ area.log.z + Traded  + (1 + Traded|Region2),
  family = negbinomial(),
  prior = prior(normal(0, 5), class = "Intercept") +
    prior(normal(0, 2), class = "b") +
    prior(normal(0, 2), class = "sd"),
  file = "Outputs/Models/KBA_traded_prop_Nov24.rds", 
  cores = 4,
  data = KBA_tr_prop_info, iter = 1000, chains = 4
)


((exp(as.data.frame(fixef(prop.mod, summary = FALSE))$TradedTr)-1)*100) %>% median_hdci(.width = .9)
p_direction(prop.mod)

coef_sum <- as.data.frame(coef(prop.mod, summary = FALSE)$Region2[, , "TradedTr"]) %>% 
  pivot_longer(everything(), names_to = "region", values_to = "coef") %>%
  group_by(region) %>% 
  mutate(PD = round((sum(sign(coef) == sign(median(coef)))/n())*100, digits = 2),
         coef = (exp(coef) - 1)*100) %>%
  group_by(region, PD) %>%
  median_hdci(coef, .width = .9)
  

glob.new.dat <- data.frame(Traded = c("Tr", "Not.Tr"), area.log.z = median(KBA_tr_prop_info$area.log.z))
glob.draws <- add_epred_draws(object = prop.mod, newdata = glob.new.dat, re_formula = NA)

glob.draws.sum <- glob.draws %>% group_by(Traded) %>% median_hdci(.epred, .width = .9)
glob.draws.sum7 <- glob.draws %>% group_by(Traded) %>% median_hdci(.epred, .width = .7)

reg.new.dat <- KBA_tr_prop_info %>% group_by(Region2, Traded) %>% tally() %>%
  mutate(area.log.z = 0) %>% select(-n)
reg.draws <- add_epred_draws(object = prop.mod, newdata = reg.new.dat, re_formula = NULL)
reg.draws.sum <- reg.draws %>% group_by(Region2, Traded) %>% median_hdci(.epred, .width = .9)
reg.draws.sum7 <- reg.draws %>% group_by(Region2, Traded) %>% median_hdci(.epred, .width = .7)

ave.kba.plt <- ggplot(glob.draws.sum, aes(.epred, Traded, colour = Traded)) +
  geom_errorbarh(data = glob.draws.sum7, aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 2) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 1) +
  geom_point(size = 4) +
  geom_hline(yintercept=c(0.5, 1.5, 2.5),color="grey80") +
  coord_cartesian(xlim = c(0, 200)) +
  scale_y_discrete(labels = c("Not Traded", "Traded"))+
  scale_color_manual(values = c("black", "darkred")) +
  xlab("Species richness") +
  ylab("Traded") +
  theme_minimal() + 
  theme(legend.position = "None", axis.text.y = element_text(angle = 0),
        axis.title.y = element_blank())

reg.kba.plt <- ggplot(filter(reg.draws.sum, Region2 != "Antarctica"), aes(.epred, Region2, colour = Traded)) +
  geom_errorbarh(data = filter(reg.draws.sum7, Region2 != "Antarctica"), aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 1.5) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 1) +
  geom_point(size = 1) +
  geom_hline(yintercept=c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5),color="grey80") +
  coord_cartesian(xlim = c(20, 180)) +
  scale_color_manual(values = c("black", "darkred")) +
  xlab("Species richness") +
  ylab("Region") +
  theme_minimal() + theme(legend.position = "None")

## numbers for text
wide_KBA <- KBA_tr_prop %>% pivot_wider(id_cols = c(KBA, KBA_area.ha.), 
                            names_from = "Traded", values_from = "SR")

sum.tab <- wide_KBA %>% mutate(Tr.gr = Tr > Not.Tr) %>%
  group_by(Tr.gr) %>% tally() %>% ungroup() %>% 
  mutate(tot = sum(n),
         perc = n/tot *100)

wide_KBA %>% filter(Not.Tr == 0)
wide_KBA %>% filter(Tr == 0)

#### Species level % in KBAs ####
## 10517 sp
## 10666 species to start with. Then 146 are removed because their AOH maps have no suitable habitat, 
##then three more are removed for other small errors. So that gives us the 10517
KBA_AOH_sum <- KBA_AOH %>%
  group_by(SIS.ID, Family, Order) %>% 
  summarise(area_inKBA = sum(as.numeric(Area_suitable_habitat_KBAs.ha.)),
            AOH_area.ha. = sum(AOH_area.ha.)) %>%
  mutate(SIS.ID = as.character(SIS.ID),
         perc_inKBA = area_inKBA/AOH_area.ha. *100) %>%
  left_join(full_traded_taxonomy, by = c("SIS.ID" = "AOH_SIS")) %>%
  ungroup() %>%
  filter(Traded%in% c(1,0)) %>%
  mutate(TradedF = ifelse(Traded == 0, "Not.Tr", "Tr"),
         AOH_area.ha.log = log10(AOH_area.ha.),
         AOH_area.ha.log.z = (AOH_area.ha.log - mean(AOH_area.ha.log) / sd(AOH_area.ha.log)))


## fix missing orders
KBA_AOH_sum_fix <- KBA_AOH_sum %>%
  mutate(Order = case_when(Sname == "Threnetes niger" ~ "CAPRIMULGIFORMES",
                           Sname == "Threnetes leucurus" ~ "CAPRIMULGIFORMES",
                           Sname == "Chalybura buffonii" ~ "CAPRIMULGIFORMES",
                           Sname == "Chalybura urochrysia" ~ "CAPRIMULGIFORMES",
                           Sname == "Eugenes fulgens" ~ "CAPRIMULGIFORMES",
                           Sname == "Macropygia tenuirostris" ~ "COLUMBIFORMES",
                           Sname == "Macropygia emiliana" ~ "COLUMBIFORMES",
                           Sname == "Gallinago paraguaiae" ~ "CHARADRIIFORMES",
                           Sname == "Accipiter fasciatus" ~ "ACCIPITRIFORMES",
                           Sname == "Accipiter cirrocephalus" ~ "ACCIPITRIFORMES",
                           Sname == "Pitta elegans" ~ "PASSERIFORMES",
                           Sname == "Zosterops chloris" ~ "PASSERIFORMES",
                           Sname == "Locustella castanea" ~ "PASSERIFORMES",
                           Sname == "Acrocephalus stentoreus" ~ "PASSERIFORMES",
                           Sname == "Acrocephalus australis" ~ "PASSERIFORMES",
                           Sname == "Phylloscopus sarasinorum" ~ "PASSERIFORMES",
                           Sname == "Carpodacus sibiricus" ~ "PASSERIFORMES",
                           Sname == "Rhynchospiza strigiceps" ~ "PASSERIFORMES",
                           Sname == "Melopyrrha portoricensis" ~ "PASSERIFORMES",
                           Sname == "Actenoides bougainvillei" ~ "CORACIIFORMES",
                           Sname == "Actenoides excelsus" ~ "CORACIIFORMES",
                           Sname == "Rhea pennata" ~ "STRUTHIONIFORMES",
                           Sname == "Rhea tarapacensis" ~ "STRUTHIONIFORMES",
                           Sname == "Cyornis banyumas" ~ "PASSERIFORMES",
                           Sname == "Otus magicus" ~ "STRIGIFORMES",
                           Sname == "Chlorostilbon poortmani" ~ "CAPRIMULGIFORMES",
                           Sname == "Crypturellus cinnamomeus" ~ "STRUTHIONIFORMES",
                           Sname == "Crypturellus occidentalis" ~ "STRUTHIONIFORMES",
                           Sname == "Bubo africanus" ~ "STRIGIFORMES",
                           Sname == "Glaucidium brasilianum" ~ "STRIGIFORMES",
                           Sname == "Glaucidium tucumanum" ~ "STRIGIFORMES",
                           Sname == "Otus manadensis" ~ "STRIGIFORMES",
                           Sname == "Tunchiornis ochraceiceps" ~ "PASSERIFORMES",
                           Sname == "Dicaeum sanguinolentum" ~ "PASSERIFORMES",
                           Sname == "Passerella schistacea" ~ "PASSERIFORMES",
                           Sname == "Passerella megarhyncha" ~ "PASSERIFORMES",
                           Sname == "Bradypterus baboecala" ~ "PASSERIFORMES",
                           Sname == "Bradypterus centralis" ~ "PASSERIFORMES",
                           Sname == "Gracupica contra" ~ "PASSERIFORMES",
                           Sname == "Alaudala rufescens" ~ "PASSERIFORMES",
                           Sname == "Oenanthe lugens" ~ "PASSERIFORMES",
                           .default = Order))

unique(KBA_AOH_sum_fix$Order)
KBA_AOH_sum_fix %>% group_by(Traded) %>% tally()
KBA_AOH_sum_fix %>% group_by(Order, Traded) %>% tally()
## 4039 (differs to the 4083 as some AOHs contain 0 AOH habitat)

library(ordbetareg)

Perc.mod <- ordbetareg(perc_inKBA ~ AOH_area.ha.log.z + TradedF + (1+TradedF|Order), 
                       data = KBA_AOH_sum_fix,
                       file = "Outputs/Models/Sp_KBA_perc_order_Nov24.rds",
                       true_bounds = c(0,100),
                       cores = 4, iter = 1000, chains = 4)

Perc.mod <- readRDS("Outputs/Models/Sp_KBA_perc_order_Nov24.rds")

as.data.frame(fixef(Perc.mod, summary = FALSE))$TradedFTr %>% median_hdci(.width = .9)
p_direction(Perc.mod)

coef_sum <- as.data.frame(coef(Perc.mod, summary = FALSE)$Order[, , "TradedFTr"]) %>% 
  pivot_longer(everything(), names_to = "order", values_to = "coef") %>%
  group_by(order) %>% 
  mutate(PD = round((sum(sign(coef) == sign(median(coef)))/n())*100, digits = 2)) %>%
  group_by(order, PD) %>%
  median_hdci(coef, .width = .9)

write.csv(coef_sum, "Outputs/Models/Order_KBA_Perc_sum.csv")

glob.new.dat.sp <- data.frame(TradedF = c("Tr", "Not.Tr"), AOH_area.ha.log.z = median(KBA_AOH_sum_fix$AOH_area.ha.log.z))
glob.sp.draws <- add_epred_draws(object = Perc.mod, newdata = glob.new.dat.sp, re_formula = NA)

glob.sp.draws.sum <- glob.sp.draws %>% group_by(TradedF) %>% median_hdci(.epred, .width = .9)
glob.sp.draws.sum7 <- glob.sp.draws %>% group_by(TradedF) %>% median_hdci(.epred, .width = .7)

ord.new.dat <- KBA_AOH_sum_fix %>%
  group_by(Order) %>%
  mutate(AOH_area.ha.log.z = median(AOH_area.ha.log.z)) %>%
  group_by(Order, TradedF, AOH_area.ha.log.z) %>% tally() %>%
  group_by(Order) %>% filter(n()>1)

ord.draws <- add_epred_draws(object = Perc.mod, newdata = ord.new.dat, re_formula = NULL)
ord.draws.sum <- ord.draws %>% group_by(Order, TradedF) %>% median_hdci(.epred, .width = .9)
ord.draws.sum7 <- ord.draws %>% group_by(Order, TradedF) %>% median_hdci(.epred, .width = .7)

ave.sp.plt <- ggplot(glob.sp.draws.sum, aes(.epred*100, TradedF, colour = TradedF)) +
  geom_errorbarh(data = glob.sp.draws.sum7, aes(xmin = .lower*100, xmax = .upper*100), height = 0, linewidth = 2) +
  geom_errorbarh(aes(xmin = .lower*100, xmax = .upper*100), height = 0, linewidth = 1) +
  geom_point(size = 4) +
  geom_hline(yintercept=c(0.5, 1.5, 2.5),color="grey80") +
  coord_cartesian(xlim = c(0.15, 0.25)*100) +
  scale_y_discrete(labels = c("Not Traded", "Traded"))+
  scale_color_manual(values = c("black", "darkred")) +
  xlab("% of AOH in KBA") +
  ylab("Traded") +
  theme_minimal() + 
  theme(legend.position = "None", axis.text.y = element_text(angle = 0),
        axis.title.y = element_blank())

ord.sp.plt <- ggplot(filter(ord.draws.sum), aes(.epred*100, reorder(str_to_title(Order), .epred), colour = TradedF)) +
  geom_errorbarh(data = filter(ord.draws.sum7), 
                 aes(xmin = .lower*100, xmax = .upper*100), height = 0, linewidth = 1.3,
                 position = position_dodge(0.4)) +
  geom_errorbarh(aes(xmin = .lower*100, xmax = .upper*100), height = 0, linewidth = 0.8,
                 position = position_dodge(0.4)) +
  geom_point(size = 1,
             position = position_dodge(0.4)) +
  #geom_hline(yintercept=c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5),color="grey80") +
  coord_cartesian(xlim = c(0, 0.5)*100) +
  scale_color_manual(values = c("black", "darkred")) +
  xlab("% of AOH in KBA") +
  ylab("Order") +
  theme_minimal() + theme(legend.position = "None")

library(ggpubr)

coverage.plt <- ggarrange(ave.kba.plt, ave.sp.plt,
                          reg.kba.plt, ord.sp.plt,
                          nrow = 2, ncol = 2,
                          labels = c("A.", "B.", "C.", "D."),
                          heights = c(1, 2), align = "hv")

ggsave(path = "Outputs/Final.Figures", coverage.plt, 
       filename = "Fig1.Coverage_plot.png",  bg = "white",
       device = "png", width = 18, height = 14, units = "cm")

ggsave(path = "Outputs/Final.Figures", coverage.plt, 
       filename = "Fig1.Coverage_plot.pdf",  bg = "white",
       device = "pdf", width = 18, height = 14, units = "cm")

hist(as.numeric(as.character(KBA_AOH_sum_fix$perc_inKBA)))
## got the prop traded species per kba

int <- 2.2
exp(int) ## 9.02
traded <- 0.75
exp(traded) ## 2.117
exp(int + traded) ## 19
exp(int) * exp(traded)

#### Raw area in KBA ####


ggplot(KBA_AOH_sum_fix, aes(TradedF, area_inKBA)) +
  geom_point()

KBA_AOH_sum_fix %>% mutate(area_inKBA)

area.mod <- brm(area_inKBA ~ TradedF + (1+TradedF|Order), 
                       data = KBA_AOH_sum_fix,
                       file = "Outputs/Models/Sp_KBA_area_order_Nov24.rds",
                family = negbinomial(),
                       cores = 4, iter = 1000, chains = 4)

((exp(as.data.frame(fixef(area.mod, summary = FALSE))$TradedFTr))*100) %>% median_hdci(.width = .9)
p_direction(prop.mod)

coef_sum_area <- as.data.frame(coef(area.mod, summary = FALSE)$Order[, , "TradedFTr"]) %>% 
  pivot_longer(everything(), names_to = "order", values_to = "coef") %>%
  group_by(order) %>% 
  mutate(PD = round((sum(sign(coef) == sign(median(coef)))/n())*100, digits = 2)) %>%
  group_by(order, PD) %>%
  median_hdci(coef, .width = .9)

write.csv(coef_sum_area, "Outputs/Models/Order_KBA_Area_sum.csv")

glob.new.dat.sp <- data.frame(TradedF = c("Tr", "Not.Tr"))
glob.sp.draws <- add_epred_draws(object = area.mod, newdata = glob.new.dat.sp, re_formula = NA)

glob.sp.draws.sum <- glob.sp.draws %>% group_by(TradedF) %>% median_hdci(.epred, .width = .9)
glob.sp.draws.sum7 <- glob.sp.draws %>% group_by(TradedF) %>% median_hdci(.epred, .width = .7)

ord.new.dat <- KBA_AOH_sum_fix %>%
  group_by(Order) %>%
  group_by(Order, TradedF) %>% tally() %>%
  group_by(Order) %>% filter(n()>1)

ord.draws <- add_epred_draws(object = area.mod, newdata = ord.new.dat, re_formula = NULL)
ord.draws.sum <- ord.draws %>% group_by(Order, TradedF) %>% median_hdci(.epred, .width = .9)
ord.draws.sum7 <- ord.draws %>% group_by(Order, TradedF) %>% median_hdci(.epred, .width = .7)

ave.sp.plt <- ggplot(glob.sp.draws.sum, aes(.epred, TradedF, colour = TradedF)) +
  geom_errorbarh(data = glob.sp.draws.sum7, aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 2) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 1) +
  geom_point(size = 4) +
  geom_hline(yintercept=c(0.5, 1.5, 2.5),color="grey80") +
  scale_x_continuous(breaks = c(10000000, 40000000)) +
  scale_y_discrete(labels = c("Not Traded", "Traded"))+
  scale_color_manual(values = c("black", "darkred")) +
  xlab("Ha of AOH in KBA") +
  ylab("Traded") +
  theme_minimal() + 
  theme(legend.position = "None", axis.text.y = element_text(angle = 0),
        axis.title.y = element_blank())

ord.sp.plt <- ggplot(filter(ord.draws.sum), aes(.epred, reorder(str_to_title(Order), .epred), colour = TradedF)) +
  geom_errorbarh(data = filter(ord.draws.sum7), 
                 aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 1.3,
                 position = position_dodge(0.4)) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, linewidth = 0.8,
                 position = position_dodge(0.4)) +
  geom_point(size = 1,
             position = position_dodge(0.4)) +
  #geom_hline(yintercept=c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5),color="grey80") +
  #coord_cartesian(xlim = c(0, 0.5)*100) +
  scale_color_manual(values = c("black", "darkred")) +
  xlab("Ha of AOH in KBA") +
  ylab("Order") +
  theme_minimal() + theme(legend.position = "None")

area.plt <- ggarrange(ave.sp.plt, ord.sp.plt,
                          nrow = 1, ncol = 2,
                          labels = c("A.", "B."),
                          widths = c(1, 2), align = "hv")

## SM figure 1
ggsave(path = "Outputs/draft_figs", area.plt, 
       filename = "Area_plot.png",  bg = "white",
       device = "png", width = 18, height = 10, units = "cm")

