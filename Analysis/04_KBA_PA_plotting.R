
library(tidyverse)

kba_pa_50 <- read.csv("Outputs/Summaries/PA_KBA_AOH_Thr_50perc_NEW.csv")
details <- read.csv("Outputs/Summaries/Thr_sp_thr_by_trade.csv")

kba_pa_50 <- read_csv("X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_2023_KBA-Trade/Analysis/Outputs/Summaries/PA_KBA_AOH_Thr_50perc_NEW.csv")
details <- read.csv("X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_2023_KBA-Trade/Analysis/Outputs/Summaries/Thr_sp_thr_by_trade.csv")

kba_pa_50 <- kba_pa_50 %>% 
  # note - this is Eos semilarvata two maps are provided for the species
  # breeding and non breeding - but these are identical.
  # all other species only have a single AOH per species
  filter(!c(SIS.ID == "22684524" & type == "Breeding")) %>%
  mutate(kba.not.pa.perc = kba.not.pa/aoh.area *100,
         pa.not.kba.perc = pa.not.kba/aoh.area *100,
         kba.in.pa.perc = kba.in.pa/aoh.area *100,
         sum = kba.not.pa.perc + pa.not.kba.perc + kba.in.pa.perc,
         pa.perc = pa.not.kba.perc + kba.in.pa.perc,
         aoh.area.km = aoh.area*0.01)

mean(filter(kba_pa_50, pa.perc>0)$pa.perc)
mean(filter(kba_pa_50, pa.perc==0)$aoh.area.km)

kba_pa_50_long <- kba_pa_50 %>% 
  select(SIS.ID, kba.not.pa.perc, pa.not.kba.perc, kba.in.pa.perc) %>%
  arrange(kba.not.pa.perc) %>%
  mutate(kba_size_order = 1:n()) %>%
  pivot_longer(!c(SIS.ID, kba_size_order), values_to = "perc", names_to = "comb") %>%
  left_join(details) %>%
  mutate(IUCN2024 = case_when(redlistCategory == "Near Threatened" ~ "NT",
                              redlistCategory == "Vulnerable" ~ "VU",
                              redlistCategory == "Endangered" ~ "EN",
                              redlistCategory == "Critically Endangered" ~ "CR",
                              redlistCategory == "Data Deficient" ~ "DD")) 


max_blank <- data.frame(Sname = unique(kba_pa_50_long$Sname), max = 100)

kba_pa_plt <- ggplot(kba_pa_50_long, aes(perc, reorder(Sname, kba_size_order), 
                           fill = comb)) +
  geom_col(data = max_blank, aes(max, Sname), fill = "grey90", width = .7) +
  geom_col(width = .7) +
  geom_text(aes(label = IUCN2024, x = 105, y = reorder(Sname, kba_size_order)),
            size = 2) +
  scale_fill_manual(values = c("#a1d76a", "#fc8d59", "#91bfdb"),
                    labels = c("Protected KBA", "Unprotected KBA", "Protected, not KBA")) +
  coord_cartesian(xlim = c(0,110), expand = FALSE) +
  xlab("Percent AOH") +
  ylab("Species") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_text(face = "italic"),
        legend.title = element_blank())

ggsave(path = "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_2023_KBA-Trade/Analysis/Outputs/Final.Figures", kba_pa_plt, 
       #path = "Outputs/draft_figs",
       filename = "Fig4.kba_pa_plt50.Jan2026.png",  bg = "white",
       device = "png", width = 18, height = 18, units = "cm")

ggsave(path = "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_2023_KBA-Trade/Analysis/Outputs/Final.Figures", kba_pa_plt, 
       #path = "Outputs/draft_figs",
       filename = "Fig4.kba_pa_plt50.Jan2026.pdf",  bg = "white",
       device = "pdf", width = 18, height = 18, units = "cm")

## check protected coverage
library(prioritizr)
area.seq <- seq(from = 1000, to = 250000)
protected <- loglinear_interpolation(area.seq, 1000, 100, 250000, 10)
protection.ref <- data.frame(area.km = area.seq, prot.thr.perc = protected)

prot.check <- kba_pa_50 %>% mutate(aoh.area.km = ceiling(aoh.area*0.01)) %>%
  left_join(protection.ref, by = c("aoh.area.km" = "area.km")) %>%
  mutate(prot.thr.perc = case_when(aoh.area.km < 1000 ~ 100,
                              aoh.area.km > 250000 ~ 10,
                              .default = prot.thr.perc),
         prot.perc = pa.not.kba.perc + kba.in.pa.perc,
         prot = prot.perc >= prot.thr.perc,
         prot.kba.perc = pa.not.kba.perc + kba.in.pa.perc + kba.not.pa.perc,
         prot.plus.kba = prot.kba.perc >= prot.thr.perc)

## 38 no, 8 yes
prot.check %>% group_by(prot) %>% tally()
prot.check %>% filter(prot == FALSE, aoh.area.km < 5000)
prot.check %>% group_by(prot.plus.kba) %>% tally()
