library(readxl)
library(tidyverse)
library(flextable)
library(broom)
library(ggpubr)
library(rstatix)
library(emmeans)


### Forbered dataene
dataset1 <- read_excel("./data/ten_vs_thirty.xlsx", na = "NA") %>%
  filter(timepoint != "mid", # Fjerner timepoint "mid" fordi jeg er interessert i pre-post
         exercise == "benchpress") %>% # Velger ut øvelsen benkpress
  mutate(timepoint = factor(timepoint, 
                            levels = c("pre",  "post"))) %>% # Gjør timepoint om til numeriske variabler
  pivot_wider(names_from = timepoint, 
              values_from = load) %>% # Lager en egen kolonne med timepointene pre og post
  mutate(change = post - pre) # Lager en ny variabel change som regner ut endringene fra pre- til post-test.


### Lag en tabell som viser gjennomsnittlig endring i de to gruppene.
tabell1 <- dataset1 %>%
  group_by(group) %>% # Grupper etter variabelen group
  filter(!is.na(change)) %>% # Fjern forsøkspersoner uten resultat
  summarise (m = mean(change),
             s = sd(change)) %>% # Regn ut gjennomsnittlig endring og standardavviket
  flextable() %>% #Lag tabell med Flextable
  colformat_num(digits = 2) %>% # Endrer antall desimaler til 2
  set_header_labels(group = "Gruppe",
                    m = "Gjennomsnittlig endring",
                    s = "Standardavvik") %>% # Endrer navn på overskriftene i tabellen.
  
  add_header_row(values = "Tabell 1: Endringer fra pre til post-test", colwidths = 3) %>% #Angir tittel på tabellen
  
  autofit() #Gjør tabellen penere

### Statistisk analyse ved hjelp av ANCOVA-modell
ggscatter(dataset1, x = "pre", y = "post",
  color = "group", add = "reg.line",) + 
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group)) +
  labs(x = "Pre-test", y = "Post-test", title = "Figur 1") # Sjekker om det er et lineært forhold mellom de to gruppene

dataset1 %>% anova_test(post ~ group*pre) # Brukes for å sjekke homogeniteten

model1 <- lm(post ~ pre + group, data = dataset1) # ANCOVA-modellen

model.metrics <- augment(model1) %>% # Brukes for å finne "fitted values" og residualene
  select(-.hat, -.sigma, -.fitted) # Fjerner unødvendige detaljer
head(model.metrics, 3)

shapiro_test(model.metrics$.resid) # Bruker en shapiro-test for å sjekke normaliteten i residualene. Testen var ikke signifikant og betyr at residualene er tilnærmet normalfordelt.

model.metrics %>% levene_test(.resid ~ group) # Bruker en levene_test for å sjekke om variansen i residualene er lik i de to gruppene. Testen var ikke signifikant og betyr at variansen er tilnærmet lik.

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame() # Sjekker om det er noen ekstreme verdier. Ingen ble funnet.

avtabell1 <- dataset1 %>% anova_test(post ~ pre + group) # Gjennomfører en test for å se på
get_anova_table(avtabell1) # Etter å ha justert for pretest-score viste anova-testen at det ikke var en signifikant forskjell mellom gruppene.


pwc1 <- dataset1 %>% # Sammenligner gruppene (pairwise comparison) for å se på forskjellen mellom de ved hjelp av en emmeans test og justerer p-verdiene etter bonferroni-metoden.
  emmeans_test(post ~ group, covariate = pre,
    p.adjust.method = "bonferroni")
pwc1

get_emmeans(pwc1) # Viser justerte gjennomnsittsverdier for gruppene.

res1 <- pwc1 %>% add_xy_position(x = "group", fun = "mean_se")
res1plot <- ggline(get_emmeans(res1), x = "group", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(res1, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(avtabell1, detailed = TRUE),
    caption = get_pwc_label(res1)
  )

res1plot

tidymodel1 <- tidy(model1) # Gjør tallene fra modellen penere og lagrer det i et nytt objekt.

cfmodel1 <- confint(model1) # Finner konfidensintervallene til modellen.

# Tabell regresjonsmodell

cbind(tidymodel1, cfmodel1) %>%
  mutate(term = factor(term, levels = c("(Intercept)",
                                        "pre",
                                        "groupRM30"),
                       labels = c("Intercept", 
                                  "Pre-test", 
                                  "Gruppe 30RM"))) %>% # Endrer navn på kolonnene under "term"
  flextable() %>% # Binder sammen konfidensintervallene og regresjonsmodellen til en tabell
  colformat_num(col_keys = c("estimate", 
                             "std.error",
                             "statistic", 
                             "p.value",
                             "2.5 %",
                             "97.5 %"), 
                digits = 3) %>% # Endrer antall desimaler på bestemte kolonner.
  set_header_labels(estimate = "Estimat", # Endrer navn 
                    std.error = "Standard Error", 
                    statistic = "T-Statistic",
                    p.value = "P-verdi",
                    term = "Term",
                    "2.5 %" = "CI 2.5 %",
                    "97.5 %" = "CI 97.5 %") %>%
  set_table_properties( width = 1, layout = "autofit") %>% # Gjør tabellen penere.
  add_header_row(values = "Tabell 2", colwidths = 7) %>% # Legger til en overskrift.
  fontsize(part = "header", size = 12) # Endrer størrelsen på overskriftene.


### Statistisk analyse ved hjelp av Mixed models
benchpress <- read_excel("./data/ten_vs_thirty.xlsx", na = "NA") %>% 
  filter(timepoint != "mid",
         exercise == "benchpress", 
         timepoint %in% c("pre", "post"),
         !is.na(load)) %>%
  mutate(timepoint = factor(timepoint, levels = c("pre", "post")))


lmer1 <- lmer(load ~ timepoint * group + (1|subject), data = benchpress)

summary(lmer1)

tidy(lmer1)


####################### PART 2 #############################

dataset2 <- read_csv("./data/strengthTests.csv", na = "NA") %>%
  filter(timepoint != "session1", # Fjerner timepoint "session1" fordi jeg er interessert i pre-post
         !is.na(load)) %>% # Fjerner na-verdier i load.
  mutate(timepoint = factor(timepoint, 
                            levels = c("pre",  "post"))) %>% # Gjør timepoint om til numeriske variabler
  pivot_wider(names_from = timepoint, 
              values_from = load) %>% # Lager en egen kolonne med timepointene pre og post
  mutate(change = post - pre) %>% # Lager en ny variabel change som regner ut endringene fra pre- til post-test.
  filter(!is.na(change)) # Fjerner forsøkspersoner som mangler verdier i change.



#### Lag en tabell som oppsummerer endringene
tabell11 <- dataset2 %>%
  group_by(group) %>% # Grupper etter variabelen group
  summarise (m = mean(change),
             s = sd(change)) %>% # Regn ut gjennomsnittlig endring og standardavviket
  flextable() %>% #Lag tabell med Flextable
  colformat_num(digits = 2) %>% # Endrer antall desimaler til 2
  set_header_labels(group = "Gruppe",
                    m = "Gjennomsnittlig endring",
                    s = "Standardavvik") %>% # Endrer navn på overskriftene i tabellen.
  
  add_header_row(values = "Tabell 1: Endringer fra pre til post-test", colwidths = 3) %>% #Angir tittel på tabellen
  set_table_properties( width = 1, layout = "autofit") %>%
  fontsize(part = "header", size = 12)


#### Statistisk analyse ved hjelp av ANVCOA-modell
ggscatter(dataset2, x = "pre", y = "post",
          color = "group", add = "reg.line") 

model2 <- lm(post ~ pre + group, data = dataset2) # Lager en ny regresjonsmodell

tidy(model1)



##### Fremgangsmåte 2 #######
hom2 <- dataset2 %>% 
  anova_test(post ~ group*pre) # Sjekker homogeniteten i gruppa
hom2 # Printer resultatet. Det er homogenitet i gruppa


model2 <- lm(post ~ pre + group, data = dataset2) # Grullaget for ANCOVA-modellen.


model.metrics2 <- augment(model2) %>% # Brukes for å finne "fitted values" og residualene
  select(-.hat, -.sigma, -.fitted) # Fjerner unødvendige detaljer
head(model.metrics2, 3)


shapiro2 <- shapiro_test(model.metrics2$.resid) # Bruker en shapiro-test for å sjekke normaliteten til residualene. Testen var ikke signifikant (p=0,05) og betyr at residualene er tilnærmet normale.
shapiro2


levene2 <- model.metrics2 %>% levene_test(.resid ~ group) # Bruker en levene_test for å sjekke om variansen i residualene er lik i de to gruppene. Testen var ikke signifikant og betyr at variansen er tilnærmet lik.
levene2


model.metrics2 %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame() # Sjekker om det er noen ekstreme verdier. Ingen ble funnet.


avtabell2 <- dataset2 %>% anova_test(post ~ pre + group) # Gjennomfører en test for å se på
tabell2.1 <- get_anova_table(avtabell2) # Etter å ha justert for pretest-score viste anova-testen at det ikke var en signifikant forskjell mellom gruppene.


pwc2 <- dataset2 %>% # Sammenligner gruppene (pairwise comparison) for å se på forskjellen mellom de ved hjelp av en emmeans test og justerer p-verdiene etter bonferroni-metoden.
  emmeans_test(post ~ group, covariate = pre,
               p.adjust.method = "bonferroni")
pwc2

get_emmeans(pwc2) # Viser justerte gjennomnsittsverdier for gruppene.


res2 <- pwc2 %>% add_xy_position(x = "group", fun = "mean_se") # Klargjør til å lage plot.
res2plot <- ggline(get_emmeans(res1), x = "group", y = "emmean") + # Lager resultatplottet
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(res2, hide.ns = TRUE, tip.length = FALSE) +
  labs(subtitle = get_test_label(avtabell1, detailed = TRUE),
       caption = get_pwc_label(res2)) 

res2plot

dataset3 <- read_csv("./data/strengthTests.csv", na = "NA")

boxplot(dataset3)$out
  

