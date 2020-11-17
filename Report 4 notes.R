library(readxl)
library(tidyverse)
library(flextable)
library(broom)
library(ggpubr)
library(rstatix)


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
  color = "group", add = "reg.line") # Sjekker om det er et lineært forhold mellom de to gruppene

model1 <- lm(post ~ pre + group, data = dataset1)

model.metrics <- augment(model1) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Velger variablene vi er interessert i
head(model.metrics, 3)

shapiro_test(model.metrics$.resid) # Bruker en shapiro-test for å sjekke normaliteten

model.metrics %>% levene_test(.resid ~ group) # Bruker en levene_test for å sjekke om variansen i residualene er lik i de to gruppene.

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame() # Sjekker om det er noen ekstreme verdier. Ingen ble funnet.

tidy(model1)
