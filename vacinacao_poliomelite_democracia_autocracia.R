
# Vacinas contra Poliomelite em Países Capitalistas e Comunistas -------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 19/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/polio ---------------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Poliomelite é uma doença infecciosa, contraída predominantemente por crianças,
### que pode levar à paralisia permanente de várias partes do corpo e pode ultimamente
### causar morte devido à imobilização dos músculos respiratórios do paciente.

### Não existe cura para os sintomas, mas nos anos de 1950 efetivas vacinas foram
### desenvolvidas e tem sido usadas em todo o mundo desde então. Isso permitiu a alguns
### países mais ricos eliminar totalmente a doença nas décadas de 60 e 70. Entretanto,
### grandes epidemias continuaram em volta do mundo. No início da década de 80, existiu
### uma estimativa de 300 mil a 400 mil casos por ano e a doença permaneceu em 125 países.

### Como resposta a "Global Polio Eradication Initiative” (GPEI)  foi fundada em  1988
### para combater a disseminação do vírus com uma campanha global de vacinação. Desde então
### o mundo tem avançado no progresso contra a doença e ainda em 2016 o número de casos de
### paralisia foi reduzido por 99.9% com 42 casos em todo o mundo nesse mesmo ano.

### Como em 2021 o vírus tem sido encontrado para circular em apenas dois países no mundo - 
### Afeganistão e Paquistão - espera-se que a doença seja totalmente erradicada no mundo.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

poli <- read.csv("polio-vaccine-coverage-of-one-year-olds.csv")
view(poli)
names(poli)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

poli <- poli %>%
  select(-Code) %>%
  rename(taxa_vacina = Pol3....of.one.year.olds.immunized.) %>%
  view()

poli1 <- poli %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(taxa_vacina),
            sd = sd(taxa_vacina), n = n(),
            se = sd/sqrt(n)) %>%
  view()

poli2 <- poli %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  view()

poli3 <- poli %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(poli1, aes(x = fct_reorder(Entity, media), y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                              "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Coreia do Norte", "China", "Alemanha",
                              "Estados Unidos", "Japão", "Cuba")) +
  labs(x = "Países", y = "Taxa de Vacinação\n contra Poliomelite (%)") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none", axis.text = element_text(color = "black"))

ggplot(poli2, aes(x = Year, y = taxa_vacina,
                  group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                              "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Taxa de Vacinação\n contra Poliomelite (%)",
       color = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

ggplot(poli3, aes(x = Year, y = taxa_vacina,
                  group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Taxa de Vacinação\n contra Poliomelite",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
