####OLA5 OPG3

####hent data####
#Undersøger data
lines <- readLines("Documents/Dataanlyse/1_semester/R/OLA5/regenskaber.csv")
head(lines, 1) # Vis de første 10 linjer

lines <- readLines("Documents/Dataanlyse/1_semester/R/OLA5/regenskaber.csv", n = 10)
cat(lines[1]) # Header
cat(lines[2]) # Første datarække

#Henter data
regnskaber <- read.csv(
  file = "Documents/Dataanlyse/1_semester/R/OLA5/regenskaber.csv", 
  header = TRUE, 
  sep = ";",          # Separator som semikolon
  stringsAsFactors = FALSE, 
  na.strings = c("NA", ""), 
  fileEncoding = "latin1" # Brug latin1 for danske tegn
)

####Analysere data####
str(regnskaber) # Oversigt over datastruktur
summary(regnskaber) # Resumé-statistik for hver kolonne

#Tæller NA og går med 2020 regnskab istedet for 2021, da der er mange NA i 2021
na_counts <- colSums(is.na(regnskaber)) # Antal NA pr. kolonne

####Rens####
#ændrer første kolonnenavn
colnames(regnskaber)[1] <- "Mulighed"

table(regnskaber$Mulighed)
#[1] "Dårlig"        "Meget dårlige" "Dårlige"       "Neutrale"      "Gode"          "Meget gode"    "Ved ikke"  

# Erstat både "Dårlig" og "Dårlige" med "Dårlige"
regnskaber$Mulighed <- gsub("^(Dårlig|Dårlige)$", "Dårlige", regnskaber$Mulighed)

# Fjern rækker, hvor der står "Ved ikke" #51 stk. ved ik
regnskaber <- regnskaber[regnskaber$Mulighed != "Ved ikke", ]

regnskaber$Mulighed <- factor(regnskaber$Mulighed, 
                              levels = c("Meget dårlige","Dårlige","Neutrale","Gode","Meget gode"),
                              ordered = TRUE)


regnskaber <- regnskaber[order(regnskaber$Mulighed), ]

#gem som RDS
saveRDS(regnskaber, "regnskaber2.rds")

####OPG3.2####
library(ordinal)
#ændrer så det en numerisk kategorifaktor - OBS: dette ændrer i dataframen permanent
regnskaber$Mulighed <- factor(regnskaber$Mulighed,
                              levels = c("Meget dårlige","Dårlige", "Neutrale", "Gode", "Meget gode"), 
                              labels = c(1, 2, 3, 4, 5),
                              ordered = TRUE)

#### CLM alle ####
udvalgte.variabler <- data.frame(
  Mulighed = factor(regnskaber$Mulighed),
  Soliditetsgrads = as.vector(regnskaber$Soliditetsgrad.2020....),
  Balance = as.vector(regnskaber$Balance.2020..1.000.kr.),
  Langfristet = regnskaber$Langfristet.gæld.2020..1.000.kr.,
  Kortfristet = regnskaber$Kortfristet.gæld.2020..1.000.kr.,
  Egenkapital = as.vector(regnskaber$Egenkapital.2020..1.000.kr.))


#fjerner NA
udvalgte.variabler <- na.omit(udvalgte.variabler)

#udregner gearing
udvalgte.variabler$Gearing <- round(((udvalgte.variabler$Kortfristet + udvalgte.variabler$Langfristet) / udvalgte.variabler$Egenkapital),2)

#udregner gældsforhold
udvalgte.variabler$Gældsforhold<- round(as.vector(udvalgte.variabler$Langfristet / udvalgte.variabler$Kortfristet),2)

#gør soliditetsgrad og balancen numerisk og egenkapital
udvalgte.variabler$Soliditetsgrads <- as.numeric(gsub(",",".", udvalgte.variabler$Soliditetsgrads))
udvalgte.variabler$Balance <- as.numeric(udvalgte.variabler$Balance)
udvalgte.variabler$Egenkapital <- as.numeric(udvalgte.variabler$Egenkapital)

#fjerner kortgæld,langgæld
udvalgte.variabler <- udvalgte.variabler[,-c(4:5)]

apply(udvalgte.variabler[, -1], 2, var)  # meget stor variation for hver variable - grundlag for skalering

str(udvalgte.variabler)

uden.scale <- udvalgte.variabler

# Standardisér numeriske variabler 
udvalgte.variabler[,-1] <- scale(udvalgte.variabler[, -1])  # Undgå første kolonne, hvis den ikke skal skaleres

clm.all <- clm(Mulighed ~ ., data = udvalgte.variabler)
summary(clm.all)


#####multikollinearitetstest####
korrelation.v <- udvalgte.variabler[,-1]
correlations <- cor(korrelation.v, use = "complete.obs")  # Brug 'complete.obs' for at ignorere rækker med NA
print(correlations)

library(corrplot)
corrplot(correlations, 
         method = "color", 
         type = "lower", 
         addCoef.col = "black", 
         tl.cex = 0.8,        # Størrelse på tekstetiketter
         tl.col = "black",    # Farve på tekstetikletter
         number.cex = 0.7,    # Størrelse på tal
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Farveskala fra blå til rød
         diag = FALSE,        # Skjul diagonal for klarere visning
         tl.srt = 45,         # Roter tekstetiketterne, så de bliver skrå
         tl.offset = 1,     # Juster afstanden for at få etiketterne tættere på bunden
         cl.pos = "n",
         outline = T)        # Placer farveskalaen på højre side


library(ordinal)
####clm på enketle####
clm.soliditet <- clm(Mulighed ~ Soliditetsgrads, data = udvalgte.variabler)
summary(clm.soliditet)

clm.balance <- clm(Mulighed ~ Balance, data = udvalgte.variabler)
summary(clm.balance)

clm.gearing <- clm(Mulighed ~ Gearing, data = udvalgte.variabler)
summary(clm.gearing)

clm.gældsforhold <- clm(Mulighed ~ Gældsforhold, data = udvalgte.variabler)
summary(clm.gældsforhold)

clm.egenkapital <- clm(Mulighed ~ Egenkapital, data = udvalgte.variabler)
summary(clm.egenkapital)


#graf af en enkelt
#Balance
library(ggplot2)
library(tidyr)
library(dplyr)

uden.scale$Balance <- log(uden.scale$Balance)

# Gruppér data efter 'Mulighed' og beregn gennemsnit og antal for alle variabler
grupperet.mulighed <- uden.scale %>%
  group_by(Mulighed) %>%  # Gruppér efter 'Mulighed'
  summarise(across(everything(), list(gennemsnit = ~mean(. , na.rm = TRUE))))  # Beregn gennemsnit af alle variabler

grupperet.mulighed$svar <- c("Meget dårlige","Dårlige", "Neutrale", "Gode", "Meget gode")

grupperet.mulighed.long <- grupperet.mulighed %>%
  pivot_longer(cols = c(Soliditetsgrads_gennemsnit, Balance_gennemsnit, Egenkapital_gennemsnit),
               names_to = "Variable",
               values_to = "Værdi")


####3.3 plots###

# Definer en ensartet farvepalet
ensartet_farver <- c(
  "1" = "#E41A1C",  # Rød
  "2" = "#377EB8",  # Blå
  "3" = "#4DAF4A",  # Grøn
  "4" = "#984EA3",  # Lilla
  "5" = "#FF7F00"   # Orange
)

# Balance plot
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Balance_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  labs(
    title = "Størrelsen på aktiverne har betydning for lånemulighederne", 
    x = NULL, 
    y = "Log(Balance)", 
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Meget dårlige", 
    "2" = "Dårlige", 
    "3" = "Neutrale", 
    "4" = "Gode", 
    "5" = "Meget gode"
  )) +
  scale_fill_manual(values = ensartet_farver) +
  geom_text(aes(label = round(Balance_gennemsnit, 2)), vjust = -0.3, fontface = "bold", size = 5) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2E3A8C"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14, color = "#4F4F4F"),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

# Gearing plot
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Gearing_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  labs(
    title = "Virksomheder med højere gearing vurderer deres lånemuligheder forskelligt",
    x = NULL,
    y = "Gearing",
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Meget dårlige", 
    "2" = "Dårlige", 
    "3" = "Neutrale", 
    "4" = "Gode", 
    "5" = "Meget gode"
  )) +
  scale_fill_manual(values = ensartet_farver) +
  geom_text(aes(label = round(Gearing_gennemsnit, 2)), vjust = -0.3, fontface = "bold", size = 5) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2E3A8C"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#4F4F4F"),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

# Soliditetsgrad plot
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Soliditetsgrads_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  labs(
    title = "Soliditetsgrad i forhold til lånemuligheder",
    x = NULL,
    y = "Soliditetsgrad",
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Meget dårlige", 
    "2" = "Dårlige", 
    "3" = "Neutrale", 
    "4" = "Gode", 
    "5" = "Meget gode"
  )) +
  scale_fill_manual(values = ensartet_farver) +
  geom_text(aes(label = round(Soliditetsgrads_gennemsnit, 2)), vjust = -0.3, fontface = "bold", size = 5) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2E3A8C"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#4F4F4F"),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

# Gældsforhold plot
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Gældsforhold_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  labs(
    title = "Gældsforhold i forhold til lånemuligheder",
    x = NULL,
    y = "Gældsforhold",
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Meget dårlige", 
    "2" = "Dårlige", 
    "3" = "Neutrale", 
    "4" = "Gode", 
    "5" = "Meget gode"
  )) +
  scale_fill_manual(values = ensartet_farver) +
  geom_text(aes(label = round(Gældsforhold_gennemsnit, 2)), vjust = -0.3, fontface = "bold", size = 5) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2E3A8C"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#4F4F4F"),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")