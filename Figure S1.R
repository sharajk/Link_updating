#Code for Figure S1: Influence of stubbornness (varying in the axis) and
#strategies (varying in the facets) on outcomes
#default is for S1A: 80/20 majority to minority ratio (change n0 for changing ratio)

#Loading required model functions (incoming model, outgoing model, consensus speeds)

source("Speed.R")
source("Incoming_model.R")
source("Outgoing_model.R")

##############################

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion { change here for S1B (n0=0.6) and S1C(n0=0.5) }

##set parameter values

#inertia
l <- 0.3

#varying stubbornnness and strategies

eg1 <- expand.grid(
  # varying some:
  BP <- seq(0.05, 0.95, by = 0.1),
  BM <- seq(0.05, 0.95, by = 0.1),
  P1P <- c(0,0,0.5),
  P1M <- c(0,0,0.5),
  # Columns for outcomes:
  im_out <- NA,
  om_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

eg2 <- expand.grid(
  # varying some:
  BP <- seq(0.05, 0.95, by = 0.1),
  BM <- seq(0.05, 0.95, by = 0.1),
  P2P <- c(0,0.5,0),
  P2M <- c(0,0.5,0),
  # Columns for outcomes:
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

eg <- data.frame(eg1,eg2)

##############################

#the following for loop generates the data

for (i in 1:nrow(eg))
{
  print(paste("Currently considering case", i, "of", nrow(eg)))
  eg$Var5[i] <- IM(eg$Var3[i], eg$Var3.1[i], eg$Var4[i], eg$Var4.1[i], eg$Var1[i], eg$Var2[i], l, n0)$M[T]
  eg$Var6[i] <- OM(eg$Var3[i], eg$Var3.1[i], eg$Var4[i], eg$Var4.1[i], eg$Var1[i], eg$Var2[i], l, n0)$M[T]
}

##############################

#labeling and cleaning the data for visualization

data <- data.frame("BP"=eg$Var1,
                    "BM"=eg$Var2,
                    "Stratp"=NA,
                    "Stratm"=NA,
                    "IM_Mf"=eg$Var5,
                    "OM_Mf"=eg$Var6)

for(j in 1:nrow(data))
{
  data$Stratp[j] <- paste(eg$Var3[j], eg$Var3.1[j], sep=",")
  data$Stratm[j] <- paste(eg$Var4[j], eg$Var4.1[j], sep=",")
}

data$Stratp <- factor(data$Stratp,
                      labels=c("Static", "Disagreement\navoidance", "Agreement\navoidance"))

data$Stratp <- factor(data$Stratp,
                      levels=c("Agreement\navoidance","Disagreement\navoidance", "Static"))

data$Stratm <- factor(data$Stratm,
                      labels=c("Static", "Disagreement\navoidance", "Agreement\navoidance"))

# save(data, file= "Stub_hm.RData")

################################

#visualizing the data

library(ggplot2)
library(dplyr)

#highlight the consensus outcomes
conim <- data[abs(data$IM_Mf) > 0.8,]
conom <- data[abs(data$OM_Mf) > 0.8,]

#Incoming model
data %>%
  ggplot(aes(x = BM, y = BP, fill = IM_Mf)) +
  facet_grid(Stratp ~ Stratm)+
  geom_tile(color = "grey") +
  geom_tile(data = conim, color = "yellow", size = 1, linetype = "dotted")+
  scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                       mid = "grey", guide = "colourbar", limits = c(-1,1))+
  theme_classic(base_size = 15)+
  labs(title = "Effect of stubbornness
  on final state (IM)", x= expression(paste("Minority stubbornness ", (beta['-']) )), 
       y = expression(paste("Majority stubbornness ", (beta['+']))), 
       fill = "Final State", subtitle= "Minority Strategy")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=20),
        legend.position = "bottom", legend.key.width = unit(2,"cm"),
        legend.title = element_text(vjust = 0.9))+
  ggpubr::rremove("grid")+ggpubr::border()

#Outgoing model
data %>%
  ggplot(aes(x = BM, y = BP, fill = OM_Mf)) +
  facet_grid(Stratp ~ Stratm)+
  geom_tile(color = "grey") +
  geom_tile(data = conom, color = "yellow", size = 1, linetype = "dotted")+
  scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                       mid = "grey", guide = "colourbar", limits = c(-1,1))+
  theme_classic(base_size = 15)+
  labs(title = "Effect of stubbornness
  on final state (OM)", x= expression(paste("Minority stubbornness ", (beta['-']) )),  #change to OM for outgoing model
       y = expression(paste("Majority stubbornness ", (beta['+']))), 
       fill = "Final State", subtitle= "Minority Strategy")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=20),
        legend.position = "bottom", legend.key.width = unit(2,"cm"),
        legend.title = element_text(vjust = 0.9))+
  ggpubr::rremove("grid")+ggpubr::border()

# FIXME: (Pranav) This generates only two images (in place of 6 in the manuscript).
# Also there is some bug in the second image, some plots are empty.
