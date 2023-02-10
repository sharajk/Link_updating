#Influence of disagreement avoiding link updating on consensus outcomes (when net preferences are similar for both subpopulations)
#Varying majority and minority disagreement avoiding probabilities while keeping agreement avoiding probabilities constant (0 as default)

#Loading required model functions (incoming model, outgoing model, consensus speeds,boundary checks)

source("Speed.R")
source("Incoming_model.R")
source("Outgoing_model.R")
source("If_boundary.R")

##############################

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

#rewiring probabilities
p1p <- 0 #phi1+
p1m <- 0 #phi1-

#inertia
l <- 0.3 

#stubbornness 
bp <- 0.05 # + stubbornness
bm <- 0.2 # - stubbornness

#input for disagreement avoidance probability

res <- 0.05 #set resolution for variation in input

eg <- expand.grid(
  P2P <- seq(0,0.9, by=res),
  P2M <- seq(0,0.9, by=res),
  im_out <- NA,
  om_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

###############################

#the following for loop generates the data
for (i in 1:nrow(eg))
{
  print(paste("Currently on step ", i, "of", nrow(eg)))
  eg$Var3[i] <- IM(p1p, eg$Var1[i], p1m, eg$Var2[i], bp, bm, l, n0)$M[T]
  eg$Var4[i] <- OM(p1p, eg$Var1[i], p1m, eg$Var2[i], bp, bm, l, n0)$M[T]
}

###############################

#cleaning and labeling data for visualization

colnames(eg) <- c("phi2p", "phi2m", "IM_Mf","OM_Mf")

###############################

#visualizing the data

library(ggplot2)
library(dplyr)

data <- eg
conim <- data[abs(data$IM_Mf) > 0.8,]
conom <- data[abs(data$OM_Mf) > 0.8,]

#Incoming model
data %>%
  ggplot(aes(x=phi2m, y=phi2p, fill=IM_Mf))+
  geom_tile(color = "grey")+
  geom_tile(data = conim, color = "yellow", linewidth = 1.5, linetype = "dotted")+
  scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                       mid = "grey",guide = "colourbar", limits = c(-1,1))+
  theme_classic(base_size = 20)+
  labs(title = "Effect of disagreement avoidance\non consensus outcomes (IM)", 
       x = expression(paste("Minority disagreement avoidance ", (phi['2-']) )), 
       y = expression(paste("Majority disagreement avoidance ", (phi['2+']) )), 
       fill = "Final State")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        axis.text = element_text(size=20),
        legend.key.width = unit(1.5,"cm"),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.9),
  )+
  ggpubr::rremove("grid")+
  ggpubr::border()

#Outgoing model
data %>%
  ggplot(aes(x=phi2m, y=phi2p, fill=OM_Mf))+
  geom_tile(color = "grey")+
  geom_tile(data = conom, color = "yellow", linewidth = 1.5, linetype = "dotted")+
  scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                       mid = "grey",guide = "colourbar", limits = c(-1,1))+
  theme_classic(base_size = 20)+
  labs(title = "Effect of disagreement avoidance\non consensus outcomes (OM)", 
       x = expression(paste("Minority disagreement avoidance ", (phi['2-']) )), 
       y = expression(paste("Majority disagreement avoidance ", (phi['2+']) )), 
       fill = "Final State")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        axis.text = element_text(size=20),
        legend.key.width = unit(1.5,"cm"),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.9),
  )+
  ggpubr::rremove("grid")+
  ggpubr::border()