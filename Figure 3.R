#Code for figure 3: Influence of stubbornness on outcomes (for static network)
# FIXME: (Pranav) This code generates 2 figures, which are not exactly identical to the
# figure in the manuscript. Maybe write what modifications were made from here?

#Loading required model functions (incoming model, outgoing model, consensus speeds)

source("Speed.R")
source("Incoming_model.R")
source("Outgoing_model.R")

##############################

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

###set parameter values

#rewiring probabilities
p1p <- 0 #phi1+
p2p <- 0 #phi2+
p1m <- 0 #phi1-
p2m <- 0 #phi2-

#inertia
l <- 0.3 

# input for stubbornness
eg <- expand.grid(
  # varying stubbornness:
  BP = seq(0.05, 0.95, by=0.05),
  BM = seq(0.05, 0.95, by=0.05),
  im_out = NA,
  om_out = NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

##############################

#the following for loop generates the data

for (i in 1:nrow(eg))
{
  print(paste("Working on case ", i, " of ", length(eg$BP)))
  eg$im_out[i] <- IM(p1p, p2p, p1m, p2m, eg$BP[i], eg$BM[i], l, n0)$M[T]
  eg$om_out[i] <- OM(p1p, p2p, p1m, p2m, eg$BP[i], eg$BM[i], l, n0)$M[T]
}

##############################

#visualizing the data

library(ggplot2)
library(dplyr)

none <- eg
conim <- none[none$im_out > 0.8,]
conom <- none[none$om_out > 0.8,]

#Incoming model
none %>%
  ggplot(aes(x=BM, y=BP, fill=im_out))+
  geom_tile(color = "grey") +
  geom_tile(data = conim, color = "yellow", linewidth = 1, linetype = "dotted")+
  scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                       mid = "gray", guide = "colourbar", limits = c(-1,1))+
  theme_classic(base_size = 17)+
  labs(title = "Effect of Stubbornness on Final state (IM)", 
       x= expression(paste("Minority stubbornness ",(beta['-']) )), 
       y = expression(paste("Majority stubbornness ", (beta['+']))), fill = "Final State", subtitle = "Static network")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=20),
        legend.position = "bottom", legend.key.width = unit(2,"cm"),
        legend.title = element_text(vjust = 0.9))+
  ggpubr::rremove("grid")+ggpubr::border()

#Outgoing model
none %>%
  ggplot(aes(x=BM, y=BP, fill=om_out))+
  geom_tile(color = "grey") +
  geom_tile(data = conom, color = "yellow", linewidth = 1, linetype = "dotted")+
  scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                       mid = "gray", guide = "colourbar", limits = c(-1,1))+
  theme_classic(base_size = 17)+
  labs(title = "Effect of Stubbornness on Final state (OM)", 
       x= expression(paste("Minority stubbornness ",(beta['-']) )), 
       y = expression(paste("Majority stubbornness ", (beta['+']))), fill = "Final State", subtitle = "Static network")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=20),
        legend.position = "bottom", legend.key.width = unit(2,"cm"),
        legend.title = element_text(vjust = 0.9))+
  ggpubr::rremove("grid")+ggpubr::border()
