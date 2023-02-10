#Influence of disagreement avoiding probabilities on consensus speeds 
#Varying majority (x) and minority (-) disagreement avoiding probabilities (axes) 
#and stubbornness (facets) to obtain convergence speeds to majority (red) or minority (blue) consensus 

#Loading required model functions (incoming model, outgoing model, consensus speeds,boundary checks)

source("Speed.R")
source("Incoming_model.R")
source("Outgoing_model.R")
source("If_boundary.R")

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

#agreement avoidant rewiring parameters 
p1p <- 0.5 #phi1+
p1m <- 0.5 #phi1-

#inertia
l <- 0.3

#varying inputs for link updating probabilities
#and stubbornnness

###varying disagreement avoidance
variation <- seq(0,0.8,0.1)

eg <- expand.grid(
  P2P <- variation,
  P2M <- variation,
  BP <- c(0.05,0.20,0.5,0.95),
  BM <- c(0.05,0.20,0.5,0.95),
  im_out <- NA,
  om_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

################################

#the following for loop generates the data

for (i in 1:nrow(eg))
{
  print(paste("Currently considering case", i, "of", nrow(eg)))
  im_Mf <- IM(p1p, eg$Var1[i], p1m, eg$Var2[i], eg$Var3[i], eg$Var4[i],l,n0)$M
  eg$Var5[i] <- CSpeed(im_Mf)*sign(im_Mf[T])
  om_Mf <- OM(p1p, eg$Var1[i], p1m, eg$Var2[i], eg$Var3[i], eg$Var4[i],l,n0)$M
  eg$Var6[i] <- CSpeed(om_Mf)*sign(om_Mf[T])
}

################################
#cleaning and labeling data
data <- eg
colnames(data) <- c("phi2p", "phi2m", "BP", "BM", "IM_speed","OM_speed")
data$BP <- factor(data$BP, levels = c(0.95, 0.5, 0.2, 0.05))

################################

library(ggplot2)
library(dplyr)
library(scales)

#Incoming model
data %>%
  ggplot(aes(x=phi2m, y=phi2p, fill=IM_speed)) +
  facet_grid(BP ~ BM) +
  geom_tile(color = "grey") +

  scale_fill_gradient2(high=scales::muted("red"),low = scales::muted("blue"),
                       na.value="#9e9e9e", guide = guide_colorbar(draw.llim = TRUE),
                       breaks=trans_breaks("log10", function(x) 10^x),
                       labels=trans_format("log10", math_format(10^.x)),
                       limits=c(-(10^-1.9),10^-1.9),
                       n.breaks=5) +
  theme_classic(base_size = 15) +

  labs(title = "Effect of disagreement avoidance \non convergence speed (IM)",
            x=expression(paste("Minority disagreement avoidance ", (phi['2-']) )), 
            y=expression(paste("Majority disagreement avoidance ", (phi['2+']) )),
            fill="Speed",
            subtitle=expression(paste("Minority stubbornness ", (beta['-']) ))) +

  theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5), 
            text = element_text(size=20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
            legend.position = "none") +

  ggpubr::rremove("grid")+ggpubr::border()

#Outgoing model
data %>%
  ggplot(aes(x=phi2m, y=phi2p, fill= OM_speed))+
  facet_grid(BP ~ BM)+
  geom_tile(color = "grey")+
  scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                       na.value = "#9e9e9e", guide = guide_colorbar(draw.llim = TRUE),
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x)),
                       limits = c(-(10^-1.8),10^-1.8),
                       n.breaks = 5)+
  theme_classic(base_size = 15)+
  labs(title = "Effect of disagreement avoidance \non convergence speed (OM)", x = expression(paste("Minority disagreement avoidance ", (phi['2-']) )), 
       y = expression(paste("Majority disagreement avoidance ", (phi['2+']) )), fill = "Speed",
       subtitle = expression(paste("Minority stubbornness ", (beta['-']) )))+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position = "none")+
  ggpubr::rremove("grid")+ggpubr::border()
