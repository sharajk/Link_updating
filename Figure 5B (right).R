#Code for Figure 5B (right) : influence of majority stubbornness on majority consensus speeds

#Loading required model functions (incoming model, outgoing model, consensus speeds)

library(devtools)
library(roxygen2)
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/057a5a7a4ce6f72fe06ea31e607518a09fbaba71/Speed.R")
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/main/Incoming%20model.R")
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/main/Outgoing%20model.R")

##############################

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

#inertia 
l <- 0.3

#minority stubbornness
bm <- 0

#rewiring parameters
p1p <- 0 #phi1+
p2p <- 0 #phi2+
p1m <- 0 #phi1-
p2m <- 0 #phi2-

#varying inertia input
eg <- expand.grid(
  bp <- seq(0.2,1,by=0.05),
  im_out <- NA,
  om_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

colnames(eg) <- c("Stubbornness", "IM_Speed", "OM_Speed")

###############################

#the following for loop generates the data

for(i in 1:nrow(eg))
{
  eg$IM_Speed[i] <- CSpeed(IM(p1p,p2p,p1m,p2m,eg$Stubbornness[i],bm,l,n0)$M)
  eg$OM_Speed[i] <- CSpeed(OM(p1p,p2p,p1m,p2m,eg$Stubbornness[i],bm,l,n0)$M)
}

###############################

#cleaning and labeling data for visualization
data <- data.frame("Stubbornness"=rep(eg$Stubbornness,2),"Model"=c(rep("Incoming",length(eg$Stubbornness)), rep("Outgoing",length(eg$Stubbornness))),
                   "Speed"=c(eg$IM_Speed, eg$OM_Speed))

###############################

#visualizing the data

library(ggplot2)
library(dplyr)
library(scales)

data %>%
  ggplot(aes(x=Stubbornness,y=Speed, linetype = Model))+
  geom_line(color = "red")+
  scale_y_log10(limits = c(10^-3.25, 10^-1.3),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_light(base_size = 13)+
  ggpubr::border()+
  scale_linetype_manual(values = c("solid", "dashed"), name = "Model")+
  labs(title = "Effect of stubbornness\non convergence speed", 
       x = expression(paste('Majority Stubbornness ',(beta['+']))), 
       y = "Speed to majority consensus") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20),
        legend.text = element_text(size=16), legend.title = element_text(size=17))
