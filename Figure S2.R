#Code for Figure S2: influence of link updating on outcomes 
#(when net preferences are similar for both subpopulations)


#######################################
  
#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

#obtaining random points from area where stalemates occur
num <- 50 #number of points to obtain
bplus <- runif(num, min=0, max= 0.2) 
bminus <- numeric(length(bplus))

op <- runif(length(bplus), min = 0.9, max = 1.1)
for (i in 1:length(op))
{
  bminus[i] <- 4*op[i]*bplus[i]
}

#input for rewiring parameters, inertia and stubbornness
eg1 <- expand.grid(
  Phi1p <- c(0, 0, 0.5),
  Phi1m <- c(0, 0, 0.5),
  # varying some:
  BP = bplus,
  # holding some constant:
  L = 0.3,  
  # Columns for outcomes:
  im_out <- NA,
  om_out<-NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

eg2 <- expand.grid(
  Phi2p <- c(0, 0.5, 0),
  Phi2m <- c(0, 0.5, 0),
  BM <- bminus
)


eg <- data.frame(eg1,eg2)

###############################

#the following for loop generates the data

for (i in 1:nrow(eg))
{
  eg$Var5[i] <- IM(eg$Var1[i], eg$Var1.1[i], eg$Var2[i], eg$Var2.1[i], eg$BP[i], eg$Var3[i], eg$L[i], n0)$M[T]
  eg$Var6[i] <- OM(eg$Var1[i], eg$Var1.1[i], eg$Var2[i], eg$Var2.1[i], eg$BP[i], eg$Var3[i], eg$L[i], n0)$M[T]
  if((ceiling(i/500) - i/500) == 0)
  {
    print(i) #indicating progress
  }
}

###############################

#cleaning and labeling data for visualization

egd <- eg[, c(1,2,5,6,7,8)]

for (j in 1:nrow(egd))
{
  egd$Var1[j] <- paste(egd$Var1[j], egd$Var1.1[j], sep = ",")
  egd$Var2[j] <- paste(egd$Var2[j], egd$Var2.1[j], sep = ",")
}

egd <- egd[,c(1,2,3,4)]

colnames(egd) <- c("Stratp", "Stratm", "IM_Mf","OM_Mf")

egd$Stratp <- factor(egd$Stratp, labels = c("Static", 
                                            "Disagreement\navoidance",
                                            "Agreement\navoidance"))

egd$Stratm <- factor(egd$Stratm, labels = c("Static", 
                                            "Disagreement\navoidance",
                                            "Agreement\navoidance"))

###############################

#visualizing the data

library(ggplot2)
library(dplyr)

data <- egd

#Incoming model
data %>%
  ggplot(aes(x = Stratm, y = IM_Mf)) +
  facet_grid(cols = vars(Stratp))+
  geom_violin()+
  ylim(-1,1)+
  scale_fill_gradient2(high = scales::muted("red"), low = scales::muted("blue"),
                       mid = "gray", guide = "colourbar")+
  stat_summary(fun.y=mean, geom="point", aes(shape= Stratm), size=2, fill = "#000000")+
  scale_shape_manual(values = c(16,15,17))+
  theme_light()+
  labs(title = "Effect of link updating strategies\non final state (IM)",  
       x = "Minority Strategy", y = "Final State", 
       subtitle = "Majority Strategy")+
  ggpubr::rremove("legend")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size = 16))

#Outgoing model
data %>%
  ggplot(aes(x = Stratm, y = OM_Mf)) +
  facet_grid(cols = vars(Stratp))+
  geom_violin()+
  ylim(-1,1)+
  scale_fill_gradient2(high = scales::muted("red"), low = scales::muted("blue"),
                       mid = "gray", guide = "colourbar")+
  stat_summary(fun.y=mean, geom="point", aes(shape= Stratm), size=2, fill = "#000000")+
  scale_shape_manual(values = c(16,15,17))+
  theme_light()+
  labs(title = "Effect of link updating strategies\non final state (OM)",  
       x = "Minority Strategy", y = "Final State", 
       subtitle = "Majority Strategy")+
  ggpubr::rremove("legend")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size = 16))

