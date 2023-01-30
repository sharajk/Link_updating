#Code for Figure 4A: influence of link updating on outcomes (when net preferences are similar for both subpopulations)

#Loading required model functions (incoming model, outgoing model, consensus speeds)

library(devtools)
library(roxygen2)
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/057a5a7a4ce6f72fe06ea31e607518a09fbaba71/Speed.R")
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/main/Incoming%20model.R")
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/main/Outgoing%20model.R")

###############################

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

#obtaining random points from area where stalemates occur
num <- 100 #number of points to obtain
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
  im_out = NA,
  om_out =NA,
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
  eg$im_out[i] <- IM(eg$Var1[i], eg$Var1.1[i], eg$Var2[i], eg$Var2.1[i], eg$BP[i], eg$Var3[i], eg$L[i], n0)$M[T]
  eg$om_out[i] <- OM(eg$Var1[i], eg$Var1.1[i], eg$Var2[i], eg$Var2.1[i], eg$BP[i], eg$Var3[i], eg$L[i], n0)$M[T]
  if((ceiling(i/100) - i/100) == 0)
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

colnames(egd) <- c("Stratp", "Stratm", "IM","OM")

egd$Stratp <- factor(egd$Stratp, labels = c("Static", 
                                   "Disagreement\navoidance",
                                   "Agreement\navoidance"))

egd$Stratm <- factor(egd$Stratm, labels = c("Static", 
                                   "Disagreement\navoidance",
                                   "Agreement\navoidance"))

# save(egd, file= "Strat_bias.RData") #if you want to save the file

##############################

#visualizing the data

library(ggplot2)
library(dplyr)

data <- egd

data$Stratm <- factor(data$Stratm, levels = c("Static",
                                              "Disagreement\navoidance", 
                                              "Agreement\navoidance"))
data$Stratp <- factor(data$Stratp, levels = c("Agreement\navoidance",
                                              "Disagreement\navoidance", 
                                              "Static"))

#Incoming model
data %>%
  ggplot(aes(x=IM_Mf, y = Stratm))+
  facet_grid(rows = vars(Stratp))+
  geom_vline(aes(xintercept=0), color = "#000000", linetype = "dotted")+
  xlim(-1,1)+
  stat_summary(fun =mean, geom="point", aes(shape= Stratm), size=3, fill = "black")+
  scale_shape_manual(values = c(16,15,17))+
  geom_smooth(method = lm, colour = "#bebebecc")+
  theme_light()+
  labs(title = "Effect of link updating strategies \n on final state (IM)", x = "Final State",
       y = "Minority Strategy")+
  ggpubr::rremove("legend")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), text = element_text(size = 17),
        axis.title.y = element_text(vjust = +2))

#Outgoing model
data %>%
  ggplot(aes(x=OM_Mf, y = Stratm))+
  facet_grid(rows = vars(Stratp))+
  geom_vline(aes(xintercept=0), color = "#000000", linetype = "dotted")+
  xlim(-1,1)+
  stat_summary(fun =mean, geom="point", aes(shape= Stratm), size=3, fill = "black")+
  scale_shape_manual(values = c(16,15,17))+
  geom_smooth(method = lm, colour = "#bebebecc")+
  theme_light()+
  labs(title = "Effect of link updating strategies \n on final state (OM)", x = "Final State",
       y = "Minority Strategy")+
  ggpubr::rremove("legend")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), text = element_text(size = 17),
        axis.title.y = element_text(vjust = +2))


#####################################################
#alternate density plots
data1 <- reshape2::melt(data,id.vars= c("Stratp","Stratm"), variable.name = "Model")

data1 %>%
  ggplot(aes(x=value, y = after_stat(count)/sum(count), fill = Model))+
  geom_density(alpha=0.1)+
  facet_grid(Stratp~Stratm)+
  geom_vline(aes(xintercept=0), color = "#000000", linetype = "dotted")+
  xlim(-1,1)+
  scale_fill_manual(values = c("green","purple"))+
  # geom_smooth(method = lm, colour = "#bebebecc")+
  theme_light()+
  labs(title = "Effect of link updating strategies \n on final state (IM)", x = "Final State",
       y = "Normalized density",
       subtitle = "Minority Strategy")+
  # ggpubr::rremove("legend")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), text = element_text(size = 17),
        axis.title.y = element_text(vjust = +2))
