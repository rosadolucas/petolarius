### Reproductive aspects of Oxyrhopus petolarius 
### Mendonça et al. 
### 
#-------------------------------------------#
rm(list = ls()) 


# Packages ----------------------------------------------------------------
{
  if(!require(tidyverse)) install.packages("tidyverse", dependencies = T)
  if(!require(DHARMa)) install.packages("DHARMa", dependencies = T)
  if(!require(Rmisc)) install.packages("Rmisc", dependencies = T)
  if(!require(MASS)) install.packages("MASS", dependencies = T)
  if(!require(car)) install.packages("car", dependencies = T)
  if(!require(vegan)) install.packages("vegan", dependencies = T)
  if(!require(gclus)) install.packages("gclus", dependencies = T)
  if(!require(betapart)) install.packages("betapart", dependencies = T)
  if(!require(devtools)) install.packages("devtools", dependencies = T)
  if(!require(ggbiplot)) install_github("vqv/ggbiplot", dependencies = T)
  if(!require(showtext)) install.packages("showtext", dependencies = T)
  if(!require(cowplot)) install.packages("cowplot", dependencies = T)
 }

###--------------------### 
# Directory ---------------------------------------------------------------
setwd("")


# Sexual Dimorphism -----------------------------------------------

data <- read.table("Sexual_dimorphism.txt", header = T)
summary(data)

data <- data %>% 
  mutate_if(is.character, as.factor) 

### PCA ### ---------------------------------------------- ###
sex <- read.table("Sexual_dimorphism_n.txt", header = T) 
head(sex)

# Removing SVL effects over the measures
{
  TL <- sex$TL/sex$SVL
  ED <- sex$ED/sex$SVL
  HW <- sex$HW/sex$SVL
  HL <- sex$HL/sex$SVL
  BM <- sex$BM/sex$SVL
}

shape.data <- cbind(TL, ED, HW, HL, BM)
shape.data <- shape.data %>% 
  as.data.frame()

### Making the probable scale number to NA specime
#m0 <- glm(sex$VS ~ 1)
#m1 <- glm(sex$VS ~ sex$SVL)
#anova(m0, m1, test = "F")
#summary(m1)

#head(sex)
#1.898e+02-1.484e-02*733

sex$VS[is.na(sex$VS)] <- 179
head(sex)

shape.data$VS <- sex$VS
shape.data$TS <- sex$TS
head(shape.data)

# 
shape.data <- `.rowNamesDF<-`(shape.data, value = c("F1", "F2", "F3",
                                                      "F4",
                                                      "F5",
                                                      "F6",
                                                      "M1",
                                                      "M2",
                                                      "M3",
                                                      "M4",
                                                      "M5",
                                                      "M6",
                                                      "M7",
                                                      "M8",
                                                      "M9",
                                                      "M10",
                                                      "M11",
                                                      "M12")); shape.data


pca.cor <- prcomp(shape.data, center = T, scale. = T) #correlation PCA
summary(pca.cor)

# Where are the individuals in the plot?
scores.pca <- pca.cor$x
plot(scores.pca[,1], scores.pca[,2], type = "n")
text(scores.pca[,1], scores.pca[,2])

biplot(pca.cor, scaling = 1)
ggbiplot(pca.cor, obs.scale = 1, var.scale = 1,
         groups = data$sex, ellipse = T,
         circle = F)+
  scale_color_discrete(name = '')+
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  theme_classic()

### Normality. P value need to be > 0.05
shapiro.test(data$SVL) # p-value = 0.9016
shapiro.test(data$TL) # p-value = 0.3115
shapiro.test(data$ED) # p-value = 0.7272 
shapiro.test(data$HW) # p-value = 0.118
shapiro.test(data$HL) # p-value = 0.5003
shapiro.test(data$VS) # p-value = 0.09171
shapiro.test(data$TS) # p-value = 0.01076 *
shapiro.test(data$BM) # p-value = 0.1073

### Homogeneity. P value need to be > 0.05
leveneTest(data$SVL ~ data$sex) # Pr(>F): 0.7381
leveneTest(data$TL ~ data$sex) # Pr(>F): 0.07203
leveneTest(data$ED ~ data$sex) # Pr(>F): 0.4793
leveneTest(data$HW ~ data$sex) # Pr(>F): 0.4988
leveneTest(data$HL ~ data$sex) # Pr(>F): 0.2387
leveneTest(data$VS ~ data$sex) # Pr(>F): 0.8518
leveneTest(data$TS ~ data$sex) # Pr(>F): 0.7616
leveneTest(data$BM ~ data$sex) # Pr(>F): 0.2277

### T-test 
shape.data$sex <- data$sex

HW = t.test(shape.data[1:6,3],shape.data[7:18,3]) %>% print #t = -3.0774, df = 10.914, p-value = 0.01061*
ED = t.test(shape.data[1:6,2],shape.data[7:18,2]) %>% print #t = -4.0816, df = 15.931, p-value = 0.0008766*
TL = t.test(shape.data[1:6,1],shape.data[7:18,1]) %>% print #t = -3.2132, df = 15.23, p-value = 0.005712*
HL = t.test(shape.data[1:6,4],shape.data[7:18,4]) %>% print #t = -3.0774, df = 10.914, p-value = 0.01061*
BM = t.test(shape.data[1:6,5],shape.data[7:18,5]) %>% print #t = 2.8307, df = 8.0591, p-value = 0.02196*
VS = t.test(shape.data[1:6,6],shape.data[7:18,6]) %>% print #t = 2.5386, df = 7.4033, p-value = 0.03701*
TS = t.test(shape.data[1:6,7],shape.data[7:18,7]) %>% print #t = -2.0014, df = 10.562, p-value = 0.07171

{
g1 <- ggplot(data= shape.data, aes(x=sex, y=HW, color = sex))+
  geom_boxplot()+
  scale_color_manual(values = c("#1b2db9", "#c050d4"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Relation Head Width", x = "Sex", y = "Head Width")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 10, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

g2 <- ggplot(data= shape.data, aes(x=sex, y=ED, color = sex))+
  geom_boxplot()+
  scale_color_manual(values = c("#1b2db9", "#c050d4"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Relation Eye Distance", x = "Sex", y = "Eye distance")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 10, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

g3 <- ggplot(data= shape.data, aes(x=sex, y=TL, color = sex))+
  geom_boxplot()+
  scale_color_manual(values = c("#1b2db9", "#c050d4"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Relation Tail Length", x = "Sex", y = "Tail length")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 10, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

g4 <- ggplot(data= shape.data, aes(x=sex, y=HL, color = sex))+
  geom_boxplot()+
  scale_color_manual(values = c("#1b2db9", "#c050d4"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Relation Head Length", x = "Sex", y = "Head length")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 10, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

g5 <- ggplot(data= shape.data, aes(x=sex, y=BM, color = sex))+
  geom_boxplot()+
  scale_color_manual(values = c("#1b2db9", "#c050d4"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Relation Body Mass", x = "Sex", y = "Body mass")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 10, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

g6 <- ggplot(data= shape.data, aes(x=sex, y=VS, color = sex))+
  geom_boxplot()+
  scale_color_manual(values = c("#1b2db9", "#c050d4"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Relation Ventral Scale", x = "Sex", y = "Ventral scale")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 10, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

g7 <- ggplot(data= shape.data, aes(x=sex, y=TS, color = sex))+
  geom_boxplot()+
  scale_color_manual(values = c("#1b2db9", "#c050d4"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Relation Sub-caudal Scale", x = "Sex", y = "Sub-caudal scale")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 10, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))
}

plot_grid(g1, g2, g3, g4, g5, g6, g7,
          nrow = 4,
          ncol = 2,
          labels = "AUTO",
          label_size = 14,
          label_fontfamily = "Roboto",
          scale = .95,
          align = "hv")


# Male --------------------------------------------------------------------
male <- read.table("Male.txt", header = T)
head(male)

maledit <- male[,-c(6:12)]
head(maledit)

# Assessing if SVL and Body mass are correlated
summary(lm(male$BM ~ male$SVL)) # p-value = 0.00164 **

ggplot(male, aes(x = SVL, y = BM))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if TM and Body mass are correlated
male$BMshape <- male$BM/male$SVL

summary(lm(male$TM ~ male$BMshape)) # p-value = 3.21e-05 ***

ggplot(male, aes(x = BM, y = TM))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if TM and SVL are correlated
summary(lm(male$TM ~ male$SVL)) # p-value = 0.011 *

ggplot(male, aes(x = BM, y = SVL))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if TV and SVL are correlated
summary(lm(male$TV ~ male$SVL)) # p-value = 0.0199 *

ggplot(male, aes(x = TV, y = SVL))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if TV and Body Mass are correlated
summary(lm(male$TV ~ male$BMshape)) # p-value = 0.000611 ***

ggplot(male, aes(x = TV, y = BM))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if GSI and Body Mass are correlated
summary(lm(male$GSI ~ male$BM)) # p-value = 0.0779 .

ggplot(male, aes(x = BM, y = GSI))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if GSI and Body Mass are correlated
summary(lm(male$GSI ~ male$SVL)) # p-value = 0.299

ggplot(male, aes(x = SVL, y = GSI))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")


###  TV ~ SEASON ### ---------------------------------------------- ###
testVol <- male$TV/male$SVL
maledit$testVol <- testVol

shapiro.test(maledit$testVol) # p-value = 0.2258
leveneTest(maledit$testVol ~ maledit$season) # p-value = 0.3534

tv <- t.test(maledit$testVol ~ maledit$season) %>% print # t = 1.7389, df = 7.5119, p-value = 0.1227

ggplot(data= maledit, aes(x=season, y=testVol, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Testis Volume seasonality", x = "Season", y = "Testis Volume")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))
  

###  TM ~ SEASON ### ---------------------------------------------- ###
testMass <- male$TM/male$SVL
maledit$testMass <- testMass

shapiro.test(maledit$testMass) # p-value = 0.2276
leveneTest(maledit$testMass ~ maledit$season) # p-value = 0.3041

tm = t.test(maledit$testMass ~ maledit$season) %>% print # t = 2.5963, df = 10, p-value = 0.02666*

ggplot(data= maledit, aes(x=season, y=testMass, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Testis Mass seasonality", x = "Season", y = "Testis Mass")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))


###  GSI ~  SEASON ### ---------------------------------------------- ###
shapiro.test(maledit$GSI) # p-value = 0.2103
leveneTest(maledit$GSI ~ maledit$season) # p-value = 0.2535

gsi = t.test(maledit$GSI ~ maledit$season) %>% print # t = 0.77255, df = 4.6456, p-value = 0.4772

 ggplot(data= maledit, aes(x=season, y=GSI, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "GSI seasonality", x = "Season", y = "GSI")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

###  GSI ~ TM ### ---------------------------------------------- ###
summary(lm(maledit$GSI ~ maledit$testMass)) # P-value = 0.000914 ***

ggplot(maledit, aes(x = testMass, y = GSI))+
  geom_point(color = "#175C41", size = 2.5)+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "#289E71", alpha = 0.08, col = "#289E71")+
  theme_test()+
  labs(title = "GSI and Testis Mass", x = "Testis Mass", y = "GSI")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))
  
###  GSI ~ TV ### ---------------------------------------------- ###
summary(lm(maledit$GSI ~ maledit$testVol)) # P-value = 0.00218 **

ggplot(maledit, aes(x = testVol, y = GSI))+
  geom_point(color = "#175C41", size = 2.5)+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "#289E71", alpha = 0.08, col = "#289E71")+
  theme_test()+
  labs(title = "GSI and Testis Volume", x = "Testis volume", y = "GSI")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))


# Female ------------------------------------------------------------------
fe <- read.table("Female.txt", header = T)
fe <- fe %>% 
  mutate_if(is.character, as.factor)

summary(fe)

# Assessing if Follicles length and SVL are correlated
summary(lm(fe$FL ~ fe$SVL)) # p-value = 0.485

ggplot(fe, aes(x = SVL, y = FL))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if Follicles length and Body Mass are correlated
summary(lm(fe$FL ~ fe$BM)) # p-value = 0.0475 *

ggplot(fe, aes(x = BM, y = FL))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if Body Mass and SVL are correlated
summary(lm(fe$BM ~ fe$SVL)) # p-value = 0.213

ggplot(fe, aes(x = SVL, y = BM))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if Follicle number and SVL are correlated
summary(lm(fe$FN ~ fe$SVL)) # p-value = 0.725

ggplot(fe, aes(x = SVL, y = FN))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")

# Assessing if Follicle number and SVL are correlated
summary(lm(fe$FN ~ fe$BM)) # p-value = 0.455

ggplot(fe, aes(x = BM, y = FN))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "red", alpha = 0.05, col = "red")


###  Follicle length ~ SEASON ### ---------------------------------------------- ###
shapiro.test(fe$FL) # p-value = 0.5011
leveneTest(fe$FL ~ fe$season) # p-value = 0.7938

fl = t.test(fe$FL ~ fe$season) %>% print # t = -5.4044, df = 3.8061, p-value = 0.006535*

ggplot(data= fe, aes(x=season, y= FL, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Follicle Length Seasonality", x = "Season", y = "Follicle Length")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

###  Follicle  number ~ SEASON ### ---------------------------------------------- ###
shapiro.test(fe$FN) # p-value = 0.1945
leveneTest(fe$FN ~ fe$season) # p-value = 0.7388

fn = t.test(fe$FN ~ fe$season) %>% print # t = -0.28029, df = 3.1464, p-value = 0.7967

ggplot(data= fe, aes(x=season, y= FN, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Follicle number Seasonality", x = "Season", y = "Follicle number")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

### BM ~ Season ### ---------------------------------------------- ###
fl = t.test(fe$BM~ fe$season) %>% print # t = -3.9168, df = 3.1921, p-value = 0.02642

ggplot(data= fe, aes(x=season, y= BM, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Body Mass Seasonality", x = "Season", y = "Body Mass")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

# Graphics ----------------------------------------------------------------
### We will need run all the analysis to do this. <<<<<<-------------------##

{
p1 <- ggplot(data= maledit, aes(x=season, y=testVol, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Testis volume seasonality", x = "Season", y = "Testis Volume")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data= maledit, aes(x=season, y=testMass, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Testis Mass seasonality", x = "Season", y = "Testis Mass")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

p3 <- ggplot(data= maledit, aes(x=season, y=GSI, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "GSI seasonality", x = "Season", y = "GSI")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

p4 <- ggplot(maledit, aes(x = testMass, y = GSI))+
  geom_point(color = "#175C41", size = 2.5)+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "#289E71", alpha = 0.12, col = "#289E71")+
  theme_test()+
  labs(title = "GSI response to the testis mass", x = "Testis Mass", y = "GSI")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

p5 <- ggplot(data= fe, aes(x=season, y= FL, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Follicle length seasonality", x = "Season", y = "Follicle length")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

p6 <- ggplot(data= fe, aes(x=season, y= FN, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Follicle number seasonality", x = "Season", y = "Follicle number")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

p7 <- ggplot(data= fe, aes(x=season, y= BM, color = season))+
  geom_boxplot()+
  scale_color_manual(values = c("#EBA859", "#5ABCEB"))+
  geom_jitter(position = position_jitter(0.2), size = 2.5)+
  theme_test()+
  labs(title = "Body Mass Seasonality", x = "Season", y = "Body Mass")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))

p8 <- ggplot(fe, aes(x = SVL, y = FN))+
  geom_point(color = "#175C41", size = 2.5)+
  geom_smooth(method = "lm", formula = y ~ x, se = T, 
              fill = "#289E71", alpha = 0.12, col = "#289E71")+
  theme_test()+
  labs(title = "Relation SVL and Follicle number", x = "SVL", y = "Follicles number")+
  theme(text = element_text(size = 16, color = "black", family = "Roboto"), axis.text = element_text(size = 12, color = "black", family = "Montserrat"), legend.position = "none", plot.title = element_text(hjust = 0.5))
}

plot_grid(p1, p2, p3, p5, p6, p7, p8,
          nrow = 4,
          ncol = 2,
          labels = "AUTO",
          label_fontfamily = "Roboto",
          label_size = 14,
          align = "hv"
          )
