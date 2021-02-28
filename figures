#title: Figures for Knight et al. 2020. Validation prediction: A protocol for increasing the efficiency of automated acoustic recognition. Ecological Applications.
#author: Elly C. Knight
#date: Mar 20, 2020

library(lme4)
library(nlme)
library(colorspace)
library(gridExtra)
library(grid)
library(tidyverse)
library(AICcmodavg)

g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <-
    which(sapply(tmp$grobs, function(x)
      x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

my_theme <- theme_classic() +
  theme(
    text = element_text(size = 16, family = "Arial"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    axis.line.x = element_line(linetype = 1),
    axis.line.y = element_line(linetype = 1),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 14)
  )

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis")

coni <- read.csv("UP_CONIpeent0mv2_20_20_results_validated.csv")
coni.cnn <- read.table("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Processing/CNN/UP_CONICNN0m_0.1_results_validated.txt", sep="\t")
colnames(coni.cnn) <- c("path", "start", "duration", "level", "quality", "score", "recognizer", "comments")
coni.mon <- read.table("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Processing/MonitoR/UP_CONIMonitoR0m_0.1_results_validated_newRSL.txt", sep="\t")
colnames(coni.mon) <- c("path", "start", "duration", "level", "quality", "score", "recognizer", "comments")
oven <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/UP2015_OVEN0mv1_20_50_results_validated.csv")

#Figure 1. Score ~ RSL----

#Wrangle
coni.merge <- coni %>% 
  mutate(recognizer = "SongScope",
         species = "CONI",
         label = "CONI - SongScope") %>% 
  rename(pres = CONI) %>% 
  filter(score >= 60) %>% 
  mutate(score.std = (score-min(score))/(max(score)-min(score))) %>% 
  dplyr::select(path, start, duration, level, quality, score, score.std, recognizer, location, recording, comments, pres, species, label)

cnn.merge <- coni.cnn %>% 
  mutate(pres = case_when(comments=="y" ~ 1,
                          comments=="m" ~ 1,
                          comments=="b" ~ 1),
         pres = ifelse(is.na(pres), 0, pres)) %>% 
  mutate(recognizer = "CNN",
         species = "CONI",
         label = "CONI - CNN") %>% 
  mutate(level = 20*log(level),
         level = level + (max(level)+abs(min(level)))) %>% 
  mutate(score.std = (score-min(score))/(max(score)-min(score))) %>% 
  separate(path, into=c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "recording"), remove=FALSE, sep="/") %>% 
  separate(recording, into=c("location", "datename", "timename", "extra"), remove=FALSE, sep="_") %>% 
  dplyr::select(path, start, duration, level, quality, score, score.std, recognizer, location, recording, comments, pres, species, label)

oven.merge <- oven %>% 
  mutate(pres = case_when(comments=="y" ~ 1,
                          comments=="m" ~ 1),
         pres = ifelse(is.na(pres), 0, pres)) %>% 
  mutate(recognizer = "SongScope",
         species = "OVEN",
         label = "OVEN - SongScope") %>% 
  mutate(score.std = (score-min(score))/(max(score)-min(score))) %>% 
  dplyr::rename(location=aru,
                recording=filename,
                path=filepath) %>% 
  dplyr::select(path, start, duration, level, quality, score, score.std, recognizer, location, recording, comments, pres, species, label)

merge <- rbind(coni.merge, cnn.merge, oven.merge)

#Visualization----

ggplot(merge, aes(x=level, score)) +
  geom_hex() +
  geom_smooth() +
  facet_grid(label ~ pres, scales="free")

ggplot(merge, aes(x=level, score.std, colour=comments)) +
  geom_point() +
  facet_grid(label ~ pres, scales="free")

#Model Song Scope CONI----
coni_0 <- subset(coni.merge, pres == 0)
coni_1 <- subset(coni.merge, pres == 1)

m_0_1 <- lm(score ~ level, data=coni_0)
m_0_2 <- lm(score ~ poly(level, 2), data=coni_0)
m_0_3 <- lm(score ~ poly(level, 3), data=coni_0)
aictab(list(m_0_1, m_0_2, m_0_3))
summary(m_0_3)

m_1_1 <- lm(score ~ level, data=coni_1)
m_1_2 <- lm(score ~ poly(level, 2), data=coni_1)
m_1_3 <- lm(score ~ poly(level, 3), data=coni_1)
m_log <- (lm(score ~ log(level), data=coni_1))
aictab(list(m_1_1, m_1_2, m_1_3, m_log))
summary(m_1_3)

#Predict
MyData <- expand.grid(level = seq(
  from = min(coni_1$level),
  to = max(coni_1$level),
  length = 1000
))

MyData_coni_1 <- predict(m_1_3, MyData, type="response", se.fit=TRUE) %>% cbind(MyData)
MyData_coni_1$upr <- MyData_coni_1$fit + 1.96 * MyData_coni_1$se.fit
MyData_coni_1$lwr <- MyData_coni_1$fit - 1.96 * MyData_coni_1$se.fit

#Model CONI CNN----
cnn_0 <- subset(cnn.merge, pres == 0)
cnn_1 <- subset(cnn.merge, pres == 1)

m_0_1 <- lm(score ~ level, data=cnn_0)
m_0_2 <- lm(score ~ poly(level, 2), data=cnn_0)
m_0_3 <- lm(score ~ poly(level, 3), data=cnn_0)
aictab(list(m_0_1, m_0_2, m_0_3))
summary(m_0_3)

m_1_1 <- lm(score ~ level, data=cnn_1)
m_1_2 <- lm(score ~ poly(level, 2), data=cnn_1)
m_1_3 <- lm(score ~ poly(level, 3), data=cnn_1)
m_log <- (lm(score ~ log(level), data=cnn_1))
aictab(list(m_1_1, m_1_2, m_1_3, m_log))
summary(m_1_2)

#Predict
MyData <- expand.grid(level = seq(
  from = min(cnn_0$level),
  to = max(cnn_0$level),
  length = 1000
))

MyData_cnn_0 <- predict(m_0_3, MyData, type="response", se.fit=TRUE) %>% cbind(MyData)
MyData_cnn_0$upr <- MyData_cnn_0$fit + 1.96 * MyData_cnn_0$se.fit
MyData_cnn_0$lwr <- MyData_cnn_0$fit - 1.96 * MyData_cnn_0$se.fit

MyData <- expand.grid(level = seq(
  from = min(cnn_1$level),
  to = max(cnn_1$level),
  length = 1000
))

MyData_cnn_1 <- predict(m_1_2, MyData, type="response", se.fit=TRUE) %>% cbind(MyData)
MyData_cnn_1$upr <- MyData_cnn_1$fit + 1.96 * MyData_cnn_1$se.fit
MyData_cnn_1$lwr <- MyData_cnn_1$fit - 1.96 * MyData_cnn_1$se.fit


#Model Song Scope OVEN----
oven_0 <- subset(oven.merge, pres == 0)
oven_1 <- subset(oven.merge, pres == 1)

m_0_1 <- lm(score ~ level, data=oven_0)
m_0_2 <- lm(score ~ poly(level, 2), data=oven_0)
m_0_3 <- lm(score ~ poly(level, 3), data=oven_0)
aictab(list(m_0_1, m_0_2, m_0_3))
summary(m_0_3)

m_1_1 <- lm(score ~ level, data=oven_1)
m_1_2 <- lm(score ~ poly(level, 2), data=oven_1)
m_1_3 <- lm(score ~ poly(level, 3), data=oven_1)
m_log <- (lm(score ~ log(level), data=oven_1))
aictab(list(m_1_1, m_1_2, m_1_3, m_log))
summary(m_1_2)

#Predict
MyData <- expand.grid(level = seq(
  from = min(oven_1$level),
  to = max(oven_1$level),
  length = 1000
))

MyData_oven_1 <- predict(m_1_2, MyData, type="response", se.fit=TRUE) %>% cbind(MyData)
MyData_oven_1$upr <- MyData_oven_1$fit + 1.96 * MyData_oven_1$se.fit
MyData_oven_1$lwr <- MyData_oven_1$fit - 1.96 * MyData_oven_1$se.fit


#Plot
gg_0 <- ggplot() +
  geom_hex(
    aes(y = score, x = level),
    data = coni_0,
    bins = 50,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c() +
  my_theme +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("False positives") +
  xlim(c(20, 90)) +
  ylim(c(60, 85))
print(gg_0)

gg_1 <- ggplot() +
  geom_hex(
    aes(y = score, x = level),
    data = coni_1,
    bins = 50,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c() +
  geom_ribbon(
    aes(x = level, ymax = upr, ymin = lwr),
    data = MyData_coni_1,
    alpha = 0.5,
    fill = "grey40",
    colour = NA
  ) +
  geom_line(aes(y = fit, x = level), data = MyData_coni_1, size = 1) +
  my_theme +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("True positives") +
  annotate(geom="text", x=90, y=74, label = "Song Scope - Common nighthawk", angle = 270, size = 4) +
  xlim(c(20, 90)) +
  ylim(c(60, 85))
print(gg_1)

gg_2 <- ggplot() +
  geom_hex(
    aes(y = score, x = level),
    data = cnn_0,
    bins = 50,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c() +
  my_theme +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(c(20, 90)) +
  ylim(c(0.2, 1.1))
print(gg_2)

gg_3 <- ggplot() +
  geom_hex(
    aes(y = score, x = level),
    data = cnn_1,
    bins = 50,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c() +
  geom_ribbon(
    aes(x = level, ymax = upr, ymin = lwr),
    data = MyData_cnn_1,
    alpha = 0.5,
    fill = "grey40",
    colour = NA
  ) +
  geom_line(aes(y = fit, x = level), data = MyData_cnn_1, size = 1) +
  my_theme +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=90, y=0.65, label = "CNN - Common nighthawk", angle = 270, size = 4) +
  xlim(c(20, 90)) +
  ylim(c(0.2, 1.1))
print(gg_3)

gg_4 <- ggplot() +
  geom_hex(
    aes(y = score, x = level),
    data = oven_0,
    bins = 50,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c() +
  my_theme +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(c(20, 90)) +
  ylim(c(50, 85))
print(gg_4)

gg_5 <- ggplot() +
  geom_hex(
    aes(y = score, x = level),
    data = oven_1,
    bins = 50,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c() +
  geom_ribbon(
    aes(x = level, ymax = upr, ymin = lwr),
    data = MyData_oven_1,
    alpha = 0.5,
    fill = "grey40",
    colour = NA
  ) +
  geom_line(aes(y = fit, x = level), data = MyData_oven_1, size = 1) +
  my_theme +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=90, y=65, label = "Song Scope - Ovenbird", angle = 270, size = 4) +
  xlim(c(20, 90)) +
  ylim(c(50, 85))
print(gg_5)


Fig2 <- grid.arrange(
  arrangeGrob(gg_0, gg_1, gg_2, gg_3, gg_4, gg_5,
              ncol = 2, nrow = 3),
  bottom = textGrob("Relative sound level (dB)", gp = gpar(fontsize =
                                                              16)),
  left = textGrob(
    "Recognizer score",
    gp = gpar(fontsize = 16),
    rot = 90
  )
)

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/Figures")
ggsave(
  "Fig1.tiff",
  plot = Fig2,
  dpi = 300,
  width = 7.5,
  height = 10.5,
  units = "in",
  device = "tiff")

#Figure 3----
setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis")
mod.lr.thresh.coni <- read.csv("Step4SetLearningRate_coni_full.csv")
mod.lr.thresh.oven <- read.csv("Step4SetLearningRate_oven_full.csv")

colours <- heat_hcl(
  4,
  h = c(50, 320),
  l = c(80, 50),
  c = c(100, 50),
  power = 1)

dev.coni <- ggplot(mod.lr.thresh.coni) +
  geom_point(aes(x = fraction, y = test.dev, colour = factor(lr))) +
  geom_line(aes(x = fraction, y = test.dev, colour = factor(lr))) +
  xlab("") +
  ylab("Residual deviance") +
  my_theme +
  scale_colour_manual(name = "Learning\nrate", values = colours) +
  scale_x_continuous(breaks=c(0,4000, 8000, 12000, 16000)) +
  scale_y_continuous(breaks=c(0.1, 0.15, 0.2, 0.25, 0.3),
                     lim=c(0.11, 0.31)) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0.5,0,0), "cm")) +
  ggtitle("Common nighthawk") +
  theme(plot.title=element_text(hjust = 0.5))
print(dev.coni)

trees.coni <- ggplot(mod.lr.thresh.coni) +
  geom_point(aes(x = fraction, y = trees, colour = factor(lr))) +
  geom_line(aes(x = fraction, y = trees, colour = factor(lr))) +
  geom_abline(aes(intercept = 1000, slope = 0), linetype = "dashed") +
  xlab("") +
  ylab("Number of trees") +
  my_theme +
  scale_colour_manual(name = "Learning\nrate", values = colours) +
  scale_x_continuous(breaks=c(4000, 8000, 12000, 16000)) +
  scale_y_continuous(breaks=c(0,2500, 5000, 7500, 10000),
                     limits=c(0,10000)) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0.5,0,0), "cm"))
print(trees.coni)

dev.oven <- ggplot(mod.lr.thresh.oven) +
  geom_point(aes(x = fraction, y = test.dev, colour = factor(lr))) +
  geom_line(aes(x = fraction, y = test.dev, colour = factor(lr))) +
  xlab("") +
  ylab("") +
  my_theme +
  scale_colour_manual(name = "Learning\nrate", values = colours)  +
  scale_x_continuous(breaks=c(4000, 8000, 12000)) +
  scale_y_continuous(breaks=c(0.1, 0.15, 0.2, 0.25, 0.3),
                     lim=c(0.11, 0.31)) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0.5,0,0), "cm")) +
  ggtitle("Ovenbird") +
  theme(plot.title=element_text(hjust = 0.5))
print(dev.oven)

trees.oven <- ggplot(mod.lr.thresh.oven) +
  geom_point(aes(x = fraction, y = trees, colour = factor(lr))) +
  geom_line(aes(x = fraction, y = trees, colour = factor(lr))) +
  geom_abline(aes(intercept = 1000, slope = 0), linetype = "dashed") +
  xlab("") +
  ylab("") +
  my_theme +
  scale_colour_manual(name = "Learning\nrate", values = colours) +
  scale_x_continuous(breaks=c(4000, 8000, 12000)) +
  scale_y_continuous(breaks=c(0,2500, 5000, 7500, 10000),
                     limits=c(0,10000)) +
#  theme(plot.margin = unit(c(0,0.5,0,0), "cm")) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0.5,0,0), "cm"))
print(trees.oven)

#legend.2 <- g_legend(trees.oven)

Fig3 <- grid.arrange(
  arrangeGrob(
    dev.coni,
    trees.coni,
    dev.oven,
    trees.oven,
    legend.2,
    nrow = 2,
    ncol = 3,
    widths = c(3, 3, 1),
    layout_matrix = rbind(c(1,3,5),
                          c(2,4,5))
  ),
  bottom = textGrob("Number of training data points",
                    gp = gpar(fontsize = 16)))

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/Figures")
ggsave(
  "Fig3.tiff",
  plot = Fig3,
  dpi = 300,
  width = 8,
  height = 6,
  units = "in",
  device = "tiff")

#Figure 4----
setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/")
all.coni <- read.csv("Step5SampleSizeTesting_coni_full.csv")
all.oven <- read.csv("Step5SampleSizeTesting_oven_full.csv")
fit.coni <- read.csv("Step5SampleSizeNLS_coni_full.csv")
fit.oven <- read.csv("Step5SampleSizeNLS_oven_full.csv")

Fig4.oven <- ggplot() +
  geom_jitter(
    aes(x = fraction.i, y = p, group = boot.i),
    colour = "grey40",
    alpha = 0.5,
    data = all.oven
  ) +
  geom_line(
    aes(x = fraction, y = p),
    data = fit.oven,
    colour = "black",
    size = 1
  ) +
  geom_hline(aes(yintercept = 0.854676), linetype = "dashed") +
  geom_vline(aes(xintercept = 5540), linetype = "dashed") +
  ylab("Precision") +
  my_theme +
  theme(legend.position = "none") +
  ggtitle("Ovenbird") +
  theme(plot.title=element_text(hjust = 0.5),
        axis.title.x = element_blank())
print(Fig4.oven)

Fig4.coni <- ggplot() +
  geom_jitter(
    aes(x = fraction.i, y = r, group = boot.i),
    colour = "grey40",
    alpha = 0.5,
    data = all.coni
  ) +
  geom_line(
    aes(x = fraction, y = r),
    data = fit.coni,
    colour = "black",
    size = 1
  ) +
  geom_hline(aes(yintercept = 0.94951), linetype = "dashed") +
  geom_vline(aes(xintercept = 10590), linetype = "dashed") +
  ylab("Recall") +
  ylim(c(0.5, 1)) +
  my_theme +
  theme(legend.position = "none") +
  ggtitle("Common nighthawk") +
  theme(plot.title=element_text(hjust = 0.5),
        axis.title.x = element_blank())
print(Fig4.coni)

Fig4 <- grid.arrange(arrangeGrob(Fig4.coni, Fig4.oven,
                     nrow=1,
                     ncol=2),
bottom = textGrob("Number of training data points",
                  gp = gpar(fontsize = 16)))

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/Figures")
ggsave(
  "Fig4.tiff",
  plot = Fig4,
  dpi = 300,
  width = 12,
  height = 6,
  units = "in",
  device = "tiff")

#Table 1. Final efficiency results----
hit.coni.full <- read.csv("Step7HitResponseVariable_coni_full.csv") %>% 
  mutate(response="hit",
         species="CONI",
         model="full") %>% 
  rename(dets=CONI)
visit.coni.full <- read.csv("Step7VisitResponseVariable_coni_full.csv") %>% 
  mutate(response="visit",
         species="CONI",
         model="full") %>% 
  rename(dets=CONI)
loc.coni.full <- read.csv("Step7LocationResponseVariable_coni_full.csv") %>% 
  mutate(response="location",
         species="CONI",
         model="full") %>% 
  rename(dets=CONI)

hit.coni.red <- read.csv("Step7HitResponseVariable_coni_reduced.csv") %>% 
  mutate(response="hit",
         species="CONI",
         model="reduced") %>% 
  rename(dets=CONI)
visit.coni.red <- read.csv("Step7VisitResponseVariable_coni_reduced.csv") %>% 
  mutate(response="visit",
         species="CONI",
         model="reduced") %>% 
  rename(dets=CONI)
loc.coni.red <- read.csv("Step7LocationResponseVariable_coni_reduced.csv") %>% 
  mutate(response="location",
         species="CONI",
         model="reduced") %>% 
  rename(dets=CONI)

hit.oven.full <- read.csv("Step7HitResponseVariable_oven_full.csv") %>% 
  mutate(response="hit",
         species="OVEN",
         model="full") %>% 
  rename(dets=oven)
visit.oven.full <- read.csv("Step7VisitResponseVariable_oven_full.csv") %>% 
  mutate(response="visit",
         species="OVEN",
         model="full") %>% 
  rename(dets=oven)
loc.oven.full <- read.csv("Step7LocationResponseVariable_oven_full.csv") %>% 
  mutate(response="location",
         species="OVEN",
         model="full") %>% 
  rename(dets=oven)

hit.oven.red <- read.csv("Step7HitResponseVariable_oven_reduced.csv") %>% 
  mutate(response="hit",
         species="OVEN",
         model="reduced") %>% 
  rename(dets=oven)
visit.oven.red <- read.csv("Step7VisitResponseVariable_oven_reduced.csv") %>% 
  mutate(response="visit",
         species="OVEN",
         model="reduced") %>% 
  rename(dets=oven)
loc.oven.red <- read.csv("Step7LocationResponseVariable_oven_reduced.csv") %>% 
  mutate(response="location",
         species="OVEN",
         model="reduced") %>% 
  rename(dets=oven)

step7 <- rbind(hit.coni.full, visit.coni.full, loc.coni.full,
               hit.coni.red, visit.coni.red, loc.coni.red,
               hit.oven.full, visit.oven.full, loc.oven.full,
               hit.oven.red, visit.oven.red, loc.oven.red)

model <- c("full", "reduced")
species <- c("CONI", "OVEN")
loop <- expand.grid(model, species) %>% 
  mutate(recall = c(0.99, 0.99, 0.99, 0.95))

#' ## 8a. Retain detections scenario
#'
min <- 0.98 #Set minimum mean recall

hit.mean <- step7 %>%
  dplyr::filter(response=="hit") %>% 
  group_by(species, model, thresh) %>%
  summarize(r = round(mean(r), 2),
            n = mean(n),
            p = mean(p)) %>% 
  arrange(desc(r), desc(n))
#'
#' ## 8b. Total error scenario
#'
visit.mean <- step7 %>%
  dplyr::filter(response=="visit") %>% 
  mutate(pr = r + p) %>%
  group_by(species, model, thresh) %>%
  summarize(
    pr = mean(pr),
    n = mean(n),
    r = mean(r),
    p = mean(p)
  ) %>%
  arrange(desc(pr))
#'
#' ## 8c. No validation scenario
#'
min <- 0.99 #Set minimum mean precision

loc.mean <- step7 %>%
  dplyr::filter(response=="location") %>% 
  group_by(species, model, thresh) %>%
  summarize(r = mean(r),
            p = round(mean(p),2),
            n = mean(n)) %>%
  arrange(desc(p), desc(r))

#Figure 5----
setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis")
hit.coni.full <- read.csv("Step7HitResponseVariable_coni_full.csv") %>% 
  mutate(response="detection",
         species="CONI",
         model="full") %>% 
  rename(dets=CONI)
visit.coni.full <- read.csv("Step7VisitResponseVariable_coni_full.csv") %>% 
  mutate(response="visit",
         species="CONI",
         model="full") %>% 
  rename(dets=CONI)
loc.coni.full <- read.csv("Step7LocationResponseVariable_coni_full.csv") %>% 
  mutate(response="location",
         species="CONI",
         model="full") %>% 
  rename(dets=CONI)

hit.oven.full <- read.csv("Step7HitResponseVariable_oven_full.csv") %>% 
  mutate(response="detection",
         species="OVEN",
         model="full") %>% 
  rename(dets=oven)
visit.oven.full <- read.csv("Step7VisitResponseVariable_oven_full.csv") %>% 
  mutate(response="visit",
         species="OVEN",
         model="full") %>% 
  rename(dets=oven)
loc.oven.full <- read.csv("Step7LocationResponseVariable_oven_full.csv") %>% 
  mutate(response="location",
         species="OVEN",
         model="full") %>% 
  rename(dets=oven)

step7 <- rbind(hit.coni.full, visit.coni.full, loc.coni.full,
               hit.oven.full, visit.oven.full, loc.oven.full) %>% 
  dplyr::select(species, var, boot, thresh, n, r, p) %>% 
  gather(key = key, value = val, n:p) %>% 
  rename(metric = key) %>% 
  dplyr::select(species, var, boot, thresh, metric, val)

sum.out2 <- step7 %>%
  group_by(species, metric, var, thresh) %>%
  summarize(mean = mean(val),
            sd = sd(val)) %>%
  mutate(up = mean + sd,
         down = mean - sd)

sum.out2$metric <- factor(sum.out2$metric, levels = c("r", "p", "n"))
summary(sum.out2$metric)
sum.out2$metric <-
  factor(sum.out2$metric, labels = c("Recall", "Precision", "Efficiency"))
summary(sum.out2$metric)

sum.out2$var <- factor(sum.out2$var)
summary(sum.out2$var)
sum.out2$var <-
  factor(sum.out2$var, labels = c("Detection", "Location", "Visit"))
sum.out2$var <-
  factor(sum.out2$var, levels = c("Detection", "Visit", "Location"))
summary(sum.out2$var)

colours <- heat_hcl(
  2,
  h = c(50, 320),
  l = c(80, 50),
  c = c(100, 50),
  power = 1)

Fig5 <- ggplot(sum.out2) +
  geom_ribbon(aes(x=thresh, ymax=up, ymin=down, group=species),
              alpha=0.5, fill="grey40", colour=NA) +
  geom_line(aes(x=thresh, y=mean, colour=species)) +
  scale_colour_manual(name="Focal species", values=colours, labels=c("Common\nnighthawk", "Ovenbird")) +
  facet_grid(metric~var) + 
  xlab("Classification threshold") +
  ylab("Metric value") +
  my_theme +
  theme(plot.title = element_text(hjust = 0.5, size=16)) +
  scale_x_continuous(breaks=c(0,0.5,1.0)) +
  scale_y_continuous(breaks=c(0,0.5,1.0))
Fig5

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/Figures")
ggsave(
  "Fig5.tiff",
  plot = Fig5,
  dpi = 300,
  width = 10,
  height = 8,
  units = "in",
  device = "tiff")

#Figure 6 - Efficiency----
hits <- data.frame(hits = seq(0, 50000, 10)) %>% 
  mutate(extra = ifelse(hits<=16000, hits, hits-16000)) %>% 
  mutate(retain = ifelse(hits<=16000, hits, 16000+extra*(1-0.737)),
         total = ifelse(hits<=16000, hits, 16000+extra*(1-0.795)),
         noval = ifelse(hits<=16000, hits, 16000),
         RSL = ifelse(hits<=16000, hits, 16000+extra*(1-0.26)),
         none = hits) %>% 
  gather(key = "scenario", value="val", retain:none)

hits$scenario <- factor(hits$scenario,levels = c("none",
                                                  "RSL",
                                                  "retain",
                                                  "total",
                                                  "noval"),
                        labels = c("No validation prediction",
                                   "Validation prediction - RSL and score only",
                                   "Validation prediction - retain detections",
                                   "Validation prediction - total error",
                                   "Validation prediction - no validation"))

intercepts <- data.frame(int = c(10000, 16000),
                         label = c("Detections required for model training",
                                   "Time required for model training"))

efficiency <- ggplot(hits) +
  geom_vline(aes(xintercept = int, group=label, linetype = label), data=intercepts) +
  geom_line(aes(x=hits, y=val, colour=scenario)) +
  scale_color_viridis_d(name = "Validation scenario") +
  scale_linetype_manual(name = "", values = c("longdash", "dotted")) +
  xlab("Detections reported by recognizer") +
  ylab("Detections requiring validation") +
  my_theme
efficiency

setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/Figures")
ggsave(
  "Fig6-EfficiencyHits.tiff",
  plot = efficiency,
  dpi = 300,
  width = 8,
  height = 4,
  units = "in",
  device = "tiff")

hits <- data.frame(hrs = seq(0, 30, 0.05),
                   hits = seq(0, 60000, 100)) %>% 
  mutate(extra = ifelse(hrs<=9.6, hrs, hrs-9.6)) %>% 
  mutate(retain = ifelse(hrs<=9.6, hrs, 9.6+extra*(1-0.737)),
         total = ifelse(hrs<=9.6, hrs, 9.6+extra*(1-0.795)),
         noval = ifelse(hrs<=9.6, hrs, 9.6),
         RSL = ifelse(hrs<=9.6, hrs, 9.6+extra*(1-0.26)),
         none = hrs) %>% 
  gather(key = "scenario", value="val", retain:none)

hits.sum <- hits %>% 
  group_by(scenario) %>% 
  summarize(hrs = mean(hrs),
            hits = mean(hits),
            extra = mean(extra),
            val = mean(val))
hits.sum

hits$scenario <- factor(hits$scenario,levels = c("none",
                                                 "RSL",
                                                 "retain",
                                                 "total",
                                                 "noval"),
                        labels = c("No validation prediction",
                                   "Validation prediction - RSL and score only",
                                   "Validation prediction - retain detections",
                                   "Validation prediction - total error",
                                   "Validation prediction - no validation"))

intercepts <- data.frame(x1 = c(10000, 0),
                         x2 = c(10000, 19200),
                         y1 = c(0, 9.6),
                         y2 = c(5, 9.6),
                         label = c("Hits needed for validation prediction\ntraining",
                                   "Time needed to train and run\nvalidation prediction"))

efficiency <- ggplot(hits) +
  geom_segment(aes(x=x1, xend=x2, y=y1, yend=y2, group=label, linetype = label), data=intercepts) +
  geom_line(aes(x=hits, y=val, colour=scenario)) +
  scale_color_viridis_d(name = "Validation scenario") +
  scale_linetype_manual(name = "", values = c("longdash", "dotted")) +
  xlab("Number of hits reported by recognizer") +
  ylab("Time to validate (hrs)") +
  my_theme
efficiency


setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/StatisticalValidation/Analysis/Analysis/Figures")
ggsave(
  "Fig6-EfficiencyHrs.tiff",
  plot = efficiency,
  dpi = 300,
  width = 8,
  height = 4,
  units = "in",
  device = "tiff")

