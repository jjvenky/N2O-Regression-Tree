# Code for regression trees used in Venkiteswaran, Rosamond, and Schiff
# "Non-linear response of riverine N2O fluxes to oxygen and temperature"
# https://github.com/jjvenky/N2O-Regression-Tree

library(mvpart)
library(rpart.plot)
library(ggplot2)
library(GGally)



# ---------- Load data ----------

VRS_data <- read.csv('VenkiteswaranRosamondSchiffRegressionTree_data.csv')
VRS_data$newDate <- as.Date(VRS_data$newDate, format = "yyyy-mm-dd")



# ---------- Subset the data for the river sections ----------

sectionOne = subset.data.frame(VRS_data, VRS_data$SecNo == "S1")
sectionTwo = subset.data.frame(VRS_data, VRS_data$SecNo == "S2")
sectionThree = subset.data.frame(VRS_data, VRS_data$SecNo == "S3")
sectionFour = subset.data.frame(VRS_data, VRS_data$SecNo == "S4")
notUrban = subset.data.frame(VRS_data, VRS_data$SecNo != "S2")



# ---------- Grow the regression trees  ----------
# Use mvpart instead of rpart since it will automatically prune at 1se and
# do multiple xvals

set.seed(1)
all.rp = mvpart(N2O.flux ~ Temperature + DO + NO3, method="anova", data=VRS_data, xv="1se", xvmult=10)
sectionOne.rp = mvpart(N2O.flux ~ Temperature + DO + NO3, method="anova", data=sectionOne, xv="1se", xvmult=10)
sectionTwo.rp = mvpart(N2O.flux ~ Temperature + DO + NO3, method="anova", data=sectionTwo, xv="1se", xvmult=10)
sectionThree.rp = mvpart(N2O.flux ~ Temperature + DO + NO3, method="anova", data=sectionThree, xv="1se", xvmult=10)
sectionFour.rp = mvpart(N2O.flux ~ Temperature + DO + NO3, method="anova", data=sectionFour, xv="1se", xvmult=10)
notUrban.rp = mvpart(N2O.flux ~ Temperature + DO + NO3, method="anova", data=notUrban, xv="1se", xvmult=10)



# ---------- Basic view of the results ----------
# Note that R2 is 1-RE (rel error) from the printcp results

printcp(all.rp) # display the results
plotcp(all.rp) # visualize cross-validation results
summary(all.rp) # detailed summary of splits



# ---------- Basic figures of regression trees with titles

prp(all.rp, varlen = 0, extra = 1, uniform = TRUE, main = expression("Grand River"~N[2]*O~"Fluxes"))
prp(sectionOne.rp, varlen = 0, extra = 1, uniform = TRUE, main = expression("Grand River Section One"~N[2]*O~"Fluxes"))
prp(sectionTwo.rp, varlen = 0, extra = 1, uniform = TRUE, main = expression("Grand River Section Two"~N[2]*O~"Fluxes"))
prp(sectionThree.rp, varlen = 0, extra = 1, uniform = TRUE, main = expression("Grand River Section Three"~N[2]*O~"Fluxes"))
prp(sectionFour.rp, varlen = 0, extra = 1, uniform = TRUE, main = expression("Grand River Section Four"~N[2]*O~"Fluxes"))
prp(notUrban.rp, varlen = 0, extra = 1, uniform = TRUE, main = expression("Grand River Not Urban (Sites 1-9, 13-23)"~N[2]*O~"Fluxes"))



# ---------- Figures with stardard deviations  ----------
# This includes Figure 4 and Suppl Figures 1-5
# Note that sd values have to be extracted and inserted by hand
# Only want the nodes that are terminal
# Steps are:
# 1. get the nodes where each of the data are categorized in the tree
# 2. get means and sds for each of the nodes we need for plotting
# 3. make data.frame of same size as the tree
# 4. put means and sds into data.frame same size as the tree
# 5. make plot  with custom node labes from the calcualted means and sds

# Figure 4: want nodes 3, 4, 6, 7
VRS_data$node = factor(all.rp$where, levels = 1:max(levels(factor(all.rp$where))))
tmpSD1 = ddply(VRS_data[, c("N2O.flux","node")], .(node), function(x) c(m = mean(x$"N2O.flux"),s = sd(x$"N2O.flux")))
tmpSD2 = data.frame(node = 1:max(levels(factor(all.rp$where))))
tmpSD2 = merge(tmpSD1, tmpSD2, by = "node", all = TRUE)
prp(all.rp, varlen = 0, extra = 0, uniform = T, node.fun = function(x, labs, digits, varlen) paste(labs, "\u00b1", format(tmpSD2[, "s"], digits = 1, nsmall = 0), "\nn =", x$frame$n), shadow.col = "grey", split.box.col = "lightgrey", split.border.col = "darkgray", space = 3, split.space = 4, round = 2, split.round = 1, shadow.offset = 0.2)

# Suppl Figure 1: want nodes 2, 3
sectionOne$node = factor(sectionOne.rp$where, levels=1:max(levels(factor(sectionOne.rp$where))))
tmpSD1 = ddply(sectionOne[, c("N2O.flux", "node")],  .(node), function(x) c(m = mean(x$"N2O.flux"), s = sd(x$"N2O.flux")))
tmpSD2 = data.frame(node=1:max(levels(factor(sectionOne.rp$where))))
tmpSD2 = merge(tmpSD1, tmpSD2, by = "node", all = TRUE)
prp(sectionOne.rp, varlen = 0, extra = 0, uniform = T, node.fun = function(x, labs, digits, varlen) paste(labs, "\u00b1", format(tmpSD2[, "s"], digits = 1, nsmall = 0), "\nn =", x$frame$n), shadow.col = "grey", split.box.col = "lightgrey", split.border.col = "darkgray", space = 3, split.space = 4, round = 2, split.round = 1, shadow.offset = 0.2)

# Suppl Figure 2: want nodes 3, 4, 6, 7
sectionTwo$node = factor(sectionTwo.rp$where, levels=1:max(levels(factor(sectionTwo.rp$where))))
tmpSD1 = ddply(sectionTwo[, c("N2O.flux", "node")],  .(node), function(x) c(m = mean(x$"N2O.flux"), s = sd(x$"N2O.flux")))
tmpSD2 = data.frame(node=1:max(levels(factor(sectionTwo.rp$where))))
tmpSD2 = merge(tmpSD1, tmpSD2, by = "node", all = TRUE)
prp(sectionTwo.rp, varlen = 0, extra = 0, uniform = T, node.fun = function(x, labs, digits, varlen) paste(labs, "\u00b1", format(tmpSD2[, "s"], digits = 1, nsmall = 0), "\nn =", x$frame$n), shadow.col = "grey", split.box.col = "lightgrey", split.border.col = "darkgray", space = 3, split.space = 4, round = 2, split.round = 1, shadow.offset = 0.2)

# Suppl Figure 3: want nodes 2, 3
sectionThree$node = factor(sectionThree.rp$where, levels=1:max(levels(factor(sectionThree.rp$where))))
tmpSD1 = ddply(sectionThree[, c("N2O.flux", "node")],  .(node), function(x) c(m = mean(x$"N2O.flux"), s = sd(x$"N2O.flux")))
tmpSD2 = data.frame(node=1:max(levels(factor(sectionThree.rp$where))))
tmpSD2 = merge(tmpSD1, tmpSD2, by = "node", all = TRUE)
prp(sectionThree.rp, varlen = 0, extra = 0, uniform = T, node.fun = function(x, labs, digits, varlen) paste(labs, "\u00b1", format(tmpSD2[, "s"], digits = 1, nsmall = 0), "\nn =", x$frame$n), shadow.col = "grey", split.box.col = "lightgrey", split.border.col = "darkgray", space = 3, split.space = 4, round = 2, split.round = 1, shadow.offset = 0.2)

# Suppl Figure 4: want 2, 3
sectionFour$node = factor(sectionFour.rp$where, levels=1:max(levels(factor(sectionFour.rp$where))))
tmpSD1 = ddply(sectionFour[, c("N2O.flux", "node")],  .(node), function(x) c(m = mean(x$"N2O.flux"), s = sd(x$"N2O.flux")))
tmpSD2 = data.frame(node=1:max(levels(factor(sectionFour.rp$where))))
tmpSD2 = merge(tmpSD1, tmpSD2, by = "node", all = TRUE)
prp(sectionFour.rp, varlen = 0, extra = 0, uniform = T, node.fun = function(x, labs, digits, varlen) paste(labs, "\u00b1", format(tmpSD2[, "s"], digits = 1, nsmall = 0), "\nn =", x$frame$n), shadow.col = "grey", split.box.col = "lightgrey", split.border.col = "darkgray", space = 3, split.space = 4, round = 2, split.round = 1, shadow.offset = 0.2)

# Suppl Figure 5: want nodes 2, 3
notUrban$node = factor(notUrban.rp$where, levels=1:max(levels(factor(notUrban.rp$where))))
tmpSD1 = ddply(notUrban[, c("N2O.flux", "node")],  .(node), function(x) c(m = mean(x$"N2O.flux"), s = sd(x$"N2O.flux")))
tmpSD2 = data.frame(node=1:max(levels(factor(notUrban.rp$where))))
tmpSD2 = merge(tmpSD1, tmpSD2, by = "node", all = TRUE)
prp(notUrban.rp, varlen = 0, extra = 0, uniform = T, node.fun = function(x, labs, digits, varlen) paste(labs, "\u00b1", format(tmpSD2[, "s"], digits = 1, nsmall = 0), "\nn =", x$frame$n), shadow.col = "grey", split.box.col = "lightgrey", split.border.col = "darkgray", space = 3, split.space = 4, round = 2, split.round = 1, shadow.offset = 0.2)



# Figure 2: scatterplot matrix  ----------
# N2O flux is umolN2O/m2/d but easier plotting if mmolN2O/m2/d
# Note that alignment of some elements was adjusted after-the-fact in Inkscape

VRS_data$N2O..flux = VRS_data$N2O.flux/1000

ggpairs(VRS_data,  columns=c("Temperature", "DO", "NO3", "N2O..flux"),
        colour="SecNo", alpha=0.5,
        lower = list(continuous = "points"), 
        diag = list(continuous = "density"),
        axisLabels = "show"
)



# Double-check the correlation matrix that is displayed by ggpairs
cor(VRS_data[, c("Temperature", "DO", "NO3", "N2O.flux")])



# ---------- Figure 3: N2O flux but section with DO as colour ----------
# Grab a colour-blind friendly palette
# http://wiki.stdout.org/rcookbook/Graphs/Colors%20%28ggplot2%29/
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = VRS_data, aes(x = SecNo, y = N2O.flux)) +
       geom_boxplot(outlier.size = 0) + geom_jitter(aes(colour = DO)) +
       scale_y_log10(limits = c(0.1, 10000)) +
       labs(x = "", y = expression(paste(N[2]*O, " flux (" ~mu, "mol/m" ^2, "/d)"))) +
       scale_x_discrete(labels = c("Till Plain", "Urban", "Recharge", "Clay Plain")) +
       scale_colour_gradientn(colours = cbbPalette[c(1, 2, 3, 4)], breaks = c(2, 5, 10, 15),
       guide = "colourbar", "DO (mg/L)") +
       theme_bw()

# EOF
