# ============================================================================ #
#         Modelling the Regulatory Resource Theory of Ego Depletion            #
#                    Improving Psychological Explanations                      #
# ============================================================================ #

# Code for paper "Improving Psychological Explanations"

## Code Contributions from:
# Jill de Ron
# Han van der Maas
# Adam Finneman
# Denny Borsboom
# Jonas Haslbeck

# ------------------------------------------------------
# --------- Load packages ------------------------------
# ------------------------------------------------------

# Running models
library(deSolve)
library(rootSolve)
library(FME)
library(Grind)

# Processing output
library(dplyr)

# Pretty plotting
library(RColorBrewer)


# ------------------------------------------------------
# --------- Run Models ---------------------------------
# ------------------------------------------------------
# Model code from  Jill de Ron & Han van der Maas

# Reproducibility
seed = 111

# List to collect model outputs
l_mo <- list()

# ===========================
#  MODEL 1: Simple depletion
# ===========================

# W  = Willpower (resource)
# d  = task demand

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dW = -d
    return(list(c(dW)))
  })
}

p <- c(d = 0.1) # parameter settings
s <- c(W = 1)   # initial values

# Run model 1:
data = run(tmax     = 50,
           table    = TRUE,
           timeplot = FALSE,
           after    = "state[1] = max(0, state[1])") # if W < 0 then W = 0

l_mo$Model1 <- data$W

# ===========================
#  MODEL 2: Gradual depletion
# ===========================

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dW = -d*W
    return(list(c(dW)))
  })
}

p <- c(d = 0.1) # parameter settings
s <- c(W = 1) # initial values

# Run model 2:
data = run(tmax     = 50,
           table    = TRUE,
           timeplot = FALSE)


l_mo$Model2 <- data$W



# ===========================
# Model 3: Recovery
# ===========================

# W  = Willpower (resource)
# Wr = Willpower resting state
# r  = recovery rate
# d  = task demand

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dW = r*(Wr-W)-d*W
    return(list(c(dW)))
  })
}

p <- c(r = .1, d = 0, Wr = 1) # parameter settings
s <- c(W = 1) # initial values

# -----------------------------------------------------------------------------
# Set up the sequential task paradigm, consisting of three phases
# Phase 1 (0-10 time steps) task demand is 0.05 for control and 0.2 for experimental
# Phase 2 (10-20 time steps) task demand is 0.4 for both groups
# Phase 3 (20-30 time steps) task demand is 0 for both groups
# -----------------------------------------------------------------------------

d_control_1 = .05 # phase 1 task demand control group
d_experimental_1 = .2 # phase 1 task demand experimental group
d_control_2 = d_experimental_2 = .4 # phase 2 task demand both groups

control = run(after = "if (t>0 & t<9) parms[2] <- d_control_1;
                       if (t>=9 & t<19) parms[2] <- d_control_2;
                       if (t>=19) parms[2] <- 0",
              table = TRUE, timeplot = FALSE, tmax = 30)

experimental = run(after = "if (t>0 & t<9) parms[2] <- d_experimental_1;
                            if (t>=9 & t<19) parms[2] <- d_experimental_2;
                            if (t>=19) parms[2] <- 0",
                   table = TRUE, timeplot = FALSE, tmax = 30)



# Compute the sequential task performance in phase 2:
N = 100 # 100 subjects per group

# Item Response Theory to couple will power to task performance
alpha = 2; beta = 0; theta_ce = control[10:19, 'W']
pr_ce = 1 / (1 + exp(-alpha * (theta_ce - beta)))
set.seed(seed)
x = matrix(runif(10*N), N, 10) < matrix(pr_ce, N, 10, byrow = TRUE)
scores_control = apply(x, 1, mean)

theta_ex = experimental[10:19, 'W']
pr_ex = 1 / (1 + exp(-alpha * (theta_ex - beta)))
set.seed(seed)
x = matrix(runif(10*N), N, 10) < matrix(pr_ex, N, 10, byrow = TRUE)
scores_experimental = apply(x, 1, mean)

# Run t-test:
tt = t.test(scores_control, scores_experimental)

l_mo$Model3 <- list("W_contr"= control$W,
                    "W_exp" = experimental$W,
                    "ttest" = tt,
                    "d_control_1" = d_control_1,
                    "d_experimental_1" = d_experimental_1,
                    "d_control_2" = d_control_2,
                    "d_experimental_2" = d_experimental_2)

l_mo$Model3$scores_control <- scores_control
l_mo$Model3$scores_experimental <- scores_experimental

# ===========================
# Robustness Analysis (Model 3)
# ===========================

# Alter the recovery rate and alpha (the mapping from the resource level to the task performance)
nIter = 100
rr = seq(0, 1, by = .05)
aa = seq(0, 5, by = 1)
data = data.frame(r = NA, a = NA, value = NA)
index = 0

for (i in 1:nIter) {
  for(j in 1:length(rr)) {
    for(k in 1:length(aa)) {

      r = rr[j]
      alpha = aa[k]
      index = index+1
      p <- c(r = r, d = 0, Wr = 1) # parameter settings
      s <- c(W = 1) # initial values

      d_control_1 = .05; d_experimental_1 = .2; d_control_2 = d_experimental_2 = .4
      control = run(after = "if (t>0 & t<9) parms[2] <- d_control_1;
                             if (t>=9 & t<19) parms[2] <- d_control_2;
                             if (t>=19) parms[2] <- 0",
                    table = TRUE, timeplot = FALSE, tmax = 30)

      experimental = run(after = "if (t>0 & t<9) parms[2] <- d_experimental_1;
                                  if (t>=9 & t<19) parms[2] <- d_experimental_2;
                                  if (t>=19) parms[2] <- 0",
                         table = TRUE, timeplot = FALSE, tmax = 30)

      beta=0; theta = control[10:19,'W']
      pr=1/(1+exp(-alpha*(theta-beta)))
      x=matrix(runif(10*N),N,10)<matrix(pr,N,10,byrow=TRUE)
      scores_control=apply(x,1,mean)

      theta=experimental[10:19,'W']
      pr=1/(1+exp(-alpha*(theta-beta)))
      x=matrix(runif(10*N),N,10)<matrix(pr,N,10,byrow=TRUE)
      scores_experimental=apply(x,1,mean)

      tt = t.test(scores_control, scores_experimental)
      data[index,] = c(r, alpha, tt$statistic)
    }
  }
  print(i)
}

data <- data %>% group_by(r, a) %>% summarize(mean = mean(value))
data <- as.matrix(data)

# saveRDS(data, "files/robustness_results.RDS")
data <- readRDS("files/robustness_results.RDS")
l_mo$Model3$robustness$data <- data

# ===========================
# APPENDIX Model 4: Training
# ===========================

# W  = Willpower (resource)
# Wr = Willpower resting state
# a  = decay resting state
# b  = built up rate
# r  = recovery rate
# d  = task demand

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dWr = b*(Wr-W) - a*Wr
    dW = r*(Wr-W) - d*W
    return(list(c(dWr,dW)))
  })
}

p <- c(r = 0.1, d = 0, a = 0.01, b = 0.02)
s <- c(Wr = 1, W = 1)

# Run model 4:
experimental = run(after = "parms[2] <-.5 *(t%/%10 %%2==0);
                            if (t>99) parms[2] <- 0",
                   tmax = 150, table = TRUE, timeplot = FALSE)

# Task demand
d = 1:200;
dd = 0.4 *(d%/%10 %%2==0);
dd[d>99]  <- 0

l_mo$Model4$dd <- dd
l_mo$Model4$W <- experimental


# ===========================
# APPENDIX Model 5: Hysteresis
# ===========================

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dW = r*(Wr-W)^3 + 2*W-d
    return(list(c(dW)))
  })
}

p <- c(r = 1, d = 0, Wr = 1)
s <- c(W = 2.73)

# Run model 5:
experimental = run(after = "parms[2] <- 5-abs(t-100)/20",
                   ymin = -2, tmax = 200, table = TRUE, timeplot = FALSE)
t = 0:200;
dd = 5-abs(t-100)/20

l_mo$Model5$dd <- dd
l_mo$Model5$W <- experimental



# ------------------------------------------------------
# --------- Plot Figures -------------------------------
# ------------------------------------------------------
# Figure Code Jonas Haslbeck

# -----------------------------------------------
# ----- Figure 3: Model 1 + 2 -------------------
# -----------------------------------------------

sc <- 0.8
pdf("figures/Figure3.pdf", height = 5*sc, width = 7*sc)

# Setup Layout
par(mar=c(4,4,2,1))
plot.new()
plot.window(xlim=c(1, 51), ylim=c(0,1))
axis(1)
axis(2, las=2)
title(xlab="Time", ylab="W")

# Data
lines(l_mo$Model1, lwd=2, lty=1)
lines(l_mo$Model2, lwd=2, lty=2)

# Legend
legend("topright", legend = c('W (Model 1)', 'W (Model 2)'), bty="n", lty=1:2)

dev.off()


# -----------------------------------------------
# ----- Figure 4: Model 3 -----------------------
# -----------------------------------------------

cols <- brewer.pal(3, "Set1")
lwd <- 2

sc <- 1
pdf("figures/Figure4.pdf", height = 4.5*sc, width = 7.5*sc)

# --- Setup Layout ---
layout(matrix(1:2, ncol=2), widths = c(2,1))


# --- Panel 1: Time Evolution ---
par(mar=c(4,4,2,1))
plot.new()
plot.window(xlim=c(1, 31), ylim=c(0,1.2))
axis(1)
axis(2, las=2, labels = seq(0, 1, length=6), at=seq(0, 1, length=6))
title(xlab="Time", ylab="W", line=2.5)
abline(v = c(10,20), lty = 3)
text(5, 1.15, "Phase 1", cex=0.9)
text(15, 1.15, "Phase 1", cex=0.9)
text(25, 1.14, "Recovery", cex=0.9)
## Task
lwd_t <- 2
# Control
c1 <- l_mo$Model3$d_control_1
c2 <- l_mo$Model3$d_control_2
segments(0, c1, 10, c1, lty=2, col=cols[1], lwd=lwd_t)
segments(10, c2, 20, c2, lty=2, col=cols[1], lwd=lwd_t)
segments(20, 0, 30, 0, lty=2, col=cols[1], lwd=lwd_t)
# Experimental
e1 <- l_mo$Model3$d_experimental_1
e2 <- l_mo$Model3$d_experimental_2
segments(0, e1, 10, e1, lty=4, col=cols[2], lwd=lwd_t)
segments(10, e2, 20, e2, lty=4, col=cols[2], lwd=lwd_t)
segments(20, 0, 30, 0, lty=4, col=cols[2], lwd=lwd_t)
# Data
lines(l_mo$Model3$W_contr, col=cols[1], lwd=2)
lines(l_mo$Model3$W_exp, col=cols[2], lwd=2)
# Legend
legend(20, 1.05,
       legend=c("W (contr)", "W (exp)", "Demand (contr)", "Demand (exp)"),
       lty=c(1,1,2,4), lwd=rep(2,4), col=rep(cols[1:2], 2),
       text.col=rep(cols[1:2], 2), bty="n", cex=0.77)


# --- Panel 2: Boxplot ---
boxplot(l_mo$Model3$scores_control, l_mo$Model3$scores_experimental, axes=FALSE,
        col="white", border = cols[1:2], ylim=c(0,1))
axis(2, las=2)
title(ylab="Proportion(correct)")

tt <- l_mo$Model3$ttest
sub1 = paste0('t = ', round(tt$statistic, 1), ' (df =', round(tt$parameter,0),')')
sub2 = paste0('p-value = ', round(tt$p.value,3))
text(1.5, .15, sub1, cex = .8)
text(1.5, .05, sub2, cex = .8)

dev.off()


# -----------------------------------------------
# ----- Figure 5: Robustness of Model 3 ---------
# -----------------------------------------------

data <- l_mo$Model3$robustness$data

cols_gr <- brewer.pal(8, "BuPu")[-(1:2)]

sc <- 0.8
pdf("figures/Figure5_Robustness.pdf", height = 5*sc, width = 7*sc)

# Setup Layout
par(mar=c(4,4,2,1))
plot.new()
plot.window(xlim=c(0, 1), ylim=c(0,5))
axis(1)
axis(2, las=2)
title(xlab="r", ylab="t-statistic", line=2.5)
# Legend
legend("topright", legend = 0:5, lwd=rep(2, 6),
       text.col=cols_gr, col=cols_gr, bty="n",
       title = expression(alpha),
       title.col="black")

# Data (copy code from Jill)
aa <- 0:5
for(a in aa) lines(data[data[,2]==a,1],data[data[,2]==a,3],
                   col=cols_gr[a+1], lwd=2)


dev.off()


# -----------------------------------------------
# ----- Figure 6: Model with Training (App) -----
# -----------------------------------------------

cols <- brewer.pal(3, "Set1")
lwd <- 2

sc <- 0.8
pdf("figures/Figure6_Model4.pdf", height = 5*sc, width = 7*sc)

# Setup Layout
par(mar=c(4,4,2,1))
plot.new()
plot.window(xlim=c(1, 151), ylim=c(0,1.5))
axis(1)
axis(2, las=2)
title(xlab="Time", ylab="W", line=2.5)

# Data
lines(l_mo$Model4$W[,3], col=cols[1], lwd=2)
lines(l_mo$Model4$W[,2], col=cols[2], lwd=2)

# Task demand
d = 1:200; dd = 0.4 *(d%/%10 %%2==0); dd[d>99]  <- 0
lines(dd)

# Legend
legend(95, .5, legend = c('Will power: W',
                             expression('Rest state W: W'^'r'),
                             'Task demand: D'),
       col = c(cols[1:2], "black"), lty = 1, bty = 'n', lwd=rep(2, 3),
       cex=0.95, text.col=c(cols[1:2], "black"))


dev.off()


# -----------------------------------------------
# ----- Figure 7: Model with Hysteresis (App) ---
# -----------------------------------------------

sc <- 0.8
pdf("figures/Figure7_Model5.pdf", height = 5*sc, width = 7*sc)

# Setup Layout
par(mar=c(4,4,2,1))
plot.new()
plot.window(xlim=c(0, 5), ylim=c(0,4))
axis(1)
axis(2, las=2)
title(xlab="Time", ylab="W", line=2.5)

# Data
lines(l_mo$Model5$dd, 1 + l_mo$Model5$W[,2], lwd=2)

# Text
text(2, 3, "Depletion (increasing demand", cex=0.7)
text(2, 1, "Recovery (decreasing demand)", cex=0.7)
arrows(1.5, 2.7, 2.5, 2.7, le = 0.1)
arrows(2.5, 1.3, 1.5, 1.3, le = 0.1)

dev.off()

