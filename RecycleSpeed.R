#--------------------------------------------------------------------------------------------------------------------
# HARLEQUINS RECYCLE SPEED ANALYSIS
#--------------------------------------------------------------------------------------------------------------------
# JAVA HEAP SIZE
options(java.parameters = "-Xmx5g")
#--------------------------------------------------------------------------------------------------------------------
# LIBRARIES
library(plotly)
library(bartMachine)
#--------------------------------------------------------------------------------------------------------------------
# DATA
# Use the QuinsRecycleSpeed.csv file: https://github.com/jmwallusch/QuinsRecycleSpeed/blob/main/QuinsRecycleSpeed.csv
#--------------------------------------------------------------------------------------------------------------------
# DUMMIES AND CATEGORICAL FEATURES
# Counterrucking 
CntR <- ifelse(RSA$Contested == "none", 0, 1)
# Pitch zone
zone <- ifelse(RSA$Zone == "Own22", 1, 
               ifelse(RSA$Zone == "Own22-10", 2, 
                      ifelse(RSA$Zone == "Own10-50", 3,
                             ifelse(RSA$Zone == "Opp50-10", 4, 
                                    ifelse(RSA$Zone == "Opp10-22", 5, 6)))))
# Clock
ClTm <- as.numeric(paste(RSA$Min,".",RSA$Sec, sep = ""))
#--------------------------------------------------------------------------------------------------------------------
# DATA FOR BART MODEL
BART_DAT <- data.frame(Time = ClTm,              # clock time
                       Zone = zone,              # pitch zone
                       Prot = RSA$Protection,    # attacking player committed 
                       CNTR = CntR,              # counterrucking (yes = 1, no = 0)
                       PASS = RSA$Pass,          # number of passes before ruck
                       OUTC = RSA$Outcome,       # ruck's outcome (e.g. ground pass, pick-and-go, jackal)
                       SHn9 = RSA$SF,            # player 
                       RecS = RSA$Speed) %>%     # recycle speed
dplyr::filter(complete.cases(.))
#--------------------------------------------------------------------------------------------------------------------
# PLOT RECYCLE SPEED DISTRIBUTION
med_no <- which.min(abs(density(BART_DAT$RecS)$x - median(BART_DAT$RecS)))

plot_ly(x = density(BART_DAT$RecS)$x,
        y = density(BART_DAT$RecS)$y,
        type = "scatter", mode = "lines", 
        line = list(color = "black", width = 2.2),
        fill = 'tonexty', fillcolor = "rgba(0,100,80,0.2)") %>%
  add_trace(x = median(BART_DAT$RecS),
            y = seq(0, density(BART_DAT$RecS)$y[med_no], length.out = 222),
            type = "scatter", mode = "lines", inherit = FALSE,
            line = list(color = "black", dash = "dash", width = 0.75)) %>% 
  add_trace(x = seq(0, density(BART_DAT$RecS)$x[med_no], length.out = 222),
            y = density(BART_DAT$RecS)$y[med_no],
            type = "scatter", mode = "lines", inherit = FALSE,
            line = list(color = "black", dash = "dash", width = 0.75)) %>%
  layout(title = "<b>Harlequins 2021<br>Recycle Speed Distribution",
         xaxis = list(title = "Recycle Speed in Seconds",
                      ticks = "outside", tickcolor = "rgb(255,255,255)",
                      gridcolor = "rgb(255,255,255)"),
         yaxis = list(title = "Kernel Density",
                      ticks = "outside", tickcolor = "rgb(255,255,255)",
                      gridcolor = "rgb(255,255,255)"),
         paper_bgcolor = "rgb(222,222,222)", 
         plot_bgcolor = "rgb(229,229,229)",
         showlegend = FALSE,
         margin = list(b = 150, t = 150, l = 150, r = 150, pad = 1)) %>% 
  add_annotations(text = paste("Median Recycle Speed:<br>", round(median(BART_DAT$RecS),2), "sec"),
                  xref= "x", yref = "y", 
                  x = median(BART_DAT$RecS) + 3, 
                  y = density(BART_DAT$RecS)$y[med_no],
                  showarrow = FALSE)
#--------------------------------------------------------------------------------------------------------------------
# BART: CROSS-VALIDATION
# See CHIPMAN, GEORGE AND MCCULLOCH (2010) for more details
QRS_bMCV <- bartMachineCV(X = BART_DAT[(R_2$Outcome == "ground pass"),][,1:5],
                          y = BART_DAT$RecS[(R_2$Outcome == "ground pass")],
                          k_cvs = c(2,5)                                         # k - shrinkage parameter: shrinks the tree parameters towards 0, keeps the individual tree components small, hence limiting their effects
                          num_tree_cvs = c(25, 50, 150),                         # number of trees
                          s_sq_y = "mse") 
# Inspect the results
QRS_bMCV$cv_stats
#--------------------------------------------------------------------------------------------------------------------
# BART: ESTIMATIONS
QRS_bMod <- bartMachine(
  X = BART_DAT[(BART_DAT$OUTC == "ground pass"),][,1:5],
  y = BART_DAT$RecS[(BART_DAT$OUTC == "ground pass")],
  alpha = 0.95,                                                                  # alpha(1 + d)^beta => probability that a node at depth d = 0, 1, ... is non-terminal
  beta = 2,
  k = 2, 
  nu = 3,                                                                        # degrees of freedom parameter for the residual variance prior (nu < 3 leads to overfitting, see CHIPMAN, GEORGE AND MCCULLOCH (2010))
  q = 0.9,
  num_trees = 25,
  num_iterations = 20000,
  num_burn_in = 10000
)
#--------------------------------------------------------------------------------------------------------------------
# BART: 'DIAGNOSTICS'
summary(QRS_bMod)
QRS_bMod$PseudoRsq                                                               # pseudo-R-square
QRS_bMod$L1_err_train                                                            # sum of absolute residuals
QRS_bMod$L2_err_train                                                            # sum of squared residuals
# Observed vs. fitted values
plot_lyx = BART_DAT$RecS[(BART_DAT$OUTC == "ground pass")],
        y = QRS_bMod$y_hat_train,
        type = "scatter", mode = "markers",
        marker = list(color = "rgba(0,100,80,0.2)",
                      line = list(color = "black", width = 0.5))) %>%
  layout(title = "<b>Actual vs. Predicted",
         xaxis = list(title = "Actual",
                      ticks = "outside", tickcolor = "rgb(255,255,255)",
                      gridcolor = "rgb(255,255,255)"),
         yaxis = list(title = "Predicted",
                      ticks = "outside", tickcolor = "rgb(255,255,255)",
                      gridcolor = "rgb(255,255,255)"),
         paper_bgcolor = "rgb(222,222,222)", 
         plot_bgcolor = "rgb(229,229,229)",
         showlegend = FALSE,
         margin = list(b = 150, t = 150, l = 150, r = 150, pad = 1)) 
