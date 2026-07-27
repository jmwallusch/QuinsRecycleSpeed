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
med_no <- which.min(abs(density(R_1$RecS)$x - median(R_1$RecS)))

plot_ly(x = density(R_1$RecS)$x,
        y = density(R_1$RecS)$y,
        type = "scatter", mode = "lines", 
        line = list(color = "black", width = 2.2),
        fill = 'tonexty', fillcolor="rgba(0,100,80,0.2)") %>%
  add_trace(x = median(R_1$RecS),
            y = seq(0, density(R_1$RecS)$y[med_no], length.out = 222),
            type = "scatter", mode = "lines", inherit = FALSE,
            line = list(color = "black", dash = "dash", width = 0.75)) %>% 
  add_trace(x = seq(0, density(R_1$RecS)$x[med_no], length.out = 222),
            y = density(R_1$RecS)$y[med_no],
            type = "scatter", mode = "lines", inherit = FALSE,
            line = list(color = "black", dash = "dash", width = 0.75)) %>%
  layout(title = paste("<b>",R_T, ": <br>Recycle Speed Distribution", sep = ""),
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
  add_annotations(text = paste("Median Recycle Speed:<br>", round(median(R_1$RecS),2), "sec"),
                  xref= "x", yref = "y", 
                  x = median(R_1$RecS) + 3, 
                  y = density(R_1$RecS)$y[med_no],
                  showarrow = FALSE)
#--------------------------------------------------------------------------------------------------------------------
# BART: CROSS-VALIDATION
# See CHIPMAN, GEORGE AND MCCULLOCH (2010) for more details
QRS_feat <- R_1$RecS[(R_1$OUTC == "ground pass")]
RSA_spee <- bartMachineCV(X = R_2[(R_2$Outcome == "ground pass"),][,1:5],
                          y = R_2$Speed[(R_2$Outcome == "ground pass")],
                          k_cvs = c(2,5)                                         # k - shrinkage parameter: shrinks the tree parameters toward 0, keeps the individual tree components small, hence limiting their effects
                          num_tree_cvs = c(),
                          s_sq_y = "mse") 
