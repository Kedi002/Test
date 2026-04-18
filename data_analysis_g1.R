library(emmeans)
library(afex)
library(ez)
library(ggplot2)
library(broom)

data_analysis_g1 <- function () {
  
  m <- {};
  d <- {};
  # Read data for each participant
  for (i in 1:12) {
    d <- rbind(d,read.delim(sprintf("./data/heart_rate_data_%d.txt",i), comment.char="#", stringsAsFactors=TRUE));
  }
  
  d$Biofeedback.Presentation <- as.factor(d$Biofeedback.Presentation);
  d$Game.Mode <- as.factor(d$Game.Mode);
  
  interaction.plot(d$Biofeedback.Presentation,d$Game.Mode,d$Game.Score);
  interaction.plot(d$Biofeedback.Presentation,d$Game.Mode,d$HRV.Stability);
  interaction.plot(d$Biofeedback.Presentation,d$Game.Mode,d$Board.Clears);
  
  # Plot using ggplot2 
  #      geom_bar(stat = "identity", color="black",position= position_dodge()) + 
  #      geom_errorbar(aes(ymin=mvs$Game.Score-svs$Game.Score, ymax=mvs$Game.Score+svs$Game.Score), width=.2, position=position_dodge(.9)) + 
  # mvs <- aggregate(Game.Score~Biofeedback.Presentation*Game.Mode,d,mean); 
  # svs <- aggregate(Game.Score~Biofeedback.Presentation*Game.Mode,d,sd);
  p1  <- ggplot(d, aes(x=Biofeedback.Presentation, y=Game.Score, fill=Game.Mode)) +
    geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE) +
    theme_classic(); 
  labs(title="Game.Score for Presentation and Adaptive Mode", x="Presentation", y = "Score");
  print(p1)
  
  
  # Plot using ggplot2 
  # mvhrv <- aggregate(HRV.Stability~Biofeedback.Presentation*Game.Mode,d,mean); 
  # svhrv <- aggregate(HRV.Stability~Biofeedback.Presentation*Game.Mode,d,sd);
  p2 <- ggplot(d, aes(x=Biofeedback.Presentation, y=HRV.Stability, fill=Game.Mode)) +
    #  geom_bar(stat = "identity", color="black",position= position_dodge()) + 
    #  geom_errorbar(aes(ymin=mvhrv$HRV.Stability-svhrv$HRV.Stability, ymax=mvhrv$HRV.Stability+svhrv$HRV.Stability), width=.2, position=position_dodge(.9)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    theme_classic(); 
  labs(title="HRV.Stability for Presentation and Adaptive Mode", x="Presentation", y = "HRV") 
  print(p2)
  
  
  # Plot using ggplot2 
  # mvbc <- aggregate(Board.Clears~Biofeedback.Presentation*Game.Mode,d,mean); 
  # svbc <- aggregate(Board.Clears~Biofeedback.Presentation*Game.Mode,d,sd);
  p3 <- ggplot(d, aes(x=Biofeedback.Presentation, y=Board.Clears, fill=Game.Mode)) +
    #    geom_bar(stat = "identity", color="black",position= position_dodge()) + 
    #    geom_errorbar(aes(ymin=mvbc$Board.Clears-svbc$Board.Clears, ymax=mvbc$Board.Clears+svbc$Board.Clears), width=.2, position=position_dodge(.9)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    theme_classic(); 
  labs(title="HRV.Stability for Presentation and Adaptive Mode", x="Presentation", y = "Board.Clears");
  print(p3)
  
  
  bpl <- levels(d$Biofeedback.Presentation);
  gml <- levels(d$Game.Mode);
  
  m$sh_sc <- array(dim=c(3,2));
  m$sh_hrv <- array(dim=c(3,2));
  m$sh_bc <- array(dim=c(3,2));
  # tests from normality 
  for (i in 1:length(bpl)) {
    for (j in 1:length(gml)) {
      m$sh_sc[i,j] <- tidy(shapiro.test(d[d$Biofeedback.Presentation==bpl[i] & d$Game.Mode==gml[j],]$Game.Score))$p.value;
      m$sh_hrv[i,j] <- tidy(shapiro.test(d[d$Biofeedback.Presentation==bpl[i] & d$Game.Mode==gml[j],]$HRV.Stability))$p.value;
      m$sh_bc[i,j] <- tidy(shapiro.test(d[d$Biofeedback.Presentation==bpl[i] & d$Game.Mode==gml[j],]$Board.Clears))$p.value;
    }
  }
  
  m$gs$anova <- ezANOVA(data=d, dv=Game.Score, within=.(Biofeedback.Presentation,Game.Mode), wid=.(Participant.ID), type=3, return_aov = TRUE);
  m$gs$pp <- pairwise.t.test(d$Game.Score,d$Game.Mode,paired=TRUE); # pairwise comparisons
  m$gs$mm <- aggregate(Game.Score~Game.Mode,d,mean); # mean values 
  
  m$hrv$anova <- ezANOVA(data=d, dv=HRV.Stability, within=.(Biofeedback.Presentation,Game.Mode), wid=.(Participant.ID), type=3, return_aov = TRUE);
  m$hrv$pp <- pairwise.t.test(d$HRV.Stability,d$Biofeedback.Presentation,paired=TRUE); # pairwise comparisons
  m$hrv$mm <- aggregate(HRV.Stability~Biofeedback.Presentation,d,mean); # mean values 
  
  m$bc$anova <- ezANOVA(data=d, dv=Board.Clears, within=.(Biofeedback.Presentation,Game.Mode), wid=.(Participant.ID), type=3, return_aov = TRUE);
  m$bc$pp <- pairwise.t.test(d$Board.Clears,d$Game.Mode,paired=TRUE); # pairwise comparisons
  m$bc$mm <- aggregate(Board.Clears~Game.Mode,d,mean); # mean values 
  
  
  do <-  read.table("./data/ixd-exp-qn-opinions.txt", header = TRUE);
  pQ1 <- ggplot(as.data.frame(table(do$Q1)),aes(y=Freq,x=Var1)) + geom_bar(stat = "identity");
  print(pQ1);
  pQ2 <- ggplot(as.data.frame(table(do$Q2)),aes(y=Freq,x=Var1)) + geom_bar(stat = "identity");
  print(pQ2);
  pQ3 <- ggplot(as.data.frame(table(do$Q3)),aes(y=Freq,x=Var1)) + geom_bar(stat = "identity");
  print(pQ3);
  pQ4 <- ggplot(as.data.frame(table(do$Q4)),aes(y=Freq,x=Var1)) + geom_bar(stat = "identity");
  print(pQ4);
  pQ5 <- ggplot(as.data.frame(table(do$Q5)),aes(y=Freq,x=Var1)) + geom_bar(stat = "identity");
  print(pQ5);
  pQ6 <- ggplot(as.data.frame(table(do$Q6)),aes(y=Freq,x=Var1)) + geom_bar(stat = "identity");
  print(pQ6);
  
  qn <- read.table("./data/ixd-exp-qn-tasks.txt", header = TRUE); 
  p4 <- ggplot(qn, aes(x=Biofeedback.Mode, y=Qn.Stress, fill=Adaptive)) +
    geom_boxplot(outlier.colour="red", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    theme_classic(); 
  labs(title="HRV.Stability for Presentation and Adaptive Mode", x="Presentation", y = "Board.Clears");
  print(p4)
  
  p5 <- ggplot(qn, aes(x=Biofeedback.Mode, y=Qn.Ability, fill=Adaptive)) +
    geom_boxplot(outlier.colour="red", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    theme_classic(); 
  labs(title="HRV.Stability for Presentation and Adaptive Mode", x="Presentation", y = "Board.Clears");
  print(p5)
  
  # Do ANOVAs
  m$qns$anova <- ezANOVA(data=qn, dv=Qn.Stress, within=.(Biofeedback.Mode,Adaptive), wid=.(Participant), type=3, return_aov = TRUE);
  m$qna$anova <- ezANOVA(data=qn, dv=Qn.Ability, within=.(Biofeedback.Mode,Adaptive), wid=.(Participant), type=3, return_aov = TRUE);
  
  
  m$d <- d;
  
  #  
  #   # Fit the anova model, doing the Analysis of Variance
  #   m$vast$anova <- ezANOVA(data=dd, dv=VAST, within=.(R), wid=.(P), type=3, return_aov = TRUE);
  #   m$vasb$anova <- ezANOVA(data=dd, dv=VASB, within=.(R), wid=.(P), type=3, return_aov = TRUE);
  #   m$hrvt$anova <- ezANOVA(data=dd, dv=HRVT, within=.(R), wid=.(P), type=3, return_aov = TRUE);
  #   m$hrvb$anova <- ezANOVA(data=dd, dv=HRVB, within=.(R), wid=.(P), type=3, return_aov = TRUE);
  #   m$hr$anova   <- ezANOVA(data=dd, dv=HR, within=.(R), wid=.(P), type=3, return_aov = TRUE);
  # 
  # # Perform pairwise comparisons 
  # m$p$vast <- pairwise.t.test(dd$VAST,dd$R,paired=TRUE);
  # m$p$vasb <- pairwise.t.test(dd$VASB,dd$R,paired=TRUE);
  # m$p$hrvt <- pairwise.t.test(dd$HRVT,dd$R,paired=TRUE);
  # m$p$hrvb <- pairwise.t.test(dd$HRVB,dd$R,paired=TRUE);
  # m$p$hr <- pairwise.t.test(dd$HR,dd$R,paired=TRUE);
  
  return(m);
  
}