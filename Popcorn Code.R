#########################################
#########STAT 230 Term Project###########
#########################################
########Seth Hyatt Winter 2019###########
#########################################


    ##How Many Observations Do I Need?##
  #Assuming within.var = 1.6 from Tolley Example#

  #18 groups - Difference in Age
power.anova.test(groups = 18, between.var = var(c(0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1)), 
                 within.var = 1.6, sig.level = .05, power = .8)$n

  #18 groups - Difference in Butter
power.anova.test(groups = 18, between.var = var(c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0)), 
                 within.var = 1.6, sig.level = .05, power = .8)$n

  #18 groups - Difference in Salt
power.anova.test(groups = 18, between.var = var(c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0)), 
                 within.var = 1.6, sig.level = .05, power = .8)$n

  #6 groups - Difference in Age or Salt
power.anova.test(groups = 6, between.var = var(c(0,0,0,0,1,1)), 
                 within.var = 1.6, sig.level = .05, power = .8)$n

  #6 groups - Difference in Butter
power.anova.test(groups = 6, between.var = var(c(0,0,0,1,1,1)), 
                 within.var = 1.6, sig.level = .05, power = .8)$n


      ##Manually Enter Data##

Taste_Scores <- c(5,5,2,6,3,5,5,4,8,5,7,3,5,9,4,3,6,9,6,2,7,2,8,4,6,8,6,9,4,
              5,7,8,10,4,8,5,5,9,8,7,7,6,7,5,8,4,7,10,10,8,10,7,7,6,3,4,10,6,
              5,7,6,8,9,8,6,6,8,6,10,8,6,6,5,6,4,6,3,4,6,6,6,7,1,10,5,2,4,1,9,6,6,5,2,
              1,4,7,7,8,3,6,3,6,2,6,4,3,3,8,3,6,7,3,6,8,6,5,3,
              7,3,4,5,2,6,5,6,4,4,9,8,4,9,6,4,5,7,6,2,7,2,6,5,2,7,
              7,8,3,6,4,4,4,8,9,6,8,6,6,8,3,7,3,5,5,7,3,6,10,3,9,6,1,7,10,5,9,7,7)

Salt_Levels <- c("Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low",
                 "Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal",
                 "High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High",
                 "Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low","Low",
                 "Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal","Normal",
                 "High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High","High")

Butter_Type <- c("Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's",
                 "Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's",
                 "Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's","Odell's",
                 "Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface",
                 "Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface",
                 "Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface","Butterface")

Age_Group <- c("18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","<18","<18","<18","<18","<18","50< = ","50< = ","18< =  <50","18< =  <50","18< =  <50","18< =  <50","50< = ","<18","18< =  <50","18< =  <50","18< =  <50","50< = ","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","50< = ","18< =  <50",
               "18< =  <50","<18","<18","50< = ","18< =  <50","18< =  <50","<18","18< =  <50","<18","18< =  <50","18< =  <50","50< = ","<18","18< =  <50","18< =  <50","18< =  <50","50< = ","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","50< = ","18< =  <50","18< =  <50",
               "50< = ","50< = ","18< =  <50","18< =  <50","<18","18< =  <50","<18","18< =  <50","<18","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","<18","<18","50< = ","18< =  <50","18< =  <50","18< =  <50","18< =  <50","<18","50< = ","50< = ","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","<18","18< =  <50","18< =  <50","50< = ","18< =  <50",
               "<18","18< =  <50","<18","50< = ","18< =  <50","<18","18< =  <50","18< =  <50","50< = ","<18","50< = ","<18","18< =  <50","18< =  <50","<18","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","50< = ",
               "<18","18< =  <50","<18","18< =  <50","18< =  <50","18< =  <50","<18","<18","18< =  <50","18< =  <50","50< = ","<18","18< =  <50","18< =  <50","18< =  <50","18< =  <50","50< = ","18< =  <50","50< = ","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50",
               "50< = ","<18","18< =  <50","<18","18< =  <50","18< =  <50","18< =  <50","18< =  <50","<18","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","18< =  <50","50< = ","<18","18< =  <50","18< =  <50","50< = ","18< =  <50","50< = ","<18","18< =  <50","50< = ","50< = ","18< =  <50","50< = ","50< = ","18< =  <50","18< =  <50","18< =  <50","<18")


  #Checking normality of Taste Scores#

hist(Taste_Scores, main = "Overall Taste Scores", xlab = "Taste Score")


    ##Making them factors##

Salt_Levels <- as.factor(Salt_Levels)
Salt_Levels <- factor(Salt_Levels, levels = list("Low", "Normal", "High"))
Butter_Type <- as.factor(Butter_Type)
Age_Group <- as.factor(Age_Group)
Age_Group <- factor(Age_Group, levels = list("<18", "18< =  <50", "50< = "))

    ##Checking Conditions##

tapply(Taste_Scores, Salt_Levels:Butter_Type:Age_Group, length)

boxplot(Taste_Scores ~ Salt_Levels:Butter_Type:Age_Group, main = "18 Group Comparsion",
        xlab = "Group", ylab = "Taste Score", col = 2:7)

max(tapply(Taste_Scores, Salt_Levels:Butter_Type:Age_Group, sd))/
  min(tapply(Taste_Scores, Salt_Levels:Butter_Type:Age_Group, sd)) #2.046311


  
    ##Interaction Plots##
  
  #Highly Significant
interaction.plot(Salt_Levels, Butter_Type, Taste_Scores, ylab = "Mean Taste Score",
                 col = 1:2,legend = FALSE, main = "Salt:Butter Interaction")
  legend("topleft", legend = c("Odell's", "Butterface"), col = c(2,1), lty = 1:2, bty = "n",
         title = "Butter")  


  #Significant
interaction.plot(Age_Group, Butter_Type, Taste_Scores, ylab = "Mean Taste Score", 
                 legend = FALSE, col = 1:2, main = "Butter:Age Interaction")
  legend("bottomright", legend = c("Odell's", "Butterface"), col = 2:1, lty = 1:2, bty = "n",
         title = "Butter", title.adj = .275)  

  
interaction.plot(Butter_Type, Age_Group, Taste_Scores, ylab = "Mean Taste Score")
interaction.plot(Butter_Type, Salt_Levels, Taste_Scores, ylab = "Mean Taste Score")
interaction.plot(Salt_Levels, Age_Group, Taste_Scores, ylab = "Mean Taste Score")
interaction.plot(Age_Group, Salt_Levels, Taste_Scores, ylab = "Mean Taste Score")

  
    ##Building and Testing Comparable ANOVA Models##

model1 <- lm(Taste_Scores ~ Salt_Levels + Butter_Type + Age_Group + Salt_Levels:Butter_Type +
                     Butter_Type:Age_Group + Salt_Levels:Age_Group + Salt_Levels:Butter_Type:Age_Group)
anova(model1)

model2 <- lm(Taste_Scores ~ Salt_Levels + Age_Group + Butter_Type + Salt_Levels:Butter_Type +
               Salt_Levels:Age_Group + Butter_Type:Age_Group + Salt_Levels:Butter_Type:Age_Group)
anova(model2)

model3 <- lm(Taste_Scores ~ Butter_Type + Salt_Levels + Age_Group + Butter_Type:Age_Group +
               Salt_Levels:Butter_Type + Salt_Levels:Age_Group + Salt_Levels:Butter_Type:Age_Group)
anova(model3)  #yields sig. for Butter:Age 0.04567

model4 <- lm(Taste_Scores ~ Butter_Type + Age_Group + Salt_Levels + Butter_Type:Age_Group +
               Salt_Levels:Age_Group + Salt_Levels:Butter_Type + Salt_Levels:Butter_Type:Age_Group)
anova(model4)

model5 <- lm(Taste_Scores ~ Age_Group + Butter_Type + Salt_Levels + Salt_Levels:Age_Group + 
               Salt_Levels:Butter_Type + Butter_Type:Age_Group + Salt_Levels:Butter_Type:Age_Group)
anova(model5) #yields sig. for Salt, Salt:Butter

model6 <- lm(Taste_Scores ~ Age_Group + Salt_Levels + Butter_Type + Salt_Levels:Age_Group +
               Butter_Type:Age_Group + Salt_Levels:Butter_Type + Salt_Levels:Butter_Type:Age_Group)
anova(model6)


    ##Model 4 & 5 offered the most significance for Salt, Salt:Butter, Butter;Age##
   
hist(model5$residuals, main = "Distribution of Overall Residuals", xlab = "Residuals")
  
MSq <- 4.5345
DegF <- 158


    ##Removing non-significant interactions##

Model_Rev <- lm(Taste_Scores ~ Age_Group + Butter_Type + Salt_Levels +
                  Salt_Levels:Butter_Type +Butter_Type:Age_Group)

anova(Model_Rev) #Not much changes from Model5


    ##Testing Actual Power for the 18 groups##

Complex_Powers <- power.anova.test(groups = 18,
                 between.var = var(tapply(Taste_Scores, Salt_Levels:Butter_Type:Age_Group, mean)), 
                 within.var = 4.5345,
                 sig.level = .05,
                 n = tapply(Taste_Scores, Salt_Levels:Butter_Type:Age_Group, length))$power

min(Complex_Powers) #refers to NBF:50< =  ; only 3 observations

  #Too low of Powers to conclude anything significant#


    ##Simplifying to Basic Models##

Basic_Model1 <- lm(Taste_Scores ~ Salt_Levels + Butter_Type + Salt_Levels:Butter_Type)
anova(Basic_Model1)

Basic_Model2 <- lm(Taste_Scores ~ Butter_Type + Salt_Levels + Salt_Levels:Butter_Type)
anova(Basic_Model2)

Basic_Model3 <- lm(Taste_Scores ~ Salt_Levels + Age_Group + Salt_Levels:Age_Group)
anova(Basic_Model3)

Basic_Model4 <- lm(Taste_Scores ~ Age_Group + Salt_Levels + Salt_Levels:Age_Group)
anova(Basic_Model4)

Basic_Model5 <- lm(Taste_Scores ~ Age_Group + Butter_Type + Butter_Type:Age_Group)
anova(Basic_Model5)

Basic_Model6 <- lm(Taste_Scores ~ Butter_Type + Age_Group + Butter_Type:Age_Group)
anova(Basic_Model6)


  #Powers

Salt_Power <- power.anova.test(groups = 3, between.var = (var(tapply(Taste_Scores, Salt_Levels, mean))),
                within.var = 4.5345, sig.level = .05, 
                n = tapply(Taste_Scores, Salt_Levels, length))$power
min(Salt_Power) #refers to Low; 53 obs
  #Yields too low of powers to mean any real significance in upcoming tests  


Salt_Butter_Power <- power.anova.test(groups = 6,
                      between.var  = var(tapply(Taste_Scores, Salt_Levels:Butter_Type, mean)), 
                      within.var = 4.6355, sig.level = .05, 
                      n = tapply(Taste_Scores, Salt_Levels:Butter_Type, length))$power

min(Salt_Butter_Power) #refers to LBF; 24 observations
  #Power is 80.87636%  = > tests are worth the time


Salt_Age_Power <- power.anova.test(groups = 6,
                    between.var  = var(tapply(Taste_Scores, Salt_Levels:Age_Group, mean)), 
                    within.var = 4.7912, sig.level = .05, 
                    n = tapply(Taste_Scores, Salt_Levels:Age_Group, length))$power

min(Salt_Age_Power) #refers to Normal:50< =  7 obs
  #Power is 22%  = > Too low of powers for tests


Butter_Age_Power <- power.anova.test(groups = 6,
                     between.var  = var(tapply(Taste_Scores, Age_Group:Butter_Type, mean)), 
                     within.var = 4.6995, sig.level = .05, 
                     n = tapply(Taste_Scores, Age_Group:Butter_Type, length))$power

min(Butter_Age_Power) #refers to Odell's:50< = ; 15 obs
  #Power is 47%  = > Too low of powers for tests


    ##Checking Salt_Butter Model Conditions##

hist(Basic_Model1$residuals, main = "Distribution of Basic Model Residuals",
     xlab = "Residuals")

max(tapply(Taste_Scores, Salt_Levels:Butter_Type, sd))/
  min(tapply(Taste_Scores, Salt_Levels:Butter_Type, sd)) #1.119254


      ##t-tests##

    #Salt#

tapply(Taste_Scores, Salt_Levels, mean)
tapply(Taste_Scores, Salt_Levels, sd)
tapply(Taste_Scores, Salt_Levels, length)

  #Low less than Normal
pt((5.169811-6.109091)/sqrt(((1/53)+(1/55))*MSq), DegF)
    #p = 0.0116249  = > stat. sig. at 0.05 / 95%
  (5.169811-6.109091) + c(1,-1) * 1.984 * sqrt(((1/53)+(1/55))*MSq)
    #95% conf int  = > (-0.126077 -1.752483)

  #Normal greater than High
pt((6.109091-5.926471)/sqrt(((1/55)+(1/68))*MSq), DegF, lower.tail = FALSE)
  #p = 0.3184695  = > insuf

  #Low less than High
pt((5.169811-5.926471)/sqrt(((1/53)+(1/68))*MSq), DegF)
    #p = 0.02712571  = > stat. sig. at 0.05 / 95%
  (5.169811-5.926471) + c(1,-1) * 1.984 * sqrt(((1/53)+(1/68))*MSq)
    #95% conf int  = > (0.01745711 -1.53077711)


interaction.plot(Salt_Levels, Butter_Type, Taste_Scores, ylab = "Mean Taste Score",
                 col = c("white", "white"),legend = FALSE, main = "Mean Taste Scores for Salt Levels",lty = c(1,1,2))
  abline(points(1,y = 5.147989, pch = 16, col = "blue"), points(2,y = 6.061671, pch = 16, col = "blue"), 
       points(3,y = 5.930303, pch = 16, col = "blue"))
  segments(1,5.147989,2,6.061671,col = "blue", lty = 2)
  segments(2,6.061671,3,5.930303,col = "blue", lty = 2)
  legend("topleft", legend = c("Mean"), col = "blue", lty = 2, bty = "n")  

  
    #Salt:Butter#
  
tapply(Taste_Scores, Salt_Levels:Butter_Type, mean)
tapply(Taste_Scores, Salt_Levels:Butter_Type, sd)
tapply(Taste_Scores, Salt_Levels:Butter_Type, length)

  #LBF less than LO
pt((4.916667-5.379310)/sqrt(((1/24)+(1/29))*MSq), DegF)
  #p = 0.2161388  = > insuf.

  #NBF less than NO
pt((5.192308 - 6.931034)/sqrt(((1/26)+(1/29))*MSq), DegF)
    #p = 0.001459537  = > stat. sig. at 0.005 / 99.5%
  (5.192308 - 6.931034) + c(1,-1) * 1.984 * sqrt(((1/26)+(1/29))*MSq)
    #95% conf int  = > (-0.5976829 -2.8797691)

  #HBF greater than HO
pt((6.060606 - 5.8)/sqrt(((1/33)+(1/35))*MSq), DegF, lower.tail = FALSE)
  #p = 0.3073492  = > insuf.


interaction.plot(Salt_Levels, Butter_Type, Taste_Scores, ylab = "Mean Taste Score",
                 col = 1:2,legend = FALSE, main = "Salt:Butter Interaction", lty = c(1,1,2))
  segments(1,4.91667,1,5.379310,col = "blue", lty = 2)
  segments(2,5.192308,2,6.931034,col = "blue", lty = 2)
  segments(3,6.060606,3,5.8,col = "blue", lty = 2)
  legend("topleft", legend = c("Odell's", "Butterface", "Diff"), col = c(2,1,"blue"),
         lty = c(1,1,2), bty = "n", title = "Butter")  


  ##Would test Butter itself, as it is nearing significance, but no time.##


    #Butter:Age#

tapply(Taste_Scores, Butter_Type:Age_Group, mean)
tapply(Taste_Scores, Butter_Type:Age_Group, sd)
tapply(Taste_Scores, Butter_Type:Age_Group, length)

  #BF:<18 lesser than O:<18
pt((4.764706 - 6.611111)/sqrt(((1/17)+(1/18))*MSq), DegF)
    #p = 0.005642478  = > stat. sig. at 0.01 / 99%
  (4.764706 - 6.611111) + c(1,-1) * 1.984 * sqrt(((1/17)+(1/18))*MSq)
    #95% conf int  = > (-0.4175779 -3.2752321)

  #BF:18< =  <50 lesser than O:18< =  <50
pt((4.980392 - 5.9666667)/sqrt(((1/51)+(1/60))*MSq), DegF)
    #p = 0.008070258  = > stat. sig. at 0.01 / 99% 
  (4.980392 - 5.9666667) + c(1,-1) * 1.984 * sqrt(((1/51)+(1/60))*MSq)
    #95% conf int  = > (-0.1816245 -1.7909249)

  #BF:50< =  greater than O:50< = 
pt((6.733333 - 5.533333)/sqrt(((1/15)+(1/15))*MSq), DegF, lower.tail = FALSE)
    #p = 0.06238063  = > insuf. ALMOST!


interaction.plot(Age_Group, Butter_Type, Taste_Scores, ylab = "Mean Taste Score", 
                 legend = FALSE, col = 1:2, main = "Butter:Age Interaction", lty = c(1,1,2))
  segments(1, 5.764706, 1, 6.611111, col = "blue", lty = 2)
  segments(2, 4.960392, 2, 5.966667, col = "blue", lty = 2)
  segments(3, 6.733333, 3, 5.533333, col = "blue", lty = 2)
  legend("bottomright", legend = c("Odell's", "Butterface"), col = 2:1, lty = c(1,1,2), bty = "n",
       title = "Butter", title.adj = .275)  



