# Call packages
#install.packages("car")
#install.packages("MASS")
library(MASS)
#install.packages("pscl")
library(pscl)
#install.packages("AER")
library(AER)
library(ggplot2)
library("car")
#install.packages("DHARMa")
library(DHARMa)
#install.packages("lme4")
library(lme4)
#install.packages("simr")
library(simr)
#install.packages("sjPlot")
library(sjPlot)
#install.packages("glmm")
library(glmm)
#install.packages("emmeans")
library(emmeans)

# Read in data set and check whether numeric, factor or characteristic

setwd("C:\\Users\\annin\\Documents\\Uni\\MSc\\Jaar 1\\Master thesis\\Statistics")
df <- read.csv("Overview Total Variables.csv",
         header = TRUE)
str(df)

setwd("C:\\Users\\annin\\Documents\\Uni\\MSc\\Jaar 1\\Master thesis\\Statistics")
df2 <- read.csv("Overview_Data2.csv",
               header = TRUE)

str(df2)

# Change characteristics variables

df$Time.Preening.Singing <- as.numeric(df$Time.Preening.Singing)
df$Total.Time.Spend.Floor <- as.numeric(df$Total.Time.Spend.Floor) 
df$Total.Time.Spend.Mesh <- as.numeric(df$Total.Time.Spend.Mesh)
df$Total.Time.Spend.Front <- as.numeric(df$Total.Time.Spend.Front) 
df$Female.ID <- as.character(df$Female.ID)
df$Ring.number <- as.character(df$Ring.number)
df$ï..Trial <- as.factor(df$Trial)
df$Tutorgroup <- as.factor(df$Tutorgroup)
df$Day <- as.character(df$Day)
df$Test.time <- as.factor(df$Test.time)
df$Treatment <- as.factor(df$Treatment)

df2$Total.Time.Spend.Floor <- as.numeric(df$Total.Time.Spend.Floor)
df2$ï..Trial <- as.factor(df2$ï..Trial)
df2$Segment <- as.character(df2$Segment)
df2$Ring.number <- as.character(df2$Ring.number)
df2$Female.ID <- as.character(df2$Female.ID)
df2$Treatment <- as.factor(df2$Treatment)
df2$Tutorgroup <- as.factor(df2$Tutorgroup)
df2$Test.time <- as.factor(df2$Test.time)
df2$Day <- as.factor(df2$Day)
df2$Time.Preening.Singing <- as.numeric(df2$Time.Preening.Singing)
df2$Total.Time.Spend.Front <- as.numeric(df2$Total.Time.Spend.Front)


# Summary 

summary(df)                                                                     # Almost all variables are 0 while singing so singing will not be included in the analysis
summary(df2)

# Difference between segment 1 and 2 (before and after rotation) Appendix ? and ? 

help(scatterplot)
scatterplot(df2$Total.Switching.Position ~ df2$Total.Switching.Position | df2$Segment) ->?
boxplot(df2$Total.Switching.Position ~ df2$Segment,
        ylab = 'Number of switching position', 
        xlab = 'Segment', 
        main = 'Total switching position in each segment')                      # Looks the same, no comment than it looks the same

boxplot(df2$Total.Time.Spend.Female ~ df2$Segment,
        ylab = 'Total time spend female (s)', 
        xlab = 'Segment', 
        main = 'Total time spend female in each segment')                       # More variance in segment 2

modelfemale2 <- aov(df2$Total.Time.Spend.Female~df2$Segment)
summary(modelfemale2)                                                           # Segment is not significant
shapiro.test(modelfemale2$residuals)
qqPlot(modelfemale2$residuals)                                                  # Not normally distributed
residualPlot(modelfemale2)                                                      # There is homoscedacity

modelfemale2.log <- aov(log(df2$Total.Time.Spend.Female + 1) ~ df2$Segment)
qqPlot(modelfemale2.log$residuals)
shapiro.test(modelfemale2.log$residuals)                                        # Not normally distributed

modelfemale2.sqrt <- aov(sqrt(df2$Total.Time.Spend.Female + 1) ~ df2$Segment)
qqPlot(modelfemale2.sqrt$residuals)
shapiro.test(modelfemale2.sqrt$residuals)    

kruskal.test(df2$Total.Time.Spend.Female ~ df2$Segment)                         # Not significant

boxplot(df2$Total.Courtship.Song ~ df2$Segment,
        ylab = 'Number of courtship song', 
        xlab = 'Segment', 
        main = 'Total courtship song in each segment')

boxplot(df2$Total.Beak.Wipe ~ df2$Segment,
        ylab = 'Number of beak wipes', 
        xlab = 'Segment', 
        main = 'Total number of beak wipes in each segment')

boxplot(df2$Total.Turning.Axis ~ df2$Segment,
        ylab = 'Number of turning axis', 
        xlab = 'Segment', 
        main = 'Total number of turning axis in each segment')                  # A bit more activity in segment 1

turningaxis2.lm <- lm(df2$Total.Turning.Axis~df2$Segment)
summary(turningaxis2.lm)
shapiro.test(turningaxis2.lm$residuals)                                          # Not normally distributed, try transforming
qqPlot(turningaxis2.lm$residuals)                                                # Looks normally distributed
residualPlot(turningaxis2.lm)                                                    # There is no homoscedacity
ncvTest(turningaxis2.lm)

turningaxis2.loglm <- lm(log(df2$Total.Turning.Axis + 1)~df2$Segment)
plot(turningaxis2.loglm)
shapiro.test(turningaxis2.loglm$residuals)
qqPlot(turningaxis2.loglm$residuals)                                            # Not normally distributed with log, was better when not transformed

turningaxis2.sqrtlm <- lm(sqrt(df2$Total.Turning.Axis + 1)~df2$Segment)         # Not normally distributed
plot(turningaxis2.sqrtlm) 
shapiro.test(turningaxis2.sqrtlm$residuals)
qqPlot(turningaxis2.sqrtlm)

poissonturningaxis2 <- glm(df2$Total.Turning.Axis~df2$Segment, family = poisson)
summary(poissonturningaxis2)
dispersiontest(poissonturningaxis2)                                             # Overdispersion, use glmm

turningaxis2.glmer <- glmer(df2$Total.Turning.Axis ~ df2$Segment
                           + df2$Test.time
                           + (1 | df2$Ring.number)
                           + (1 | df2$Tutorgroup),
                           family = "poisson"(link = "log"))
summary(turningaxis2.glmer)

anova(turningaxis2.glmer)

1 - pf(144.14, 1, 1)
1 - pf(124.69, 1, 1)

boxplot(df2$Total.Zig.Zag.Hopping ~ df2$Segment,
        ylab = 'Number of zig zag hops', 
        xlab = 'Segment', 
        main = 'Total number of zig zag hops in each segment')                  # A bit more activity in segment 1

zigzag2.lm <- lm(df2$Total.Zig.Zag.Hopping~df2$Segment)
summary(zigzag2.lm)
shapiro.test(zigzag2.lm$residuals)                                              # Not normally distributed, try transforming
qqPlot(zigzag2.lm$residuals)                                                    
residualPlot(zigzag2.lm)                                                        # There is no homoscedacity
ncvTest(zigzag2.lm)

zigzag2.loglm <- lm(log(df2$Total.Zig.Zag.Hopping + 1)~df2$Segment)
plot(zigzag2.loglm)
shapiro.test(zigzag2.loglm$residuals)
qqPlot(zigzag2.loglm$residuals)                                                 # Not normally distributed with log

zigzag2.sqrtlm <- lm(sqrt(df2$Total.Zig.Zag.Hopping + 1)~df2$Segment)           # Not normally distributed
plot(zigzag2.sqrtlm) 
shapiro.test(zigzag2.sqrtlm$residuals)
qqPlot(zigzag2.sqrtlm)

poissonzigzag2 <- glm(df2$Total.Zig.Zag.Hopping~df2$Segment, family = poisson)
summary(poissonzigzag2)
dispersiontest(poissonzigzag2)                                                  # Overdispersion, use glmm

zigzag2.glmer <- glmer(df2$Total.Zig.Zag.Hopping ~ df2$Segment
                            + df2$Test.time
                            + df2$Segment : df2$Test.time
                            + (1 | df2$Ring.number)
                            + (1 | df2$Tutorgroup),
                            family = "poisson"(link = "log"))
summary(zigzag2.glmer)

anova(zigzag2.glmer)

1 - pf(338.423, 1, 1)
1 - pf(268.133, 1, 1)
1 - pf(35.089, 1, 1)

boxplot(df2$Total.Time.Preening ~ df2$Segment,
        ylab = 'Total time preening (s)', 
        xlab = 'Segment', 
        main = 'Total time preening in each segment')                           # A bit more activity in segment 2

modelpreening2 <- aov(df2$Total.Time.Preening~df2$Segment)
summary(modelpreening2)                                                         # Segment is not significant
shapiro.test(modelpreening2$residuals)
qqPlot(modelpreening2$residuals)                                                # Not normally distributed
residualPlot(modelpreening2)                                                    # There is homoscedacity

modelpreening2.log <- aov(log(df2$Total.Time.Preening + 1) ~ df2$Segment)
qqPlot(modelpreening2.log$residuals)
shapiro.test(modelpreening2.log$residuals)                                      # Not normally distributed
residualPlot(modelpreening2.log)
influenceIndexPlot(modelpreening2.log) 

modelpreening2.sqrt <- aov(sqrt(df2$Total.Time.Preening + 1) ~ df2$Segment)
qqPlot(modelpreening2.sqrt$residuals)
shapiro.test(modelpreening2.sqrt$residuals)                                     # Not normally distributed

kruskal.test(df2$Total.Time.Preening ~ df2$Segment)                             # No significant difference!

boxplot(df2$Total.Pecking.At.Object ~ df2$Segment,
        ylab = 'Number of pecking at objects', 
        xlab = 'Segment', 
        main = 'Total number of pecking at objects in each segment')

boxplot(df2$Total.Pecking.At.Female ~ df2$Segment,
        ylab = 'Number of pecking at female', 
        xlab = 'Segment', 
        main = 'Total number of pecking at female in each segment')

boxplot(df2$Total.Time.Spend.Perch ~ df2$Segment,
        ylab = 'Total time spend perch (s)', 
        xlab = 'Segment', 
        main = 'Total time spend on perch in each segment')

boxplot(df2$Total.Time.Spend.Floor ~ df2$Segment,
        ylab = 'Total time spend floor (s)', 
        xlab = 'Segment', 
        main = 'Total time spend on floor in each segment')

boxplot(df2$Total.Time.Spend.Mesh ~ df2$Segment,
        ylab = 'Total time spend mesh (s)', 
        xlab = 'Segment', 
        main = 'Total time spend on mesh in each segment')

boxplot(df2$Total.Time.Spend.Front ~ df2$Segment,
        ylab = 'Total time spend front (s)', 
        xlab = 'Segment', 
        main = 'Total time spend in the front in each segment')

boxplot(df2$Total.Time.Spend.Back ~ df2$Segment,
        ylab = 'Total time spend back (s)', 
        xlab = 'Segment', 
        main = 'Total time spend in the back in each segment')

# Dependent variables female presence (time spend) Appendix ?

## Switching position

plot(df$Total.Switching.Position ~ df$Total.Time.Spend.Female,
        ylab = 'Number of switching position',
        xlab = 'Log(Total time spend female (s))',
        main = 'Total number of switching position against the time that the female spend with a male',
        log = 'x')

switchpos.lm <- lm(df$Total.Switching.Position ~ df$Total.Time.Spend.Female)
summary(switchpos.lm)                                                           # Total time spend female is not significant

## Beak wipe

plot(df$Total.Beak.Wipe ~ df$Total.Time.Spend.Female,
        ylab = 'Number of beak wipes',
        xlab = 'Log(Total time spend female (s))',
        main = 'Total number of beak wipes against the time that the female spend with a male',
        log = 'x')

beakwp.lm <- lm(df$Total.Beak.Wipe ~ df$Total.Time.Spend.Female)
summary(beakwp.lm)                                                              # Total time spend female is not significant 

qqPlot(beakwp.lm$residuals)
shapiro.test(beakwp.lm$residuals)                                               # Not normally distributed
residualPlot(beakwp.lm)                                                         # No homoscedacity

beakwp.loglm <- lm(log(df$Total.Beak.Wipe + 1) ~ df$Total.Time.Spend.Female)
qqPlot(beakwp.loglm$residuals)
shapiro.test(beakwp.loglm$residuals)                                            # Normally distributed
residualPlot(beakwp.loglm)                                                      # There is homoscedacity

summary(beakwp.loglm)                                                           # Not significant
 
## Turning axis

plot(df$Total.Turning.Axis ~ df$Total.Time.Spend.Female,
     ylab = 'Number of turning axis',
     xlab = 'Log(Total time spend female (s))',
     main = 'Total number of turning axis against the time that the female spend with a male',
     log = 'x')

turn.lm <- lm(df$Total.Turning.Axis ~ df$Total.Time.Spend.Female)
summary(turn.lm)                                                                # Total time spend female is not significant

## Zig zag hopping

plot(df$Total.Zig.Zag.Hopping ~ df$Total.Time.Spend.Female,
     ylab = 'Number of zig zag hops',
     xlab = 'Log(Total time spend female (s))',
     main = 'Total number of zig zag hops against the time that the female spend with a male',
     log = 'x')

zig.lm <- lm(df$Total.Zig.Zag.Hopping ~ df$Total.Time.Spend.Female)
summary(zig.lm)                                                                 # Total time spend female is not significant

## Preening

plot(df$Total.Time.Preening ~ df$Total.Time.Spend.Female,
     ylab = 'Time preening (s)',
     xlab = 'Log(Total time spend female (s))',
     main = 'Total time preening against the time that the female spend with a male',
     log = 'x')

preen.lm <- lm(df$Total.Time.Preening ~ df$Total.Time.Spend.Female)
summary(preen.lm)                                                               # Total time spend female is not significant

## Pecking at object

plot(df$Total.Pecking.At.Object ~ df$Total.Time.Spend.Female,
     ylab = 'Number of pecks at object',
     xlab = 'Log(Total time spend female (s))',
     main = 'Total number of pecks at object against the time that the female spend with a male',
     log = 'x')

peck.lm <- lm(df$Total.Pecking.At.Object ~ df$Total.Time.Spend.Female)
summary(peck.lm)                                                                # Total time spend female is not significant
cor.test(df$Total.Pecking.At.Object, df$Total.Time.Spend.Female)

## Location

### Perch

plot(df$Total.Time.Spend.Perch ~ df$Total.Time.Spend.Female,
     ylab = 'Log(Time spend on perch (s))',
     xlab = 'Total time spend female (s)',
     main = 'Total time spend on perch against the time that the female spend with a male',
     log = 'y')

cor.test(df$Total.Time.Spend.Perch, df$Total.Time.Spend.Female)                 # Total time spend female is not significant/correlated

### Floor

plot(df$Total.Time.Spend.Floor ~ df$Total.Time.Spend.Female,
     ylab = 'Log(Time spend on floor (s))',
     xlab = 'Total time spend female (s)',
     main = 'Total time spend on floor against the time that the female spend with a male',
     log = 'y')

cor.test(df$Total.Time.Spend.Floor, df$Total.Time.Spend.Female)                 # Total time spend female is not significant/correlated

### Front

plot(df$Total.Time.Spend.Front ~ df$Total.Time.Spend.Female,
     ylab = 'Log(Time spend in the front (s))',
     xlab = 'Total time spend female (s)',
     main = 'Total time spend in the front against the time that the female spend with a male',
     log = 'y')

cor.test(df$Total.Time.Spend.Front, df$Total.Time.Spend.Female)                 # Total time spend female is not significant/correlated

### Back

plot(df$Total.Time.Spend.Back ~ df$Total.Time.Spend.Female,
     ylab = 'Time spend in the back (s)',
     xlab = 'Log(Total time spend female (s))',
     main = 'Total time spend in the back against the time that the female spend with a male',
     log = 'x')

cor.test(df$Total.Time.Spend.Back, df$Total.Time.Spend.Female)                  # Total time spend female is not significant/correlated

# Dependent variables against treatments visualisation data Appendix ?

boxplot(df$Total.Time.Spend.Female~df$Treatment, ylab = 'Time spend female (s)', 
        xlab = 'Treatment', 
        main = 'Time spend female at each treatment')

boxplot(df$Total.Courtship.Song~df$Treatment,                                   # Courtship song does not have to be included in further analysis
        ylab = 'Number courtship song', 
        xlab = 'Treatment', 
        main = 'Total courtship song for each treatment')

boxplot(df$Total.Switching.Position~df$Treatment, 
        ylab = 'Number of switching postion', 
        xlab = 'Treatment', 
        main = 'Total number of switching position male for each treatment')

boxplot(df$Total.Beak.Wipe~df$Treatment, 
        ylab = 'Number of beak wipes', 
        xlab = 'Treatment', 
        main = 'Total number of beak wipes for each treatment')

boxplot(df$Total.Turning.Axis~df$Treatment, 
        ylab = 'Number of turning axis', 
        xlab = 'Treatment', 
        main = 'Total number of turning axis for each treatment')

boxplot(df$Total.Zig.Zag.Hopping~df$Treatment, 
        ylab = 'Number of zig zag hopping', 
        xlab = 'Treatment', 
        main = 'Total number of zig zag hopping for each treatment')

boxplot(df$Total.Time.Preening~df$Treatment, 
        ylab = 'Time preening (s)', ylim = c(0, 700), 
        xlab = 'Treatment', 
        main = 'Total time preening for each treatment')

boxplot(df$Total.Pecking.At.Object~df$Treatment, 
        ylab = 'Number of pecking at object', 
        ylim = c(0, 500),
        xlab = 'Treatment', 
        main = 'Total number of pecking at object for each treatment')

boxplot(df$Total.Pecking.At.Female~df$Treatment,                                # Pecking at female not included in further analysis
        ylim = c(0,3),   
        ylab = 'Number of pecking at female', 
        xlab = 'Treatment', 
        main = 'Total pecking at female for each treatment')

boxplot(df$Total.Time.Spend.Perch~df$Treatment,                                 # Perch, needs to be together with floor and mesh
        ylab = 'Time spend on perch (s)',
        ylim = c(0, 10000),
        xlab = 'Treatment',
        main = 'Total time spend on perch for each treatment')

boxplot(df$Total.Time.Spend.Floor~df$Treatment,                                 # Floor, needs to be together with perch and mesh
        ylim = c(0,10000),
        ylab = 'Time spend on floor (s)',                                       # One outlier -> limit
        xlab = 'Treatment',
        main = 'Total time spend on floor for each treatment')

boxplot(df$Total.Time.Spend.Mesh~df$Treatment,                                  # Mesh, needs to be together with perch and floor
        ylab = 'Time spend on mesh (s)',                                        # Mesh does not have to be included in analysis
        xlab = 'Treatment',
        main = 'Total time spend on mesh for each treatment')

boxplot(df$Total.Time.Spend.Front~df$Treatment,                                 # Front, needs to be together with back
        ylim = c(0,10000),                                                      # One outlier -> limit
        ylab = 'Time spend in the front (s)',                         
        xlab = 'Treatment',
        main = 'Total time spend in the front for each treatment')

boxplot(df$Total.Time.Spend.Back~df$Treatment,                                  # Back, needs to be together with front
        ylab = 'Time spend in the back (s)',
        xlab = 'Treatment',
        main = 'Total time spend in the back for each treatment')

# Construct and run model and diagnostics Appendix ? 

## Time spend female

Model.TimeFemale.lm <- lm(df$Total.Time.Spend.Female ~ df$Treatment 
                          + df$Total.Beak.Wipe 
                          + df$Total.Turning.Axis 
                          + df$Total.Zig.Zag.Hopping 
                          + df$Total.Pecking.At.Object
                          + df$Total.Time.Preening
                          + df$Total.Time.Spend.Back
                          + df$Total.Time.Spend.Floor
                          + df$Total.Time.Spend.Front
                          + df$Total.Time.Spend.Floor
                          + df$Total.Time.Spend.Perch)
summary(Model.TimeFemale.lm)                                                    # Only zig zag hopping is significant 

Model.TimeFemale.lm3 <- lm(df$Total.Time.Spend.Female ~ df$Total.Zig.Zag.Hopping)
summary(Model.TimeFemale.lm3)                                                   # Zig zag hopping not significant


Model.TimeFemale.lm2 <- lm(df$Total.Time.Spend.Female ~ df$Treatment
                          + df$Total.Zig.Zag.Hopping)
summary(Model.TimeFemale.lm2)                                                   # Zig zag hopping is not significant here

modelfemale <- aov(df$Total.Time.Spend.Female~df$Treatment)
shapiro.test(modelfemale$residuals)
qqPlot(modelfemale$residuals)                                                   # Not normally distributed
residualPlot(modelfemale)                                                       # There is homoscedacity

modelfemale.log <- aov(log(df$Total.Time.Spend.Female + 1) ~ df$Treatment)
qqPlot(modelfemale.log$residuals)
shapiro.test(modelfemale.log$residuals)                                         # Not normally distributed

modelfemale.sqrt <- aov(sqrt(df$Total.Time.Spend.Female + 1) ~ df$Treatment)
qqPlot(modelfemale.sqrt$residuals)
shapiro.test(modelfemale.sqrt$residuals)                                        # Not normally distrubuted

modelfemale.bc <- boxcox((df$Total.Time.Spend.Female + 0.0001) ~ df$Treatment)  # Lambda = 0,1?
modelfemale.transformed <- aov((df$Total.Time.Spend.Female^0.1) ~ df$Treatment)
qqPlot(modelfemale.transformed$residuals)
shapiro.test(modelfemale.transformed$residuals)                                 # Fail

kruskal.test(df$Total.Time.Spend.Female ~ df$Treatment)

## Preening

modelpreening <- aov(df$Total.Time.Preening~df$Treatment)
summary(modelpreening)                                                          # Treatment is not significant
shapiro.test(modelpreening$residuals)
qqPlot(modelpreening$residuals)                                                 # Not normally distributed
residualPlot(modelpreening)                                                     # There is homoscedacity

modelpreening.log <- aov(log(df$Total.Time.Preening + 1) ~ df$Treatment)
qqPlot(modelpreening.log$residuals)
shapiro.test(modelpreening.log$residuals)                                       # Normally distributed
residualPlot(modelpreening.log)
influenceIndexPlot(modelpreening.log)                                           # Line 25 and 55 maybe outliers

summary(modelpreening.log)

power.anova.test(groups = 4, n = 16, between.var = 6.761, within.var = 3.317)   # Power = 1

TukeyHSD(modelpreening.log)
plot(TukeyHSD(modelpreening.log))

## Location

### Perch

modelperch <- aov(df$Total.Time.Spend.Perch~df$Treatment)
shapiro.test(modelperch$residuals)
qqPlot(modelperch$residuals)                                                    # Not normally distributed
residualPlot(modelperch)                                                        # There is homoscedacity

modelperch.log <- aov(log(df$Total.Time.Spend.Perch + 1) ~ df$Treatment)
qqPlot(modelperch.log$residuals)
shapiro.test(modelperch.log$residuals)                                          # Not normally distributed

modelperch.sqrt <- aov(sqrt(df$Total.Time.Spend.Perch + 1) ~ df$Treatment)
qqPlot(modelperch.sqrt$residuals)
shapiro.test(modelperch.sqrt$residuals)                                         # Not normally distributed

modelperch.bc <- boxcox((df$Total.Time.Spend.Perch + 0.001) ~ df$Treatment)     # Lambda = 0.25?

kruskal.test(df$Total.Time.Spend.Perch ~ df$Treatment)                          # There is a significant difference
pairwise.wilcox.test(df$Total.Time.Spend.Perch, df$Treatment,
                     p.adjust.method = "BH")                                    # Only A and AF are significantly different

### Floor

modelfloor <- aov(df$Total.Time.Spend.Floor~df$Treatment)
shapiro.test(modelfloor$residuals)
qqPlot(modelfloor$residuals)                                                    # Not normally distributed, almost a straight line
residualPlot(modelfloor)                                                        # There is homoscedacity

modelfloor.log <- aov(log(df$Total.Time.Spend.Floor + 1) ~ df$Treatment)
qqPlot(modelfloor.log$residuals)
shapiro.test(modelfloor.log$residuals)                                          # Not normally distributed

modelfloor.sqrt <- aov(sqrt(df$Total.Time.Spend.Floor + 1) ~ df$Treatment)
qqPlot(modelfloor.sqrt$residuals)
shapiro.test(modelfloor.sqrt$residuals)                                         # Not normally distributed

modelfloor.bc <- boxcox((df$Total.Time.Spend.Floor + 1) ~ df$Treatment)         # Lambda = 0.05?

kruskal.test(df$Total.Time.Spend.Floor ~ df$Treatment)
pairwise.wilcox.test(df$Total.Time.Spend.Floor, df$Treatment,
                     p.adjust.method = "BH")

### Front

modelfront <- aov(df$Total.Time.Spend.Front~df$Treatment)
shapiro.test(modelfront$residuals)
qqPlot(modelfront$residuals)                                                    # Not normally distributed, almost a straight line
residualPlot(modelfront)                                                        # Homoscedacity

modelfront.log <- aov(log(df$Total.Time.Spend.Front + 1) ~ df$Treatment)
qqPlot(modelfront.log$residuals)
shapiro.test(modelfront.log$residuals)                                          # Not normally distributed

modelfront.sqrt <- aov(sqrt(df$Total.Time.Spend.Front + 1) ~ df$Treatment)
qqPlot(modelfront.sqrt$residuals)
shapiro.test(modelfront.sqrt$residuals)                                         # Not normally distributed

modelfront.bc <- boxcox((df$Total.Time.Spend.Front + 1) ~ df$Treatment)         # Lambda = -0.9?

kruskal.test(df$Total.Time.Spend.Front ~ df$Treatment)                          # Not significant!

### Back

modelback <- aov(df$Total.Time.Spend.Back~df$Treatment)
shapiro.test(modelback$residuals)
qqPlot(modelback$residuals)                                                     # Not normally distributed
residualPlot(modelback)                                                         # There is homoscedacity

modelback.log <- aov(log(df$Total.Time.Spend.Back + 1) ~ df$Treatment)
qqPlot(modelback.log$residuals)
shapiro.test(modelback.log$residuals)                                           # Not normally distributed

modelback.sqrt <- aov(sqrt(df$Total.Time.Spend.Back + 1) ~ df$Treatment)
qqPlot(modelback.sqrt$residuals)
shapiro.test(modelback.sqrt$residuals)                                          # Not normally distributed

modelback.bc <- boxcox((df$Total.Time.Spend.Back + 1) ~ df$Treatment)           # Lambda = 0.05?

kruskal.test(df$Total.Time.Spend.Back ~ df$Treatment)

## Counted data

### Switching position

boxplot(df$Total.Switching.Position ~ df$Segment)

switch.lm <- lm(df$Total.Switching.Position~df$Treatment)
summary(switch.lm)
shapiro.test(switch.lm$residuals)
qqPlot(switch.lm$residuals)                                                     # Not normally distributed, try transform
residualPlot(switch.lm)
ncvTest(switch.lm)

switch.loglm <- lm(log(df$Total.Switching.Position + 1)~df$Treatment)
plot(switch.loglm)
shapiro.test(switch.loglm$residuals)                                            # No log
qqPlot(switch.loglm$residuals)

switch.squarelm <- lm(df$Total.Switching.Position^2~df$Treatment)
plot(switch.squarelm)
qqPlot(switch.squarelm$residuals)
shapiro.test(switch.squarelm$residuals)                                         # Log was better

switch.sqrtlm <- lm(sqrt(df$Total.Switching.Position + 1)~df$Treatment)
plot(switch.sqrtlm)
shapiro.test(switch.sqrtlm$residuals)                                           # Log was better -> GLM
qqPlot(switch.sqrtlm$residuals)

poissonswitch <- glm(df$Total.Switching.Position~df$Treatment, family = poisson(link = "log"))
summary(poissonswitch)
residualPlot(poissonswitch)
qqPlot(poissonswitch$residuals)
plot(poissonswitch)
dispersiontest(poissonswitch)                                                   # Overdispersion, use glmm
simulateResiduals(poissonswitch, plot = T)

poissonswitch.sqrt <- glm(df$Total.Switching.Position~df$Treatment, family = poisson(link = "sqrt"))
summary(poissonswitch.sqrt)
residualPlot(poissonswitch.sqrt)
qqPlot(poissonswitch.sqrt$residuals)

MaleSet <- df$Tutorgroup
Male.ID <- df$Ring.number
Test.Time <- df$Test.time
Treatment <- df$Treatment
Total.Switching.Position <- df$Total.Switching.Position

switch.glmer <- glmer(Total.Switching.Position ~ Treatment
                      + Test.Time
                      + (1 | Male.ID)
                      + (1 | MaleSet),
                      family = "poisson"(link = "log"))

summary(switch.glmer)                                                           # AIC = 54912.3
qqPlot(resid(switch.glmer))
plot(switch.glmer)                                                              # Normal distribution
Modelswitch_Res <- lm(resid(switch.glmer) ~ df$Treatment)
influenceIndexPlot(Modelswitch_Res)                                             # Row 168 might be an outlier, check labjourna, no homoscedacity
ncvTest(Modelswitch_Res)

plot_model(switch.glmer,                                                        # A = 0.66, AF = 1.10, R = 0.93, RN = 0.89
           axis.labels = c("A", "AF", "R", "RN"),
           show.values = TRUE, show.p = TRUE,
           title = "Effect of treatments on male zebra finch activity")

switch.glmer2 <- glmer(Total.Switching.Position ~ Treatment
                       + Test.Time
                       + Treatment * Test.Time
                       + (1 | Male.ID)
                       + (1 | MaleSet),
                       family = "poisson"(link = "log"))
summary(switch.glmer2)                                                          # AIC = 53104.7, is smaller so better fitting model!

anova(switch.glmer2)

pf(0.102, 3, 1)
1 - pf(6370.380, 1, 3)
1 - pf(606.093, 1, 3)


### Beak wipe

beakwipe.lm <- lm(df$Total.Beak.Wipe~df$Treatment)
summary(beakwipe.lm)
shapiro.test(beakwipe.lm$residuals)
qqPlot(beakwipe.lm$residuals)                                                   # Not normally distributed -> transform
residualPlot(beakwipe.lm)                                                       # There is homoscedacity
ncvTest(beakwipe.lm)

beakwipe.loglm <- lm(log(df$Total.Beak.Wipe + 1)~df$Treatment)
plot(beakwipe.loglm)
shapiro.test(beakwipe.loglm$residuals)
qqPlot(beakwipe.loglm$residuals)                                                # Not normally distributed with log

beakwipe.sqrtlm <- lm(sqrt(df$Total.Beak.Wipe + 1)~df$Treatment)
plot(beakwipe.sqrtlm)                                                           # Log looked better -> GLM

poissonbeakwipe <- glm(df$Total.Beak.Wipe~df$Treatment, family = poisson(link = "log"))
summary(poissonbeakwipe)
dispersiontest(poissonbeakwipe)                                                 # Overdispersion, use glmm

beak.glmer <- glmer(df$Total.Beak.Wipe ~ df$Treatment
                    + df$Test.time
                    + df$Treatment * df$Test.time
                    + (1 | df$Ring.number)
                    + (1 | df$Tutorgroup),
                    family = "poisson"(link = "log"))
summary(beak.glmer)

anova(beak.glmer)

pf(0.8626, 3, 1)
1 - pf(9.2731, 1, 3)
1 - pf(5.5778, 3, 1)

### Turning axis

turningaxis.lm <- lm(df$Total.Turning.Axis~df$Treatment)
summary(turningaxis.lm)
shapiro.test(turningaxis.lm$residuals)                                          # Not normally distributed, try transforming
qqPlot(turningaxis.lm$residuals)                                                # Looks normally distributed
residualPlot(turningaxis.lm)                                                    # There is homoscedacity
ncvTest(turningaxis.lm)

turningaxis.loglm <- lm(log(df$Total.Turning.Axis + 1)~df$Treatment)
plot(turningaxis.loglm)
shapiro.test(turningaxis.loglm$residuals)
qqPlot(turningaxis.loglm$residuals)                                             # Not normally distributed with log, was better when not transformed

turningaxis.sqrtlm <- lm(sqrt(df$Total.Turning.Axis + 1)~df$Treatment)          # Not normally distributed
plot(beakwipe.sqrtlm) 

poissonturningaxis <- glm(df$Total.Turning.Axis~df$Treatment, family = poisson)
summary(poissonturningaxis)
dispersiontest(poissonturningaxis)                                              # Overdispersion, use glmm

turningaxis.glmer <- glmer(df$Total.Turning.Axis ~ df$Treatment
                           + df$Test.time
                           + df$Treatment : df$Test.time
                           + (1 | df$Ring.number)
                           + (1 | df$Tutorgroup),
                           family = "poisson"(link = "log"))
summary(turningaxis.glmer)

anova(turningaxis.glmer)

1 - pf(1.9386, 3, 1)
1 - pf(103.6335, 1, 3)
1 - pf(37.2737, 3, 1)

### Zig zag hopping

zigzag.lm <- lm(df$Total.Zig.Zag.Hopping~df$Treatment)
summary(zigzag.lm)
shapiro.test(zigzag.lm$residuals)                                               # Not normally distributed, try transforming
qqPlot(zigzag.lm$residuals)                                                     # Not normally distributed
residualPlot(zigzag.lm)                                                         # No homoscedacity
ncvTest(turningaxis.lm)

zigzag.loglm <- lm(log(df$Total.Zig.Zag.Hopping + 1)~df$Treatment)
plot(zigzag.loglm)
shapiro.test(zigzag.loglm$residuals)
qqPlot(zigzag.loglm$residuals)                                                  # Not normally distributed

zigzag.sqrtlm <- lm(sqrt(df$Total.Zig.Zag.Hopping + 1)~df$Treatment)            # Not normally distributed -> glm
plot(zigzag.sqrtlm)

poissonzigzag <- glm(df$Total.Zig.Zag.Hopping~df$Treatment, family = poisson)
summary(poissonzigzag)
dispersiontest(poissonzigzag)                                                   # Overdispersion, use glmm

zigzag.glmer <- glmer(df$Total.Zig.Zag.Hopping ~ df$Treatment
                      + df$Test.time
                      + df$Treatment : df$Test.time
                      + (1 | df$Ring.number)
                      + (1 | df$Tutorgroup),
                      family = "poisson"(link = "log"))
summary(zigzag.glmer)

anova(zigzag.glmer)

pf(0.4324, 3, 1)
1 - pf(175.6200, 1, 3)
1 - pf(25.7484, 3, 1)

### Pecking at object

peckingobject.lm <- lm(df$Total.Pecking.At.Object~df$Treatment)
summary(peckingobject.lm)
shapiro.test(peckingobject.lm$residuals)                                        # Not normally distributed, try transforming
qqPlot(peckingobject.lm$residuals)                                              # Not normally distributed
residualPlot(peckingobject.lm)                                                  # No homoscedacity
ncvTest(peckingobject.lm)

peckingobject.loglm <- lm(log(df$Total.Pecking.At.Object + 1)~df$Treatment)
plot(peckingobject.loglm)
shapiro.test(peckingobject.loglm$residuals)
qqPlot(peckingobject.loglm$residuals)                                           # Normally distributed!
residualPlot(peckingobject.loglm)
ncvTest(peckingobject.loglm)

summary(peckingobject.loglm)

AIC(peckingobject.loglm)

peckingobject2.loglm <- lm(log(df$Total.Pecking.At.Object + 1)~df$Treatment + df$Test.time + df$Treatment : df$Test.time)
plot(peckingobject2.loglm)
shapiro.test(peckingobject2.loglm$residuals)
qqPlot(peckingobject2.loglm$residuals)                                          # Normally distributed!
residualPlot(peckingobject2.loglm)
ncvTest(peckingobject2.loglm)

summary(peckingobject2.loglm)                                                   # No interaction

anova(peckingobject2.loglm)

emmeans(peckingobject2.loglm, specs = pairwise ~ Treatment)


# Just to be sure pecking at object glmm

peckingobject.glmer <- glmer(df$Total.Pecking.At.Object ~ df$Treatment
                      + df$Test.time
                      + df$Treatment : df$Test.time
                      + (1 | df$Ring.number)
                      + (1 | df$Tutorgroup),
                      family = "poisson"(link = "log"))
summary(peckingobject.glmer)

anova(peckingobject.glmer)
