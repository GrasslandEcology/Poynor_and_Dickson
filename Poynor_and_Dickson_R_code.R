# List the packages to be used

library(car)
library(emmeans)
library(GGally)
library(lme4)
library(tidyverse)

# Note that 2024_02_12_GCP_preliminary_glmer.csv contains only the data from visit 3 (June 3) onwards
# because many of the stems that were clipped in half during visit 1 were not found visit 2 and so they were 
# replaced with randomly chosen new clones during visit 2 (May 30) and then data were recorded from visit 3 
# onwards for all these stems.

# Note also that these data are recorded in the following ways: 1) All subsequent visits for a stem are recorded as
# zeros for alive1_dead0 and for height and eggs where there is a note that the plant is dead or when the 
# height of the plant was recorded as zero for every subsequent visit; 2) If stems could not be found then they 
# are recorded as "NA" for all subsequent visits because we cannot be sure if the stem was eaten, even though
# the most likely scenario is that the stem died (i.e. senesced) but recorded as missing data (i.e. "NA") is
# the most conservative option to make certain we do not inflate the percentage of dead stems.

# Set the working directory (i.e. folder where .csv data files are located)

setwd(choose.dir())


####### First examine survival per stem, averaged per clone (Fig. 1 in manuscript)

AVG_clones <- read.csv("Poynor_Dickson_average_per_clone_June3_Sept16.csv")
head(AVG_clones)

# Change variables that are numbers to factors (i.e. categorical variables), as appropriate

AVG_clones <- within(AVG_clones, {
  visit <- factor(visit)
  rep_ID <- factor(rep_ID)
  clone <- factor(clone)
})

# Complete a repeated-measures mixed-effect Analysis of Variance (i.e. linear mixed-effect analysis)

clone_survival_analysis <- lmer(formula = AVGalive1_dead0 ~ visit * treatment + (1 | rep_ID) , data = AVG_clones)

summary(clone_survival_analysis)

anova(clone_survival_analysis)

# Histogram of residuals of analysis

hist(resid(clone_survival_analysis))

# Examine contrasts between treatments across all visits

pairs(emmeans(clone_survival_analysis, "treatment"))

# List of means and SE for treatment * visit

emmeans(clone_survival_analysis, "visit", by = "treatment")

# Use the immediately previous emmeans output to make Fig. 1 in the manuscript (can use visit rather than date_labels or user can manually enter date_labels)

survival_graph <- ggplot(data = emmeans_clone_survival_analysis,
                         mapping = aes(x = date_labels)) +  
  geom_ribbon(mapping = aes(ymin = (CON_lower.CL),
                            ymax = (CON_upper.CL)),
              alpha = 0.2) +
  geom_line(mapping = aes(y = CON_emmean),
            color = 'black',
            linewidth = 1.5) +
  geom_line(mapping = aes(y = FL_emmean),
            color = 'royalblue',
            linewidth = 1.5) +
  geom_line(mapping = aes(y = EX_emmean),
            color = 'violet',
            linewidth = 1.5) +
  geom_text(data = emmeans_clone_survival_analysis %>% filter(visit == 25),
            mapping = aes(x = date_labels,
                          y = CON_emmean,
                          label = "Control"),
            color = 'black',
            size = 6,
            hjust = 0,
            vjust = -0.3) +
  geom_text(data = emmeans_clone_survival_analysis %>% filter(visit == 25),
            mapping = aes(x = date_labels,
                          y = FL_emmean,
                          label = "Floral clipping"),
            color = 'royalblue',
            size = 6,
            hjust = 1.3,
            vjust = -0.3) +
  geom_text(data = emmeans_clone_survival_analysis %>% filter(visit == 25),
            mapping = aes(x = date_labels,
                          y = EX_emmean,
                          label = "Stem clipping"),
            color = 'violet',
            size = 6,
            hjust = 1.3,
            vjust = -1.1) +
  labs( x = "Date", 
        y = "Common milkweed stem survival") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_light() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Print the graph using abbreviated month/Year(capital letter means no abbreviation)
survival_graph + scale_x_date(date_breaks = "1 month", date_labels = "%B")



####### Next examine the number of monarch eggs per stem, averaged per clone (Fig. 2 in manuscript) 

# Complete a repeated-measures mixed-effect Analysis of Variance (i.e. linear mixed-effect analysis)

clone_eggs_analysis <- lmer(formula = AVGeggs ~ treatment * visit + (1 | rep_ID), data = AVG_clones)

summary(clone_eggs_analysis)

anova(clone_eggs_analysis)

# Histogram of residuals of analysis

hist(resid(clone_eggs_analysis))

# Examine contrasts between treatments across all visits

pairs(emmeans(clone_eggs_analysis, "treatment"))

# List of means and SE for treatment * visit

emmeans(clone_eggs_analysis, "visit", by = "treatment")

# Use the immediately previous emmeans output to make Fig. 2 in the manuscript (can use visit rather than date_labels or user can manually enter date_labels)

eggs_stem0_graph <- ggplot(data = emmeans_clone_eggs_analysis,
                           mapping = aes(x = date_labels)) +  
  geom_ribbon(mapping = aes(ymin = (CON_lower.CL),
                            ymax = (CON_upper.CL)),
              alpha = 0.2) +
  geom_line(mapping = aes(y = CON_emmean),
            color = 'black',
            size = 1.5) +
  geom_line(mapping = aes(y = FL_emmean),
            color = 'royalblue',
            size = 1.5) +
  geom_line(mapping = aes(y = EX_emmean),
            color = 'violet',
            size = 1.5) +
  geom_text(data = emmeans_clone_eggs_analysis %>% filter(visit == 21),
            mapping = aes(x = date_labels,
                          y = CON_emmean,
                          label = "Control"),
            color = 'black',
            size = 6,
            hjust = 0.12,
            vjust = -0.8) +
  geom_text(data = emmeans_clone_eggs_analysis %>% filter(visit == 24),
            mapping = aes(x = date_labels,
                          y = FL_emmean,
                          label = "Floral clipping"),
            color = 'royalblue',
            size = 6,
            hjust = -0.1,
            vjust = 0.8) +
  geom_text(data = emmeans_clone_eggs_analysis %>% filter(visit == 22),
            mapping = aes(x = date_labels,
                          y = EX_emmean,
                          label = "Stem clipping"),
            color = 'violet',
            size = 6,
            hjust = 0.5,
            vjust = -0.3) +
  labs( x = "Date", 
        y = "Monarch eggs per common milkweed stem") +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme_light() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Print the graph using abbreviated month/Year(capital letter means no abbreviation)
eggs_stem0_graph + scale_x_date(date_breaks = "1 month", date_labels = "%B")



####### Next examine the number of monarch eggs per stem, NOT averaged per clone and ignoring dead (i.e. senesced) stems (Figs. 3 and S3 in manuscript) 
# Read and inspect the data for visit 3-21 (June3 - August5)

stems_deadNA <- read.csv("Poyner_Dickson_all_data_June3_Aug5_senesced=NA.csv")
head(stems_deadNA)

ggpairs(stems_deadNA, mapping = aes(colour = treatment), columns = 4:9,
        lower = list(continuous = "smooth"))

# Change variables that are numbers to factors (i.e. categorical variables), as appropriate

stems_deadNA <- within(stems_deadNA, {
  visit <- factor(visit)
  treatment_num <- factor(treatment_num)
  rep_ID <- factor(rep_ID)
  clone <- factor(clone)
  stem <- factor(stem)
  alive1_dead0 <- factor(alive1_dead0)
})

# Complete generalized linear mixed models using only living stems (note that a warning message appears that a 
# large eigenvalue ratio is present and that stem height should be rescaled). We complete the analysis
# immediately below to create a graph of the raw stem heights (Fig. 3), but we calculate P-values for the analysis
# and create Fig. S3 from the generalized_eggs_per_stem_sc model below that scales stem height to a Z-score.

# Force the glmer to run more iterations to increase the likelihood of the model being identifiable by including the following:
# glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000))

generalized_eggs_per_stem <- glmer(formula = R_eggs ~ visit + P_height_mm + (1 | rep_ID), 
             data = stems_deadNA, family = poisson(link = log),
             control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)))

# Estimate the trendline for P_height_mm explanatory variable and add values to the data frame under new "pi.hat" variable
summary(generalized_eggs_per_stem)
# The pi.hat predicted trendline uses the summary coefficients to solve the equation y = exp(intercept) * exp(slope*P_height_mm)
stems_deadNA$pi.hat <- ((exp(-5.6588618)) * (exp(0.0041263 * stems_deadNA$P_height_mm)))
# The minimum and maximum confidence intervals below subtract and add 1.96*(SE of the slope) to the slope estimate
stems_deadNA$pi.hat_min <- ((exp(-5.6588618)) * (exp((0.0041263 - (1.96*0.0007445)) * stems_deadNA$P_height_mm)))
stems_deadNA$pi.hat_max <- ((exp(-5.6588618)) * (exp((0.0041263 + (1.96*0.0007445)) * stems_deadNA$P_height_mm)))

# The following code uses ggplot to graph data and predicted line for Fig. 3 (pi.hat calculated above)

height_eggs_graph <- ggplot(data = stems_deadNA,
                         aes(x = P_height_mm, y = jitter(R_eggs), 
                             color = factor(treatment_num))) +
  geom_ribbon(mapping = aes(ymin = (pi.hat_min),
                            ymax = (pi.hat_max)),
              color = "black",
              alpha = 0.2) +
  geom_line(mapping = aes(y = pi.hat),
            color = "black",
            linewidth = 1.25) +
  geom_point(show.legend = FALSE, size = 1.75) +
  scale_color_manual(values = c("black", "royalblue", "violet")) +
    labs( x = "Stem height (mm)", 
        y = "Monarch eggs per common milkweed stem") +
    theme_light() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

height_eggs_graph

# We used methods from the following website to remove warning messages about very large eigenvalue in the generalized_eggs_per_stem model: 
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# To remove the very large eigenvalue warning message, the following code scales the predictor ("^P\\_") variables

numcols <- grep("^P\\_",names(stems_deadNA))
stems_deadNA_scaled <- stems_deadNA
stems_deadNA_scaled[,numcols] <- scale(stems_deadNA_scaled[,numcols])

# Force the glmer to run more iterations to increase the likelihood of the model being identifiable by including the following:
# glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000))

generalized_eggs_per_stem_sc <- glmer(formula = R_eggs ~ visit + treatment + P_height_mm + visit*treatment + treatment*P_height_mm + (1 | rep_ID), 
             data = stems_deadNA_scaled, family = poisson(link = log),
             control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)))

summary(generalized_eggs_per_stem_sc)


######### Now we're going to look for the model where we remove predictor variables from generalized_eggs_per_stem_sc to get the model with the lowest AIC
# The full model (generalized_eggs_per_stem_sc) has AIC=1134.6
# The only decreases in AIC occur by first removing the interactions (AIC=1100.2)
# then also removing "treatment" leads to the lowest AIC=1097.4 of any combination of fixed predictor variables (see Table 1 of manuscript).

generalized_eggs_per_stem_sc_aic <- glmer(formula = R_eggs ~ visit + P_height_mm + (1 | rep_ID), 
                data = stems_deadNA_scaled, family = poisson(link = log),
                control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)))

summary(generalized_eggs_per_stem_sc_aic)

Anova(generalized_eggs_per_stem_sc_aic, test.statistic="Chi")

# Now calculating the trendline for scaled stem height from the output of summary(generalized_eggs_per_stem_sc_aic)

# The pi.hat predicted trendline uses the summary coefficients to solve the equation y = exp(intercept) * exp(slope*P_height_mm)
stems_deadNA_scaled$pi.hat <- ((exp(-3.30007)) * (exp(0.72163 * stems_deadNA_scaled$P_height_mm)))
# The minimum and maximum confidence intervals below subtract and add 1.96*(SE of the slope) to the slope estimate
stems_deadNA_scaled$pi.hat_min <- ((exp(-3.30007)) * (exp((0.72163 - (1.96*0.12617)) * stems_deadNA_scaled$P_height_mm)))
stems_deadNA_scaled$pi.hat_max <- ((exp(-3.30007)) * (exp((0.72163 + (1.96*0.12617)) * stems_deadNA_scaled$P_height_mm)))

# The following code uses ggplot to graph data and predicted line for Fig. S3 (pi.hat calculated above)
# Note this is the graph for scaled stem height data based on the fixed predictor variables with
# the lowest model AIC (i.e. visit + P_height_mm)

height_eggs_graph_scaled <- ggplot(data = stems_deadNA_scaled,
                            aes(x = P_height_mm, y = jitter(R_eggs), 
                                color = factor(treatment_num))) +
  geom_ribbon(mapping = aes(ymin = (pi.hat_min),
                            ymax = (pi.hat_max)),
              color = "black",
              alpha = 0.2) +
  geom_line(mapping = aes(y = pi.hat),
            color = "black",
            linewidth = 1.25) +
  geom_point(show.legend = FALSE, size = 1.75) +
  scale_color_manual(values = c("black", "royalblue", "violet")) +
  labs( x = "Stem height (scaled Z-score)", 
        y = "Monarch eggs per common milkweed stem") +
  theme_light() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

height_eggs_graph_scaled



####### Next examine the average height of stems per clone, ignoring dead (i.e. senesced) stems (Fig. 4 in manuscript)

# First create a new dataframe that is mean height per stem within each clone at each visit from June3-August5

clones_deadNA <- stems_deadNA %>% group_by(visit,treatment,rep_ID) %>% summarise(AVG_height_mm = mean(P_height_mm,na.rm=T)) #%>% merge(siteyear)
head(clones_deadNA)

# Complete a repeated-measures mixed-effect Analysis of Variance for the average height per clone (i.e. linear mixed-effect analysis)

clone_height_analysis <- lmer(formula = AVG_height_mm  ~ visit * treatment + (1 | rep_ID), data = clones_deadNA)

summary(clone_height_analysis)

anova(clone_height_analysis)

# Histogram of residuals of analysis

hist(resid(clone_height_analysis))

# Examine contrasts between treatments across all visits

pairs(emmeans(clone_height_analysis, "treatment"))

# List of means and SE for treatment * visit

emmeans(clone_height_analysis, "visit", by = "treatment")

# Use the immediately previous emmeans output to make Fig. 4 in the manuscript (can use visit rather than date_labels or user can manually enter date_labels)

height_stemNA_graph <- ggplot(data = emmeans_height_deadNA,
       mapping = aes(x = date_labels)) +  
  geom_ribbon(mapping = aes(ymin = (CON_lower.CL),
                            ymax = (CON_upper.CL)),
              alpha = 0.2) +
  geom_line(mapping = aes(y = CON_emmean),
            color = 'black',
            linewidth = 1.5) +
  geom_line(mapping = aes(y = FL_emmean),
            color = 'royalblue',
            linewidth = 1.5) +
  geom_line(mapping = aes(y = EX_emmean),
            color = 'violet',
            linewidth = 1.5) +
  geom_text(data = emmeans_height_deadNA %>% filter(visit == 21),
            mapping = aes(x = date_labels,
                          y = CON_emmean,
                          label = "Control"),
            color = 'black',
            size = 6,
            hjust = 0,
            vjust = -0.6) +
  geom_text(data = emmeans_height_deadNA %>% filter(visit == 21),
            mapping = aes(x = date_labels,
                          y = FL_emmean,
                          label = "Floral clipping"),
            color = 'royalblue',
            size = 6,
            hjust = 0,
            vjust = 0.6) +
  geom_text(data = emmeans_height_deadNA %>% filter(visit == 21),
            mapping = aes(x = date_labels,
                          y = EX_emmean,
                          label = "Stem clipping"),
            color = 'violet',
            size = 6,
            hjust = 0,
            vjust = 0) +
  labs( x = "Visit", 
        y = "Milkweed height with senesced stems ignored (mm)") +
  coord_cartesian(ylim = c(0, 700)) +
  theme_light() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Print the graph using abbreviated month/Year(capital letter means no abbreviation)
height_stemNA_graph + scale_x_date(date_breaks = "1 month", date_labels = "%B")