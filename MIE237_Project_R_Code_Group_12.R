library(car)
library(dplyr)
library(ggplot2)
library(pastecs)

data <- read.table("MIE237_Project_Data.csv", header=T, sep = ",")
data$RT <- data$RT * 1000 #converting RT into milliseconds

#DESCRIPTIVE STATISTICS
-----------------------------------------------------------------------------------------
by(data$RT, list(data$Hand, data$Sensitivity), function(x) {c(mean = mean(x), sd = sd(x))})
by(data$ACC, list(data$Hand, data$Sensitivity), function(x) {c(mean = mean(x), sd = sd(x))})

ggplot(data, aes(x = interaction(Hand, Sensitivity), y = ACC, fill = Hand)) +
  geom_boxplot() +
  labs(
    title = "Accuracy Across Conditions",
    x = "Condition (Hand x DPI)",
    y = "Accuracy (%)"
  ) +
  theme_minimal()

ggplot(data, aes(x = interaction(Hand, Sensitivity), y = RT, fill = Hand)) +
  geom_boxplot() +
  labs(
    title = "Reaction Time Across Conditions",
    x = "Condition (Hand x DPI)",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal()

data %>%
  group_by(Hand, Sensitivity) %>%
  summarise(mean_rt = mean(RT), .groups = 'drop') %>%
  ggplot(aes(x = Sensitivity, y = mean_rt, color = Hand, group = Hand)) +
  geom_line(linewidth = 1.2) +
  geom_point(linewidth = 3) +
  labs(
    title = "Interaction Plot: Reaction Time",
    x = "Mouse Sensitivity (DPI)",
    y = "Mean Reaction Time (ms)"
  ) +
  theme_minimal()
print(shapiro.test(data$RT))

#TESTING ASSUMPTIONS
------------------------------------------------------------------------------------------
#VISUAL INSPECTION
rt_plot <- ggplot(data, aes(x = RT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 16, fill = "white", colour = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(data$RT, na.rm = TRUE), sd = sd(data$RT, na.rm = TRUE)),
                colour = "blue", linewidth = 1) +
  labs(title = "Histogram of Reaction Time with Normal Curve",
       x = "Reaction Time", y = "Density") +
  theme_minimal()
print(rt_plot)

acc_plot <- ggplot(data, aes(x = ACC)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "white", colour = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(data$ACC, na.rm = TRUE), sd = sd(data$ACC, na.rm = TRUE)),
                colour = "blue", linewidth = 1) +
  labs(title = "Histogram of Accuracy with Normal Curve",
       x = "Accuracy", y = "Density") +
  theme_minimal()
print(acc_plot)

qqplot_rt <- ggplot(data, aes(sample = RT)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Reaction Time") +
  theme_minimal()
print(qqplot_rt)

qqplot_acc <- ggplot(data, aes(sample = ACC)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Accuracy") +
  theme_minimal()
print(qqplot_acc)

#NORMALITY TESTS: SHAPIRO-WILK TEST
print(stat.desc(data$RT, basic = FALSE, norm = TRUE))  
print(summary(data$RT))                               
print(shapiro.test(data$RT))                           
print(stat.desc(data$ACC, basic = FALSE, norm = TRUE))  
print(summary(data$ACC))                               
print(shapiro.test(data$ACC))                           

#HOMOGENEITY OF VARIANCE TESTS: LEVENE'S TEST
print(leveneTest(RT ~ Sensitivity, data = data))
print(leveneTest(RT ~ Hand, data = data))
print(leveneTest(RT ~ interaction(Sensitivity, Hand), data = data))
print(leveneTest(ACC ~ Sensitivity, data = data))
print(leveneTest(ACC ~ Hand, data = data))
print(leveneTest(ACC ~ interaction(Sensitivity, Hand), data = data))

#FINDING SUITABLE TRANSFORMATIONS
-------------------------------------------------------------------------------------------
data$invRT <- 1/(data$RT)
data$logRT <- log(data$RT)
data$sqrtRT <- sqrt(data$RT)

#1/RT Transformation
print(shapiro.test(data$invRT))
print(leveneTest(invRT ~ Sensitivity, data = data))
print(leveneTest(invRT ~ Hand, data = data))
print(leveneTest(invRT ~ interaction(Sensitivity, Hand), data = data))

#log(RT) Transformation
print(shapiro.test(data$logRT))                            
print(leveneTest(logRT ~ Sensitivity, data = data))
print(leveneTest(logRT ~ Hand, data = data))
print(leveneTest(logRT ~ interaction(Sensitivity, Hand), data = data))

#sqrt(RT) Transformation
print(shapiro.test(data$sqrtRT))                            
print(leveneTest(sqrtRT ~ Sensitivity, data = data))
print(leveneTest(sqrtRT ~ Hand, data = data))
print(leveneTest(sqrtRT ~ interaction(Sensitivity, Hand), data = data))

#TWO-WAY REPEATED MEASURED ANOVA USING LOG(RT)
------------------------------------------------------------------------------------------
#TRANSFORMED RT PLOTS
rt_plot <- ggplot(data, aes(x = logRT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 16, fill = "white", colour = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(data$logRT, na.rm = TRUE), sd = sd(data$logRT, na.rm = TRUE)),
                colour = "blue", linewidth = 1) +
  labs(title = "Histogram of log(RT) with Normal Curve",
       x = "Reaction Time", y = "Density") +
  theme_minimal()
print(rt_plot)

qqplot_rt <- ggplot(data, aes(sample = logRT)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of log(RT)") +
  theme_minimal()
print(qqplot_rt)

aov_rt <- aov(logRT ~ Sensitivity * Hand + Error(Participant / (Sensitivity * Hand)), data = data)
aov_acc <- aov(ACC ~ Sensitivity * Hand + Error(Participant / (Sensitivity * Hand)), data = data)
summary(aov_rt)
summary(aov_acc)