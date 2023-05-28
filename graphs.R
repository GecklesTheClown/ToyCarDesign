library(tidyverse)
library(ggpubr)
library(broom)

df <- read.csv("R Data/data.csv")


distance <- ggplot(data = df, aes(x = "", y = Distance_avg))+
  geom_boxplot()+
  geom_point()+
  labs(y = "Average distance (cm)",
       x = "Response variable")+
  theme_bw()


surface <- ggplot(data = df, aes(x = Surface_type, y = Distance_avg))+
  geom_boxplot()+
  geom_point()+
  labs(y = "Average distance (cm)",
       x = "Surface type")+
  theme_bw()

weight <- ggplot(data = df, aes(x = as.factor(Weight), y = Distance_avg))+
  geom_boxplot()+
  geom_point()+
  labs(y = "Average distance (cm)",
       x = "Additional weight (g)")+
  theme_bw()

angle <- ggplot(data = df, aes(x = as.factor(Angle), y = Distance_avg))+
  geom_boxplot()+
  geom_point()+
  labs(y = "Average distance (cm)",
       x = expression('Angle ('*degree*')'))+
  theme_bw()

ggarrange(distance, surface, weight, angle)

exp3 <- ggplot(df, aes(x=Angle, y= Distance_avg)) +
  geom_point() +
  facet_grid( ~ Surface_type)+
  labs(x = expression('Angle ('*degree*')'), 
       y = "Average distance (cm)")+
  theme_bw()


exp4 <- ggplot(df, aes(x=Weight, y= Distance_avg)) +
  geom_point() +
  facet_grid( ~ Surface_type)+
  labs(x = "Weight (g)", 
       y = "Average distance (cm)")+
  theme_bw()

ggarrange(exp3,exp4, nrow=2, ncol=1)





# # Now let's visualise the models
# # log model
# # Reduced model
# t.model <- lm(formula = log(Distance_avg) ~ Surface_type + Angle + Surface_type:Angle, 
#    data = df)
# 
# summary(t.model)
# glance(t.model)
# 
# t.model_df <- fortify(t.model)
# 
# 
# # residuals vs each explanatory variable
# 
# #resid vs surface_type
# log.resid.surface <- ggplot(data = t.model_df, aes(x = Surface_type, y = .resid))+
#   geom_point(alpha = 0.5)+
#   geom_smooth(se = F)+
#   geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#   labs(x = "Surface Type",
#        y = "Residuals")+
#   theme_bw()
# 
# #resid vs weight
# #log.resid.weight <- ggplot(data = t.model_df, aes(x = Weight, y = .resid))+
# #  geom_point(alpha = 0.5)+
# #  geom_smooth(se = F)+
# #  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
# #  labs(x = "Additional weight (g)",
# #       y = "Residuals")+
# #  scale_x_continuous(breaks = c(0, 10, 20))+
# #  theme_bw()
# 
# #resid vs angle
# log.resid.angle <- ggplot(data = t.model_df, aes(x = Angle, y = .resid))+
#   geom_point(alpha = 0.5)+
#   geom_smooth(se = FALSE)+
#   geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#   labs(x = expression('Angle ('*degree*')'),
#        y = "Residuals")+
#   theme_bw()
# 
# # residuals vs fitted values
# log.resid.fitted <- ggplot(data = t.model_df, aes(x = .fitted, y = .resid))+
#   geom_point()+
#   geom_smooth(se = F)+
#   geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#   labs(x = "Fitted Values",
#        y = "Residuals")+
#   theme_bw()
# 
# 
# 
# # histogram of residuals
# log.histo_resid <- ggplot(data = t.model_df, aes(x = .resid))+
#   geom_histogram(bins = 15)+
#   stat_function(fun = dnorm,
#                 args = list(
#                   mean = mean(t.model_df$.resid),
#                   sd = sd(t.model_df$.resid)))+
#   labs(x = "Residuals")+
#   theme_bw()
# 
# 
# # indepence plot
# log.independence <- ggplot(data = t.model_df, aes(x = seq(1:18), y = .resid))+
#   geom_line()+
#   geom_smooth(method = lm, se = F)+
#   geom_abline(slope = 0, intercept = 0, linetype = 3)+
#   labs(x = "Observation number",
#        y = "Residuals")+
#   theme_bw()
# 
# 
# # QQ plot
# log.qq <- ggplot(data = t.model_df, aes(sample = .stdresid))+
#   stat_qq()+
#   geom_abline(intercept = 0,
#               slope = 1)+
#   coord_equal()+
#   labs(x = "Theoretical quantiles",
#        y = "Sample quantiles")+
#   theme_bw()
# 
# ggarrange(log.resid.surface, #log.resid.weight, 
#           log.resid.angle, 
#           log.resid.fitted, log.histo_resid, log.independence, log.qq)

  





# # non-log model
# non_t.model = lm(Distance_avg ~ Surface_type + Weight + Angle + Surface_type:Weight + Surface_type:Angle, data = df)
# 
# glance(non_t.model)
# summary(non_t.model)
# 
# non_t.model_df <- fortify(non_t.model)
# 
# # residuals vs each explanatory variable
# 
# #resid vs surface_type
# resid.surface <- ggplot(data = non_t.model_df, aes(x = Surface_type, y = .resid))+
#   geom_point(alpha = 0.5)+
#   geom_smooth(se = F)+
#   geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#   labs(x = "Surface Type",
#        y = "Residuals")+
#   theme_bw()
# 
# #resid vs weight
# resid.weight <- ggplot(data = non_t.model_df, aes(x = Weight, y = .resid))+
#   geom_point(alpha = 0.5)+
#   geom_smooth(se = F)+
#   geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#   labs(x = "Additional weight (g)",
#        y = "Residuals")+
#   scale_x_continuous(breaks = c(0, 10, 20))+
#   theme_bw()
# 
# #resid vs angle
# resid.angle <- ggplot(data = non_t.model_df, aes(x = Angle, y = .resid))+
#   geom_point(alpha = 0.5)+
#   geom_smooth(se = F)+
#   geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#   labs(x = expression('Angle ('*degree*')'),
#        y = "Residuals")+
#   theme_bw()
# 
# # residuals vs fitted values
# resid.fitted <- ggplot(data = non_t.model_df, aes(x = .fitted, y = .resid))+
#   geom_point()+
#   geom_smooth(se = F)+
#   geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#   labs(x = "Fitted Values",
#        y = "Residuals")+
#   theme_bw()
# 
# 
# 
# # histogram of residuals
# histo_resid <- ggplot(data = non_t.model_df, aes(x = .resid))+
#   geom_histogram(bins = 15)+
#   stat_function(fun = dnorm,
#                 args = list(
#                   mean = mean(non_t.model_df$.resid),
#                   sd = sd(non_t.model_df$.resid)))+
#   labs(x = "Residuals")+
#   theme_bw()
# 
# 
# # indepence plot
# independence <- ggplot(data = non_t.model_df, aes(x = seq(1:18), y = .resid))+
#   geom_line()+
#   geom_smooth(method = lm, se = F)+
#   geom_abline(slope = 0, intercept = 0, linetype = 3)+
#   labs(x = "Observation number",
#        y = "Residuals")+
#   theme_bw()
# 
# 
# # QQ plot
# qq <- ggplot(data = non_t.model_df, aes(sample = .stdresid))+
#   stat_qq()+
#   geom_abline(intercept = 0,
#               slope = 1)+
#   labs(x = "Theoretical quantiles",
#        y = "Sample quantiles")+
#   theme_bw()
# 
# ggarrange(resid.surface, resid.weight, resid.angle, 
#           resid.fitted, histo_resid, independence, qq)




###### CATEGORICAL MODEL #####

df <- read.csv("R Data/data.csv")

#Convert all x variables to factors
c_var <- c("Surface_type","Weight","Angle") #make all our variables categorical
df[,c_var] <- lapply(df[,c_var],factor)


# Interaction plots

# Angle and weight
inter.angle_weight <- ggplot(data = df, aes(y = Distance_avg))+
  geom_line(aes(x = Angle, 
                color = Weight,
                group = Weight), 
            linewidth = 1)+
  labs(y = "Average distance (cm)")+
  facet_grid(~Surface_type)+
  theme_bw()

# weight and angle
inter.weight_angle <- ggplot(data = df, aes(y = Distance_avg))+
  geom_line(aes(x = Weight, 
                color = Angle,
                group = Angle), 
            linewidth = 1)+
  labs(y = "Average distance (cm)")+
  facet_grid(~Surface_type)+
  theme_bw()

# surface type
inter.surface_type <- ggplot(data = df, aes(y = Distance_avg))+
  geom_line(aes(x = Surface_type, 
                color = Weight,
                group = Weight), 
            linewidth = 1)+
  labs(x = "Surface type",
       y = "Average distance (cm)")+
  facet_grid(~Angle)+
  theme_bw()

ggarrange(inter.weight_angle, inter.angle_weight, inter.surface_type)



df_long <- df[, c(5, 6, 7)]

df_long$Treatment <- 1:18

df_long1 <- data.frame(df_long[,1])
colnames(df_long1)[1] = "x"
df_long2 <- data.frame(df_long[,2])
colnames(df_long2)[1] = "x"
df_long3 <- data.frame(df_long[,3])
colnames(df_long3)[1] = "x"

LONG <- rbind(df_long1, df_long2, df_long3)

quart <- quantile(LONG$x)
outliers <- c(quart[2] - (quart[2] * 1.5), quart[3] + (quart[3] * 1.5))
outliers

range(LONG)

# Test for outliers
ggplot(data = df_long)+
  geom_point(aes(x = Treatment, y = Distance_1), color = "blue")+
  geom_point(aes(x = Treatment, y = Distance_2), color = "green")+
  geom_point(aes(x = Treatment, y = Distance_3), color = "red")+
  geom_abline(slope = 0, intercept = outliers[2])+
  theme_bw()


model1_cat <- lm(formula = Distance_avg ~ Surface_type + Angle, data = df)

glance(model1_cat)
summary(model1_cat)

model1_cat_df <- fortify(model1_cat)


# residuals vs each explanatory variable

#resid vs surface_type
resid.surface <- ggplot(data = model1_cat_df, aes(x = Surface_type, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Surface Type",
       y = "Residuals")+
  theme_bw()

#resid vs angle
resid.angle <- ggplot(data = model1_cat_df, aes(x = Angle, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = expression('Angle ('*degree*')'),
       y = "Residuals")+
  theme_bw()

# residuals vs fitted values
resid.fitted <- ggplot(data = model1_cat_df, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Fitted Values",
       y = "Residuals")+
  theme_bw()



# histogram of residuals
histo_resid <- ggplot(data = model1_cat_df, aes(x = .resid))+
  geom_histogram(bins = 15)+
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(model1_cat_df$.resid),
                  sd = sd(model1_cat_df$.resid))
                )+
  labs(x = "Residuals")+
  theme_bw()


# indepence plot
independence <- ggplot(data = model1_cat_df, aes(x = seq(1:18), y = .resid))+
  geom_line()+
  geom_smooth(method = lm, se = F)+
  geom_abline(slope = 0, intercept = 0, linetype = 3)+
  labs(x = "Observation number",
       y = "Residuals")+
  theme_bw()


# QQ plot
qq <- ggplot(data = model1_cat_df, aes(sample = .stdresid))+
  stat_qq()+
  geom_abline(intercept = 0,
              slope = 1)+
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")+
  theme_bw()

ggarrange(resid.surface,resid.angle, 
          resid.fitted, histo_resid, independence, qq)





##### FINAL MODEL #####


df <- read.csv("R Data/data.csv")

#Convert all x variables to factors
c_var <- c("Surface_type","Weight","Angle") #make all our variables categorical
df[,c_var] <- lapply(df[,c_var],factor)

df <- df[, c(1, 2, 3, 4, 8)]

final_model <- lm(formula = log(Distance_avg) ~ Surface_type + Angle + 
                    Surface_type:Angle, data = df)

glance(final_model)
summary(final_model)

final_model_df <- fortify(final_model)


# residuals vs each explanatory variable

#resid vs surface_type
resid.surface <- ggplot(data = final_model_df, aes(x = Surface_type, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Surface Type",
       y = "Residuals")+
  theme_bw()

#resid vs angle
resid.angle <- ggplot(data = final_model_df, aes(x = Angle, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = expression('Angle ('*degree*')'),
       y = "Residuals")+
  theme_bw()

# residuals vs fitted values
resid.fitted <- ggplot(data = final_model_df, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Fitted Values",
       y = "Residuals")+
  theme_bw()


# histogram of residuals
histo_resid <- ggplot(data = final_model_df, aes(x = .resid))+
  geom_histogram(bins = 15)+
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(final_model_df$.resid),
                  sd = sd(final_model_df$.resid)))+
  labs(x = "Residuals")+
  theme_bw()


# indepence plot
independence <- ggplot(data = final_model_df, aes(x = seq(1:18), y = .resid))+
  geom_line()+
  geom_smooth(method = lm, se = F)+
  geom_abline(slope = 0, intercept = 0, linetype = 3)+
  labs(x = "Observation number",
       y = "Residuals")+
  theme_bw()


# QQ plot
qq <- ggplot(data = final_model_df, aes(sample = .stdresid))+
  stat_qq()+
  geom_abline(intercept = 0,
              slope = 1)+
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")+
  theme_bw()

ggarrange(resid.surface,resid.angle,
          resid.fitted, histo_resid, independence, qq)



##### ANOVA on final model #####



fit.anova <- aov(lm(formula = log(Distance_avg) ~ Surface_type + Angle + 
                         Surface_type:Angle, data = df))

summary(fit.anova)

# Tukey's HSD
tukeys_HSD <- TukeyHSD(fit.anova)

par(mar = c(5,10,4,5))
plot(TukeyHSD(fit.anova, conf.level=.95), las = 2)


# Plot final model

# Does surface type have an effect?
surface_type_plot <- ggplot(data = final_model_df, 
                            aes(x = Surface_type, 
                            y = .fitted,
                            color = Angle,
                            group = Angle))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = .fitted - (.sigma / 2),
                    ymax = .fitted + (.sigma / 2)))+
  labs(x = "Surface type",
       y = "Fitted values")+
  theme_bw()

# Does Angle have an effect
angle_plot <- ggplot(data = final_model_df, 
                     aes(x = Angle, 
                         y = .fitted,
                         color = Surface_type,
                         group = Surface_type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = .fitted - (.sigma / 2),
                    ymax = .fitted + (.sigma / 2)))+
  labs(x = expression('Angle ('*degree*')'),
       y = "Fitted values",
       color = "Surface type")+
  theme_bw()


ggarrange(surface_type_plot, angle_plot)

