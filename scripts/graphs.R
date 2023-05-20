
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
       x = "Angle (�)")+
  theme_bw()

ggarrange(distance, surface, weight, angle)

exp3 <- ggplot(df, aes(x=Angle, y= Distance_avg)) +
  geom_point() +
  facet_grid( ~ Surface_type)+
  labs(x = "Angle (�)", 
       y = "Average distance (cm)")+
  theme_bw()


exp4 <- ggplot(df, aes(x=Weight, y= Distance_avg)) +
  geom_point() +
  facet_grid( ~ Surface_type)+
  labs(x = "Weight (g)", 
       y = "Average distance (cm)")+
  theme_bw()

ggarrange(exp3,exp4, nrow=2, ncol=1)


inter.angle_weight <- ggplot(data = df, aes(y = Distance_avg))+
  geom_line(aes(x = Angle, color = Surface_type), linewidth = 1)+
  facet_grid(~Weight)+
  theme_bw()

inter.weight_angle <- ggplot(data = df, aes(y = Distance_avg))+
  geom_line(aes(x = Weight, color = Surface_type), linewidth = 1)+
  facet_grid(~Angle)+
  theme_bw()

ggarrange(inter.weight_angle, inter.angle_weight)


# Now let's visualise the models
# log model
# Reduced model
t.model <- lm(formula = log(Distance_avg) ~ Surface_type + Angle + Surface_type:Angle, 
   data = df)

summary(t.model)
glance(t.model)

t.model_df <- fortify(t.model)


# residuals vs each explanatory variable

#resid vs surface_type
log.resid.surface <- ggplot(data = t.model_df, aes(x = Surface_type, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Surface Type",
       y = "Residuals")+
  theme_bw()

#resid vs weight
#log.resid.weight <- ggplot(data = t.model_df, aes(x = Weight, y = .resid))+
#  geom_point(alpha = 0.5)+
#  geom_smooth(se = F)+
#  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
#  labs(x = "Additional weight (g)",
#       y = "Residuals")+
#  scale_x_continuous(breaks = c(0, 10, 20))+
#  theme_bw()

#resid vs angle
log.resid.angle <- ggplot(data = t.model_df, aes(x = Angle, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Angle (�)",
       y = "Residuals")+
  theme_bw()

# residuals vs fitted values
log.resid.fitted <- ggplot(data = t.model_df, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Fitted Values",
       y = "Residuals")+
  theme_bw()



# histogram of residuals
log.histo_resid <- ggplot(data = t.model_df, aes(x = .resid))+
  geom_histogram(bins = 15)+
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(t.model_df$.resid),
                  sd = sd(t.model_df$.resid)))+
  labs(x = "Residuals")+
  theme_bw()


# indepence plot
log.independence <- ggplot(data = t.model_df, aes(x = seq(1:18), y = .resid))+
  geom_line()+
  geom_smooth(method = lm, se = F)+
  geom_abline(slope = 0, intercept = 0, linetype = 3)+
  labs(x = "Observation number",
       y = "Residuals")+
  theme_bw()


# QQ plot
log.qq <- ggplot(data = t.model_df, aes(sample = .stdresid))+
  stat_qq()+
  geom_abline(intercept = 0,
              slope = 1)+
  coord_equal()+
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")+
  theme_bw()

ggarrange(log.resid.surface, #log.resid.weight, 
          log.resid.angle, 
          log.resid.fitted, log.histo_resid, log.independence, log.qq)

  





# non-log model
non_t.model = lm(Distance_avg ~ Surface_type + Weight + Angle + Surface_type:Weight + Surface_type:Angle, data = df)

glance(non_t.model)
summary(non_t.model)

non_t.model_df <- fortify(non_t.model)

# residuals vs each explanatory variable

#resid vs surface_type
resid.surface <- ggplot(data = non_t.model_df, aes(x = Surface_type, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Surface Type",
       y = "Residuals")+
  theme_bw()

#resid vs weight
resid.weight <- ggplot(data = non_t.model_df, aes(x = Weight, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Additional weight (g)",
       y = "Residuals")+
  scale_x_continuous(breaks = c(0, 10, 20))+
  theme_bw()

#resid vs angle
resid.angle <- ggplot(data = non_t.model_df, aes(x = Angle, y = .resid))+
  geom_point(alpha = 0.5)+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Angle (�)",
       y = "Residuals")+
  theme_bw()

# residuals vs fitted values
resid.fitted <- ggplot(data = non_t.model_df, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_smooth(se = F)+
  geom_abline(slope = 0, intercept = 0, color = "black", linewidth = 1, linetype = 3)+
  labs(x = "Fitted Values",
       y = "Residuals")+
  theme_bw()



# histogram of residuals
histo_resid <- ggplot(data = non_t.model_df, aes(x = .resid))+
  geom_histogram(bins = 15)+
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(non_t.model_df$.resid),
                  sd = sd(non_t.model_df$.resid)))+
  labs(x = "Residuals")+
  theme_bw()


# indepence plot
independence <- ggplot(data = non_t.model_df, aes(x = seq(1:18), y = .resid))+
  geom_line()+
  geom_smooth(method = lm, se = F)+
  geom_abline(slope = 0, intercept = 0, linetype = 3)+
  labs(x = "Observation number",
       y = "Residuals")+
  theme_bw()


# QQ plot
qq <- ggplot(data = non_t.model_df, aes(sample = .stdresid))+
  stat_qq()+
  geom_abline(intercept = 0,
              slope = 1)+
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")+
  theme_bw()

ggarrange(resid.surface, resid.weight, resid.angle, 
          resid.fitted, histo_resid, independence, qq)

