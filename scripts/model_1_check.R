## Original Model

# non-log model
non_t.model = lm(formula = Distance_avg ~ Surface_type + Angle, data = df)

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

