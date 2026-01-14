#download the matching versions of Matrix and lme4 so that they are compatible.
oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)


#makes your random factor a factor
your.data$Random.factor <- as.factor(your.data$Random.factor)


#fits your model. You can have as many random factors as you need, but it does impact model fit, so be cautious here.
model1 <- lmer(outcome.variable ~ Input.variable + (1|Random.factor), dat=your.data)


#computes the effect sie for your input variable used in the model above
effects.model1 <- effects::effect(term= "Input.variable", mod= model1)


#makes the effect sie into a data frame. After you do everything below, take the data from this file and plot a line in excel to get the line equation.
effects.model1.data <- as.data.frame(effects.model1)


#plots the effect sie data with your model and the confidence interval surounding the fit of your line.
effects.model1.plot <- ggplot() +
  #2
  geom_point(data=your.data, aes(Input.variable, Outcome.variable)) +
  #3
  geom_point(data=effects.host.mod_dat, aes(x=Input.variable, y=fit), color="darkgray") +
  #4
  geom_line(data=effects.host.mod_dat, aes(x=Input.variable, y=fit), color="darkgray") +
  #5
  geom_ribbon(data=effects.host.mod_dat , aes(x=Input.variable, ymin=lower, ymax=upper), alpha= 0.3, fill="darkgray") +
  #6
  labs(x="x axis label", y="y axis label")


#changes model plot name to a new name
Plot1 <- effects.host.mod_plot


#opens your plot so you can view it
Plot1


#computes a p-value for your line. Did your treatment have a significant effect?
Anova(model1, confint = TRUE)


#computes confidence interval you need to report for your data
confint(model1, method = "Wald", level=0.95)
