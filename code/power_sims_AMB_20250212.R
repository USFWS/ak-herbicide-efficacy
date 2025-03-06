### Evaluate sample size requirements for invasive species treatments in Southern Alaska
### Coefficient values based on 2023-3034 data, Cold Bay daisy #picked that scenario cause hardest to detect a change
###Anna-Marie Benson
library(tidyverse)
test
#Set parameters
n0 <- 120 #sample size before
n1 <- 120 #samples size after
N <- n0 + n1 #total sample size
lambda = 2 #average number of points with invasive
alpha = 0.10 #probability of type one statistical error
nsims = 1000
#from Apriori data
B0 = 1.0709
B1 = -0.241
#effect size
effects <- seq(from = 1.01, to = 1.75, by = 0.01)##effect size-percent change after treatment
#Run simulation
poisson.power.1cov <- function(alpha = 0.10,
                               nsims = NULL,
                               n0 = NULL,
                               n1 = NULL,
                               covariate = NULL,
                               B0 = NULL,
                               B1 = NULL,
                               possible.effects = NULL)
{
  #hold power estimates for each effect
  powers <- rep(NA, length(possible.effects))
  
  #treatment indicator
  treat <- c(rep(0, n0), rep(1, n1))
  
  for (i in 1:length(possible.effects))
    #loop over effects sizes
  {
    hold_power <- c()
    
    for (j in 1:nsims)
    {
      #linear predictor using assumed B0, B1 and the simulated covariate Y0
      lin_pred <- B0 + B1 * Y0 + log(possible.effects[i]) * treat
      
      #exponentiate
      mu <- exp(lin_pred)
      
      #simulate Y1 based on mu (so here we are creating n0+n1 Y1 values, each of which has a different mu)
      Y1 <- rpois(n = length(mu), lambda = mu)
      
      #fit model
      mod <- glm(as.integer(Y1) ~ as.integer(Y0) + treat, family = poisson)
      
      #extract p-value
      sum_mod <- summary(mod)
      hold_power <- rbind(hold_power, sum_mod$coefficients[3, 4])
      
    } #loop sims
    powers[i] <- mean(hold_power < alpha)
    
  } #loop effects
  
  return(powers)
  
}


#determined from aprior data
Y0 <- as.integer(rpois(N, lambda))
#run power analysis with fixed B0 and B1
powers <- poisson.power.1cov(
  alpha = alpha,
  n0 = n0,
  n1 = n1,
  nsims = nsims,
  covariate = Y0,
  B0 = B0,
  B1 = B1,
  possible.effects = effects
)

data <- data.frame(effects, powers)
#plot


n120120 <- ggplot(data = data, aes(x = effects, y = powers)) +
  geom_point() +
  geom_line(aes(y = 0.80), col = "blue") +
  theme_classic() +
  ggtitle("Power Simulations n0=120, n1=120") +
  xlab("Effect Size") +
  ylab("Power")

ggsave(
  "C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2025_Sampling/PowerSimResults_n120120_20250218.jpg",
  n120120,
  width = 4,
  height = 4
)
