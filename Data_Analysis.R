rm(list = ls())
source("Packages.R")

data(framingham)

# Statistics

mean_ldl <- mean(framingham$LDLC, na.rm = T)
median_ldl <- median(framingham$LDLC, na.rm = T)
quartiles_ldl <- quantile(framingham$LDLC, na.rm = T, probs = c(0.25,0.5, 0.75))
var_ldl <- var(framingham$LDLC, na.rm = T)
sd_ldl <- sd(framingham$LDLC, na.rm = T)
range_ldl <- range(framingham$LDLC, na.rm = T)
iqr_ldl <- IQR(framingham$LDLC, na.rm = T)

# plot of distribution
p_hist <- ggplot(framingham, aes(x = LDLC)) + geom_histogram(binwidth = 10) +
  geom_vline(xintercept = mean_ldl, colour = "blue") +
  geom_vline(xintercept = median_ldl, colour = "red") + 
  xlab(label = "LDL-C [mg/dl]") + ylab(label = "")

# CIs Student's
n <- length(which(!is.na(framingham$LDLC)))
tQuant <- qt(p = 0.975, df = n-1)
ldl_SE <- sd_ldl/sqrt(n)

CI_upper <- mean_ldl + tQuant*ldl_SE
CI_lower <- mean_ldl - tQuant*ldl_SE

# Comparison of LDL-C in people who died
framingham <- framingham %>% mutate(DEATH = factor(DEATH))

p_ldl_death <- ggplot(framingham, aes(x = DEATH, y = LDLC)) + geom_boxplot() +
  xlab(label = "Died") + ylab("LDL-C serum concentration [mg/dl]") + theme_bw()

p_ldl_death_dist <- ggplot(framingham, aes(x = LDLC, fill = DEATH)) + geom_histogram(aes(y = after_stat(density)), colour = "white") +
  xlab("LDL-C serum concentration [mg/dl]") + ylab("Density") + facet_wrap(~DEATH) + theme_bw() 

# Do means differ significantly
t.test.ldl <- t.test(LDLC~DEATH, data = framingham)
