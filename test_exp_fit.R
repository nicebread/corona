source("helpers.R", local=TRUE)
source("download_data.R", local=TRUE)
source("preprocess_data.R", local=TRUE)

# helper function for exponential reference line
growth <- function(x, percGrowth=33, intercept=100) {intercept*(1 + percGrowth/100)^(x-1)}
growth_m1 <- function(x, intercept, slope) {intercept*(slope^(x-1))}

# Extract some data for testing the function
df1 <- dat_ECDC %>% filter(country %in% c("Germany"), cum_cases > 50) %>% mutate(day_since_start = 1:n())
df2 <- dat_ECDC %>% filter(country %in% c("Spain", "USA"), cum_cases > 50) %>% mutate(day_since_start = 1:n())

lm1 <- lm(log(cum_cases) ~ 1 + I(day_since_start-1), data=df1)
summary(lm1)

nls1 <- nls(cum_cases ~ intercept*(1+b)^day_since_start, start = c(b = 0.30, intercept = 50), data=df1)
summary(nls1)

df1$PRED_lm1 <- exp(predict(lm1))
df1$PRED_nls1 <- predict(nls1)

p1 <- ggplot(df1, aes(x=day_since_start, y=cum_cases)) + 
	geom_point(color="red") + 
	geom_point(aes(y=PRED_lm1), color="blue", shape=3) +
	geom_point(aes(y=PRED_nls1), color="green", shape=3)
p1


# recover fit from lm
p1 <- ggplot(df1, aes(x=day_since_start, y=cum_cases)) + 
	geom_point(color="red") + 
	geom_point(aes(y=PRED_lm1), color="blue", shape=3) + 
	stat_function(fun = growth, args=list(percGrowth=(exp(coef(lm1)[2])-1)*100, intercept=exp(coef(lm1)[1])), color="black", linetype="dashed")
p1

p1 + coord_trans(y = "log10")


plot(day, log(cases))
points(day, predict(lm1), col="red")
points(day, log(predict(nls1)), col="green")

# -> lm fit much better

# multiple countries

library(lme4)

# random intercept, common slope
lmer1 <- lmer(log(cum_cases) ~ 1 + day_since_start + (1|country), data=df)
summary(lmer1)

df$fixef_PRED <- exp(predict(lmer1, re.form=NA))
df$ranef_PRED <- exp(predict(lmer1, re.form= ~(1|country)))

p1 <- ggplot(df, aes(x=day_since_start, y=cum_cases, shape=country, color=country)) + geom_point()
p1 <- p1 + geom_point(aes(y=fixef_PRED), size=4, color="blue")
p1

p1 <- ggplot(df, aes(x=day_since_start, y=cum_cases, shape=country, color=country)) + geom_point() +
	geom_point(aes(y=ranef_PRED), shape=3)
p1





# random intercept, random slope
lmer2 <- lmer(log(cum_cases) ~ 1 + day_since_start + (day_since_start|country), data=df)
summary(lmer2)

df$fixef_PRED2 <- exp(predict(lmer2, re.form=NA))
df$ranef_PRED2 <- exp(predict(lmer2))

p1 <- ggplot(df, aes(x=day_since_start, y=cum_cases, shape=country, color=country)) + geom_point()
p1 <- p1 + geom_point(aes(y=fixef_PRED2), size=4, color="blue")
p1

p1 <- ggplot(df, aes(x=day_since_start, y=cum_cases, shape=country, color=country)) + geom_point() +
	geom_point(aes(y=ranef_PRED2), shape=3)
p1


# estimate growth curve
estimate_exponential_curves <- function(df, target="cum_cases", random_slopes=FALSE) {
	df$day_since_start_m1 <- df$day_since_start - 1
	n_countries <- length(unique(df$country))
	
	if (n_countries == 1) {
		# single country
		f <- formula(paste0("log(", target, ") ~ 1 + day_since_start_m1"))
		fit <- lm(f, data=df)
		intercept <- coef(fit)[1]
		slope <- coef(fit)[2]
		RE <- NULL
	} else {
		# multiple country: do mixed effects model
		if (random_slopes == TRUE) {
			f <- formula(paste0("log(", target, ") ~ 1 + day_since_start_m1 + (1 + day_since_start_m1 | country)"))
		} else {
			f <- formula(paste0("log(", target, ") ~ 1 + day_since_start_m1 + (1 | country)"))
		}
	
	  fit <- lmer(f, data=df)
		intercept <- fixef(fit)[1]
		slope <- fixef(fit)[2]
		RE <- coef(fit)$country
	}
	
  return(list(n_countries = n_countries, fit=fit, intercept=intercept, slope=slope, RE=RE))
}


f1 <- estimate_exponential_curves(df2, target="cum_cases", random_slopes=TRUE)
summary(f1$fit)

# multiple random effects regression lines
p1 <- ggplot(df2, aes(x=day_since_start, y=cum_cases, shape=country, color=country)) + 
	geom_point() +
	stat_function(fun = growth_m1, args=list(slope=exp(fixef(f1$fi)[2]), intercept=exp(fixef(f1$fi)[1])), color="black", linetype="dashed")

RE <- coef(f1$fi)$country
for (co in 1:nrow(RE)) {
	p1 <- p1 + stat_function(fun = growth_m1, args=list(slope=exp(RE[co, 2]), intercept=exp(RE[co, 1])), color="red", linetype="dotted")
}
p1
