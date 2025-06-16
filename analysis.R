# monthly data party level

install.packages("dplyr") 
install.packages("sjPlot")
install.packages ("sjmisc")
install.packages ("ggplot2")
install.packages("broom.mixed")
install.packages("glmmTMB")
install.packages("stargazer")



library(sjPlot)
library(sjmisc)
library(ggplot2)
library(broom.mixed)
library(glmmTMB)
library(ggplot2)
library(dplyr)
library(stargazer)




#data preparation
toxic_parliamentarians<-toxic_aggregated_party_min_10_per_month

toxic_parliamentarians$mf_party_family <- as.factor(toxic_parliamentarians$mf_party_family)

toxic_parliamentarians$mf_party_family <- recode(toxic_parliamentarians$mf_party_family, "10" = "Eco", "20" = "Left", "30" = "SD", "40" = "Lib",
                                                                            "50" = "CD", "60" = "Cons", "70" = "RR", "80" = "Agr",
                                                                            "90" = "Eth", "95" = "SI")
toxic_parliamentarians <- toxic_parliamentarians %>%
  mutate(
    is_in_cabinet = factor(recode(as.character(is_in_cabinet), "0" = "no", "1" = "yes"), levels = c("no", "yes")),
    is_election = factor(recode(as.character(is_election), "0" = "no", "1" = "yes"), levels = c("no", "yes")),
    during_covid_chock = factor(recode(as.character(during_covid_chock), "0" = "no", "1" = "yes"), levels = c("no", "yes"))
  )
toxic_parliamentarians$country <- as.factor(toxic_parliamentarians$country)

toxic_parliamentarians$ toxicity_mean_lt <- log(toxic_parliamentarians$ toxicity_mean / (1 - toxic_parliamentarians$ toxicity_mean))


#Model 

model1<- lm(toxicity_mean_lt ~ month_number + is_election + is_in_cabinet + mf_party_family + during_covid_chock + country,
           data = toxic_parliamentarians)

summary(model1)

# fig1
fig1 <- plot_model(model1, 
                   type = "pred", 
                   terms = c("mf_party_family"), 
                   title = "") +  
  labs(x = "Party Family", y = "Toxicity")
ggsave("fig1.png", plot = fig1, width = 8, height = 6, dpi = 300)

# fig2
fig2 <- plot_model(model1, 
                   type = "pred", 
                   terms = c("month_number"), 
                   title = "") + 
  labs(x = "Month", y = "Toxicity")
ggsave("fig2.png", plot = fig2, width = 8, height = 6, dpi = 300)

# fig3
fig3 <- plot_model(model1, 
                   type = "est", 
                   show.values = TRUE, 
                   value.offset = 0.3,
                   terms = c("is_election [yes]", "is_in_cabinet [yes]", "during_covid_chock [yes]"),
                   title = "")
ggsave("fig3.png", plot = fig3, width = 8, height = 6, dpi = 300)

#supplementary material

# histogram 1
png("hist1.png", width = 800, height = 600)
hist(toxic_parliamentarians$toxicity_mean, 
     col = "lightblue", 
     border = "black", 
     main = " ", 
     xlab = "Variable", 
     ylab = "Frequency")
dev.off()

# histogram 2
png("hist2.png", width = 800, height = 600)
hist(toxic_parliamentarians$toxicity_mean_lt, 
     col = "lightblue", 
     border = "black", 
     main = " ", 
     xlab = "Variable", 
     ylab = "Frequency")
dev.off()


stargazer(model1, type = "latex", title = "Linear Regression", out = "regression_results.tex")

stargazer(model1, type = "latex", title = "Linear", omit = "country")


##beta regression

model_beta <- glmmTMB(
  toxicity_mean~ is_election +is_in_cabinet + mf_party_family + during_covid_chock + month_number + country,
  data = toxic_parliamentarians,
  family = beta_family(link = "logit")  
)

summary(model_beta)

#figs1

figs1 <- plot_model(model_beta, 
                    type = "pred", 
                    terms = c("mf_party_family"), 
                    title = "") +  
  labs(x = "Party Family", y = "Toxicity")

# figs2
figs2 <- plot_model(model_beta, 
                    type = "pred", 
                    terms = c("month_number"), 
                    title = "") + 
  labs(x = "Month", y = "Toxicity")

#  figs3
figs3 <- plot_model(model_beta, 
                    type = "est", 
                    show.values = TRUE, 
                    value.offset = 0.3,
                    terms = c("is_election [yes]", "is_in_cabinet [yes]", "during_covid_chock [yes]"),
                    title = "")


ggsave("figs1.png", plot = figs1, width = 8, height = 6, dpi = 300)
ggsave("figs2.png", plot = figs2, width = 8, height = 6, dpi = 300)
ggsave("figs3.png", plot = figs3, width = 8, height = 6, dpi = 300)


