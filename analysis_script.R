
#clear the environment
rm(list = ls())

#disable scientific notation
options(scipen=999)

#install packages
library(pacman)
p_load(ggplot2, readxl, janitor, dplyr, ggthemes, tidyr, ggpubr, lubridate, cowplot, MASS, rsq, zoo, sjPlot, psych, lmtest, here, car, geepack, nortest)

#load data
#Time variable has been added to the dataframe to capture natural growth in RF over time
#Added a documentary * time variable for each documentary to capture months since documentary was released
romance_fraud <- read_excel(here("data/romance_fraud_time.xlsx")) %>% clean_names()

#make sure R treats month variable as a date
romance_fraud$month <- as.Date(romance_fraud$month)
class(romance_fraud$month)

#descriptive analysis for outcome variable, dating scam
summary(romance_fraud$dating_scam)
hist(romance_fraud$dating_scam, breaks = 50)

#descriptive analysis for variable google_trends
summary(romance_fraud$google_trends)
hist(romance_fraud$google_trends, breaks = 50)

#descriptive analysis for variable news_articles
summary(romance_fraud$news_articles)
hist(romance_fraud$news_articles, breaks = 50)

#combine all documentary measures together to have a single measure of combined influence of documentaries
romance_fraud <- romance_fraud %>%
  mutate(doc = ifelse(doc_dirty_john == 1 | doc_love_fraud == 1 |
                        doc_puppet_master == 1 |doc_the_tinder_swindler == 1 |
                        doc_bad_vegan == 1, 1, 0),
         doc_sum = doc_dirty_john + doc_love_fraud + doc_puppet_master +
           doc_the_tinder_swindler + doc_bad_vegan)

#calculate moving averages for plotting

#define the window size for the moving average
window_size <- 6

#calculate the moving average
romance_fraud <- romance_fraud %>%
  arrange(month) %>%
  mutate(moving_avg_dating_scam = rollmean(dating_scam, window_size, fill = NA, align = "center"),
         moving_avg_news_articles = rollmean(news_articles, window_size, fill = NA, align = "center"),
         moving_avg_google_trends = rollmean(google_trends, window_size, fill = NA, align = "center"))

#calculate standard error and 95% confidence interval
romance_fraud <- romance_fraud %>%
  mutate(se_dating_scam = sd(dating_scam, na.rm = TRUE) / sqrt(window_size),
         lower_ci_dating_scam = moving_avg_dating_scam - 1.96 * se_dating_scam,
         upper_ci_dating_scam = moving_avg_dating_scam + 1.96 * se_dating_scam,
         se_news_articles = sd(news_articles, na.rm = TRUE) / sqrt(window_size),
         lower_ci_news_articles = moving_avg_news_articles - 1.96 * se_news_articles,
         upper_ci_news_articles = moving_avg_news_articles + 1.96 * se_news_articles,
         se_google_trends = sd(google_trends, na.rm = TRUE) / sqrt(window_size),
         lower_ci_google_trends = moving_avg_google_trends - 1.96 * se_google_trends,
         upper_ci_google_trends = moving_avg_google_trends + 1.96 * se_google_trends)

#create data frame for plotting documentaries
doc_data <- data.frame(
  documentary = c('Dirty John', 'Love Fraud', 'Puppet Master', 'The Tinder Swindler', 'Bad Vegan'),
  start_date = as.Date(c('2019-02-01', '2020-09-01', '2022-01-01', '2022-02-01', '2022-03-01')),
  end_date = as.Date(c('2019-05-01', '2020-12-01', '2022-04-01', '2022-05-01', '2022-06-01')),
  y_position = c(1, 1, 1, 2, 3)  # Adjust this to avoid overlapping labels
)

#plots

#line graphs
documentaries_plot <- ggplot(doc_data, aes(x = start_date, xend = end_date, y = y_position, yend = y_position)) +
  geom_segment(arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  geom_text(aes(label = documentary), vjust = -0.5, hjust = 0, size = 3.5, color = "black") +
  scale_y_continuous(limits = c(0, 8)) +  # Remove y-axis ticks and labels
  scale_x_date(limits = as.Date(c('2014-04-01', '2024-01-01')), date_breaks = "1 year", date_labels = "%Y") +  # Set date limits and format
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("(a) Dates of release of romance fraud documentaries")

dating_scam_plot <- ggplot(romance_fraud, aes(x = month, y = dating_scam)) +
  geom_line(color = "black") +
  geom_line(aes(y = moving_avg_dating_scam), color = "blue", size = 0.8) +  # Moving average line
  geom_ribbon(aes(ymin = lower_ci_dating_scam, ymax = upper_ci_dating_scam), fill = "blue", alpha = 0.2) +  # Confidence interval ribbon
  labs(title = "(b) Reported cases of romance fraud (2014-2024)",
       x = NULL, y = NULL) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

google_trends_plot <- ggplot(romance_fraud, aes(x = month, y = google_trends)) +
  geom_line(color = "black") +
  geom_line(aes(y = moving_avg_google_trends), color = "blue", size = 0.8) +  # Moving average line
  geom_ribbon(aes(ymin = lower_ci_google_trends, ymax = upper_ci_google_trends), fill = "blue", alpha = 0.2) +  # Confidence interval ribbon
  labs(title = "(c) Google searches for romance fraud (2014-2024)",
       x = NULL, y = NULL) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

news_articles_plot <- ggplot(romance_fraud, aes(x = month, y = news_articles)) +
  geom_line(color = "black") +
  geom_line(aes(y = moving_avg_news_articles), color = "blue", size = 0.8) +  # Moving average line
  geom_ribbon(aes(ymin = lower_ci_news_articles, ymax = upper_ci_news_articles), fill = "blue", alpha = 0.2) +  # Confidence interval ribbon
  labs(title = "(d) News articles on romance fraud cases (2014-2024)",
       x = NULL, y = NULL) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggarrange(documentaries_plot, dating_scam_plot, google_trends_plot, news_articles_plot)

#ggsave(here('plots/trends.jpg'), width = 13, height = 6)

#create a unique identifier for each row
romance_fraud <- romance_fraud %>%
  mutate(id = 1:n())

#scale all variables to standardise coefficients so they can be compared
romance_fraud.s <- romance_fraud %>%
  mutate(
    doc_dirty_john = scale(doc_dirty_john),
    doc_love_fraud = scale(doc_love_fraud),
    doc_puppet_master = scale(doc_puppet_master),
    doc_the_tinder_swindler = scale(doc_the_tinder_swindler),
    doc_bad_vegan = scale(doc_bad_vegan),
    google_trends.s = scale(google_trends),
    news_articles.s = scale(news_articles),
    doc = scale(doc),
    doc_sum = scale(doc_sum),
    spring = scale(spring),
    summer = scale(summer),
    autumn = scale(autumn),
    winter = scale(winter),
    covid = scale(covid),
    t = scale(t)
    )

#Model 1 Poisson regression via generalized estimating equations (GEE) with AR(1) - effect of documentaries combined on recorded RF
model.1.gee <- geeglm(dating_scam ~ t + doc_sum + spring + summer + autumn + covid + doc_sum*t,
                  data = romance_fraud.s,
                  family = poisson,
                  corstr = "ar1",
                  id = id)

summary(model.1.gee)
QIC(model.1.gee)
sum(residuals(model.1.gee, type = "pearson")^2) / model.1.gee$df.residual

#Model 1 sensitivity: Negative Binomial
model.1.nb <- glm.nb(dating_scam ~ t + doc_sum +
                 spring + summer + autumn + covid + doc_sum*t,
               data = romance_fraud.s)

summary(model.1.nb)

#estimate model 1 without scaling to enable predictions
model.1.nb.ns <- glm.nb(dating_scam ~ t + doc_sum +
                       spring + summer + autumn + covid + doc_sum*t,
                     data = romance_fraud)

#Model 2 Poisson regression via generalized estimating equations (GEE) with AR(1) - effect of combined documentaries on news articles
model.2.gee <- geeglm(news_articles ~ t + doc_sum +
                    spring + summer + autumn + covid + doc_sum*t,
                  data = romance_fraud.s,
                  family = poisson,
                  corstr = "ar1",
                  id = id)

summary(model.2.gee)
QIC(model.2.gee)
sum(residuals(model.2.gee, type = "pearson")^2) / model.2.gee$df.residual

#Model 2 sensitivity: Negative Binomial
model.2.nb <- glm.nb(news_articles ~ t + doc_sum +
                spring + summer + autumn + covid + doc_sum*t,
               data = romance_fraud.s)

summary(model.2.nb)

#Model 3 Poisson regression via generalized estimating equations (GEE) with AR(1) - effect of documentaries combined on google trends
model.3.gee <- geeglm(google_trends ~ t + doc_sum +
                    spring + summer + autumn + covid + doc_sum*t,
                  data = romance_fraud.s,
                  family = poisson,
                  corstr = "ar1",
                  id = id)

summary(model.3.gee)
QIC(model.3.gee)
sum(residuals(model.3.gee, type = "pearson")^2) / model.3.gee$df.residual

#Model 3 sensitivity: Negative Binomial
model.3.nb <- glm.nb(google_trends ~ t + doc_sum +
                       spring + summer + autumn + covid + doc_sum*t,
                     data = romance_fraud.s)

summary(model.3.nb)

#Model 4 Poisson regression via generalized estimating equations (GEE) with AR(1) - google trends and news on recorded RF
model.4.gee <- geeglm(dating_scam ~ t + doc_sum + google_trends.s + news_articles.s +
                        spring + summer + autumn + covid + doc_sum*t + google_trends.s*doc_sum +
                        news_articles.s*doc_sum,
                      data = romance_fraud.s,
                      family = poisson,
                      corstr = "ar1",
                      id = id)

summary(model.4.gee)
QIC(model.4.gee)
sum(residuals(model.4.gee, type = "pearson")^2) / model.4.gee$df.residual

#Model 4 sensitivity: Negative Binomial
model.4.nb <- glm.nb(dating_scam ~ t + doc_sum + google_trends.s + news_articles.s +
                 spring + summer + autumn + covid + doc_sum*t + google_trends.s*doc_sum +
                 news_articles.s*doc_sum,
               data = romance_fraud.s)

summary(model.4.nb)

#now estimate models for each documentary separately

#Model 5 Poisson regression via generalized estimating equations (GEE) with AR(1) - Dirty John on recorded RF
model.5.gee <- geeglm(dating_scam ~ t +
                     doc_dirty_john + doc_dirty_john*t +
                     spring + summer + autumn + covid,
                   data = romance_fraud.s,
                   family = poisson,
                   corstr = "ar1",
                   id = id)

summary(model.5.gee)
QIC(model.5.gee)
sum(residuals(model.5.gee, type = "pearson")^2) / model.5.gee$df.residual

#Model 5 sensitivity: Negative Binomial
model.5.nb <- glm.nb(dating_scam ~ t +
                             doc_dirty_john + doc_dirty_john*t +
                             spring + summer + autumn + covid,
                           data = romance_fraud.s)

summary(model.5.nb)

#Model 6 Poisson regression via generalized estimating equations (GEE) with AR(1) - Love Fraud on recorded RF
model.6.gee <- geeglm(dating_scam ~ t +
                     doc_love_fraud + doc_love_fraud*t +
                     spring + summer + autumn + covid,
                   data = romance_fraud.s,
                   family = poisson,
                   corstr = "ar1",
                   id = id)

summary(model.6.gee)
QIC(model.6.gee)
sum(residuals(model.6.gee, type = "pearson")^2) / model.6.gee$df.residual

#Model 6 sensitivity: Negative Binomial
model.6.nb <- glm.nb(dating_scam ~ t +
                  doc_love_fraud + doc_love_fraud*t +
                  spring + summer + autumn + covid,
                data = romance_fraud.s)

summary(model.6.nb)

#Model 7 Poisson regression via generalized estimating equations (GEE) with AR(1) - Puppet Master on recorded RF
model.7.gee <- geeglm(dating_scam ~ t +
                     doc_puppet_master + doc_puppet_master*t +
                     spring + summer + autumn + covid,
                   data = romance_fraud.s,
                   family = poisson,
                   corstr = "ar1",
                   id = id)

summary(model.7.gee)
QIC(model.7.gee)
sum(residuals(model.7.gee, type = "pearson")^2) / model.7.gee$df.residual

#Model 7 sensitivity: Negative Binomial
model.7.nb <- glm.nb(dating_scam ~ t +
                  doc_puppet_master + doc_puppet_master*t +
                  spring + summer + autumn + covid,
                data = romance_fraud.s)

summary(model.7.nb)

#Model 8 Poisson regression via generalized estimating equations (GEE) with AR(1) - Tinder Swindler on recorded RF
model.8.gee <- geeglm(dating_scam ~ t +
                     doc_the_tinder_swindler + doc_the_tinder_swindler*t +
                     spring + summer + autumn + covid,
                   data = romance_fraud.s,
                   family = poisson,
                   corstr = "ar1",
                   id = id)

summary(model.8.gee)
QIC(model.8.gee)
sum(residuals(model.8.gee, type = "pearson")^2) / model.8.gee$df.residual

#Model 8 sensitivity: Negative Binomial
model.8.nb <- glm.nb(dating_scam ~ t +
                  doc_the_tinder_swindler + doc_the_tinder_swindler*t +
                  spring + summer + autumn + covid,
                data = romance_fraud.s)

summary(model.8.nb)

#Model 9 Poisson regression via generalized estimating equations (GEE) with AR(1) - Bad Vegan on recorded RF
model.9.gee <- geeglm(dating_scam ~ t +
                     doc_bad_vegan + doc_bad_vegan*t +
                     spring + summer + autumn + covid,
                   data = romance_fraud.s,
                   family = poisson,
                   corstr = "ar1",
                   id = id)

summary(model.9.gee)
QIC(model.9.gee)
sum(residuals(model.9.gee, type = "pearson")^2) / model.9.gee$df.residual

#Model 9 sensitivity: Negative Binomial
model.9.nb <- glm.nb(dating_scam ~ t +
                  doc_bad_vegan + doc_bad_vegan*t +
                  spring + summer + autumn + covid,
                data = romance_fraud.s)

summary(model.9.nb)

#print models
tab_model(
  model.1.gee,
  model.2.gee,
  model.3.gee,
  dv.labels = c("Model 1 - Reported romance fraud", "Model 2 - News articles", "Model 3 - Google searches"),
  pred.labels = c("(Intercept)", "Time", "Documentaries", "Spring", "Summer", "Autumn", "COVID",
                  "Time x Documentaries"),
  #transform = NULL,
  show.r2 = TRUE, show.ngroups = FALSE, show.aic = TRUE, show.loglik = TRUE,
  file = here("plots/tab_models1to3.doc"))

rsq(model.1.gee) #Pseudo R2
rsq(model.2.gee) #Pseudo R2
rsq(model.3.gee) #Pseudo R2

QIC(model.1.gee)
QIC(model.2.gee)
QIC(model.3.gee)

tab_model(model.4.gee,
          dv.labels = c("Model 4 - Reported romance fraud"),
          pred.labels = c("(Intercept)", "Time", "Documentaries", "Google searches", "News articles", "Spring", "Summer", "Autumn", "COVID",
                          "Time x Documentaries", "Documentaries x Google searches", "Documentaries x News articles"),
          show.r2 = TRUE, show.ngroups = FALSE, show.aic = TRUE, show.loglik = TRUE,
          file = here("plots/tab_model4.doc"))

rsq(model.4.gee) #Pseudo R2
QIC(model.4.gee)

tab_model(model.5.gee, model.6.gee, model.7.gee, model.8.gee, model.9.gee,
          dv.labels = c("(M5) Dirty John", "(M6) Love Fraud", "(M7) Puppet Master", "(M8) Tinder Swindler", "(M9) Bad Vegan"),
          pred.labels = c("(Intercept)", "Time", "Documentary", "Spring", "Summer", "Autumn", "COVID",
                          "Time x Documentary", "Documentary", "Time x Documentary", "Documentary", "Time x Documentary",
                          "Documentary", "Time x Documentary", "Documentary", "Time x Documentary"),
          show.r2 = TRUE, show.ngroups = FALSE, show.aic = TRUE, show.loglik = TRUE, show.ci = FALSE,
          file = here("plots/tab_model5to9.doc"))

rsq(model.5.gee) #Pseudo R2
rsq(model.6.gee) #Pseudo R2
rsq(model.7.gee) #Pseudo R2
rsq(model.8.gee) #Pseudo R2
rsq(model.9.gee) #Pseudo R2

QIC(model.5.gee)
QIC(model.6.gee)
QIC(model.7.gee)
QIC(model.8.gee)
QIC(model.9.gee)

tab_model(
  model.1.nb,
  model.2.nb,
  model.3.nb,
  dv.labels = c("Model 1 - Reported romance fraud", "Model 2 - News articles", "Model 3 - Google searches"),
  pred.labels = c("(Intercept)", "Time", "Documentaries", "Spring", "Summer", "Autumn", "COVID",
                  "Time x Documentaries"),
  #transform = NULL,
  show.r2 = TRUE, show.ngroups = FALSE, show.aic = TRUE, show.loglik = TRUE,
  file = here("plots/tab_models1to3_nb.doc"))

rsq(model.1.nb) #Pseudo R2
rsq(model.2.nb) #Pseudo R2
rsq(model.3.nb) #Pseudo R2

tab_model(model.4.nb,
          dv.labels = c("Model 4 - Reported romance fraud"),
          pred.labels = c("(Intercept)", "Time", "Documentaries", "Google searches", "News articles", "Spring", "Summer", "Autumn", "COVID",
                          "Time x Documentaries", "Documentaries x Google searches", "Documentaries x News articles"),
          show.r2 = TRUE, show.ngroups = FALSE, show.aic = TRUE, show.loglik = TRUE,
          file = here("plots/tab_model4_nb.doc"))

rsq(model.4.nb) #Pseudo R2

tab_model(model.5.nb, model.6.nb, model.7.nb, model.8.nb, model.9.nb,
          dv.labels = c("(M5) Dirty John", "(M6) Love Fraud", "(M7) Puppet Master", "(M8) Tinder Swindler", "(M9) Bad Vegan"),
          pred.labels = c("(Intercept)", "Time", "Documentary", "Spring", "Summer", "Autumn", "COVID",
                          "Time x Documentary", "Documentary", "Time x Documentary", "Documentary", "Time x Documentary",
                          "Documentary", "Time x Documentary", "Documentary", "Time x Documentary"),
          show.r2 = TRUE, show.ngroups = FALSE, show.aic = TRUE, show.loglik = TRUE, show.ci = FALSE,
          file = here("plots/tab_model5to9_nb.doc"))

rsq(model.5.nb) #Pseudo R2
rsq(model.6.nb) #Pseudo R2
rsq(model.7.nb) #Pseudo R2
rsq(model.8.nb) #Pseudo R2
rsq(model.9.nb) #Pseudo R2
