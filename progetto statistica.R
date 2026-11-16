
# CODICE COMPLETO (NASCAREA e staciv) 


rm(list = ls())
set.seed(1234)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

packages <- c("factoextra", "lattice", "cluster", "car", "lmtest", "ROCR", "ggplot2")
invisible(lapply(packages, install_if_missing))

# 1) CARICAMENTO DATI 
file_path <- file.choose()   
load(file_path)

print(ls())

# Conversioni richieste
m4$NASCAREA <- as.factor(m4$NASCAREA)
m4$staciv   <- as.factor(m4$staciv)

# ===============================
# 2) REGRESSIONE LINEARE
# ===============================
mod_lin <- lm(YL ~ NASCAREA + staciv, data = m4)
print(summary(mod_lin))

# Diagnostica
op <- par(no.readonly = TRUE)
par(mfrow = c(1,3), mar = c(4,4,2,1))
plot(mod_lin$fitted.values, resid(mod_lin),
     main = "Residui vs Predetti", xlab = "Predetti", ylab = "Residui")
abline(h = 0, col = "red")
resid_std <- rstudent(mod_lin)
plot(mod_lin$fitted.values, resid_std,
     main = "Omoschedasticità", xlab = "Predetti", ylab = "Residui Std")
abline(h = 0, col = "red")
qqnorm(resid(mod_lin), main = "Q-Q residui")
qqline(resid(mod_lin), col = "red")
par(op)

# ===============================
# 3) REGRESSIONE LOGISTICA
# ===============================
if (!("occupato" %in% names(m4)) && ("nesplav" %in% names(m4))) {
  m4$occupato <- ifelse(m4$nesplav == 1, 1, 0)
}

m4_clean <- na.omit(m4[, c("occupato", "NASCAREA", "staciv")])
m4_clean$occupato <- as.integer(m4_clean$occupato)

mod_log <- glm(occupato ~ NASCAREA + staciv, data = m4_clean, family = binomial)
print(summary(mod_log))
cat("Odds Ratio:\n"); print(exp(coef(mod_log)))

prob_pred   <- predict(mod_log, type = "response")
classe_pred <- ifelse(prob_pred >= 0.5, 1, 0)

confusione <- table(
  Predetto  = factor(classe_pred, levels = c(0,1)),
  Osservato = factor(m4_clean$occupato, levels = c(0,1))
)
print(confusione)

tp <- confusione["1","1"]; tn <- confusione["0","0"]
fp <- confusione["1","0"]; fn <- confusione["0","1"]

accuracy    <- (tp + tn) / sum(confusione)
sensibilita <- ifelse((tp + fn) > 0, tp / (tp + fn), NA)
specificita <- ifelse((tn + fp) > 0, tn / (tn + fp), NA)

cat("Accuratezza:", round(accuracy * 100, 2), "%\n")
cat("Sensibilità:", ifelse(is.na(sensibilita), "NA", paste0(round(sensibilita * 100, 2), "%")), "\n")
cat("Specificità:", ifelse(is.na(specificita), "NA", paste0(round(specificita * 100, 2), "%")), "\n")

pred_roc <- ROCR::prediction(prob_pred, m4_clean$occupato)
perf_roc <- ROCR::performance(pred_roc, "tpr", "fpr")
plot(perf_roc, colorize = TRUE, main = "Curva ROC - Logistica",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(a = 0, b = 1, lty = 2, col = "gray")
auc <- ROCR::performance(pred_roc, "auc")@y.values[[1]]
cat("AUC:", round(auc, 3), "\n")

# ===============================
# 4) BOXPLOT
# ===============================
op <- par(no.readonly = TRUE)
par(mfrow = c(1,2), mar = c(4,4,2,1))
boxplot(YL ~ NASCAREA, data = m4,
        main = "YL per NASCAREA", xlab = "NASCAREA", ylab = "YL")
boxplot(YL ~ staciv, data = m4,
        main = "YL per Stato civile", xlab = "staciv", ylab = "YL")
par(op)

# ===============================
# 5) CLUSTER ANALYSIS
# ===============================
m4_clu <- na.omit(m4[, c("NASCAREA", "staciv")])
X <- model.matrix(~ NASCAREA + staciv - 1, data = m4_clu)
nzv <- apply(X, 2, sd) > 0
X <- X[, nzv, drop = FALSE]
Xsc <- scale(X)

factoextra::fviz_nbclust(Xsc, kmeans, method = "wss") +
  ggplot2::ggtitle("Metodo del Gomito (solo NASCAREA, staciv)")

factoextra::fviz_nbclust(Xsc, kmeans, method = "silhouette") +
  ggplot2::ggtitle("Metodo della Silhouette (solo NASCAREA, staciv)")

set.seed(1234)
km <- kmeans(Xsc, centers = 3, iter.max = 100, nstart = 100)

factoextra::fviz_cluster(
  km,
  data = Xsc,
  geom = "point",
  ellipse = FALSE,
  show.clust.cent = FALSE,
  palette = "Set2",
  ggtheme = ggplot2::theme_minimal(),
  main = "Cluster (solo NASCAREA e staciv)"
)

cluster_labels <- data.frame(m4_clu, cluster = factor(km$cluster))
print(head(cluster_labels))

