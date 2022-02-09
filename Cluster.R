library(purrr)
library(ggplot2)
library(dplyr)
library(writexl)

df <- churn_clean
df1 <- df[, c("Bandwidth_GB_Year", "MonthlyCharge", "Contract")]
df1$Contract <- ifelse(df1$Contract == "Month-to-month", 1,
                       ifelse(df1$Contract == "One year", 2, 3))
df1 <- data.frame(scale(df1))
summary(df1)
package <- df[, c("Bandwidth_GB_Year", "MonthlyCharge", "Contract", "Churn")]
package$Churn <- ifelse(package$Churn == "Yes", 1, 0)
#####
#BM CLUSTER
tot.ss.bm <- map_dbl(1:10, function(k) {
  kbm <- kmeans(df1[, c("Bandwidth_GB_Year", "MonthlyCharge")], centers = k)
  kbm$tot.withinss
})
elbow.bm <- data.frame(k = 1:10, tot.withinss = tot.ss.bm)
print(elbow.bm)
ggplot(elbow.bm, aes(x = k, y = tot.withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
set.seed(10)
bm <- kmeans(df1[, c("Bandwidth_GB_Year", "MonthlyCharge")],
                center = 4, nstart = 20)
ggplot(df1, aes(x = Bandwidth_GB_Year, y = MonthlyCharge,
              col = bm$cluster)) +
  geom_point() + 
  theme_bw() +
  scale_color_gradient(low = "black", high = "red")
(bm.accuracy <- bm$betweenss/bm$totss)

head(bm)
package <- mutate(package, BM.cluster = bm$cluster)
head(package)

#BM CLUSTER WITH CHURN RATES
#0.7311776
bm4 <- package[package$BM.cluster == "4", ]
mean(bm4$Churn)
#0.1343511
bm3 <- package[package$BM.cluster == "3", ]
mean(bm3$Churn)
#0.006953642
bm2 <- package[package$BM.cluster == "2", ]
mean(bm2$Churn)
#0.2888209
bm1 <- package[package$BM.cluster == "1", ]
mean(bm1$Churn)
#####
#MC CLUSTER
tot.ss.mc <- map_dbl(1:10, function(k) {
  kmc <- kmeans(df1[, c("MonthlyCharge", "Contract")], centers = k)
  kmc$tot.withinss
})
elbow.mc <- data.frame(k = 1:10, tot.withinss = tot.ss.mc)
print(elbow.mc)
ggplot(elbow.mc, aes(x = k, y = tot.withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
set.seed(10)
mc <- kmeans(df1[, c("MonthlyCharge", "Contract")],
             center = 3, nstart = 20)
ggplot(df1, aes(x = Contract, y = MonthlyCharge,
                col = mc$cluster)) +
  geom_point() + 
  theme_bw() +
  scale_color_gradient(low = "black", high = "red")
(mc.accuracy <- mc$betweenss/mc$totss)

package <- mutate(package, MC.cluster = mc$cluster)
head(package)

#MC CLUSTER WITH CHURN RATES
#0.2766808
mc3 <- package[package$MC.cluster == "3", ]
mean(mc3$Churn)
#0.06980017
mc2 <- package[package$MC.cluster == "2", ]
mean(mc2$Churn)
#0.5294358
mc1 <- package[package$MC.cluster == "1", ]
mean(mc1$Churn)
#####
#BC CLUSTER
tot.ss.bc <- map_dbl(1:10, function(k) {
  kbc <- kmeans(df1[, c("Bandwidth_GB_Year", "Contract")], centers = k)
  kbc$tot.withinss
})
elbow.bc <- data.frame(k = 1:10, tot.withinss = tot.ss.bc)
print(elbow.bc)
ggplot(elbow.bc, aes(x = k, y = tot.withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
set.seed(10)
bc <- kmeans(df1[, c("Bandwidth_GB_Year", "Contract")],
             center = 4, nstart = 20)
ggplot(df1, aes(x = Contract, y = Bandwidth_GB_Year,
                col = bc$cluster)) +
  geom_point() + 
  theme_bw() +
  scale_color_gradient(low = "black", high = "red")
(bc.accuracy <- bc$betweenss/bc$totss)

package <- mutate(package, BC.cluster = bc$cluster)
head(package)

#BC CLUSTER WITH CHURN RATES
#0.2617778
bc4 <- package[package$BC.cluster == "4", ]
mean(bc4$Churn)
#0.09594645
bc3 <- package[package$BC.cluster == "3", ]
mean(bc3$Churn)
#0.01176983
bc2 <- package[package$BC.cluster == "2", ]
mean(bc2$Churn)
#0.6418504
bc1 <- package[package$BC.cluster == "1", ]
mean(bc1$Churn)
#####
##clusters for best
  #bm - 2 - 0.0069
  #mc - 2 - 0.0698
  #bc - 2 - 0.0117
##clusters for worst
  #bm - 4 - 0.7312
  #mc - 1 - 0.5294
  #bc - 1 - 0.6419

head(package)
#BEST
best.package <- package[package$BM.cluster == '2', ]
best.package$Contract <- as.factor(best.package$Contract)
summary(best.package)
(Bandwidth.best <- range(best.package$Bandwidth_GB_Year))
(MonthlyCharge.best <- range(best.package$MonthlyCharge))
  #CONTRACT
#0.01211
cm.churn <- best.package[best.package$Contract == 'Month-to-month', ]
(cm.avg <- mean(cm.churn$Churn))
#0.00161
co.churn <- best.package[best.package$Contract == 'One year', ]
(co.avg <- mean(co.churn$Churn))
#0.0
ct.churn <- best.package[best.package$Contract == 'Two Year', ]
(ct.avg <- mean(ct.churn$Churn))

#WORST
worst.package <- package[package$BM.cluster == '4', ]
worst.package$Contract <- as.factor(worst.package$Contract)
summary(worst.package)
(Bandwidth.worst <- range(worst.package$Bandwidth_GB_Year))
(MonthlyCharge.worst <- range(worst.package$MonthlyCharge))
  #CONTRACT
#0.87716
cm.churn.w <- worst.package[worst.package$Contract == 'Month-to-month', ]
(cm.avg.w <- mean(cm.churn.w$Churn))
#0.57110
co.churn.w <- worst.package[worst.package$Contract == 'One year', ]
(co.avg.w <- mean(co.churn.w$Churn))
#0.52500
ct.churn.w <- worst.package[worst.package$Contract == 'Two Year', ]
(ct.avg.w <- mean(ct.churn.w$Churn))

#CHURN FOR BEST PACKAGE
  ##BANDWIDTH - 0.0576
b.churn <- package[package$Bandwidth_GB_Year >= 
                     min(best.package$Bandwidth_GB_Year), ]
(b.avg <- mean(b.churn$Churn))
  ##MONTHLYCHARGE - 0.1529
m.churn <- package[package$MonthlyCharge <= 
                     max(best.package$MonthlyCharge), ]
(m.avg <- mean(m.churn$Churn))
  ##CONTRACT - 0.1265
c.churn <- package[package$Contract == 'Two Year', ]
(c.avg <- mean(c.churn$Churn))

#CHURN FOR WORST PACKAGE
  ##BANDWIDTH - 0.4702026
b.churn.w <- package[package$Bandwidth_GB_Year <= 
                       max(worst.package$Bandwidth_GB_Year), ]
(b.avg.w <- mean(b.churn.w$Churn))
  ##MONTHLYCHARGE - 0.4347
m.churn.w <- package[package$MonthlyCharge >= 
                       min(worst.package$MonthlyCharge), ]
(m.avg.w <- mean(m.churn.w$Churn))
  ##CONTRACT - 0.3728
c.churn.w <- package[package$Contract == 'Month-to-month', ]
(c.avg.w <- mean(c.churn.w$Churn))

#OVERALL CHURN RATE
#0.00152 - (changed two year to one year since ty is 0)
best.churn <- package[package$Bandwidth_GB_Year >= 
                          min(best.package$Bandwidth_GB_Year) &
                        package$MonthlyCharge <= 
                          max(best.package$MonthlyCharge) &
                        package$Contract == 'One year', ]
(best.avg <- mean(best.churn$Churn))
#0.87737
worst.churn <- package[package$Bandwidth_GB_Year <= 
                         max(worst.package$Bandwidth_GB_Year) &
                        package$MonthlyCharge >= 
                          min(worst.package$MonthlyCharge) &
                        package$Contract == 'Month-to-month', ]
(worst.avg <- mean(worst.churn$Churn))


write_xlsx(package, "Package_cluster.xlsx")
write_xlsx(df1, "DF1_cluster.xlsx")
write_xlsx(best.package, "Low_Churn_cluster.xlsx")
write_xlsx(worst.package, "High_Churn_cluster.xlsx")