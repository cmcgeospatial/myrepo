library(BiocManager)
BiocManager::install(c("BiocGenerics", "S4Vectors", "toxpiR"))

help(package = "toxpiR")

## S4 method for signature 'list,data.frame'
txpCalculateScores(
  model,
  input,
  id.var = NULL,
  rank.ties.method = c("average", "first", "last", "random", "max", "min"),
  negative.value.handling = c("keep", "missing")
)

## Load example dataset & model; see ?TxpModel for building model objects
data(txp_example_input, package = "toxpiR")
data(txp_example_model, package = "toxpiR")

## Calculate scores for single model; returns TxpResult object
res <- txpCalculateScores(model = txp_example_model,
                          input = txp_example_input,
                          id.var = "name")

## Calculate scores for list of models; returns TxpResultList object
txpCalculateScores(model = TxpModelList(m1 = txp_example_model,
                                        m2 = txp_example_model),
                   input = txp_example_input,
                   id.var = "name")

resLst <- txpCalculateScores(model = list(m1 = txp_example_model,
                                          m2 = txp_example_model),
                             input = txp_example_input,
                             id.var = "name")
## duplicated
duplicated(resLst)

## Coercion
as(list(resLst[[1]], resLst[[2]]), "TxpResultList")
as.TxpResultList(list(res1 = resLst[[1]], res2 = resLst[[2]]))

as(resLst[[1]], "TxpResultList")
as.TxpResultList(resLst[[1]])

library(grid)
plot(res)
plot(res[order(txpRanks(res))[1:4]])

library(ggplot2)
plot(res, package = "gg")
plot(res[order(txpRanks(res))], package = "gg", ncol = 5) +
  theme(legend.position = "bottom")

plot(res, txpRanks(res))
plot(res, txpRanks(res), pch = 16, size = unit(0.75, "char"))

## Will likely make inaccurate labels within a GUI, e.g. RStudio
## use png, pdf, etc. to get accurate labels
## Not run: 
tmpPdf <- tempfile()
pdf(tmpPdf)
plot(res, txpRanks(res), labels = c(10, 4, 2), pch = 16)
dev.off()

data(txp_example_input, package = "toxpiR")
data(txp_example_model, package = "toxpiR")
txp_example_input
txp_example_model

## Code to create txp_example_model
tf1 <- TxpTransFuncList(linear = function(x) x)
sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
                   s2 = TxpSlice("metric3"),
                   s3 = TxpSlice(sprintf("metric%d", 4:7),
                                 tf1[rep("linear", 4)]),
                   s4 = TxpSlice("metric8", tf1))
tf2 <- TxpTransFuncList(NULL, linear = function(x) x, NULL, NULL)
TxpModel(txpSlices = sl, txpWeights = c(2, 1, 3, 2), txpTransFuncs = tf2)

##End of example
##Beginning of analysis of Q1 2025 Det air sensor data

#q1_txp <- read.csv("C:/Users/christine.calleja/RCode/myrepo/c2025q1_aqi_summary_wide_ExportFeatures_table.csv")
q24_txp <- df24_wide_clean
q24_txp_mod <- q24_txp[, c(1,2,3,4,5,6,7,8,9,10,11)]
q1_txp_mod <- q1_txp[, c(1, 2, 3, 4, 5, 6, 7, 8)]

colnames(q24_txp_mod)
colnames(q1_txp_mod)

# creates toxpi model
tfq24 <- TxpTransFuncList(linear = function(x) x)
slq24 <- TxpSliceList(pm1 = TxpSlice("PM1_mean"),
                      pm2.5 = TxpSlice("PM2.5_mean"),
                      pm10 = TxpSlice("PM10_mean"),
                      o3 = TxpSlice("O3_mean"),
                      no2 = TxpSlice("NO2_mean"),
                      no = TxpSlice("NO_mean"),
                      co = TxpSlice("CO_mean"),
                      so2 = TxpSlice("SO2_mean", tfq24))
tf2q24 <- TxpTransFuncList(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
txp_q24_model <- TxpModel(
  txpSlices = slq24, 
  txpWeights = c(1, 1, 1, 1, 1, 1, 1, 1), 
  txpTransFuncs = tf2q24
)

# creates toxpi model
tfq1 <- TxpTransFuncList(linear = function(x) x)
slq1 <- TxpSliceList(pm2.5 = TxpSlice("PM25aqi"),
                   pm10 = TxpSlice("PM10aqi"),
                   o3 = TxpSlice("O3aqi"),
                   no2 = TxpSlice("NO2aqi"),
                   so2 = TxpSlice("SO2aqi", tf1))
tf2q1 <- TxpTransFuncList(NULL, linear = function(x) x, NULL, NULL, NULL)
txp_q1_model <- TxpModel(
  txpSlices = slq1, 
  txpWeights = c(1, 1, 1, 1, 1), 
  txpTransFuncs = tf2q1
)
# creates toxpi model
tfq1 <- TxpTransFuncList(linear = function(x) x)
slq1 <- TxpSliceList(pm2.5 = TxpSlice("PM25aqi"),
                   pm10 = TxpSlice("PM10aqi"),
                   o3 = TxpSlice("O3aqi"),
                   no2 = TxpSlice("NO2aqi"),
                   so2 = TxpSlice("SO2aqi", tf1))
tf2q1 <- TxpTransFuncList(NULL, linear = function(x) x, NULL, NULL, NULL)
txp_q1_model <- TxpModel(
  txpSlices = slq1, 
  txpWeights = c(1, 1, 1, 1, 1), 
  txpTransFuncs = tf2q1
)

## Calculate scores for single model; returns TxpResult object
res_q24 <- txpCalculateScores(model = txp_q24_model,
                             input = q24_txp_mod, #modified data
                             id.var = "Name")

## Calculate scores for single model; returns TxpResult object
res_q1 <- txpCalculateScores(model = txp_q1_model,
                             input = q1_txp_mod,
                             id.var = "Name")
## Calculate scores for single model; returns TxpResult object
res_q1 <- txpCalculateScores(model = txp_q1_model,
                             input = q1_txp_mod,
                             id.var = "Name")


## duplicated
#duplicated(resLst_q1)

## Coercion
#as(list(resLst_q1[[1]], resLst_q1[[2]]), "TxpResultList")
#as.TxpResultList(list(res1_q1 = resLst_q1[[1]], res2_q2 = resLst_q1[[2]]))

#as(resLst_q1[[1]], "TxpResultList")
#as.TxpResultList(resLst_q1[[1]])

plot(res_q1)
plot(res_q1[order(txpRanks(res_q1))[1:12]])

plot(res_q24)
plot(res_q24[order(txpRanks(res_q24))[1:12]])
plot(res_q24[order(txpRanks(res_q24))]) 

plot(res_q24, package = "gg")
plot(res_q24[order(txpRanks(res_q24))], package = "gg", ncol = 10) +
  theme(legend.position = "")

plot(res_q24, txpRanks(res_q24))

input <- q24_txp_mod
model <- txp_q24_model

txpExportGui(
  fileName = "C:/Users/christine.calleja/RCode/myrepo/txp2024Model.csv",
  input,
  model,
  id.var = "Name",
  fills = NULL
)


plot(res_q1, package = "gg")
plot(res_q1[order(txpRanks(res_q1))], package = "gg", ncol = 10) +
  theme(legend.position = "bottom")

plot(res_q1, txpRanks(res_q1))
plot(res_q1, txpRanks(res_q1), pch = 16, size = unit(0.75, "char"))

# model 2 adds weights for pm2.5, pm10 and o3
tfq1 <- TxpTransFuncList(linear = function(x) x)
slq1 <- TxpSliceList(pm2.5 = TxpSlice("PM25aqi"),
                     pm10 = TxpSlice("PM10aqi"),
                     o3 = TxpSlice("O3aqi"),
                     no2 = TxpSlice("NO2aqi"),
                     so2 = TxpSlice("SO2aqi", tf1))
tf2q1 <- TxpTransFuncList(NULL, linear = function(x) x, NULL, NULL, NULL)
txp_q1_model2 <- TxpModel(
  txpSlices = slq1, 
  txpWeights = c(3, 2, 2, 1, 1), 
  txpTransFuncs = tf2q1
)

## Calculate scores for single model; returns TxpResult object
res2_q1 <- txpCalculateScores(model = txp_q1_model2,
                          input = q1_txp_mod,
                          id.var = "Name")

plot(res2_q1)
plot(res2_q1[order(txpRanks(res2_q1))[1:12]])


plot(res2_q1, package = "gg")
plot(res2_q1[order(txpRanks(res2_q1))], package = "gg", ncol = 10) +
  theme(legend.position = "bottom")

plot(res2_q1, txpRanks(res2_q1))
plot(res2_q1, txpRanks(res2_q1), pch = 16, size = unit(0.75, "char"))

input <- q1_txp_mod
model <- txp_q1_model

txpExportGui(
  fileName = "C:/Users/christine.calleja/RCode/myrepo/txpQ1Model.csv",
  input,
  model,
  id.var = "Name",
  fills = NULL
  )

