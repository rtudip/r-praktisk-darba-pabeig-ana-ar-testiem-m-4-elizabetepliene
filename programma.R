kordat <- read.delim("variants3.txt", strip.white = TRUE, row.names = 1)

kordat <- as.data.frame(lapply(kordat, function(x) 
{
  if (is.character(x)) {
    x <- gsub(",", ".", x)
    if (all(grepl("^-?\\d+(\\.\\d+)?([eE][-+]?\\d+)?$", x))) {
      return(as.numeric(x))
    } else {
      return(as.factor(x))
    }
  }
  return(x)
}))

rownames(kordat) <- rownames(read.delim("variants3.txt", strip.white = TRUE, row.names = 1))
kordat[, 9:ncol(kordat)] <- lapply(kordat[, 9:ncol(kordat)], as.factor)

sink("results.txt")

cat("Faktoru līmeņu biežums:\n")

for (col in names(kordat)[9:ncol(kordat)]) 
{
  cat(paste("\nKolonna:", col, "\n"))
  print(summary(kordat[[col]]))
}
sl.by.b <- split(kordat$Slope, kordat$b)
cat("\nSadalītā Slope pēc b faktora:\n")
print(sl.by.b)

kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)
cat("\nStandartnovirze (Average) pa f faktora līmeņiem:\n")
std_dev_f <- tapply(kordat$Average, kordat$f, sd, na.rm = TRUE)
print(std_dev_f)

prockordat <- kordat[kordat$adj.r.squared > 0.7, ]
prockordat$Slope <- 1 - 1 / prockordat$Slope
cat("\nFiltrētie un pārveidotie dati (prockordat):\n")
print(prockordat)

sink()

svg("scatter.svg")
plot(kordat$MAD, kordat$Average, main = "Scatterplot", xlab = "MAD", ylab = "Average")
dev.off()

svg("boxplot.svg")
boxplot(Intercept ~ f, data = kordat, col = rainbow(length(unique(kordat$f))),
        main = "Boxplot of Intercept by f", xlab = "f Faktors", ylab = "Intercept")
dev.off()
