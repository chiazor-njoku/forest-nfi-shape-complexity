suppressPackageStartupMessages({
  library(tidyverse)
  library(ragg)
  library(data.table)
})


dt <- fread("C:\\Users\\USER\\Downloads\\National_Forest_Inventory_England_2025_-7031548051610948535.csv", encoding = "UTF-8")
setnames(dt,1,"FID")
d <- dt[Area_Ha >= 0.5]

d[, shape_index := Shape__Length / (2*sqrt(pi * Shape__Area))]
cat(sprintf("Using %s polygons (>=0.5 ha)\n\n",
            format(nrow(d),
                   big.mark = ","),
            format(nrow(dt), big.mark = ",")))

# Class Summary

tot <- sum(d$Area_Ha)

summ <- d[, .(n = .N,
                 total_ha = round(sum(Area_Ha)),
                 pct_of_area = round(100*sum(Area_Ha) / tot, 1),
                 median_ha = round(median(Area_Ha), 2),
                 mean_ha = round(mean(Area_Ha), 2),
                 median_shape = round(median(shape_index),2)),
             by = .(CATEGORY, IFT_IOA)][order(-total_ha)]
print(summ, nrows = 30)


# Model

#Woodland classes wih >= 10,000 patches; Broadleaved is the refernce level

keep <- d[CATEGORY == "Woodland", .N, by = IFT_IOA][N >=10000,IFT_IOA]
m <- d[IFT_IOA %in% keep]
m[, cls := relevel(factor(IFT_IOA), ref = "Broadleaved")]
m[, log_si := log(shape_index)] 
m[, log_area := log(Area_Ha)]

# Class Only 

fit_unadj <- lm(log_si ~ cls, data = m)

# + Patch size (Linear in log area)

fit_adj <- lm(log_si ~ log_area + cls, data = m)

# Flexible effect size

fit_cubic <- lm(log_si ~ poly(log_area,3) + cls, data = m)

summary(fit_unadj)
summary(fit_adj)
summary(fit_cubic)

b <- coef(fit_adj)["log_area"]

# Class effects as % difference in shape index relative to Broadleaved

pct <- function(fit, cls_names) {
  cf <- coef(fit)[cls_names]
  ci <- confint(fit)[cls_names, ,drop = FALSE]
  data.table(cls = sub("^cls", "", cls_names), 
             est = 100* (exp(cf) -1),
             lo = 100 * (exp(ci[,1])-1),
             hi = 100 * (exp(ci[,2])-1))
}
