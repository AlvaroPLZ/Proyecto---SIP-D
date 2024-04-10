## limpia la memoria
rm(list=ls()) 

## rutas de trabajo
wd <- "/home/eric/Downloads/Desktop/sip/proy/alvaro/Proyecto---SIP-D/data/"
setwd(wd)

## lee los votos
d <- read.csv("countypres_1980-2020.csv")
d[1,]
str(d)
v <- data.frame(ord=1:nrow(d))
v$state <- d$State
v$st <- d$state_po
v$stn <- NA
v$fips <- d$county_fips
v$stn <- as.integer(v$fips/1000)
v$cty <- d$County.name
v$repsh80 <- round(as.numeric(d$repsh80), 4)
v$repsh00 <- round(as.numeric(d$repsh00), 4)
v$repsh16 <- round(as.numeric(d$repsh16), 4)
v$repsh20 <- round(as.numeric(d$repsh20), 4)
v$v2tot80 <- d$tot2pty80
v$v2tot00 <- d$tot2pty00
v$v2tot16 <- d$tot2pty16
v$v2tot20 <- d$tot2pty20
head(v)
##
rm(d)

## quita ALASKA
sel.r <- which(v$st=="AK")
v <- v[-sel.r,]

## lee mexicanos
d <- read.csv("mex-per-cty-2020.csv")
d[1,]
d$fips <- d$county_fips
d$mex <- d$Total
## AQUI FALTA LA POBLACION TOTAL 2020 DEL CONDADO PARA OBTENER POBLACIÓN RELATIVA MEX
##d$mex <- d$mex / d$ptot
##
## quita columnas innecesarias
d <- d[, c("fips","mex")]

## merge
dim(v)
dim(d)
v <- merge(x = v, y = d, by = "fips", all.x = TRUE, all.y = FALSE)
tail(v)
rm(d)

## ESTO ES TEMPORAL, usa voto como denominador en vez de población total...
v$mexsh20 <- round(v$mex / v$v2tot20, 4)

summary(v$repsh80)
summary(v$mexsh20)

## REVISA ESTOS CASOS CUANDO TENGAS EL DENOMINADOR CORRECTO
sel.r <- which(v$mexsh20 > 1)
v[sel.r,]

## para sacar del plot los casos > 1
sel.r <- which(v$mexsh20 <= 1)
plot(v$mexsh20[sel.r], v$repsh20[sel.r], pch = 19, col = "gray", cex = .3)
r <- lm("repsh20 ~ mexsh20", data = v, subset = mexsh20 < 1)
abline(reg=r)

plot(log(v$mexsh20[sel.r]), v$repsh20[sel.r], pch = 19, col = "gray", cex = .3)
r <- lm("repsh20 ~ log(mexsh20)", data = v, subset = mexsh20 < 1)
abline(reg=r)

