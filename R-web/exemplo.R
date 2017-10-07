# script exemplo.R
# Comunicação php-R

args <- commandArgs(TRUE)     
# Recebe o valor
valor <- args[1]
# Distribuição Normal
x <- rnorm(valor,0,1)
# figura fomato png
png(filename="figura.png", width=400, height=500)
#histograma
hist(x)    
dev.off()