library("jrvFinance")
library(readxl)

df <- read_excel("apm466/new data.xlsx")
View(df)

coupon <- as.numeric(df$Coupon)
maturity_date <- df$Maturity_Date
bond_name <- df$Bond_Name
clean_price_date <- c('2023-01-16','2023-01-17','2023-01-18','2023-01-19','2023-01-20'
                      ,'2023-01-23','2023-01-24','2023-01-25','2023-01-26','2023-01-27')
clean_price_matrix <- matrix(c(df$'2023_1_16',df$"2023_1_17",df$"2023_1_18",df$"2023_1_19",df$"2023_1_20",df$"2023_1_23",
                               df$"2023_1_24",df$"2023_1_25",df$"2023_1_26",df$"2023_1_27"), nrow=10, ncol=10,byrow=TRUE)

# YTM 

## Calculate YTM
matrix_ytm <- matrix("numeric", nrow=10, ncol=10)
for (j in c(1:10)){
  for (i in c(1:10)){
    matrix_ytm[i,j]= bond.yield(settle=clean_price_date[i],mature=maturity_date[j],coupon=coupon[j],
                                freq=2,clean_price_matrix[,j][i], comp.freq=2,redemption_value = 100)
  }
}

matrix_ytm[1,]

## Plot of Yield to Maturity
date <- c(16,17,18,19,20,23,24,25,26,27)
color <- c("blue","orange","green","purple","pink","gray","yellow","dark blue","red")
plot(date, matrix_ytm[1,],type="o", xlab="Date", ylab="YTM",ylim=c(0.026,0.031))
for (i in c(2:10)){
  lines(date, matrix_ytm[i,],type="o",col=color[i-1])
}

legend("bottomright", legend=bond_name,pch=c(20,20),col=color,lty=1,cex=0.3)

## spot_rate:

## only the first four bonds hold the term from 1-5 years

matrix_spot <- matrix("numeric", nrow=4, ncol=10)
for (j in 1:10){
  for (i in 1:4){ 
    dirty_price = as.numeric(dirty[j,i])
    coupon_value = as.numeric(df$Coupon[i]*100/2)
    face = 100
    year_to_maturity = as.numeric(df$Years_to_Maturity[i])
    matrix_spot[i,j] = 2*(((coupon_value+face)/dirty_price)^(1/(2*year_to_maturity))-1)
  }
}


## Plot of spot rate
plot(date, matrix_spot[1,],type="o", main=" 5-year spot curve", xlab="Date", ylab="Spot Rate",ylim=c(-0.01,0.02))
for (i in c(2:4)){
  lines(date, matrix_spot[i,],type="o",col=color[i])
}


legend("bottomright", legend=bond_name[1:4],pch=c(20,20),col=color,lty=1,cex=0.3)

# Forward Rate

## only the first four bonds hold the term from 2-5 years
## start from estimating 5 years spot rate on 2023-01-16, 2024-01-16, 2025-01-16,2026-01-16,2027-01-16

long_frac <- c(yearFraction("2023-12-01","2024-06-01"),yearFraction("2024-12-01","2025-06-01"),
               yearFraction("2025-12-01","2026-06-01"),yearFraction("2026-12-01","2027-06-01"),
               yearFraction("2027-12-01","2028-06-01"))

short_frac <- c(yearFraction("2023-12-01","2024-02-01"),yearFraction("2024-12-01","2025-02-01"),
                yearFraction("2025-12-01","2026-02-01"),yearFraction("2026-12-01","2027-02-01"),
                yearFraction("2027-12-01","2028-02-01"))

s=1
spot_Dec1 <- c(matrix_spot[1,][1],matrix_spot[1,][3],matrix_spot[1,][5],matrix_spot[1,][7],matrix_spot[1,][9])
spot_June1 <- c(matrix_spot[1,][2],matrix_spot[1,][4],matrix_spot[1,][6],matrix_spot[1,][8],matrix_spot[1,][10])

estimated_spot1 <- c()
for (i in 1:5){
  ratio <- short_frac[i]/long_frac[i]
  estimated_spot1[i] = as.numeric(spot_Dec1[i]) +(as.numeric(spot_June1[i])- as.numeric(spot_Dec1[i]))*ratio
}

s=2
spot_Dec2 <- c(matrix_spot[2,][1],matrix_spot[2,][3],matrix_spot[2,][5],matrix_spot[2,][7],matrix_spot[2,][9])
spot_June2 <- c(matrix_spot[2,][2],matrix_spot[2,][4],matrix_spot[2,][6],matrix_spot[2,][8],matrix_spot[2,][10])

estimated_spot2 <- c()
for (i in 1:5){
  ratio <- short_frac[i]/long_frac[i]
  estimated_spot2[i] = as.numeric(spot_Dec2[i]) +(as.numeric(spot_June2[i])- as.numeric(spot_Dec2[i]))*ratio
}

s=3
spot_Dec3 <- c(matrix_spot[3,][1],matrix_spot[3,][3],matrix_spot[3,][5],matrix_spot[3,][7],matrix_spot[3,][9])
spot_June3 <- c(matrix_spot[3,][2],matrix_spot[3,][4],matrix_spot[3,][6],matrix_spot[3,][8],matrix_spot[3,][10])

estimated_spot3 <- c()
for (i in 1:5){
  ratio <- short_frac[i]/long_frac[i]
  estimated_spot3[i] = as.numeric(spot_Dec3[i]) +(as.numeric(spot_June3[i])- as.numeric(spot_Dec3[i]))*ratio
}

s=4
spot_Dec4 <- c(matrix_spot[4,][1],matrix_spot[4,][3],matrix_spot[4,][5],matrix_spot[4,][7],matrix_spot[4,][9])
spot_June4 <- c(matrix_spot[4,][2],matrix_spot[4,][4],matrix_spot[4,][6],matrix_spot[4,][8],matrix_spot[4,][10])

estimated_spot4 <- c()
for (i in 1:5){
  ratio <- short_frac[i]/long_frac[i]
  estimated_spot4[i] = as.numeric(spot_Dec4[i]) +(as.numeric(spot_June4[i])- as.numeric(spot_Dec4[i]))*ratio
}

estimated_5years <- rbind(estimated_spot1,estimated_spot2,estimated_spot3,estimated_spot4,deparse.level=0)

## calculate 1yr1yr, 1yr2yr,1yr3yr,1yr4yr forward rate
matrix_forward <- matrix(nrow=4, ncol=4)
for (j in 1:4){
  for (i in 1:4){ 
    func = function(x) ((1+estimated_5years[j,1]/2)^2)*(1+x/2)^(2*i)-(1+estimated_5years[j,1+i]/2)^(2*(i+1))
    matrix_forward[j,i] = uniroot(func,c(0,1),extendInt = "yes")$root                                   
  }
}

## Plot of Forward Curve
year_to_year <- c(1,2,3,4)
plot(year_to_year, matrix_forward[1,],type="o", main="Forward curve", xlab="1 Year vs Year", ylab="Forward Rate",ylim=c(-0.01,0.02))

for (i in c(2:4)){
  lines(year_to_year, matrix_forward[i,],type="o",col=color[i-1])
}

legend("bottomright", legend=bond_name[1:4],pch=c(20,20),col=color,lty=1,cex=0.3)


# Covariance Matrix

## Covariance Matrix for logYTM

log_YTM_matrix <- matrix(nrow=9, ncol=5)
for (j in c(1:5)){
  for (i in c(1:9)){
    log_YTM_matrix[i,j] <- log(as.numeric(matrix_ytm[i+1,j])/as.numeric(matrix_ytm[i,j]))
  }
}

ytm_covariance_matrix <- cov(log_YTM_matrix,log_YTM_matrix)
ytm_covariance_matrix 


## Covariance Matrix for logfwd

log_forward_matrix <- matrix(nrow=3, ncol=4)
for (j in c(1:4)){
  for (i in c(1:3)){
    log_forward_matrix[i,j] <- log(abs(as.numeric(matrix_forward[i+1,j])/as.numeric(matrix_forward[i,j])))
  }
}

forward_covariance_matrix <- cov(log_forward_matrix,log_forward_matrix)
forward_covariance_matrix 

# Eigenvalue and Eigenvector
print(eigen(ytm_covariance_matrix)$values)
print(eigen(ytm_covariance_matrix)$vector)
print(eigen(forward_covariance_matrix)$values)
print(eigen(forward_covariance_matrix)$vector)


