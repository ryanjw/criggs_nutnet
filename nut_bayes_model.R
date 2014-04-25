library(ggplot2)
library(rjags)


nut_data<-read.csv(file.choose())
names(nut_data)

dataset<-nut_data[-c(1917:1941),c(1:3,8,70)]
head(dataset)
ggplot(dataset)+geom_histogram(aes(log(soil_pctC)))
nut_model<-"model{
	
	for(n in 1:Ndata){
		y[n]~dnorm(mu[n],tau[site[n]])
		mu[n]<-beta0[site[n]]+beta1[site[n]]*x[n]
		}
	for(s in 1:Nsite){
		beta0[s]~dnorm(mu_beta0, tau_beta0)
		beta1[s]~dgamma(0.001,0.001)
		tau[s]~dgamma(sG,rG)		
	}
	mu_beta0~dnorm(0,0.00001)
	tau_beta0~dgamma(0.00001,0.00001)
	mu_beta1~dnorm(0,0.00001)
	tau_beta1~dgamma(0.00001,0.00001)
	sG<-pow(m,2)/pow(d,2)
	rG<-m/pow(d,2)
	m~dgamma(0.0001,0.0001)
	d~dgamma(0.0001,0.0001)
}"
Ndata
Ndata<-nrow(dataset)

site<-as.integer(factor(dataset[,"site_code"]))
block<-as.integer(factor(dataset[,"block"]))


