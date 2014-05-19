rm(list=ls())


library(ggplot2)
library(rjags)
library(reshape)

nut_data<-read.csv(file.choose())
names(nut_data)

dataset<-nut_data[-c(1917:1941),c(1:8,70)]
dataset<-na.omit(dataset)
ggplot(dataset)+geom_histogram(aes(log(soil_pctC)))
head(dataset)

###trying multiple sites###


##also trying the shitty way, doing block, site, region, seperately


#block first#
dataset.sub<-subset(dataset, block < 4)
dataset.sub
nut_model<-"model{
	for(r in 1:Ndata){
		y[r]~dnorm(mu[r],tau[block[r]])
		y_site[r]~dnorm(mu_site[block[r]],tau_site[block[r]])
		y_region[r]~dnorm(mu_region[site[block[r]]],tau_region[site[block[r]]])
		mu[r]<-beta0[block[r]]+beta1_block[block[r]]*x[r]
		mu_site[r]<-beta0_site[site[block[r]]]+beta1_site[site[block[r]]]*x[r]
		mu_region[r]<-beta0_region[region[site[block[r]]]]+beta1_region[region[site[block[r]]]]*x[r]
	}
	for(b in 1:Nblock){
		beta0[b]~dnorm(mu0G,tau0G)
		beta1_block[b]~dnorm(mu1G,tau1G)
		tau[b]~dgamma(sG,rG)
	}
	
	 for(s in 1:Nsite){
		 beta0_site[s]~dnorm(mu0G,tau0G)
		 beta1_site[s]~dnorm(mu1G,tau1G)
		 tau_site[s]~dgamma(sG,rG)
	 }
	
	for(g in 1:Nregion){
		beta0_region[g]~dnorm(mu0G,tau0G)
		beta1_region[g]~dnorm(mu1G,tau1G)
		tau_region[g]~dgamma(sG,rG)
	}
	
	mu0G~dnorm(0,.001)
	tau0G~dgamma(.001,.001)
	mu1G~dnorm(0,.001)
	tau1G~dgamma(.001,.001)
	sG<-pow(m,2)/pow(d,2)
	rG<-m/pow(d,2)
	m~dgamma(.001,.001)
	d~dgamma(.001,.001)
}"

Nrow<-nrow(dataset.sub)
block<-as.integer(factor(dataset.sub[,"block"]))
Nblock<-length(unique(block))
site<-as.integer(factor(dataset.sub[,"site_code"]))
Nsite<-(length(unique(site)))
Nblock
region<-as.integer(factor(dataset.sub[,"region"]))
Nregion<-length(unique(region))

x = as.numeric((dataset.sub[,"plot"]))
y = (dataset.sub[,"soil_pctC"])
zx=(x-mean(x))
zy=(y-mean(y))


model.part2=jags.model(textConnection(nut_model), data=list("Ndata"=Nrow,"Nblock"=Nblock,"block"=block, "Nsite"=Nsite,"region"=region,"Nregion"=Nregion,"site"=site,"x"=zx,"y"=zy),n.chains=3)

chains=coda.samples(model=model.part2, c("tau","tau_site","tau_region"),n.iter=100000,thin=200)

#these are some diagnostics, not necessary for now#
summary(chains)
plot(chains)
autocorr.plot(chains, 50, auto.layout = FALSE)


nut_model_ver2<-"model{
	
	for(i in 1:Ndata){
	y[i]~dnorm(mu[i],tau.e)
	mu[i]<-beta[1]+beta[2]*block[i]+beta[3]*site[i]+beta[4]*region[i]
	}
	tau.e~dgamma(0.0001,0.0001)
	sigma2.e<-1/tau.e

	for(q in 1:4){beta[q]~dnorm(0,0.0001)}

}"


nut_model_ver3<-"model{
	
	for(i in 1:Ndata){
	y[i]~dnorm(mu[i],tau.e)
	mu[i]<-alpha+beta*x[i]+theta_block[block[i]]+theta_site[site[i]]+theta_region[region[i]]
	}
	#three random effects for spatial scale (block,site,region)
	for(j in 1:Nblock){theta_block[j]~dnorm(0, tau_block)}
	for(k in 1:Nsite){theta_site[k]~dnorm(0, tau_site)}
	for(l in 1:Nregion){theta_region[l]~dnorm(0, tau_region)}
	
	#establishing flat priors for everything
	tau.e~dgamma(0.0001,0.0001)
	sigma2.e<-1/tau.e
	tau_block~dgamma(0.0001,0.0001)
	sigma2_block<-1/tau_block
	tau_site~dgamma(0.0001,0.0001)
	sigma2_site<-1/tau_site
	tau_region~dgamma(0.0001,0.0001)
	sigma2_region<-1/tau_region

	#intercept
	alpha~dnorm(0,0.0001)
	beta~dnorm(0,0.0001)
	#variance explained at each scale
	VPS_block<-sigma2_block/(sigma2.e+sigma2_block+sigma2_site+sigma2_region)
	VPS_site<-sigma2_site/(sigma2.e+sigma2_block+sigma2_site+sigma2_region)
	VPS_region<-sigma2_region/(sigma2.e+sigma2_block+sigma2_site+sigma2_region)
	

}"

#establishing factors for random effects
block<-as.integer(factor(dataset.sub[,"block"]))
site<-as.integer(factor(dataset.sub[,"site_code"]))
region<-as.integer(factor(dataset.sub[,"region"]))

#Nvariables for counting

Ndata<-nrow(dataset.sub)
Nblock<-length(unique(block))
Nsite<-(length(unique(site)))
Nregion<-length(unique(region))

#variables for the model
x = as.numeric((dataset.sub[,"plot"]))
y = (dataset.sub[,"soil_pctC"])
zx=(x-mean(x))
zy=(y-mean(y))
model_ver3=jags.model(textConnection(nut_model_ver3),
 data=list("Ndata"=Ndata,"Nblock"=Nblock,"Nsite"=Nsite,"Nregion"=Nregion, "block"=block,"site"=site, "region"=region,"y"=zy,"x"=zx  ),
 n.chains=3)

chains=coda.samples(model=model.part2, c("beta"),n.iter=100000,thin=200)
summary(chains)
plot(chains)