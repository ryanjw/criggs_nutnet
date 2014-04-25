library(ggplot2)
library(rjags)


nut_data<-read.csv(file.choose())
names(nut_data)

dataset<-nut_data[-c(1917:1941),c(1:3,8,70)]
head(dataset)
dataset<-na.omit(dataset)
summary(dataset)
ggplot(dataset)+geom_histogram(aes(log(soil_pctC)))


###starting with just one site###
dataset.sub<-subset(dataset, site_code=="cbgb.us")
dataset.sub
m.part2<-"model{
	for(r in 1:Ndata){
		y[r]~dnorm(mu[r],tau[subj[r]])
		mu[r]<-beta0[subj[r]]+beta1[subj[r]]*x[r]
	}
	for(s in 1:Nsubj){
		beta0[s]~dnorm(mu0G,tau0G)
		beta1[s]~dnorm(mu1G,tau1G)
		tau[s]~dgamma(sG,rG)
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
data.melt
N.data.2<-nrow(dataset.sub)
head(data.melt)
N.data.2
subj.2<-as.integer(factor(dataset.sub[,"block"]))
subj.2
Nsubj.2<-length(unique(subj.2))
Nsubj.2
x.2 = as.numeric((dataset.sub[,"plot"]))
x.2
head(data.melt)
y.2 = (dataset.sub[,"soil_pctC"])
zx.2=(x.2-mean(x.2))
zy.2=(y.2-mean(y.2))


model.part2=jags.model(textConnection(m.part2), data=list("Ndata"=N.data.2,"Nsubj"=Nsubj.2,"subj"=subj.2,"x"=zx.2,"y"=zy.2),n.chains=3)
chains=coda.samples(model=model.part2, c("beta0","beta1","tau","mu0G","mu1G","tau0G","tau1G"),n.iter=40000,thin=20)
summary(chains)
