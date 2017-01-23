data_asli = read.csv("belum_normal.csv")

data=apply(data_asli,MARGIN=2,FUN=function(X)(X-min(X))/diff(range(X)))

y=data[,ncol(data)]
x=data[,1:(ncol(data)-1)]
x=cbind(1,x)
m=nrow(x)
tetha=vector(mode="double",length=(ncol(x))-1)
tetha=c(1,tetha)
temp_tetha=vector(mode="double",length=ncol(x))
alpha=0.5
iteration=1000
cost_value=vector(mode="double",iteration)

for(i in 1:iteration){
	for(j in 1:ncol(x)){
		temp_tetha[j] = tetha[j] - alpha * (1/m) * sum(((x%*%tetha)-y)*x[,j])

	}

	tetha = temp_tetha

	cost = sum(((x%*%tetha)-y)^2)/(2*m)
	cost_value[i] = cost
}
plot(cost_value)
func_testing=function(x_testing,tetha,y,x){
	tn=x_testing
	tn=c(1,tn)
	for(j in 2:ncol(x)){
		tn[j]=(tn[j]-min(data_asli[,j-1]))/diff(range(data_asli[,j-1]))
	}
	
	prediksi = tn %*% tetha
	harga = (prediksi*diff(range(data_asli[,ncol(data_asli)]))) + min(data_asli[,ncol(data_asli)])
	harga
}
data_testing=c(135,582,1,1,1,1,0,4,2200,4,2,6,6,0,6)
func_testing(data_testing,tetha,y,x)
