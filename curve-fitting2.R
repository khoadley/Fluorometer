


library(ggplot2)
library(car)
library(MVN)
library(MASS)
//library(doBy)
rm(list=ls())


#establish x-axis
x<-c(0)
n <-0
for (i in 1:400)
{
	n <- (n + 1)
	x <- append(x, n)
}
xvalue <- as.matrix(x)


#arrary for removing .5 usec intervals from data set
xer<-c(0)
s <-0
for (i in 1:401)
{
	s <- (s + 2)
	xer <- append(xer, s)
}
xskip <- as.matrix(xer)


# 1. Read In Excitation Profiles for all 8 LED channels
ex<-read.table("aug15-excit-profile.csv",skip = 1, header = FALSE,  sep=",")

ex567 <-ex[4:14,]
ex597 <-ex[19:29,]
ex615 <-ex[34:44,]
ex625 <-ex[49:59,]
ex447 <-ex[64:74,]
ex475 <-ex[79:88,]
ex505 <-ex[93:103,]
ex530 <-ex[108:117,]

excitations <-list(ex447, ex475, ex505, ex530, ex567, ex597, ex615, ex625)
finalprofiles <-list()


for(P in 1:8)
{	file<-excitations[[P]]
	num<-dim(file)[[1]]
	averaged <- list()
	for(N in 1:num)
	{	#Sys.sleep(.1)
		for (i in 40:80)
		{	if (file[N,i]>0.2)
			{	reducer<-as.matrix(file[N,i:1000])
				y1 <- list()
				y1[[N]]<-(reducer[xskip])
				normalized<-data.matrix(do.call(rbind,y1))
				averaged[[N]] <-c(normalized)
				#plot(xvalue, normalized)
				#Sys.sleep(.3)
				break
			}
		}
	}
	harvest<-data.matrix(do.call(rbind,averaged))
	res<-colMeans(harvest, na.rm=TRUE)
	finalprofiles[[P]] <- c(res)
	
}
profile447<-finalprofiles[[1]]
profile475<-finalprofiles[[2]]
profile505<-finalprofiles[[3]]
profile530<-finalprofiles[[4]]
profile567<-finalprofiles[[5]]
profile597<-finalprofiles[[6]]
profile615<-finalprofiles[[7]]
profile625<-finalprofiles[[8]]

##check each profile
profile<-profile447
resu<-data.frame(xvalue, profile)
pl<-ggplot(resu, aes(xvalue, profile))+geom_point()
pl+scale_y_continuous(limits = c(0, 6.5))



#2. Read in folders with individual file/samples and normalize to respective excitation profiles
filenames <- list.files("LvsH-light", pattern=".csv", full.names=TRUE)
Results <- list()
SIG <- list()
Namer <- list()
seconds<-0
Foo<-summary(filenames)[[1]]
F <- Foo
#F <- 30
for(F in 1:Foo)
{
	if(file.info(filenames[F])$size>0)
	{		data<-read.table(filenames[F],skip = 1, header = FALSE,  sep=",")
			file<-data
			num<-dim(file)[[1]]
			averaged <- list()
			royal447<-regexpr("447nm.csv", filenames[F], ignore.case=FALSE)
			royal475<-regexpr("475nm.csv", filenames[F], ignore.case=FALSE)
			royal505<-regexpr("505nm.csv", filenames[F], ignore.case=FALSE)
			royal530<-regexpr("530nm.csv", filenames[F], ignore.case=FALSE)
			royal567<-regexpr("567nm.csv", filenames[F], ignore.case=FALSE)
			royal597<-regexpr("597nm.csv", filenames[F], ignore.case=FALSE)
			royal615<-regexpr("615nm.csv", filenames[F], ignore.case=FALSE)
			royal625<-regexpr("625nm.csv", filenames[F], ignore.case=FALSE)
			if (royal447[[1]]>0)
			{	 seconds <- 90
				 prof <-royal447
				 excit <-34172
			}
			
			if (royal475[[1]]>0)
			{ 	 seconds <- 90
				 prof <-royal475
				 excit <-38545
			 }
			
			if (royal505[[1]]>0)
			{	 seconds <- 175
				 prof <-royal505
				 excit <-16209
			 }
			
			if (royal530[[1]]>0)
			{	 seconds <- 175
				 prof <-royal530
				 excit <-18401
			}
			
			if (royal567[[1]]>0)
			{	 seconds <- 175
				 prof <-royal567
				 excit <-20352
			}
			
			if (royal597[[1]]>0)
			{	 seconds <- 175
				 prof <-royal597
				 excit <-24089
			}
			
			if (royal625[[1]]>0)
			{	 seconds <- 175
				 prof <-royal625
				 excit <-43662
			}
			
			if (royal615[[1]]>0)
			{	 seconds <- 175
				 prof <-royal615
				 excit <-35983
			}			

			for(N in 2:num)
			{	#Sys.sleep(.1)
				for (i in 40:80)
				{	if (file[N,i]>0.1)
					{	reducer<-as.matrix(file[N,i:800])
						y1 <- list()
						y1[[N]]<-(reducer[xskip])
						normalized<-data.matrix(do.call(rbind,y1))
						averaged[[N]] <-c(normalized/prof)
						#plot(x1, normalized)
						break
					}
				}
			}
			norm<-data.matrix(do.call(rbind,averaged))
			normal<-colMeans(norm, na.rm=TRUE)
						
			x1<-xvalue[3:seconds]
			yvalue<-normal[3:seconds]

			
			v <- -999
			try(v <- nls(yvalue ~ A + B*(1-exp(-(C*x1)/1)), 
			start = list(A = 2, B = 2, C = .04), 
			trace = TRUE,control = list(maxiter = 500), algorithm = "port"), silent = TRUE)
			
			if(mode(v)=="numeric")
			{	try(v <- nls(yvalue ~ A + B*(1-exp(-(C*x1)/1)), 
				start = list(A = 2, B = 2, C = .02), 
				trace = TRUE,control = list(maxiter = 500), algorithm = "port"), silent = TRUE)
			}			
			if(mode(v)=="numeric")
			{	coef <- -999
				Aest <- -999
				Best <- -999
				Cest <- -999
				Aerr <- -999
				Berr <- -999
				Cerr <- -999
				sigma_se <- -999
				Fv_se <- -999
				Fm_se <- -999
				Fo_se <- -999
				Fv_Fm_se <- -999
				maxp <- -999
				Fv <- -999
				Fm <- -999
				Fo <- -999
				sigma <- -999
				Fv_Fm <- -999				
				yest <- yvalue
				plot(x1, yvalue)
				lines(yest, col = "red")
			}
			else
			{	coef <- summary(v)$coefficients
				Aest <- coef[1,1]
				Best <- coef[2,1]
				Cest <- coef[3,1]
				Aerr <- coef[1,2]
				Berr <- coef[2,2]
				Cerr <- coef[3,2]
				sigma_se <- Cerr
				Fv_se <- Berr
				Fm_se <- sqrt(Aerr^2+Berr^2)
				Fo_se <- sqrt(Fm_se^2+Fv_se^2)
				Fv_Fm_se <- (Best/(Aest+Best))*sqrt((Fv_se/Best)^2+(Fm_se/(Aest+Best))^2)
				maxp <- max(coef[,4])
				Fo <- Aest + Best*(1-exp(-(Cest*1)/1))
				Fm <- Aest + Best*(1-exp(-(Cest*x1[length(x1)])/1))
				Fv <- Fm-Fo
				sigma <- Cest/(excit/1000000)
				Fv_Fm <- Fv/Fm
				yest <- Aest + Best*(1-exp(-(Cest*x1)/1))
				#yest <- 1.6482 + 1.45*(1-exp(-(0.047*x1)/1))
				plot(x1, yvalue)
				grid(col = "grey")
				lines(yest, col ="red")	
				#Sys.sleep(.2)
			}		
			# conest <- 0.5
			# fmest <-Fm
			# foest <- Fo

			# bigC <-list()
			# for (i in 1:length(yvalue))
			# {
			# Cestimator <- (yvalue[i] - foest)/((1-conest)*(fmest-foest)+conest*(yvalue[i]-foest)) 
			# bigC[[i]]<-c(Cestimator)
			# }
			# Cnvalue<-data.matrix(do.call(rbind,bigC))

			# Fn <- nls(yvalue ~ foest + (B-foest)*(Cnvalue)*((1-P)/(1-Cnvalue*P)), 
			# start = list(B = .5, P = 0.05), 
			# trace = TRUE,control = list(maxiter = 500), algorithm = "port")
			
			# Cn <- nls(Cnvalue ~ Cnvalue[x1-1] + Rsig*((1-Cnvalue[x1-1])/((1-Cnvalue[x1-1])*P)), 
			# start = list(Rsig = sigma, P = 0.05), 
			# trace = TRUE,control = list(maxiter = 500), algorithm = "port")			
			
			
			Results[[F]] <-c(Fo,Fm,Fv_Fm,sigma)
			Namer[[F]] <-c(filenames[F])
		}
		else
		{}
}
Finals<-data.matrix(do.call(rbind,Results))
Namecode<-data.matrix(do.call(rbind,Namer))
Finalresults = data.frame(Namecode, Finals)
Out<-capture.output(Finalresults)
Finalresults
write(Out, file="121116-lvsh-light", sep="\n", append=FALSE)
