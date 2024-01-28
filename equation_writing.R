library(squidSim)

dd<-simulate_population(
		data_structure = make_structure("individual(100)",rep=3),
		parameters = list(
			intercept = 0,
			individual = list(
				vcov=1
			),
			blah = list(
				beta=0.2,group = "individual"
			),
			observation = list(
				beta=c(2,-1)
			),
			residual=list(
				vcov=1
			)
		)

	)

# dd <- simulate_population(n=20,
# 	parameters=list(
# 		residual=list(
# 		vcov=1)))

components <- names(dd$parameters)
colors <- c()

names(dd$parameters)
# dd$parameters[components[! components %in% c("intercept","interactions","residual")]]

fix_beta <- sapply(dd$parameters[components[! components %in% c("intercept","interactions","residual")]
], function(x){
	if(all(x$beta%in%c(0,1))){ FALSE }else{ TRUE}
})
predictor <- names(fix_beta)[fix_beta]
random <- names(fix_beta)[!fix_beta]

## give predictors and random variables different letters
# "x","w","z"
# "t","u","v"

## collapse_predictor option would make them all x

## assigning subscripts to different components
hlevels <-unique(sapply(dd$parameters[-1], function(x) x$group ))
subscripts <- rep(NA,length(hlevels))
names(subscripts)<- hlevels
subscripts[c("observation","residual")] <- "i"
subscripts[!names(subscripts)%in% c("observation","residual") ] <- letters[9+sum(!names(subscripts)%in% c("observation","residual") )]

all_subscripts <- lapply(dd$parameters[-1], function(x) subscripts[as.character(x$group)] )


print_random <- sapply(dd$parameters[random], function(x){
	x$names[x$beta==1]
})

# dd$parameters[2]

"intercept"
"beta_0"


