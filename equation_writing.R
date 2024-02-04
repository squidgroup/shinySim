# library(squidSim)

# dd<-simulate_population(
# 		data_structure = make_structure("individual(100)",rep=3),
# 		parameters = 



# 		list(
# 			intercept = 0,
# 			individual = list(
# 				vcov=1
# 			),
# 			blah = list(
# 				beta=0.2,group = "individual"
# 			),
# 			observation = list(
# 				names=c("temp","rain"),
# 				beta=c(2,-1)
# 			),
# 			interactions = list(
# 				names = ("temp:rain")
# 			),
# 			residual=list(
# 				vcov=1
# 			)
# 		)

# 	)

# dd <- simulate_population(n=20,
# 	parameters=list(
# 		residual=list(
# 		vcov=1)))

# parameters<-dd$parameters


make_equation<-function(parameters, print_colours=TRUE){

	components <- names(parameters)
	params<-lapply(components,function(x)c(parameters[[x]], component=x))
	names(params)<-components




	# all_names <- sapply(params[-1], function(x) x$names)
	# names(params)

	# # params[components[! components %in% c("intercept","interactions","residual")]]

	# fix_beta <- sapply(params[components[! components %in% c("intercept","interactions","residual")]
	# ], function(x){
	# 	if(all(x$beta%in%c(0,1))){ FALSE }else{ TRUE}
	# })
	# predictor <- names(fix_beta)[fix_beta]
	# random <- names(fix_beta)[!fix_beta]

	## collapse_predictor option would make them all x

	added_comp <- components[! components %in% c("intercept","interactions","residual")]



	## assigning subscripts to different components
	hlevels <-unique(sapply(params[added_comp], function(x) x$group ))

	subscripts <- rep(NA,length(hlevels))
	names(subscripts)<- hlevels
	subscripts[c("observation")] <- "i"
	subscripts[!names(subscripts)%in% c("observation") ] <- letters[10:(9+sum(!names(subscripts)%in% c("observation") ))]

	# all_subscripts <- sapply(params[components[! components %in% c("intercept","interactions","residual")]], function(x) unname(subscripts[x$group] ))


	colors <- all_letters <- rep(NA,length(added_comp))
	names(colors)<- names(all_letters)<- added_comp
	all_letters["observation"] <- "x"
	colors["observation"] <- palette.colors()[3]

	all_letters[!names(all_letters)%in% c("observation") ] <- 
	letters[23:(24-sum(!names(all_letters)%in% c("observation") ))]

	colors[!names(colors)%in% c("observation") ] <- 
	palette.colors()[4:(3+sum(!names(colors)%in% c("observation") ))]


	# colors <- palette.colors()
	## intercept 1, residual 2



	## give predictors and random variables different letters
	# "x","w","z"
	# "t","u","v"

	## maybe of all predictors, then compress in here?
	all <- do.call(rbind,c(lapply(params[components[! components %in% c("intercept","interactions","residual")]], function(x) data.frame(
		component = x$component,
		group=x$group,
		names=x$names, 
		beta=x$beta, 
		predictor = ifelse(x$beta%in%c(0,1),FALSE,TRUE),
		display = ifelse(x$beta==0,FALSE,TRUE),
		variable_n = 1:length(x$names),
		display_n = ifelse(length(x$names)>1,TRUE,FALSE),
		letter = all_letters[x$component],
		color = colors[x$component],
		subscript = subscripts[x$group]
	)),make.row.names = FALSE))# 


	all$beta_display <- ifelse(all$predictor,paste0("\\beta_{",all$letter,ifelse(all$display_n,paste0(",",all$variable_n),""),"} "),"")

	all$variable_display <- ifelse(all$display,paste0(all$letter,"_{",ifelse(all$display_n,paste0(all$variable_n,","),""),all$subscript,"}"),"")

## add in interactions. colour - beta interaction colour, and other terms their respective colours

	if(print_colours){
		list(
			equation = paste(
				c(paste0("\\color{",palette.colors()[1],"}{\\beta_0} +"),
					paste0(ifelse(all$display,paste0("\\color{",all$color,"}{"),""),all$beta_display,all$variable_display,ifelse(all$display,"} +","")),
					paste0("\\color{",palette.colors()[2],"}{\\epsilon_i}")),
				collapse=" "),

			components = paste(
				components,
				collapse=" + ")	
		)
	}else{
		list(
			equation = paste(
				c("\\beta_0",
					paste0(all$beta_display,all$variable_display),
					"\\epsilon_i"),
				collapse=" + "),
	
			components = paste(
				components,
				collapse=" + ")	
		)
	}






}

# make_equation(dd$parameters)
# https://stackoverflow.com/questions/71616552/how-do-i-dynamically-change-label-text-color-of-r-shiny-radiobuttons-widget-when

   # paste(
   #  "<span style=\"color:#000000\">intercept</span>",
   #  "<span style=\"color:#009E73\">individual_random</span>",
   #  "<span style=\"color:#F0E442\">individual_predictors</span>",
   #  "<span style=\"color:#56B4E9\">observation</span>",
   #  "<span style=\"color:#E69F00\">residual</span>",
   #   sep=" + ")