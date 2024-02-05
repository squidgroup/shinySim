# library(squidSim)

# dd <- simulate_population(n=20,
# 	parameters=list(
# 		residual=list(
# 		vcov=1)))
# parameters <- dd$parameters

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
# parameters <- dd$parameters



make_equation<-function(parameters, print_colours=TRUE){

	components <- names(parameters)
	params<-lapply(components,function(x)c(parameters[[x]], component=x))
	names(params)<-components

	reserved_names <- c("intercept","observation","interactions","residual")



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


	### make a colour for each component
	colors <- rep(NA,length(components))
	names(colors) <- components
	colors["intercept"] <- palette.colors()[1]
	colors["residual"] <- palette.colors()[2]
	# colors["observation"] <- palette.colors()[3]
	colors[!names(colors) %in% c("intercept","residual") ] <- 
	palette.colors()[4:(3+sum(!names(colors)%in% c("intercept","residual") ))]


	### make a letter for each component

	added_comp <- components[! components %in% c("intercept","interactions")]
	base_levels <- c("observation","residual")

	all_letters <- rep(NA,length(added_comp))
	names(all_letters)<- added_comp
	all_letters[base_levels] <- c("x","\\epsilon")
	# all_letters["residual"] <- "\\epsilon"
	all_letters[!names(all_letters)%in% base_levels ] <- 
	letters[23:(24-sum(!names(all_letters)%in% base_levels ))]
	


	## assign subscripts to different groups
	hlevels <-unique(sapply(params[added_comp], function(x) x$group ))

	subscripts <- rep(NA,length(hlevels))
	names(subscripts)<- hlevels
	subscripts[base_levels] <- "i"
	subscripts[!names(subscripts)%in% base_levels ] <- letters[10:(9+sum(!names(subscripts)%in% base_levels ))]


	## maybe of all predictors, then compress in here?
	all <- do.call(rbind,c(lapply(params[added_comp], function(x) data.frame(
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


	if(print_colours){
		list(
			equation = paste(
				c(paste0("\\color{",palette.colors()[1],"}{\\beta_0}"),
					paste0(ifelse(all$display,paste0("\\color{",all$color,"}{"),""),all$beta_display,all$variable_display,ifelse(all$display,"}",""))
					# ,paste0("\\color{",palette.colors()[2],"}{\\epsilon_i}")
					),
				collapse=" + "),

			components = paste(paste0(
				"<span style=\"color:",colors ,"\">",components,"</span>"),
				collapse=" + "),

			code = paste(paste0(
				"<span style=\"color:",colors ,"\">",components,"</span>"),
				collapse=" + ")	
		)
	}else{
		list(
			equation = paste(
				c("\\beta_0",
					paste0(all$beta_display,all$variable_display)),
				collapse=" + "),
	
			components = paste(
				components,
				collapse=" + "),

			code = paste0(
				paste0("parameters = list(\n", 
				"  intercept = c(", paste0(parameters[["intercept"]],collapse=", "),"),\n"),
				paste0(sapply(params[added_comp], function(x) { 
					paste0("  ",x$component, " = list(\n",
						
						paste0(c(	
							if(x$component !=	x$group) paste0("    group = \"",x$group,"\""),
							if(all(x$beta!=1)) paste0("    beta = c(",paste0(x$beta,collapse=","),")"),
							if(all(x$mean!=0)) paste0("    mean = c(",paste0(x$mean,collapse=","),")"),
							if(x$group=="residual"|(!all(diag(x$vcov)==1) & !all(x$vcov[lower.tri(x$vcov)]==0))){ 
								if(ncol(x$vcov)==1 | all(x$vcov[lower.tri(x$vcov)]==0)){
									paste0("    vcov = c(",paste0(diag(x$vcov),collapse=","),")")
								}else{
									paste0("    vcov = matrix( c(",paste0(x$vcov,collapse=","),"), nrow = ",ncol(x$vcov),", ncol = ",ncol(x$vcov), ")")
								}
						}),collapse=", \n"),
						"\n  )", collapse="")
				})
				,collapse=", \n")
			, "\n)", collapse="")

		)
	}
}

x<-params[["blah"]]


paste0(x$component, "= list(\n"),

cat(
paste0(
	paste0("parameters = list(\n", 
	"  intercept = c(", paste0(parameters[["intercept"]],collapse=", "),"),\n"),
	paste0(sapply(params[added_comp], function(x) { 
		paste0("  ",x$component, " = list(\n",
			
			paste0(c(	
				if(x$component !=	x$group) paste0("    group = \"",x$group,"\""),
				if(all(x$beta!=1)) paste0("    beta = c(",paste0(x$beta,collapse=","),")"),
				if(all(x$mean!=0)) paste0("    mean = c(",paste0(x$mean,collapse=","),")"),
				if(x$group=="residual"|(!all(diag(x$vcov)==1) & !all(x$vcov[lower.tri(x$vcov)]==0))){ 
					if(ncol(x$vcov)==1 | all(x$vcov[lower.tri(x$vcov)]==0)){
						paste0("    vcov = c(",paste0(diag(x$vcov),collapse=","),")")
					}else{
						paste0("    vcov = matrix( c(",paste0(x$vcov,collapse=","),"), nrow = ",ncol(x$vcov),", ncol = ",ncol(x$vcov), ")")
					}
			}),collapse=", \n"),
			"\n  )", collapse="")
	})
	,collapse=", \n")
, "\n)", collapse="")

)


vcov_out<-sapply(params[added_comp], function(x) { 
	paste0("  ",x$component, " = list(\n",
		
		paste0(c(	
			if(x$component !=	x$group) paste0("    group = \"",x$group,"\""),
			if(all(x$beta!=1)) paste0("    beta = c(",paste0(x$beta,collapse=","),")"),
			if(all(x$mean!=0)) paste0("    mean = c(",paste0(x$mean,collapse=","),")"),
			if(x$group=="residual"|(!all(diag(x$vcov)==1) & !all(x$vcov[lower.tri(x$vcov)]==0))){ 
				if(ncol(x$vcov)==1 | all(x$vcov[lower.tri(x$vcov)]==0)){
					paste0("    vcov = c(",paste0(diag(x$vcov),collapse=","),")")
				}else{
					paste0("    vcov = matrix( c(",paste0(x$vcov,collapse=","),"), nrow = ",ncol(x$vcov),", ncol = ",ncol(x$vcov), ")")
				}
		}),collapse=", \n"),
		"\n  )", collapse="")
})

cat(paste(vcov_out,collapse=", \n"))
vcov_out)



## TODO
## add in interactions. colour - beta interaction colour, and other terms their respective colours
## re-order so residual is at the end
## compress predictors

# make_equation(dd$parameters)
# https://stackoverflow.com/questions/71616552/how-do-i-dynamically-change-label-text-color-of-r-shiny-radiobuttons-widget-when

