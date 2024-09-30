# library(squidSim)
# dd <- simulate_population(n=20,
# 	parameters=list(
# 		residual=list(
# 		vcov=1)))
# parameters <- dd$parameters

# dd<-simulate_population(
# 	data_structure = make_structure("individual(100)",rep=3),
# 	parameters = 
# 	list(
# 		intercept = 0,
# 		individual = list(
# 			vcov=c(1,2)
# 		),
# 		blah = list(
# 			beta=0.2,group = "individual"
# 		),
# 		observation = list(
# 			names=c("temp","rain"),
# 			beta=c(2,-1)
# 		),
# 		interactions = list(
# 			names = ("temp:rain"),
# 			beta = 0.5
# 		),
# 		residual=list(
# 			vcov=1
# 		)
# 	)
# )
# parameters <- dd$parameters

#' @title manyToggle
#' @description multiple toggles
#' @param show n/a
#' @param hide n/a
#' @keywords internal
#' @export
manyToggle <- function(show=NULL,hide=NULL){
  for(i in 1:length(show)) shinyjs::show(show[i])
  for(j in 1:length(hide)) shinyjs::hide(hide[j])
}

# https://shiny.posit.co/r/articles/improve/modules/
# print_table <- function(x){
#   js <- "table.on('click', 'td', function() { 
#     $(this).dblclick();
#   });"
  
#   DT::renderDT(
#     DT::datatable(
#       x,
#       editable = list(target = "cell"),
#       selection = 'none',
#       rownames = FALSE,
#       colnames = NULL,
#       callback = DT::JS(js),
#       class = list(stripe = FALSE),
#       options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
#     ) |> DT::formatStyle(1,`text-align` = 'left')#if(is.matrix(x)){1:ncol(x)}else{1}
#   )
# }

  # ro <- shiny::reactiveValues(x = NULL)
  # editTable <- function(table_name,cell_edit,reactive_object,session){
      
  #     ro$x <<- reactive_object
  #     proxy <<- DT::dataTableProxy(table_name,session=session)
      
  #     ro$x[cell_edit$row, cell_edit$col+1] <<- DT::coerceValue(cell_edit$value, ro$x[cell_edit$row, cell_edit$col+1])
  #     DT::replaceData(proxy, ro$x, resetPaging = FALSE)
  # }

#' @title order_components
#' @description function to order component names
#' @param components n/a
#' @keywords internal
#' @export
order_components <- function(components){
 	c("intercept",components[! components %in% c("intercept","interactions","residual")],if("interactions" %in% components){"interactions"},"residual")
}

#' @title make_colors
#' @description function to make color list to match components
#' @param components n/a
#' @keywords internal
#' @export
make_colors <- function(components){
 	colors <- rep(NA,length(components))
	names(colors) <- components
	colors[c("intercept","residual")] <- palette.colors()[1:2]
	# colors["observation"] <- palette.colors()[3]
	colors[!names(colors) %in% c("intercept","residual") ] <- palette.colors()[4:(3+sum(!names(colors)%in% c("intercept","residual") ))]
	colors
}


#' @title make_equation
#' @description make equation
#' @param parameters parameters to make the equation
#' @param print_colours do you wantt to print colors? 
#' @keywords internal
#' @export

make_equation<-function(parameters, print_colours=TRUE){

	## reorder names
	components <- order_components(names(parameters))

	### make a colour for each component
	colors <- make_colors(components)

	## give each component in the parameter list a component name and color
	params<-lapply(components,function(x)c(parameters[[x]], component=x, color=as.character(colors[x])))
	names(params)<-components

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
		subscript = subscripts[x$group],
		row.names = NULL
	)),make.row.names = FALSE))# 

	## add colors and subscripts to betas
	all$beta_display <- ifelse(all$predictor,
		paste0(
			if(print_colours){paste0("\\color{",all$color,"}{")},
			"\\beta_{",all$letter,
			ifelse(all$display_n,paste0(",",all$variable_n),"")
			,"} ",
			if(print_colours){"}"}
		),
	"")

	## add colours and subscripts to variables
	all$variable_display <- ifelse(all$display,
		paste0(
			if(print_colours){paste0("\\color{",all$color,"}{")},
			all$letter,"_{",
			ifelse(all$display_n,paste0(all$variable_n,","),""),
			all$subscript,"}",
			if(print_colours){"}"}
		),
	"")


	## make interaction terms
	if("interactions" %in% components){
		int_names <- parameters[["interactions"]]$names
		int_betas <- parameters[["interactions"]]$beta
		# int_names <- c("temp:rain","temp:blah_effect")
	 # x<-strsplit(parameters[["interactions"]]$names,":")[[1]]
		## get the variables from the interactions, find varialbes and add a beta
		int_beta_print <- ifelse(int_betas!=1,paste0(
					if(print_colours){paste0("\\color{",colors["interactions"],"}{")},
					"\\beta_{z",
					if(length(int_names)>1){paste0("_",1:length(int_names))},"}",
					if(print_colours){"}"}
				),"")

		int_print <- paste(int_beta_print,sapply(strsplit(int_names,":"), function(x) paste(all$variable_display[all$names %in% x],collapse=" ")))
	}

	part_print <- paste0(all$beta_display,all$variable_display)

	## make latex equation
	print_equation <- 
			paste(
				c("y = \\beta_0",
					part_print[which(all$component!="residual")],
				  if("interactions" %in% components){int_print},
				  part_print[which(all$component=="residual")]),
				collapse=" + ")
	
	## make components string
	print_components <- paste("response =",paste(paste0(
			if(print_colours){paste0("<span style=\"color:",colors ,"\">")},
			components,
			if(print_colours){"</span>"}),
			collapse=" + "))

	## make code to print
	print_code <- paste0(
				paste0("parameters = list(\n", 
				"  intercept = ", print_vector(parameters[["intercept"]]),",\n"),
				paste0(sapply(params[components!="intercept"], write_code_part,print_colours=print_colours),collapse=", \n")
			, "\n)", collapse="")
	

	list(
		equation = print_equation,
		components = print_components,
		code = print_code
	)

}

# x<-params[["interactions"]]

#' @title print_vector
#' @description prints a vector
#' @param x eqation
#' @keywords internal
#' @export
print_vector <- function(x){

	if(length(x)>1){
		paste0("c(",paste0(x,collapse=","),")")
	}else{
		x
	}
}

#' @title write_code_part
#' @description writes code
#' @param x n/a
#' @param print_colors n/a
#' @keywords internal
#' @export
write_code_part <- function(x, print_colours) { 
	if(x$component=="interactions"){
		x$covariate <- x$fixed <- FALSE
	}
	if(x$component=="interactions"|| x$covariate || x$fixed){
		show_beta <- TRUE
		show_names <- if(x$covariate) FALSE else TRUE
		show_group <- show_mean <- show_vcv <- FALSE
	}else{
		show_group <- x$component !=	x$group
		show_names <- !all(grepl("residual",x$group)) & !all(grepl(paste0(x$component,"_effect"),x$names))
		show_beta <- any(x$beta!=1)
		show_mean <- any(x$mean!=0)
		random <- !show_beta & !show_mean
		show_vcv <- x$group=="residual"|random|(!all(diag(x$vcov)==1) & !all(x$vcov[lower.tri(x$vcov)]==0))
		show_vcv_mat <- !all(x$vcov[lower.tri(x$vcov)]==0)
	}

	# show_covariate<-
	# if(x$fixed){
		
	# }
	# if(x$covariate){

	# }

	paste0(
		if(print_colours){paste0("<span style=\"color:",x$color,"\">")},
		"  ", x$component, " = list(\n",	
		paste0(c(	
			if(show_group) paste0("    group = \"",x$group,"\""),
			if(show_names) paste0("    names = c(\"",paste0(x$names,collapse="\",\""),"\")"),
			if(show_beta) paste0("    beta = ",print_vector(x$beta)),
			if(show_mean) paste0("    mean = ",print_vector(x$mean)),
			if(show_vcv){ 
				if(show_vcv_mat){
					paste0("    vcov = matrix( c(",paste0(x$vcov,collapse=","),"), nrow = ",ncol(x$vcov),", ncol = ",ncol(x$vcov), ")")		
				}else{
					paste0("    vcov = ",print_vector(diag(x$vcov)))
				}
			},
			if(x$fixed) "    fixed=TRUE",
			if(x$covariate) "    covariate=TRUE"
		),collapse=", \n"),
	"\n  )",if(print_colours){"</span>"},collapse="")
}



# cat(y)


## TODO
## equations for fixed factors
## compress predictors in equation
	## collapse_predictor option would make them all x

# x<-list(intercept = 0,residual = list(vcov = matrix(1), beta=matrix(1), mean=0,group="residual",names="residual"))
# make_equation(x)
#  make_equation(dd$parameters, print_colours=FALSE)
# # https://stackoverflow.com/questions/71616552/how-do-i-dynamically-change-label-text-color-of-r-shiny-radiobuttons-widget-when


## ideas
## shiny::runApp("/Users/joelpick/github/shinySim")