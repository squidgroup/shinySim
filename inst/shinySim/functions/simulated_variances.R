#' @title prod_var
#' @description produces variance
#' @param means n/a
#' @param vcov n/a
#' @keywords internal
## function to compute expected variance of the product of variables. if two variables are given, accounts for non-independence of variables (i.e. non-0 covariance), but for more than 3 assumes that they are independent (don't' think there is a generalisation for the produce of 3 or more dependent variables)
prod_var <- function(means,vcov){
	vars <- diag(vcov)
	
	if(length(means)>2){
		prod(vars + means^2) - prod(means)^2
	}else{
		c <- vcov[lower.tri(vcov)]
		prod(vars+means^2) + c^2 + 2*c*prod(means) - prod(means)^2
	}
}

#' @title prod_means
#' @description produces means
#' @param means n/a
#' @param vcov n/a
#' @keywords internal
prod_means <- function(means,vcov){
	if(length(means)>2){
		prod(means)
	}else{
		prod(means) + vcov[lower.tri(vcov)]
	}
}

#' @title make_big_matrix
#' @description produces a big matrix
#' @param x n/a
#' @keywords internal
#makes a big matrix out of list of vcov matrices
make_big_matrix<-function(x){
	all_names <- c(sapply(x, function(i) colnames(i) ), recursive=TRUE)
	mat_index <- c(0,cumsum(sapply(x, function(i) nrow(i))))
	
	mat <- diag(max(mat_index))
	colnames(mat) <- rownames(mat) <- all_names

	for(i in 2:length(mat_index)){
		indexes<-(mat_index[i-1]+1):mat_index[i]
		mat[indexes,indexes] <- x[[i-1]]
	}
	mat
}

simVar <- function(parameters,data_structure){
	
	# order parameter names
	components <- order_components(names(parameters))

	### make a colour for each component
	colors <- make_colors(components)

	intercept <- parameters$intercept

	param<-lapply(components[components!="intercept"],function(x)c(parameters[[x]], component=x, color=as.character(colors[x])))
	names(param)<-components[components!="intercept"]
	
	#makes sure all the components have the right names
	for(i in 1:length(param)){
		names(param[[i]]$mean) <- colnames(param[[i]]$vcov) <- rownames(param[[i]]$vcov) <- rownames(param[[i]]$beta) <- param[[i]]$names
	}

	p_names <- names(param)[names(param)!="interactions"]
	
	fixed <- p_names[sapply(p_names, function(i) param[[i]]$fixed)]
	if(length(fixed)>0){
		for (i in fixed){
			levels<-as.vector(table(data_structure[,param[[i]]$group]))
			p <- levels/sum(levels)
			param[[i]]$mean <- p
			param[[i]]$vcov <- diag(p*(1-p), nrow=length(levels))
			names(param[[i]]$mean) <- colnames(param[[i]]$vcov) <- rownames(param[[i]]$vcov) <- param[[i]]$names
		}
	}
	
	covariate <- p_names[sapply(p_names, function(i) param[[i]]$covariate)]
	if(length(covariate)>0){
		for (i in covariate){
			z <- data_structure[,param[[i]]$group]
			param[[i]]$mean <- rep(mean(z), length(param[[i]]$names))
			param[[i]]$vcov <- diag(stats::var(z), length(param[[i]]$names))
			names(param[[i]]$mean) <- colnames(param[[i]]$vcov) <- rownames(param[[i]]$vcov) <- param[[i]]$names
		}
	}

	if("interactions" %in% names(param)){
		
		means1 <- do.call(c,c(lapply(p_names, function(i) param[[i]]$mean ), use.names=FALSE))

		covs1 <- make_big_matrix(lapply(p_names, function(i) param[[i]]$vcov ))
		int_names <- param[["interactions"]]$names
		int_var <- lapply(strsplit(int_names,":"), function(j){
			#For two way interactions, the expected means and variances take into account 
			if(length(j)>2) warning("For three way interactions and above, covariance between variables is ignored when calculating expected means and variances (with the exception of polynomials)")
			list(
				cov = prod_var(means1[j],covs1[j,j]),
				mean = prod_means(means1[j],covs1[j,j])
				)
		})

		param[["interactions"]]$vcov <- diag(sapply(int_var,function(x)x$cov), nrow=length(int_names))
		param[["interactions"]]$mean <- sapply(int_var,function(x)x$mean)
		names(param[["interactions"]]$mean) <- colnames(param[["interactions"]]$vcov) <- rownames(param[["interactions"]]$vcov) <- int_names
	}

	means <- do.call(c,c(lapply(param, function(p) p$mean ), use.names=FALSE))
	covs <- make_big_matrix(lapply(param, function(p) p$vcov ))
	betas <- do.call(rbind,lapply(param, function(p) p$beta ))

	out <- list( 
		## total
		total = c(
			mean = intercept + sum(betas * means),
			var = as.vector(t(betas) %*% covs %*% betas)
		),
		
		##hierarchy
		groups = data.frame(
			mean= c(intercept=intercept,sapply(param, function(p) sum(p$beta*p$mean))),
			var=c(intercept=0,sapply(param, function(p) t(p$beta) %*% p$vcov %*% p$beta ))
			),
	
		variables = data.frame(
			mean = rbind(intercept=intercept,betas * means),
			var = rbind(intercept=0,betas * covs %*% betas),
			names = c("intercept",do.call(rbind,lapply(param, function(p) cbind(p$names,p$component)))[,2])
		)
	 	
	)
	return(out)

}
