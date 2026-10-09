#' @title sorted_levels
#' @description levels of a grouping factor in the order squidSim uses them (for numbered levels,
#' beta i is level i) and the order of table()
#' @param x n/a
#' @keywords internal
sorted_levels <- function(x) {
	lv <- unique(x)
	if (all(lv %in% seq_along(lv))) as.character(sort(as.numeric(as.character(lv)))) else as.character(sort(lv))
}

#' @title gauss_moment
#' @description expected value of a product of jointly normal variables (Isserlis' theorem). Variables
#' can be repeated (e.g. c("x","x") gives E[x^2]), so this works for interactions of any order and polynomials
#' @param idx names of the variables in the product
#' @param mu named vector of means
#' @param Sigma variance-covariance matrix with matching dimnames
#' @keywords internal
gauss_moment <- function(idx, mu, Sigma){
	if(length(idx)==0) return(1)
	if(length(idx)==1) return(unname(mu[idx]))
	first <- idx[1]
	rest <- idx[-1]
	out <- mu[[first]] * gauss_moment(rest, mu, Sigma)
	for(k in seq_along(rest)) out <- out + Sigma[first, rest[k]] * gauss_moment(rest[-k], mu, Sigma)
	unname(out)
}

#' @title structural_columns
#' @description the variables that are columns of the data structure rather than simulated: one 0/1
#' column per level of a fixed categorical effect and the values of a covariate
#' @param param n/a
#' @param data_structure n/a
#' @keywords internal
structural_columns <- function(param, data_structure){
	cols <- list()
	for(p in param){
		if(isTRUE(p$fixed)){
			x <- as.character(data_structure[[p$group]])
			lv <- sorted_levels(data_structure[[p$group]])
			# names are the levels, unless the levels are numbered 1, 2, ..., when beta i is level i
			lv_names <- if(all(p$names %in% lv)) p$names else lv
			for(i in seq_along(p$names)) cols[[p$names[i]]] <- as.numeric(x == lv_names[i])
		}else if(isTRUE(p$covariate)){
			for(nm in p$names) cols[[nm]] <- as.numeric(data_structure[[p$group]])
		}
	}
	if(length(cols)) do.call(cbind, cols) else NULL
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

## Expected mean and variance of the response, and how they split across components and variables.
## Every term in the model (each variable, and each interaction) is a product of variables, so its mean
## and its covariance with every other term are expectations of products:
## - simulated variables (predictors, random effects, residuals) are multivariate normal, so these
##   expectations are exact by Isserlis' theorem, including interactions with non-zero means
## - fixed categorical levels and covariates are columns of the data structure, so their means and
##   (co)variances - including correlations between different columns, e.g. a covariate whose values
##   differ between sexes - are taken directly from the data structure (over its rows)
## - the two kinds are independent, as squidSim simulates the normal variables independently of the data structure
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
		rownames(param[[i]]$beta) <- param[[i]]$names
	}

	p_names <- names(param)[names(param)!="interactions"]
	is_struc <- sapply(p_names, function(i) isTRUE(param[[i]]$fixed) || isTRUE(param[[i]]$covariate))

	## simulated (normal) variables: means and vcov, independent between components
	gauss <- p_names[!is_struc]
	mu_g <- do.call(c,c(lapply(gauss, function(i) param[[i]]$mean ), use.names=FALSE))
	names(mu_g) <- do.call(c, lapply(gauss, function(i) param[[i]]$names))
	Sigma_g <- make_big_matrix(lapply(gauss, function(i) {
		v <- as.matrix(param[[i]]$vcov)
		dimnames(v) <- list(param[[i]]$names, param[[i]]$names)
		v
	}))

	## data-structure variables
	S <- structural_columns(param[p_names[is_struc]], data_structure)
	struc_names <- colnames(S)

	## expectation of a product of variables
	moment <- function(v){
		s <- v[v %in% struc_names]
		g <- v[!v %in% struc_names]
		if(any(!g %in% names(mu_g))) stop("unknown variable in an interaction: ", paste(setdiff(g, names(mu_g)), collapse=", "))
		e_s <- if(length(s)) mean(apply(S[, s, drop=FALSE], 1, prod)) else 1
		e_s * gauss_moment(g, mu_g, Sigma_g)
	}

	## every term in the model, in the order of the components, as the variables multiplied together
	terms <- do.call(c, lapply(param, function(p) {
		if(p$component=="interactions") strsplit(p$names, ":") else as.list(p$names)
	}))
	term_names <- do.call(c, lapply(param, function(p) p$names))
	names(terms) <- term_names
	term_comp <- do.call(c, lapply(param, function(p) rep(p$component, length(p$names))))

	means <- sapply(terms, moment)
	covs <- matrix(0, length(terms), length(terms), dimnames=list(term_names, term_names))
	for(a in seq_along(terms)){
		for(b in seq_len(a)){
			covs[a,b] <- covs[b,a] <- moment(c(terms[[a]], terms[[b]])) - means[a]*means[b]
		}
	}

	betas <- do.call(rbind,lapply(param, function(p) p$beta ))
	names(means) <- rownames(betas) <- term_names

	## each component's own mean and vcov (variance within a component, as shown in the groups table)
	for(i in names(param)){
		idx <- which(term_comp==i)
		param[[i]]$mean <- means[idx]
		param[[i]]$vcov <- covs[idx, idx, drop=FALSE]
	}

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
			names = c("intercept",term_comp)
		)

	)
	return(out)

}

#' @title variance_barplot
#' @description stacked bar of variance contributions, with a legend of the components and the larger
#' segments labelled. Draws nothing (rather than failing) when the plot area is too small.
#' @param var variance of each segment
#' @param names name of each segment
#' @param colors colour of each segment
#' @param legend_names components to show in the legend (default: the segments themselves)
#' @param legend_colors their colours
#' @keywords internal
variance_barplot <- function(var, names, colors, legend_names = names, legend_colors = colors){
	# the intercept has no variance
	keep <- names != "intercept"
	var <- var[keep]
	names <- names[keep]
	colors <- colors[keep]
	legend_colors <- legend_colors[legend_names != "intercept"]
	legend_names <- legend_names[legend_names != "intercept"]

	# too small to draw (e.g. a very narrow window, or while the layout is still settling):
	# draw nothing, as even plot.new() can fail with "figure margins too large" on a tiny device
	if(any(grDevices::dev.size("in") < c(1.5, 1))) return(invisible(NULL))
	graphics::par(mar = c(0.5, 3, 0.5, 0.5), bg = NA)
	bar <- graphics::barplot(matrix(var, dimnames = list(names)), beside = FALSE, col = colors,
		width = 1, space = 0.2, xlim = c(0, 3), las = 1, cex.axis = 0.75)
	graphics::mtext("Variance", side = 2, line = 2, cex = 0.8)

	# label the segments that are big enough, in black or white depending on the fill
	pos <- pmax(var, 0)
	mid <- cumsum(pos) - pos/2
	big <- pos > 0.08 * max(sum(pos), 1e-12)
	dark <- colSums(grDevices::col2rgb(colors) * c(0.299, 0.587, 0.114)) < 140
	if(any(big)) graphics::text(bar, mid[big], names[big], cex = 0.65, col = ifelse(dark[big], "white", "black"))

	graphics::legend("topright", legend = rev(legend_names), fill = rev(legend_colors), bty = "n", cex = 0.75, inset = 0.01)
	invisible(NULL)
}
