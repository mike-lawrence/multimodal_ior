library(data.table) #for data.table & rbindlist

lag1diff <- function (x,lx){ #faster than diff(x,lag=1)
	return(x[-1]-x[-lx])
}
lag2diff <- function (x,lx){ #faster than diff(x,lag=2)
	return(x[-(1:2)]-x[-((lx-1):lx)])
}

get_itd = function(y,x,ly){
	y_lag1diff = lag1diff(y,ly)
	y_lag1diff_is_zero = y_lag1diff==0
	reps = NULL
	if(any(y_lag1diff_is_zero)){ #deal with ties
		values_to_change = c(F,y_lag1diff_is_zero)|c(y_lag1diff_is_zero,F)
		z = cumsum(y != c(0, y[1:(ly-1)]))
		df = data.table(
			x = x[values_to_change]
			, y = y[values_to_change]
			, z = z[values_to_change]
		)
		out = df[ , list(y = mean(y),x=mean(x),reps=length(x)), by="z"]
		reps = c(rep(1,sum(!values_to_change)),out$reps)
		x = c(x[!values_to_change],as.numeric(out$x))
		y = c(y[!values_to_change],as.numeric(out$y))
		reps = reps[order(x)]
		y = y[order(x)]
		x = sort(x)
		ly = length(y)
		y_lag1diff = lag1diff(y,ly)
		y_lag1diff_is_zero = y_lag1diff==0
	}
	is_extrema = as.logical(abs(c(1,sign(lag1diff(sign(y_lag1diff),ly-1)),1)))
	if(sum(is_extrema)<3){ 
		if(!is.null(reps)){
			y = rep(y,times=reps)
		}
		return(list(y=y,resid=NULL,n=sum(is_extrema)))
	}else{
		extrema_vals = y[is_extrema]
		extrema_index = (1:ly)[is_extrema]
		lex = length(extrema_vals)
		rise = lag2diff(extrema_vals,lex)
		run = lag2diff(x[extrema_index],lex)

		slope = rise/run
		intercept = extrema_vals[1:length(slope)] - x[extrema_index][1:length(slope)]*slope

		old_extrema_lag1diff = lag1diff(extrema_vals,lex)
		predicted = x[extrema_index][2:(length(slope)+1)]*slope + intercept
		observed = extrema_vals[2:(length(slope)+1)]
		new_extrema_vals = observed - (observed - predicted)/2
		new_first = extrema_vals[1] + old_extrema_lag1diff[1]/2
		new_last = extrema_vals[length(extrema_vals)] - old_extrema_lag1diff[length(extrema_vals)-1]/2
		new_extrema_vals = c(new_first,new_extrema_vals,new_last) 

		new_extrema_lag1diff = lag1diff(new_extrema_vals,lex)
		scaling = new_extrema_lag1diff/old_extrema_lag1diff

		index_of_last_x = cumsum(abs(is_extrema))

		last_extrema = extrema_vals[index_of_last_x]
		old_lag1diff = y - last_extrema
		new_lag1diff = old_lag1diff*scaling[index_of_last_x]
		new_val = new_extrema_vals[index_of_last_x] + new_lag1diff
		new_val[length(new_val)] = new_extrema_vals[length(new_extrema_vals)]
		if(!is.null(reps)){
			new_val = rep(new_val,times=reps)
			y = rep(y,times=reps)
		}
		return(list(y=y-new_val,resid=new_val,n=sum(is_extrema)))
	}
}


get_all_itds = function(y,freq,show_progress=T){
	ly = length(y)
	x = 1:ly
	out = list()
	out[[1]] = data.frame(
		x = x
		, y = y
		, signal = 'Raw'
	)
	itd_num = 0
	result = list(
		resid = y
		, x = x
	)
	delta = y
	done = FALSE
	all_start = proc.time()[3]
	while(!done){
		itd_num = itd_num+1
		this_start = proc.time()[3]
		result = get_itd(y=result$resid,x=x,ly=ly)
		this_stop = proc.time()[3]
		out[[itd_num+1]] = data.frame(
			x = x
			, y = result$y
			, signal = paste0('ITD',itd_num)
			, hz = (result$n/2)/( ly / freq )
		)
		delta = delta - result$y
		if(is.null(result$resid)){
			done = TRUE
		}else{
			if(show_progress){
				print(paste0('ITD',itd_num,' obtained in ',round(this_stop-this_start),' seconds. Extrema: ',result$n))
			}
		}
	}
	out[[length(out)]]$signal = 'Residual'
	out[[itd_num+2]] = data.frame(
		x = x
		, y = delta
		, signal = 'Delta'
		, hz = NA
	)
	if(show_progress){
		print(paste('All ITDs obtained in ',round(this_stop-all_start) ,' seconds.'))
	}
	return(out)
}
