

closest_value <- function(data, val, error=0.01)
{
	pos <- which.min(abs(data - val))

	if (data[pos] > (val+error) || data[pos] < (val-error))
	{
		# Out of interval, keep looking
	}
	else {
		# Good enough value
		return (pos)
	}

}