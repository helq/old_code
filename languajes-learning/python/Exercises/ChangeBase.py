def base(n, b):
	if n != 0:
		base(n/5,b)
		print n%5,