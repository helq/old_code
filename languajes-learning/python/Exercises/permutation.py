def comb(array):
	if len(array)==0: return [""]
	a = comb(array[1:])
	return a + map(lambda x:array[0]+x,a)

def perm(array):
	def between(x,array):
		if len(array)==0: return [x]
		else: return [x+array] + map(lambda a: array[0]+a,between(x,array[1:]))
	if len(array)==0: return [""]
	else: return reduce(lambda x,y:x+y,map(lambda x: between(array[0],x), perm(array[1:])))

	
def perm2(a): # by ilan ProjectEuler
	if len(a)==1:
		yield a
	else:
		for i in range(len(a)):
			this = a[i]
			rest = a[:i] + a[i+1:]
			for p in perm2(rest):
				yield [this] + p


def perm3(a):
	def between(a0,a):
		aF=[]
		for i in a:
			temp=[]
			for j in range(len(i)+1):
				temp += [i[:j]+[a0]+i[j:]]
			aF += temp
		return aF
	if len(a)==0: return [[]]
	return between(a[0],perm3(a[1:]))

def perm4(a):
	def between(a0,a):
		if len(a)==0: return []
		temp=[]
		for i in range(len(a[0])+1):
			temp += [a[0][:i]+[a0]+a[0][i:]]
		return temp + between(a0,a[1:])
	if len(a)==0: return [[]]
	return between(a[0],perm4(a[1:]))


def perm5(a):
	def between(a0,a):
		def betw(b,a0,a1):
			if len(a1)==0: return [b+[a0]]
			return [b+[a0]+a1] + betw(b+[a1[0]],a0,a1[1:])
		if len(a)==0: return []
		return betw([],a0,a[0]) + between(a0,a[1:])
	if len(a)==0: return [[]]
	return between(a[0],perm5(a[1:]))