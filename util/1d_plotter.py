import sys
import matplotlib.pyplot as pl

x = []
phi = []
Ex = []

filename = sys.argv[1]

f = open(filename, "r")
data = f.readlines()
f.close()

for i in range(1,len(data)):
   temp = data[i].strip().split()
   x.append(float(temp[0]))
   phi.append(float(temp[1]))
   Ex.append(float(temp[2]))

pl.plot(x, phi, marker='o')
pl.xlabel("x")
pl.ylabel("Potential(x)")
pl.savefig("potential_1dplot.pdf")

pl.figure()
pl.plot(x, Ex, marker='o')
pl.xlabel("x")
pl.ylabel("E(x)")
pl.savefig("Ex_1dplot.pdf")
