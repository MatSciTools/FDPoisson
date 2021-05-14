import sys
import numpy as np
import matplotlib.pyplot as pl
from mpl_toolkits import mplot3d

raw_x = []
raw_y = []
raw_potential = []
raw_efieldx = []
raw_efieldy = []
x = []
y = []

filename = sys.argv[1]
nx = int(sys.argv[2])
ny = int(sys.argv[3])
f = open(filename, "r")
data = f.readlines()

for i in range(1,len(data)):
   temparray = data[i].strip().split()
   raw_x.append(float(temparray[0]))
   raw_y.append(float(temparray[1]))
   raw_potential.append(float(temparray[2]))
   raw_efieldx.append(float(temparray[3]))
   raw_efieldy.append(float(temparray[4]))


for i in range(nx):
   x.append(raw_x[i])

for i in range(ny):
   y.append(raw_y[nx*i])

potential = np.array(raw_potential).reshape(ny, nx)
Ex = np.array(raw_efieldx).reshape(ny, nx)
Ey = np.array(raw_efieldy).reshape(ny, nx)
X, Y = np.meshgrid(x, y)
ax = pl.axes(projection='3d')
ax.plot_surface(X, Y, potential)
ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_zlabel("Potential(x,y)")
pl.savefig("potential_surface.pdf")

ax1 = pl.axes(projection='3d')
ax1.plot_surface(X, Y, Ex)
ax1.set_xlabel("x")
ax1.set_ylabel("y")
ax1.set_zlabel("Ex")
pl.savefig("Ex_surface.pdf")

ax2 = pl.axes(projection='3d')
ax2.plot_surface(X, Y, Ey)
ax2.set_xlabel("x")
ax2.set_ylabel("y")
ax2.set_zlabel("Ey")
pl.savefig("Ey_surface.pdf")
