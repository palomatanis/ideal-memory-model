import matplotlib.pyplot as plt
import numpy as np

#file = open('results.txt','r')
tests = np.genfromtxt('results2.txt', delimiter=',', skip_header=0, names=['lista'])
#tests = file.readlines()
#file.close()
print(tests)
# print (tests)
rang = np.arange(0, 4000, 24).tolist()
print(len(rang))
plt.plot(rang, tests['lista'])
plt.grid()

###plt.plot(tests)
plt.ylabel('some numbers')
plt.show()

