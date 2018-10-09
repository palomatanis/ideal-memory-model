import matplotlib.pyplot as plt
from sklearn import preprocessing
import numpy as np
import sys

file_name = str(sys.argv[1])
print(file_name)

myList = []

with open(file_name, 'r') as f:
    for line in f:
    # myList = [line.replace('"', '').strip() for line in f]
        myList.append(list(map(float, line.replace('"', '').strip().split())))

        
myMean = np.mean(np.asarray(myList), axis=0)
myMean = np.delete(myMean, 0, 0)
myMean = np.delete(myMean, np.s_[-11:-1], 0)

#myMean.reshape(-1, 1)
mean_normalized = (myMean-min(myMean))/(max(myMean)-min(myMean))

#mean_normalized = preprocessing.normalize(myMean, norm='l2')

binom = np.genfromtxt('resultados_test.txt', delimiter=',', skip_header=2, names=['n', 'bi', 'mul'])

multi_s = binom['mul'][1::2]
binom_s = binom['bi'][1::2]
print(len(multi_s))

#rang = np.arange(0, 4000, 24).tolist()
rang = np.arange(24, 3745, 24).tolist()

plt.plot(rang, mean_normalized)
plt.hold('on')
plt.plot(rang, multi_s)
plt.grid()
plt.ylabel('Probability of eviction set for random address')
plt.xlabel('Number of addresses')
plt.show()

