import matplotlib.pyplot as plt
import numpy as np
import sys

import matplotlib.patches as mpatches
import matplotlib.pyplot as plt

file_name = str(sys.argv[1])

file_name2 = str(sys.argv[2])


myList = []
with open(file_name, 'r') as f:
    for line in f:
        myList.append(list(map(float, line.replace('"', '').strip().split())))

myList2 = []
with open(file_name2, 'r') as f2:
    for line2 in f2:
        myList2.append(list(map(float, line2.replace('"', '').strip().split())))        

myMean = np.mean(np.asarray(myList), axis=0)
myMean2 = np.mean(np.asarray(myList2), axis=0)


rang = np.arange(0, 4032, 32).tolist()
plt.plot(rang, myMean)
plt.hold('on')
plt.plot(rang, myMean2)
plt.grid()
plt.ylim(0.0, 1.0)
plt.ylabel('Eviction rate')
plt.xlabel('Number of addresses')
plt.show()

