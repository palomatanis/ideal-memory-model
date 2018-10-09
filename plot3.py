import matplotlib.pyplot as plt
import numpy as np
import sys

import matplotlib.patches as mpatches
import matplotlib.pyplot as plt

file_name = str(sys.argv[1])
file_name2 = str(sys.argv[2])
file_name3 = str(sys.argv[3])

myList = []
with open(file_name, 'r') as f:
    for line in f:
        myList.append(list(map(float, line.replace('"', '').strip().split())))

myList2 = []
with open(file_name2, 'r') as f2:
    for line2 in f2:
        myList2.append(list(map(float, line2.replace('"', '').strip().split())))        


myList3 = []
with open(file_name3, 'r') as f3:
    for line3 in f3:
        myList3.append(list(map(float, line3.replace('"', '').strip().split())))        

        
myMean = np.mean(np.asarray(myList), axis=0)
myMean2 = np.mean(np.asarray(myList2), axis=0)
myMean3 = np.mean(np.asarray(myList3), axis=0)


rang = np.arange(0, 4032, 32).tolist()
plt.plot(rang, myMean, 'r')
plt.hold('on')
plt.plot(rang, myMean2, 'g')
plt.hold('on')
plt.plot(rang, myMean3, 'b')
re_patch = mpatches.Patch(color='red', label='Reduction')
gr_patch = mpatches.Patch(color='green', label='Naive reduction')
bl_patch = mpatches.Patch(color='blue', label='Noisy Reduction')
plt.legend(handles=[re_patch, gr_patch, bl_patch], loc='upper left')
plt.grid()
plt.ylim(0.0, 1.1)
plt.ylabel('')
plt.xlabel('Number of addresses')
plt.show()
