import matplotlib.pyplot as plt
import numpy as np
import sys

file_name = str(sys.argv[1])
print(file_name)

with open(file_name, 'r') as f:
    for line in f:
    # myList = [line.replace('"', '').strip() for line in f]
        myList = list(map(float, line.replace('"', '').strip().split()))

rang = np.arange(0, 4000, 24).tolist()
plt.plot(rang, myList)
plt.grid()
plt.ylabel('some numbers')
plt.show()

