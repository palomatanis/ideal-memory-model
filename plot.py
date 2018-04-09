import matplotlib.pyplot as plt
import numpy as np
import sys

file_name = str(sys.argv[1])
print(file_name)

myList = []

with open(file_name, 'r') as f:
    for line in f:
    # myList = [line.replace('"', '').strip() for line in f]
        myList.append(list(map(float, line.replace('"', '').strip().split())))

# with open("C:\name\MyDocuments\numbers") as file:
#     for line in file:
#         line = line.strip() #preprocess line
#         doSomethingWithThisLine(line) #

#print (myList)

myMean = np.mean(np.asarray(myList), axis=0)
#print(myMean)

# with open('resultsTLB.txt', 'r') as f:
#     for line in f:
#     # myList = [line.replace('"', '').strip() for line in f]
#         myList2 = list(map(float, line.replace('"', '').strip().split()))


rang = np.arange(0, 6000, 32).tolist()
plt.plot(rang, myMean)
# plt.hold('on')
# plt.plot(rang, myList2)
plt.grid()
plt.ylim(0.0, 1.0)
plt.ylabel('Number of addresses in eviction sets')
plt.xlabel('Number of addresses')
plt.show()

