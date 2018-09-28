import os
import numpy as np
import matplotlib.pyplot as plt

directory = "/home/paloma.pedregal/memory-model/adaptive"
my_list = os.listdir(directory)

files = [x.split("adaptive_eviction_test_50it_512psel_lru_bip_", 1)[1] for x in my_list if ("adaptive_eviction_test_50it_512psel_lru_bip" in x) and ("_psel" not in x) and ("hits" not in x)]

rang = np.arange(0, 4032, 32).tolist()

def num_accesses (eviction_strategy):
    c = eviction_strategy[0]
    d = eviction_strategy[2]
    l = eviction_strategy[4]
    return [(x//(int(l))*int(c)*int(d)) if x else 0 for x in rang]


# myList = []
# for fs in files:
#     with open(fs) as f:
#         myList.append(f.read().splitlines())

plt.rcParams.update({'figure.max_open_warning': 0})

for fs in files:
    with open(directory + "/adaptive_eviction_test_50it_512psel_lru_bip_" + fs) as f:
        myList = [float(x) for x in f.read().splitlines()]
        with open(directory + "/adaptive_eviction_test_50it_512psel_lru_bip_" + fs + "_psel") as f2:
            listPsel = [float(x) for x in f2.read().splitlines()]
            fig, ax1 = plt.subplots()
            ax1.plot(rang, myList)
            ax1.grid()
            ax1.set_ylim([0.0, 1.0])
            ax2 = ax1.twinx()
            color = 'tab:grey'
            ax2.set_ylabel('Psel', color=color)  # we already handled the x-label with ax1
            ax2.plot(rang, listPsel, color=color)
            ax2.set_ylim(400, 624)
#            ax2.set_ylim(0, 1024)
            ax2.hold('on')
            ax2.axhline(y=512)
            plt.title(fs)
            plt.ylabel('Eviction rate')
            plt.xlabel('Number of addresses')
            #plt.show()
            fig.savefig(directory + "/figures/adaptive" + fs, dpi = fig.dpi)

        

#fig.savefig('temp.png', dpi=fig.dpi)

#myMean = np.mean(np.asarray(myList), axis=0)

# plt.plot(rang, myMean)
# # plt.hold('on')
# # plt.plot(rang, myList2)
# plt.grid()
# plt.ylim(0.0, 1.0)
# plt.title('LRU-noisy')
# plt.ylabel('Eviction rate')
# plt.xlabel('Number of addresses')
# plt.show()
