import os
import numpy as np
import matplotlib.pyplot as plt




directory = "/home/paloma.pedregal/memory-model/adaptive/"

filesToPlot = "congruent_adaptive_eviction_test_100it_"

# my_list = os.listdir(directory)

# files = [x.split(filesToPlot, 1)[1] for x in my_list if (filesToPlot in x) and ("_psel" not in x) and ("hits" not in x)]

rang = np.arange(6, 31, 1).tolist()

def num_accesses (eviction_strategy):
    c = int(eviction_strategy[0])
    d = int(eviction_strategy[2])
    l = int(eviction_strategy[4])
    return [(x//l)*c*d if x else 0 for x in rang]

def numCongruent (n):
    return n - 5

plt.rcParams.update({'figure.max_open_warning': 0})

# for fs in files:
#     with open(directory + filesToPlot + fs) as f:
#         myList = [float(x) for x in f.read().splitlines()]
#         fig, ax1 = plt.subplots()
#         ax1.plot(rang, myList)
#         ax1.grid()
#         ax1.set_ylim([0.0, 1.0])
#         color = 'tab:grey'
#         plt.title(fs)
#         plt.ylabel('Eviction rate')
#         plt.xlabel('Number of addresses')
#         #plt.show()
#         fig.savefig(directory + "/figures/adaptive_congruent" + fs, dpi = fig.dpi)


# policies = ('lru', 'fifo', 'bip', 'lip', 'mru', 'rr')
policies = ('lru', 'bip', 'rr')

victims = ["v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15"]

cols = [p + "_" + v for p in policies for v in victims]


# rows = files

# the_table = plt.table(cellText=cell_text,
#                       rowLabels=rows,
#                       rowColours=colors,
#                       colLabels=columns,
#                       loc='bottom')

rows = ["1_1_1", "1_2_1", "1_2_2", "1_3_1", "1_3_2", "1_3_3", "1_4_1", "1_4_2", "1_4_3", "1_4_4", "1_5_1", "1_5_2", "1_5_3", "1_5_4", "1_5_5", "1_6_1", "1_6_2", "1_6_3", "1_6_4", "1_6_5", "1_6_6", "2_1_1", "2_2_1", "2_2_2", "2_3_1", "2_3_2", "2_3_3", "2_4_1", "2_4_2", "2_4_3", "2_4_4", "2_5_1", "2_5_2", "2_5_3", "2_5_4", "2_5_5", "2_6_1", "2_6_2", "2_6_3", "2_6_4", "2_6_5", "2_6_6", "3_1_1", "3_2_1", "3_2_2", "3_3_1", "3_3_2", "3_3_3", "3_4_1", "3_4_2", "3_4_3", "3_4_4", "3_5_1", "3_5_2", "3_5_3", "3_5_4", "3_5_5", "3_6_1", "3_6_2", "3_6_3", "3_6_4", "3_6_5", "3_6_6", "4_1_1", "4_2_1", "4_2_2", "4_3_1", "4_3_2", "4_3_3", "4_4_1", "4_4_2", "4_4_3", "4_4_4", "4_5_1", "4_5_2", "4_5_3", "4_5_4", "4_5_5", "4_6_1", "4_6_2", "4_6_3", "4_6_4", "4_6_5", "4_6_6", "5_1_1", "5_2_1", "5_2_2", "5_3_1", "5_3_2", "5_3_3", "5_4_1", "5_4_2", "5_4_3", "5_4_4", "5_5_1", "5_5_2", "5_5_3", "5_5_4", "5_5_5", "5_6_1", "5_6_2", "5_6_3", "5_6_4", "5_6_5", "5_6_6", "6_1_1", "6_2_1", "6_2_2", "6_3_1", "6_3_2", "6_3_3", "6_4_1", "6_4_2", "6_4_3", "6_4_4", "6_5_1", "6_5_2", "6_5_3", "6_5_4", "6_5_5", "6_6_1", "6_6_2", "6_6_3", "6_6_4", "6_6_5", "6_6_6"] #"10x1_1_1"]
  

for i in range (6, 32):
    print (i)
    elem = numCongruent (i)
    # Number of misses
    rowsDataMisses = []
    rowsDataEviction = []
    for s in rows:
        rowElemMisses = []
        rowElemEviction = []
        for pol in cols:
            openDir = directory + filesToPlot + pol + "_" + s
            with open (openDir + "_hits") as f:
                rowElemMisses.append (([float('%.3f'%(num_accesses(s)[elem] - (float(x)))) for x in f.read().splitlines()][elem]))
            with open (openDir) as f:
                rowElemEviction.append ([float('%.3f'%(float(x))) for x in f.read().splitlines()][elem])
        rowsDataMisses.append(rowElemMisses)
        rowsDataEviction.append(rowElemEviction)        
        
        
    # vals = [[int(j) for j in i] for i in rowsData]    
    # normal = plt.Normalize(vals.min()-1, vals.max()+1)  
    saveDir = directory + "figures/congruent_eviction_strategies_" + str(i)

    print ("a")
    
    vals = np.around(np.array(rowsDataMisses))
    normal = plt.Normalize(vals.min()-1, vals.max()+1)

    fig, ax = plt.subplots()
    # Hide axes
    ax.xaxis.set_visible(False) 
    ax.yaxis.set_visible(False)
    ax.axis('off')

    clust_data = rowsDataMisses
    ax.table(cellText=clust_data,rowLabels=rows, colLabels=cols, loc='center', colWidths=[0.1 for x in cols], cellColours=plt.cm.YlGnBu(normal(vals))) ## colormaps: pink, bone, cool, hot
    #plt.title(str(i) + " congruent addresses")
    # plt.show()
    plt.tight_layout()
    fig.savefig(saveDir + "_misses", bbox_inches='tight', dpi = fig.dpi)

    print ("b")
    vals = np.array(rowsDataEviction)
###    normal = plt.Normalize(vals.min()-1, vals.max()+1)
    normal = plt.Normalize(vals.min()+0, vals.max()+1)

    fig, ax = plt.subplots()
    # Hide axes
    ax.xaxis.set_visible(False) 
    ax.yaxis.set_visible(False)
    ax.axis('off')

    clust_data = rowsDataEviction
    ax.table(cellText=clust_data,rowLabels=rows, colLabels=cols, loc='center', colWidths=[0.1 for x in cols], cellColours=plt.cm.YlGnBu(normal(vals))) ## colormaps: pink, bone, cool, hot
    # plt.title(str(i) + " congruent addresses")
    # plt.show()
    plt.tight_layout()
    fig.savefig(saveDir, bbox_inches='tight', dpi = fig.dpi)
