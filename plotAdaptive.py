import os

my_list = os.listdir(path)

files = [x.split("adaptive_eviction_test_50it_512psel_lru_bip_", 1)[1] for x in my_list if ("adaptive_eviction_test_50it_512psel_lru_bip" in x) and ("_psel" not in x) and ("hits" not in x)]

rang = np.arange(0, 4032, 32).tolist()

def num_accesses (eviction_strategy):
    c = eviction_strategy[0]
    d = eviction_strategy[2]
    l = eviction_strategy[4]
    return ([(l//x)*c*d for r in rang])
