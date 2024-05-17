import subprocess
import time
import numpy as np 
import matplotlib.pyplot as plt

resultats = [[],[],[]]

puissance = int(input("Puissance: "))
puiss_max = int(input("Puissance max: "))
nb_tests = int(input("Nombre de tests par puissance: "))

for exp in range(puiss_max+1):
  resultats[0].append(0)
  resultats[1].append(0)
  resultats[2].append(0)
  print(f"Current size = {puissance**exp}")

  for i in range(nb_tests):
    print(f"{int(i*100/nb_tests)}%", end='\r')
    subprocess.call(
        ["../generateur_formule/gen", "test_perf.txt",
         f"{puissance**exp}"])
    debut = time.time()
    subprocess.call(["../satsolver", "test_perf.txt", "-q1", "-nofnc", "-hide", "-noinfo"])
    resultats[0][exp] += (time.time() - debut)
    debut = time.time()
    subprocess.call(["../satsolver", "test_perf.txt", "-q2", "-nofnc", "-hide", "-noinfo"])
    resultats[1][exp] += (time.time() - debut)
    debut = time.time()
    subprocess.call(["../satsolver", "test_perf.txt", "-q3", "-nofnc", "-hide", "-noinfo"])
    resultats[2][exp] += (time.time() - debut)

  resultats[0][exp] /= nb_tests
  resultats[1][exp] /= nb_tests
  resultats[2][exp] /= nb_tests



# Affiche les résultats

x = np.arange(puiss_max+1) 
width = 0.25 
multiplier = 0

fig, ax = plt.subplots(layout='constrained')

for i in range(3):
  offset = width * multiplier
  rects = ax.bar(x + offset, resultats[i], width, label=f"v{i+1}")
  ax.bar_label(rects, padding=3, fmt = '%.2E')
  multiplier += 1

ax.set_ylabel('Temps (s)')
ax.set_xlabel('Taille de la formule')
ax.set_title("Temps d'execution des algorithmes")
ax.set_xticks(x + width, [f"{puissance**i}" for i in range(puiss_max+1)])
ax.legend(loc='upper left', ncols=3)

print("Done!")

plt.show()