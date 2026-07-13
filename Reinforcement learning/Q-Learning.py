import gymnasium as gym  # Libreria de openAI
import numpy as np 
import matplotlib.pyplot as plt

from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator

#creacion del ambiente determinista y lo muestra para el usuario
env = gym.make("FrozenLake-v1", is_slippery = False, render_mode="none")

#Cantidad de estados y acciones para la tabla Q
N_Estados = env.observation_space.n
N_Acciones = env.action_space.n

print(f"Estados: {N_Estados}, Acciones: {N_Acciones}")


#Definicion de tabla Q
Tabla_Q = np.zeros((N_Estados, N_Acciones))
#Parametros para Q-learning
alpha = 0.5
gamma = 0.9
epsilon = 1.0
epsilon_min = 0.1
decaimiento_e = 0.995
episodios = 2000
ruta = []


for episodio in range(episodios):
    print(f"episodio: {episodio}")
    estado, info = env.reset() #Reinicia el mundo
    terminado = False
    truncado = False
    print(Tabla_Q)# avanzamos: el "nuevo estado" ahora es el estado actual

    while not (terminado or truncado):
        if np.random.random() < epsilon:
            accion = env.action_space.sample()
        else:
            accion = np.argmax(Tabla_Q[estado])
        
        nuevo_estado, recompensa, terminado, truncado, info = env.step(accion)
        
        ruta.append([episodio, estado, accion])   # <-- movido aquí, antes de actualizar estado
        
        Tabla_Q[estado, accion] = Tabla_Q[estado, accion] + alpha * (
            recompensa + gamma * np.max(Tabla_Q[nuevo_estado]) - Tabla_Q[estado, accion]
        )
        estado = nuevo_estado
    # aquí, fuera del while pero dentro del for, decae epsilon
    epsilon = max(epsilon_min, epsilon * decaimiento_e)



env.close()
ruta = np.array(ruta)  # columnas: episodio, estado, accion

episodios_muestra = [0, 1, 10, 100, 500, 1000, 1999]

for ep in episodios_muestra:
    pasos_ep = ruta[ruta[:,0] == ep]
    estados_ep = pasos_ep[:,1].tolist()
    print(f"Episodio {ep}: {estados_ep}")

def graficar_todas_trayectorias(ruta, n_episodios, filas=4, columnas=4):
    ruta = np.array(ruta)  # columnas: episodio, estado, accion
    holes = [5, 7, 11, 12]
    goal = 15

    fig, ax = plt.subplots(figsize=(7, 7))

    # Dibujar la cuadrícula base
    for i in range(filas * columnas):
        col = i % columnas
        row = i // columnas
        y = filas - 1 - row
        if i in holes:
            color = "black"
        elif i == goal:
            color = "green"
        else:
            color = "whitesmoke"
        ax.add_patch(plt.Rectangle((col, y), 1, 1, facecolor=color, edgecolor="gray", zorder=1))
        ax.text(col + 0.5, y + 0.92, str(i), ha="center", va="top", fontsize=8, color="gray", zorder=3)

    # Colores: episodios tempranos = morado oscuro, tardíos = amarillo (viridis)
    colores = plt.cm.viridis(np.linspace(0, 1, n_episodios))

    for ep in range(n_episodios):
        pasos_ep = ruta[ruta[:, 0] == ep]
        if len(pasos_ep) == 0:
            continue
        estados_ep = pasos_ep[:, 1]

        xs = [(e % columnas) + 0.5 + np.random.uniform(-0.08, 0.08) for e in estados_ep]
        ys = [filas - 1 - (e // columnas) + 0.5 + np.random.uniform(-0.08, 0.08) for e in estados_ep]

        ax.plot(xs, ys, color=colores[ep], alpha=0.05, linewidth=1, zorder=2)

    ax.set_xlim(0, columnas)
    ax.set_ylim(0, filas)
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_aspect("equal")

    # Barra de color para indicar qué episodio representa cada tono
    sm = plt.cm.ScalarMappable(cmap="viridis", norm=plt.Normalize(0, n_episodios))
    sm.set_array([])
    cbar = plt.colorbar(sm, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label("Episodio")

    plt.title(f"Todas las trayectorias ({n_episodios} episodios)")
    plt.tight_layout()
    plt.savefig("trayectorias.png", dpi=150)
    plt.show()


graficar_todas_trayectorias(ruta, episodios)

def graficar_pasos_por_episodio(ruta, n_episodios):
    ruta = np.array(ruta)  # columnas: episodio, estado, accion

    pasos_por_episodio = []
    for ep in range(n_episodios):
        pasos_ep = ruta[ruta[:, 0] == ep]
        pasos_por_episodio.append(len(pasos_ep))

    plt.figure(figsize=(10, 5))
    plt.plot(range(n_episodios), pasos_por_episodio, linewidth=0.6, alpha=0.7, color="#185FA5")
    plt.xlabel("Episodio")
    plt.ylabel("Número de pasos")
    plt.title("Pasos por episodio durante el entrenamiento")
    plt.tight_layout()
    plt.savefig("pasos_por_episodio.png", dpi=150)
    plt.show()


graficar_pasos_por_episodio(ruta, episodios)
