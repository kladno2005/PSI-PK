import pandas as pd
import numpy as np
import itertools
from collections import Counter

import networkx as nx
import matplotlib.pyplot as plt

from community import community_louvain
from adjustText import adjust_text


df = pd.read_csv("konstytucja_czysta.csv")

texts = df["text_clean"].fillna("").astype(str)


cooc = Counter()

for txt in texts:

    words = txt.split()

    words = list(set(words))

    for a, b in itertools.combinations(sorted(words), 2):
        cooc[(a, b)] += 1

edges = pd.DataFrame(
    [(a, b, w) for (a, b), w in cooc.items()],
    columns=["source", "target", "weight"]
)


MIN_WEIGHT = 10

edges = edges[edges["weight"] >= MIN_WEIGHT]

print("Liczba krawędzi:", len(edges))


G = nx.Graph()

for _, row in edges.iterrows():
    G.add_edge(
        row["source"],
        row["target"],
        weight=row["weight"]
    )

G.remove_nodes_from(list(nx.isolates(G)))

print(
    "Węzły:", G.number_of_nodes(),
    "Krawędzie:", G.number_of_edges()
)


partition = community_louvain.best_partition(
    G,
    weight="weight",
    resolution=1.0,
    random_state=42
)


strength = dict(G.degree(weight="weight"))

node_sizes = np.array([
    strength[n]
    for n in G.nodes()
])

node_sizes = (
    200
    + 1800 *
    (node_sizes - node_sizes.min())
    / (node_sizes.max() - node_sizes.min())
)


pos = nx.spring_layout(
    G,
    weight="weight",
    k=0.55,
    iterations=500,
    seed=42
)


clusters = sorted(set(partition.values()))

palette = [
    "#4F8EF7",
    "#B76CFD",
    "#FF6B6B",
    "#78C850",
    "#F4C430",
    "#4DD0CF",
    "#FF9F43",
    "#9B59B6"
]

cluster_color = {
    c: palette[i % len(palette)]
    for i, c in enumerate(clusters)
}

node_colors = [
    cluster_color[partition[n]]
    for n in G.nodes()
]


plt.figure(figsize=(16, 10))
ax = plt.gca()

weights = np.array([
    G[u][v]["weight"]
    for u, v in G.edges()
])

edge_widths = (
    0.5
    + 5 *
    (weights - weights.min())
    / (weights.max() - weights.min())
)

edge_colors = [
    cluster_color[partition[u]]
    for u, v in G.edges()
]

nx.draw_networkx_edges(
    G,
    pos,
    width=edge_widths,
    edge_color=edge_colors,
    alpha=0.55
)

nx.draw_networkx_nodes(
    G,
    pos,
    node_size=node_sizes,
    node_color=node_colors,
    edgecolors="white",
    linewidths=1.2,
    alpha=0.95
)

texts_plot = []

for node in G.nodes():

    x, y = pos[node]

    texts_plot.append(
        plt.text(
            x,
            y,
            node,
            fontsize=9,
            ha="center",
            va="center"
        )
    )

adjust_text(texts_plot)

plt.title(
    "Sieć współwystępowania słów w Konstytucji RP\n"
    "Tylko silne połączenia (n ≥ 10)",
    loc="left",
    fontsize=20,
    weight="bold"
)

plt.axis("off")
plt.tight_layout()

plt.savefig(
    "siec_konstytucja.png",
    dpi=300,
    bbox_inches="tight"
)

plt.show()


nx.write_gexf(G, "konstytucja_network.gexf")