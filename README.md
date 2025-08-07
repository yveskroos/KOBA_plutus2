
## 🧨 DÉMINEUR TERMINAL — HASKELL

Un jeu de démineur en mode texte (terminal) écrit entièrement en **Haskell**, avec affichage en couleur, interactions clavier et logique complète de jeu.

---

### 📷 Aperçu

```
=== 🧨 DÉMINEUR TERMINAL ===
Plateau 8×8 | Mines : 10

Mouvements : 0
   0 1 2 3 4 5 6 7
  +---------------
0 |? ? ? ? ? ? ? ?
1 |? ? ? ? ? ? ? ?
2 |? ? ? ? ? ? ? ?
3 |? ? ? ? ? ? ? ?
4 |? ? ? ? ? ? ? ?
5 |? ? ? ? ? ? ? ?
6 |? ? ? ? ? ? ? ?
7 |? ? ? ? ? ? ? ?

Commande (r row col = révéler | f row col = drapeau | q = quitter) :
```

---

### 🛠️ Fonctionnalités

✅ Plateau de 8×8 avec 10 mines aléatoires
✅ Révélation automatique des zones vides
✅ Système de drapeaux
✅ Détection de victoire/défaite
✅ Couleurs ANSI pour une meilleure lisibilité
✅ Sécurité sur les coordonnées et les entrées
✅ Affichage du nombre de mouvements effectués

---

### 🚀 Installation

#### 1. Cloner le projet (si sur GitHub)

```bash
git clone https://github.com/yveskroos/KOBA_plutus2.git
cd KOBA_plutus2
```

#### 2. Compiler avec GHC

Assurez-vous d’avoir GHC installé 

```bash
sudo apt install ghc
ghc --version
ghc demineur.hs -o demineur
```

#### 3. Lancer le jeu

```bash
./demineur
```

---

### 🎮 Commandes du jeu

| Commande        | Action                   |
| --------------- | ------------------------ |
| `r <row> <col>` | Révéler une case         |
| `f <row> <col>` | Poser/enlever un drapeau |
| `q`             | Quitter le jeu           |

*Exemple :*

```bash
r 3 4
f 2 1
```

---

### 📦 Fichiers

* `demineur.hs` – code source principal
* `README.md` – ce fichier

---

### 🔧 Personnalisation

Tu peux modifier ces constantes pour ajuster la difficulté :

```haskell
largeur = 8       
hauteur = 8        
nbMines = 10       -

```

---

### 📄 Licence

Ce projet est open source sous licence MIT. Tu peux l’utiliser, le modifier et le redistribuer librement.

---

