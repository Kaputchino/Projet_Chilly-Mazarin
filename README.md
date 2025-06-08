# **Projet : *Chilly-Mazarin* – Mini-langage de logiques & de portes**

> *Cours IN213 — ENSTA Paris*
> *Auteurs : Mulard Andreas*
> *Dernière mise à jour : 2025-06-08*

---

## 1 . Motivation

Ce est un **langage dédié** à la description et à l’exécution de
petits circuits logiques :

* syntaxe proche d’un pseudo-code C;
* possibilité de définir des *gates* réutilisables et de les instancier
  hiérarchiquement ;
* gestion d’un troisième état **`undet`** pour explorer tous les scénarios
  de test quand une entrée reste indéterminée.

Le runtime OCaml :

* interprète le programme ;
* calcule tous les fils (signaux) ;
* peut exécuter automatiquement **2ᴺ scénarios** (avec N le nombre de signaux indéterminé) ;
* exporte les résultats dans des CSV ou les afficher.

---

## 2 . Syntaxe de base

| Élément                  | BNF simplifiée                 | Exemple                           |
| ------------------------ | ------------------------------ | --------------------------------- |
| **Déclaration d’entrée** | `input ID [= bool] ;`          | `input a = true;`<br>` |
| **Déclaration d’entrée non déterminée** | `input ID ;`          | `input a ;` |
| **Déclaration de gate**  | `gate ID (in*) (out*) {stmt*}` | ex. ci-dessous                    |
| **Instanciation**        | `ID = GateID (expr*) ;`        | `ha = halfadder(a,b);`            |
| **Accès champ**          | `inst.output`                  | `ha.sum`                          |
| **Affectation interne**  | `ID = expr ;`                  | `sum = x + y;`                    |
| **Print**                | `print( STRING , expr ) ;`     | `print("a:", a);`                 |
| **Write CSV**            | `write( STRING , target+ ) ;`  | `write("table.csv", a, ha.sum);`  |

### 2.1 Opérateurs logiques

| Symbole(s)       | Opération | 
| ---------------- | --------- |
| `!`              | NOT       | 
| `- & ∩ ∧`        | AND       | 
| `+ \| ∪ ∨`       | OR        | 
| Parenthèses `()` | —         |
---

## 3 . Sémantique des signaux

```text
TTrue   ⟹ logique 1
TFalse  ⟹ logique 0
TUndet  ⟹ indéterminé   (exploration « tous scénarios »)
Opération   TTrue   TFalse  TUndet
NOT         TFalse  TTrue   TUndet
AND         —       TFalse  —
OR          TTrue   —       —

(* Les cases marquées « — » suivent les règles décrites :
AND renvoie TFalse si un seul opérande vaut TFalse, TTrue si les deux
vaut TTrue, sinon TUndet. Pareil pour OR.)
```

Quand au moins une entrée est undet, l’interpréteur parcourt 2ᴺ
combinaisons (N = entrées undet) et :

affiche :

```arduino
=== scénario 3/4 ===
a = true
sel = false
...
```

regroupe les résultats pour chaque scénario dans le CSV de sortie.

---

## 4 . Exemple complet

```chilly
input a;              // indéterminé -> 2 scénarios
input b = true;
input cin = false;
input sel;

gate halfadder(x, y)(sum, carry) {
  sum   = x + y - (x - y);
  carry = x - y;
}

gate mux2(a0, a1, s)(out) {
  out = (s - a1) + ((!s) - a0);
}

gate fulladder(x, y, c)(sum, carry) {
  ha1 = halfadder(x, y);
  ha2 = halfadder(ha1.sum, c);
  sum   = ha2.sum;
  carry = ha1.carry + ha2.carry;
}

ha   = halfadder(a, b);
fa   = fulladder(a, b, cin);
selM = mux2(ha.sum, fa.sum, sel);

print("↪ a        :", a);
print("↪ fa.sum   :", fa.sum);
print("↪ mux out  :", selM.out);

write("all_signals.csv",
      a, b, cin, sel,
      ha.sum, ha.carry,
      fa.sum, fa.carry,
      selM.out);
```

Sortie console (extrait)

```python-repl
=== scénario 1/2 ===
input a = false
...
↪ fa.sum   fa.sum = false
...

=== scénario 2/2 ===
input a = true
...
↪ fa.sum   fa.sum = true
...
→ CSV mis à jour : all_signals.csv (2 scénarios)
```

Fichier `all_signals.csv`

```csv
a,b,cin,sel,ha.sum,ha.carry,fa.sum,fa.carry,selM.out
FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE
TRUE ,TRUE,FALSE,FALSE,TRUE ,FALSE,TRUE ,FALSE,TRUE
```

---

## 5 . Compilation & exécution

```bash
make clean && make        # compile l'interpréteur
./pcfloop exemple.circ    # exécute un fichier source
```

L’interpréteur affiche les scénarios, met à jour
les CSV et renvoie un code de sortie 0 si tout s’est bien passé.

---

