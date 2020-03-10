# Experimenting with miniKanren #

It runs with [chicken scheme][] and it requires the egg [chicken scheme][].

[low-level-macros]: http://wiki.call-cc.org/eggref/4/low-level-macros
[chicken scheme]: https://call-cc.org/

How to install:

```
mkdir -p ./chiken-scm-eggs
chicken-install -init ./chiken-scm-eggs
export CHICKEN_REPOSITORY="${PWD}/chiken-scm-eggs"
chicken-install low-level-macros
```

How to run. Run `csi`, load miniKanren `(load "playing.scm")`, and play with
the definitions from `playing.scm`.
