# Proof that:   (A ⇒ ¬ E ⇒ ¬ C)    from    (A ∨ (B ⇒ C) ∨ (D ⇒ E))
from typing import Iterator, List


def truth_table_inputs(n: int = 0) -> Iterator[List[bool]]:
    if n > 0:
        for ins in truth_table_inputs(n-1):
            yield [True] + ins
            yield [False] + ins
    elif n == 0:
        yield []


print("A B C D E ((A ∨ B ⇒ (C ∨ D ⇒ E)))   ⇒   (A ⇒ ¬ E ⇒ ¬ C)")
for a, b, c, d, e in truth_table_inputs(5):
    note = not e
    notc = not c
    noteimnotc = (not note) or notc
    right = (not a) or noteimnotc
    aorb = a or b
    cord = c or d
    cordime = (not cord) or e
    left = (not aorb) or cordime
    total = (not left) or right
    print(f"{int(a)} {int(b)} {int(c)} {int(d)} {int(e)}"
          f"     {int(aorb)}   {int(left)}    {int(cord)}   {int(cordime)}        "
          f"{int(total)}      {int(right)}  {int(note)}  {int(noteimnotc)}  {int(notc)}")
