load ../../harness

@test "c7660c4329d8" {
  check 'y  :=z+     2   ;

 
zC  :=    -1' '⇒ skip; zC := -1, {y → 2}
⇒ zC := -1, {y → 2}
⇒ skip, {y → 2, zC → -1}'
}
