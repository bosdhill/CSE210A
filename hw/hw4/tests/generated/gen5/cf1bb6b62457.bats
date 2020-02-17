load ../../harness

@test "cf1bb6b62457" {
  check 'y :=     y     -     z ;  
 y    :=     1+    4' '⇒ skip; y := (1+4), {y → 0}
⇒ y := (1+4), {y → 0}
⇒ skip, {y → 5}'
}
