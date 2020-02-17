load ../../harness

@test "2f7de74be900" {
  check 'z   :=  x  -     y   ;   skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
