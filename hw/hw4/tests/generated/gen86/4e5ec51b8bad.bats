load ../../harness

@test "4e5ec51b8bad" {
  check 'y :=    A8  +    MG   ;z     :=  2+    -2    ' '⇒ skip; z := (2+-2), {y → 0}
⇒ z := (2+-2), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
