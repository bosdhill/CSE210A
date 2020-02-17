load ../../harness

@test "cf560d22ec44" {
  check 'ui   :=     -2  -   -2     ;y :=   z-    z' '⇒ skip; y := (z-z), {ui → 0}
⇒ y := (z-z), {ui → 0}
⇒ skip, {ui → 0, y → 0}'
}
