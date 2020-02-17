load ../../harness

@test "5315d383abf5" {
  check 'x := x  - b   ;y   :=  gc     *   -4  ' '⇒ skip; y := (gc*-4), {x → 0}
⇒ y := (gc*-4), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
