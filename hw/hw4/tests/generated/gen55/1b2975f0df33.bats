load ../../harness

@test "1b2975f0df33" {
  check 'skip   ; qh     := -3 *     x    ' '⇒ qh := (-3*x), {}
⇒ skip, {qh → 0}'
}
