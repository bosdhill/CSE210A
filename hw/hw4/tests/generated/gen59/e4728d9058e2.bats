load ../../harness

@test "e4728d9058e2" {
  check 'x     :=  4   -     y   ' '⇒ skip, {x → 4}'
}
