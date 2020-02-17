load ../../harness

@test "769912601d62" {
  check 'while false∨    -1     -   w     =    Zm   -     3  do     skip  ' '⇒ skip, {}'
}
