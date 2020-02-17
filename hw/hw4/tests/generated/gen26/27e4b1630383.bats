load ../../harness

@test "27e4b1630383" {
  check 'while true   ∧   false    do skip   ' '⇒ skip, {}'
}
