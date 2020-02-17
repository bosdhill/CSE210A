load ../../harness

@test "d3edd33f5719" {
  check 'while false    ∧    true      do  x     :=    x +  hG    ' '⇒ skip, {}'
}
