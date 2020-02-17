load ../../harness

@test "168882196e6f" {
  check 'y   :=-4   *-2    ;
  y     :=    -2    -  a    ' '⇒ skip; y := (-2-a), {y → 8}
⇒ y := (-2-a), {y → 8}
⇒ skip, {y → -2}'
}
