load ../../harness

@test "135b3d599d8d" {
  check 'while 0  + 2   <     -1   -    x    +  -3   ∧    y<     y   do  y     := y     -    -2 ' '⇒ skip, {}'
}
