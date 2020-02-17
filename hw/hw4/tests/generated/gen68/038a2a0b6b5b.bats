load ../../harness

@test "038a2a0b6b5b" {
  check 'if (0  * 4     =     hc    -1    ∨    true)    then  skip     else  x :=1 ' '⇒ skip, {}'
}
