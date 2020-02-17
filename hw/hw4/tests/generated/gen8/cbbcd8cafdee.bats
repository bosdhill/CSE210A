load ../../harness

@test "cbbcd8cafdee" {
  check 'if (-4  *    -3  =    x   -     4    ∨true) then  skip   else u0     :=    3   +  y' '⇒ skip, {}'
}
