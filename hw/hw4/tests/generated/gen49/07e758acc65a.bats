load ../../harness

@test "07e758acc65a" {
  check 'if (x    +  x=3 + z ∨   true)      then skip    else  y    :=  -3' '⇒ skip, {}'
}
