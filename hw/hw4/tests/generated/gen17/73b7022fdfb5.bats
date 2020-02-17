load ../../harness

@test "73b7022fdfb5" {
  check 'if (¬(y   +  -1  =    x * z))     then skip else 
x    :=    -2   -     x  ' '⇒ skip, {}'
}
