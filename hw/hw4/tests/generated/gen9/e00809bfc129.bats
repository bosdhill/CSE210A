load ../../harness

@test "e00809bfc129" {
  check 'if (4 +3<  2   *     -2   ∨     F1   +     G=     2    +    x)      then    
skip    else    skip' '⇒ skip, {}'
}
