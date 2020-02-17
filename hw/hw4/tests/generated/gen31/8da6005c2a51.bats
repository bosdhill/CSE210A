load ../../harness

@test "8da6005c2a51" {
  check 'if (true    ∨    0  +2  <    x+0)    then skip    else 
 y :=    -4 +    3 ' '⇒ skip, {}'
}
