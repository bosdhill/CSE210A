load ../../harness

@test "f9236a149aa2" {
  check 'if (¬false)    then 

z:=  1    *    x    else 

skip  ' '⇒ z := (1*x), {}
⇒ skip, {z → 0}'
}
