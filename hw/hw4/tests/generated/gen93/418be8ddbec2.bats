load ../../harness

@test "418be8ddbec2" {
  check 'y:=  x    - y ;
 z :=  z   *  x   ' '⇒ skip; z := (z*x), {y → 0}
⇒ z := (z*x), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
