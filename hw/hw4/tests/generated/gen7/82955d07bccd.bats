load ../../harness

@test "82955d07bccd" {
  check 'y := 1     -     x   ;  
 U    :=x   +     -1 ' '⇒ skip; U := (x+-1), {y → 1}
⇒ U := (x+-1), {y → 1}
⇒ skip, {U → -1, y → 1}'
}
