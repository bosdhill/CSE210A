load ../../harness

@test "b97f36dd40c1" {
  check 'y:=  3 -     y ;

y   :=     4  -  -3     ' '⇒ skip; y := (4--3), {y → 3}
⇒ y := (4--3), {y → 3}
⇒ skip, {y → 7}'
}
