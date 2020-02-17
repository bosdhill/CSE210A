load ../../harness

@test "e9d7f1fd7971" {
  check 'R := -4+z  ;z :=     fv+     x  ' '⇒ skip; z := (fv+x), {R → -4}
⇒ z := (fv+x), {R → -4}
⇒ skip, {R → -4, z → 0}'
}
