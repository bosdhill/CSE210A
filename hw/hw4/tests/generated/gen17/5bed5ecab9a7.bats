load ../../harness

@test "5bed5ecab9a7" {
  check 'x  :=    2   +    1     ;

x     :=     z+  0    ' '⇒ skip; x := (z+0), {x → 3}
⇒ x := (z+0), {x → 3}
⇒ skip, {x → 0}'
}
