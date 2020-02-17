load ../../harness

@test "db349f6414d9" {
  check 'z     :=   3     -     y    ;
 x:= Nc  +  z    ' '⇒ skip; x := (Nc+z), {z → 3}
⇒ x := (Nc+z), {z → 3}
⇒ skip, {x → 3, z → 3}'
}
