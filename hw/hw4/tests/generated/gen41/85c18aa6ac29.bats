load ../../harness

@test "85c18aa6ac29" {
  check 'Ds    :=x    +     1 ;
N :=     z   - 3     ' '⇒ skip; N := (z-3), {Ds → 1}
⇒ N := (z-3), {Ds → 1}
⇒ skip, {Ds → 1, N → -3}'
}
