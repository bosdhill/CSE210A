load ../../harness

@test "401023283082" {
  check 'z:=    3 +    4  ;

skip' '⇒ skip; skip, {z → 7}
⇒ skip, {z → 7}'
}
