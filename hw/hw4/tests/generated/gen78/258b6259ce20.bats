load ../../harness

@test "258b6259ce20" {
  check 'z:=  4  +-1  ; 

  y     :=     z' '⇒ skip; y := z, {z → 3}
⇒ y := z, {z → 3}
⇒ skip, {y → 3, z → 3}'
}
