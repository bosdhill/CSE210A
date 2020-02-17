load ../../harness

@test "298fa609f783" {
  check 'if (¬true)  then  
z := 4  -x    else i2 :=x   *-2     ' '⇒ i2 := (x*-2), {}
⇒ skip, {i2 → 0}'
}
