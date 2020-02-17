load ../../harness

@test "293d1236ed3a" {
  check 'if (true     ∨     true)   then  
 h     :=    z    *0      else y  :=  -1  + 3   ' '⇒ h := (z*0), {}
⇒ skip, {h → 0}'
}
