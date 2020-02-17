load ../../harness

@test "993d8fc5adb3" {
  check 'if (-1  *     3<  z   ∨     z     <     4) then  z   :=    4      else  


z    :=   zu   +    -3   ' '⇒ z := 4, {}
⇒ skip, {z → 4}'
}
