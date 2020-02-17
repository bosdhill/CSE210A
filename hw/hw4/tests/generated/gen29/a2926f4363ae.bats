load ../../harness

@test "a2926f4363ae" {
  check 'if (true  ∨  x-   z <B*   4)    then  
   y   :=    z    *KG      else  skip ' '⇒ y := (z*KG), {}
⇒ skip, {y → 0}'
}
