load ../../harness

@test "bb44963dacb9" {
  check 'if (¬(A+    x     <   -4   --4))      then 
Gk:=y + 4      else  
{skip ;
   skip}' '⇒ Gk := (y+4), {}
⇒ skip, {Gk → 4}'
}
