load ../../harness

@test "2043befe2858" {
  check 'W    :=     1     ; if (z +  -2   =  p   ∧   0 -LH     < -2*    y)   then 

skip     else 
 skip   ' '⇒ skip; if (((z+-2)=p)∧((0-LH)<(-2*y))) then { skip } else { skip }, {W → 1}
⇒ if (((z+-2)=p)∧((0-LH)<(-2*y))) then { skip } else { skip }, {W → 1}
⇒ skip, {W → 1}'
}
