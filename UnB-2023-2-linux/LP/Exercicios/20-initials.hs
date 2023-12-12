initials :: String -> String -> String
initials firsname lastname = [f] ++ ". "++[l]++"."
   where (f:_)=firsname
         (l:_)=lastname