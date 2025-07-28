calcChange owed given = if change > 0
  then change
  else 0
  where change = given - owed

calcChangeMax owed given = max change 0
  where change = given - owed