val combinations = for (
  x <- 0 to 4;
  y <- 0 to 4;
  z <- 0 to 4;
  if(x + y + z == 4)
) yield (x, y, z)

combinations.length