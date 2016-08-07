import bigraph._

val p0 = (Ion(20) | Ion(30) | Ion(123)| Ion(3))
val p1 = (Ion(50) | Ion(60) | Ion(70) | Ion(45) | Ion(43)) || Unit || Unit || Unit

p0 compose p1



