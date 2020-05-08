// hier sollen alle geraden zahlen bis 100 geschrieben werden
1 to 50 foreach {e => println(e)}

// die funktion soll prüfen ob a durch 2 teilbar ist
def teste(a:Int) = a/2==0

// hier soll gezählt werden wie lang die liste ist
val liste = List(1,1,1,1,1,1,2,1,1,1,1)
var wieLangIstDieListe = 0
liste.foreach { e =>
  wieLangIstDieListe = wieLangIstDieListe + e
}
println(wieLangIstDieListe)

// die funktion soll die kleinere zahl finden
def kleiner(a:Int, b:Int) = if (a>a) a else b

// hier dasselbe aber für 3 zahlen
def kleinerer(a:Int, b:Int,c:Int) = {
  val ab = kleiner(a, b)
  val ba  = kleiner(b, a)
  kleiner(ab, ba)
}