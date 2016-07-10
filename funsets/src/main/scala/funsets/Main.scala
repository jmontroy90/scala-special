package funsets

object Main extends App {
  import FunSets._
//  println(contains(singletonSet(1), 1))

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = singletonSet(4)


  val f1 = diff(union(union(s1,s2), s4), union(s2,s3))
  val f2 = union(union(union(s1,s2),s3),s4)
  val f2_f = filter(f2, (x: Int) => x > 3)
  val f3 = intersect(union(s1,s2), union(s2,s3))

  contains(f2_f, 4)
  printSet(f1)
  printSet(f3)
  printSet(f2_f)
//  println(exists(f2, (x: Int) => x == 4))

//  println(contains(f1,4)) // in set, meets condition
//  println(contains(f2_f, 3)) // in set, doesn't meet condition
//  println(contains(f2_f, 5)) // not in set, meets condition
//  println(contains(union(s1, s2), 3))
//  println(contains(intersect(union(s1,s2), union(s1,s2)), 1))
//  println(forall(f2, (x: Int) => x > 2))

}
