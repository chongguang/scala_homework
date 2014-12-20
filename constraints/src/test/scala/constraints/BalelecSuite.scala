package constraints

import regolic.asts.fol.Trees._
import cafesat.api.API._

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalelecSuite extends FunSuite {

  import Balelec._

  def checkAssignment(
    assignment: Array[List[Int]],
    people: Array[List[Int]], slots: Array[Int], max: Int
  ): Unit = {
    val counter = Array.fill(people.size)(0)
    assert(assignment.size === slots.size)
    for(i <- 0 until assignment.size) {
      assert(assignment(i).size === slots(i))
      assignment(i).foreach(p => {
        assert(people(p).contains(i))
        counter(p) = counter(p) + 1
      })
    }
    for(i <- 0 until people.size) {
      assert(counter(i) <= max)
    }
  }

  def checkNumber(res: List[Formula], context: Set[Formula], expected: List[Boolean]): Boolean = {

    val rres = res.reverse

    val digitsOk = and(rres.zip(expected.reverse).map(p => p._1 iff p._2):_*)
    val leadingZeros = !or(rres.drop(expected.size):_*)

    val theorem = digitsOk && leadingZeros && and(context.toSeq:_*)
    expected.size <= res.size && solveForSatisfiability(theorem) != None
  }

  test("checkNumber is testing what it is supposed to") {
    assert(checkNumber(List(true), Set(), List(true)))
    assert(checkNumber(List(false), Set(), List(false)))
    assert(checkNumber(List(true, false), Set(), List(true, false)))
    assert(checkNumber(List(true, true), Set(), List(true, true)))
    assert(checkNumber(List(false, true), Set(), List(true)))
    assert(checkNumber(List(false, false, true), Set(), List(true)))
    assert(!checkNumber(List(true), Set(), List(false)))
    assert(!checkNumber(List(false), Set(), List(true)))
    assert(!checkNumber(List(true), Set(), List(true, true)))
    assert(!checkNumber(List(false), Set(), List(true, false)))
  }


  test("sumBits is implemented") {
    sumBits(List(true))
  }

  test("sumBits of single bit") {
    val bits1: List[Formula] = List(false)
    val (res1, constraints1) = sumBits(bits1)
    assert(checkNumber(res1, constraints1, List(false)))
    assert(!checkNumber(res1, constraints1, List(true)))

    val bits3: List[Formula] = List(true)
    val (res3, constraints3) = sumBits(bits3)
    assert(checkNumber(res3, constraints3, List(true)))
    assert(!checkNumber(res3, constraints3, List(false)))
  }

  test("SumBits is working") {
    val bits2: List[Formula] = List(true, false, true, false, true)
    val (res2, constraints2) = sumBits(bits2)
    assert(checkNumber(res2, constraints2, List(true, true)))
    assert(!checkNumber(res2, constraints2, List(true, true, true)))

    val bits4: List[Formula] = List(false, true)
    val (res4, constraints4) = sumBits(bits4)
    assert(checkNumber(res4, constraints4, List(true)))
  }

  test("sumBits of empty list counts 0") {
    val bits1: List[Formula] = List()
    val (res1, constraints1) = sumBits(bits1)
    assert(checkNumber(res1, constraints1, List(false)))
    assert(!checkNumber(res1, constraints1, List(true)))
  }


  test("basic scheduling") {

    val people1 = Array(List(0))
    val slots1 = Array(1)
    val maxWorkload1 = 1
    val res1 = schedule(people1, slots1, maxWorkload1)
    assert(!res1.isEmpty)
    res1.foreach(assignments => {
      checkAssignment(assignments, people1, slots1, maxWorkload1)
      assert(assignments.size === 1)
      assert(assignments(0) === List(0))
    })

    val people2 = Array(List(0), List(1))
    val slots2 = Array(1, 1)
    val maxWorkload2 = 1
    val res2 = schedule(people2, slots2, maxWorkload2)
    assert(!res2.isEmpty)
    res2.foreach(assignments => {
      checkAssignment(assignments, people2, slots2, maxWorkload2)
      assert(assignments.size === 2)
      assert(assignments(0) === List(0))
      assert(assignments(1) === List(1))
    })


    val people3 = Array(List(1), List(0))
    val slots3 = Array(1, 1)
    val maxWorkload3 = 1
    val res3 = schedule(people3, slots3, maxWorkload3)
    assert(!res3.isEmpty)
    res3.foreach(assignments => {
      checkAssignment(assignments, people3, slots3, maxWorkload3)
      assert(assignments.size === 2)
      assert(assignments(0) === List(1))
      assert(assignments(1) === List(0))
    })

    val people4 = Array(List(2, 1), List(1), List(2,0))
    val slots4 = Array(1, 1, 1)
    val maxWorkload4 = 1
    val res4 = schedule(people4, slots4, maxWorkload4)
    assert(!res4.isEmpty)
    res4.foreach(assignments => {
      checkAssignment(assignments, people4, slots4, maxWorkload4)
      assert(assignments.size === 3)
      assert(assignments(0) === List(2))
      assert(assignments(1) === List(1))
      assert(assignments(2) === List(0))
    })

    val people5 = Array(List(0, 1, 2), List(1), List(2,1))
    val slots5 = Array(1, 1, 1)
    val maxWorkload5 = 1
    val res5 = schedule(people5, slots5, maxWorkload5)
    assert(!res5.isEmpty)
    res5.foreach(assignments => {
      checkAssignment(assignments, people5, slots5, maxWorkload5)
      assert(assignments.size === 3)
      assert(assignments(0) === List(0))
      assert(assignments(1) === List(1))
      assert(assignments(2) === List(2))
    })
  }

  test("small scheduling") {

    val people1 = Array(
      List(0,3),
      List(1,2),
      List(0,1,2,3,4),
      List(2,4),
      List(1,3)
    )
    val slots1 = Array(2,1,2,1,2)
    val maxWorkload1 = 2
    val res1 = schedule(people1, slots1, maxWorkload1)
    assert(!res1.isEmpty)
    res1.foreach(assignments => {
      checkAssignment(assignments, people1, slots1, maxWorkload1)
    })

    val people2 = Array(
      List(0,1,2),
      List(0,1,2),
      List(0,1,2),
      List(0,1,2),
      List(0,1,2)
    )
    val slots2 = Array(4, 2, 3)
    val maxWorkload2 = 2
    val res2 = schedule(people2, slots2, maxWorkload2)
    assert(!res2.isEmpty)
    res2.foreach(assignments => {
      checkAssignment(assignments, people2, slots2, maxWorkload2)
    })

  }

  /*
   * The following tests require a few optimizations in order
   * to complete in a reasonable amount of time. In particular,
   * sum needs to be implemented carefully with a maximum of intermediate
   * constraints introduced to manage the size of the formula.
   */

  //test("medium scheduling", 10) {
  //  val people1 = Array(
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2),
  //    List(0,1,2)
  //  )
  //  val slots1 = Array(7,2,3)
  //  val maxWorkload1 = 2
  //  val res1 = schedule(people1, slots1, maxWorkload1)
  //  assert(!res1.isEmpty)
  //  res1.foreach(assignments => {
  //    checkAssignment(assignments, people1, slots1, maxWorkload1)
  //  })

  //}

  //test("\"large\" scheduling", 10) {
  //  val people1 = Array(
  //    List(0,1,2,3),
  //    List(0,1,2,4),
  //    List(0,1,2,5),
  //    List(0,1,2,6),
  //    List(0,1,2,3,4,5),
  //    List(2,4,6),
  //    List(2,4,6,7),
  //    List(1,2,4,6),
  //    List(0,1,2,5,6),
  //    List(4,5,6),
  //    List(4,5,6),
  //    List(4,5,6),
  //    List(0,1,2,5),
  //    List(0,1,2,6),
  //    List(0,1,2,3,4,5),
  //    List(2,4,6),
  //    List(2,4,6,7),
  //    List(2,3,6,7),
  //    List(1,2,4,6),
  //    List(0,1,2,5,6),
  //    List(4,5,6),
  //    List(4,5,6),
  //    List(4,5,6),
  //    List(1,2,7),
  //    List(1,4,5,7),
  //    List(4,6,7),
  //    List(1,4,5,7),
  //    List(3,7),
  //    List(0,1,2,3,4,5,6,7)
  //  )
  //  val slots1 = Array(2,3,1,4,2,2,3,2)
  //  val maxWorkload1 = 2
  //  val res1 = schedule(people1, slots1, maxWorkload1)
  //  assert(!res1.isEmpty)
  //  res1.foreach(assignments => {
  //    checkAssignment(assignments, people1, slots1, maxWorkload1)
  //  })

  //}

  //test("Scheduling volonteer for Balelec", 10) {
  //  //16 people with various availability
  //  val people = Array(
  //    List(0, 1, 2),
  //    List(0, 1, 2),
  //    List(0, 1, 2, 5),
  //    List(0, 1, 2, 3),
  //    List(0, 1, 2, 3, 5),
  //    List(0, 2, 4, 5),
  //    List(0, 2, 4, 5),
  //    List(0, 1, 2, 4, 5),
  //    List(0, 3, 4, 5),
  //    List(0, 3, 4, 5),
  //    List(1, 2, 4, 5),
  //    List(1, 2, 3),
  //    List(1, 2, 3),
  //    List(1, 2, 3),
  //    List(0, 1, 2, 3, 4, 5),
  //    List(0, 1, 2, 3, 4, 5)
  //  )

  //  //6 tasks
  //  val tasks = Array(5, 3, 2, 4, 5, 7)

  //  //no more than 3 tasks
  //  val workload = 3

  //  val res = schedule(people, tasks, workload)

  //  assert(!res.isEmpty)
  //  res.foreach(assignment => checkAssignment(assignment, people, tasks, workload))

  //}

  def checkBalelecScheduling(
    solution: Map[Task, List[Volunteer]],
    volunteers: List[Volunteer], tasks: List[Task],
    availability: Map[Volunteer, List[Task]], maxWorkload1: Int
  ): Boolean = {
    solution.forall{ case (t, vs) => t.capacity == vs.size } &&
    solution.keys.toSet == tasks.toSet &&
    solution.forall{ case (t, vs) => vs.forall(v => availability(v).contains(t)) }
  }

  //test("Balelec volunteers assignment", 10) {
  //  val Regis = Volunteer("Regis")
  //  val Mano = Volunteer("Mano")
  //  val Manos = Volunteer("Manos")
  //  val Heather = Volunteer("Heather")
  //  val Martin = Volunteer("Martin")
  //  val Viktor = Volunteer("Viktor")
  //  val Vojin = Volunteer("Vojin")

  //  val homeworkPreparation = Task("Homework preparation", 2)
  //  val homeworkCorrection = Task("Homework correction", 4)
  //  val lectureConstraints = Task("Lecture constraints", 3)
  //  val lectureRecursivity = Task("Lecture recursivity", 3)
  //  val exerciseAssistant = Task("Exercise assistant", 4)
  //  val examPreparation = Task("Exam preparation", 2)
  //  val examCorrection = Task("Exam correction", 4)

  //  val availability = Map(
  //    Regis -> List(homeworkPreparation, homeworkCorrection, lectureConstraints, exerciseAssistant),
  //    Mano -> List(homeworkPreparation, homeworkCorrection, lectureRecursivity, exerciseAssistant, examPreparation),
  //    Manos -> List(lectureConstraints, exerciseAssistant, examPreparation, examCorrection),
  //    Heather -> List(homeworkPreparation, homeworkCorrection, lectureRecursivity, examCorrection),
  //    Martin -> List(lectureRecursivity, examPreparation, examCorrection),
  //    Viktor -> List(lectureConstraints, examPreparation, examCorrection),
  //    Vojin -> List(homeworkPreparation, homeworkCorrection, exerciseAssistant, examPreparation, examCorrection)
  //  )

  //  val volunteers = List(Regis, Mano, Manos, Heather, Martin, Viktor, Vojin)
  //  val tasks = List(homeworkPreparation, homeworkCorrection, lectureConstraints, lectureRecursivity, exerciseAssistant, examPreparation, examCorrection)
  //  val maxWorkload = 4

  //  val res = Balelec.schedule(volunteers, tasks, availability, maxWorkload)
  //  assert(!res.isEmpty)
  //  assert(checkBalelecScheduling(res.get, volunteers, tasks, availability, maxWorkload))
  //}


  def getRandomSubSet(n: Int): List[Int] = {
    (0 until n).filter(_ => math.random < 0.5).toList
  }

  def generateProblem(nbPeople: Int, nbSlots: Int): (Array[List[Int]], Array[Int]) = {
    val people: Array[List[Int]] = Array.fill(nbPeople)(Nil).map(_ => getRandomSubSet(nbSlots))
    val slots: Array[Int] = Array.fill(nbSlots)((math.random * (nbPeople/10)).toInt + 1)
    (people, slots)
  }


  //test("Generated problem") {
  //  val (people, slots) = generateProblem(10, 10)
  //  val workload = 5

  //  val res = schedule(people, slots, workload)

  //  assert(!res.isEmpty)
  //  res.foreach(assignment => checkAssignment(assignment, people, slots, workload))
  //}
}
