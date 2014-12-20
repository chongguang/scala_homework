package constraints

import regolic.asts.fol.Trees._
import cafesat.api.API._

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ConcertsPlannerSuite extends FunSuite {

  import ConcertsPlanner._

  def checkAssignment(
    solution: Map[Band, Slot],
    preferences: Map[Band, List[Slot]]
  ): Boolean = {
    solution.keySet == preferences.keySet &&
    solution.forall(p =>
      preferences(p._1).contains(p._2)
    ) && {
      val values: List[Slot] = solution.values.toList
      values.size == values.toSet.size
    }
  }

  test("getUniqueSlots is working") {
    val band1 = Band("Band1")
    val band2 = Band("Band2")
    val slot1 = (Stage("Venue1"), Time(""))
    val slot2 = (Stage("Venue2"), Time(""))

    val preferences = Map(
      band1 -> List(slot1, slot2),
      band2 -> List(slot2)
    )

    assert(getUniqueSlots(preferences) === Set(slot1, slot2))

  }

  test("plan one band on one slot") {
    val band1 = Band("Band1")
    val slot1 = (Stage("Venue1"), Time(""))
    val res = ConcertsPlanner.plan(Map(band1 -> List(slot1)))
    assert(res != None)
    res.foreach(m => assert(m(band1) === slot1))
  }

  test("plan is working on a simple example") {
    val band1 = Band("Band1")
    val band2 = Band("Band2")
    val slot1 = (Stage("Venue1"), Time(""))
    val slot2 = (Stage("Venue2"), Time(""))

    val preferences = Map(
      band1 -> List(slot1, slot2),
      band2 -> List(slot2)
    )
    val goodPlan = ConcertsPlanner.plan(preferences)
    assert(!goodPlan.isEmpty)
    assert(checkAssignment(goodPlan.get, preferences))

    val badPlan = Map(
      band1 -> slot2,
      band2 -> slot1
    )

    assert(!checkAssignment(badPlan, preferences))

    val impossiblePlan = Map(
      band1 -> slot2,
      band2 -> slot2
    )

    assert(!checkAssignment(impossiblePlan, preferences))
  }

  test("plan a small number of bands") {
    val band1 = Band("Band1")
    val band2 = Band("Band2")
    val band3 = Band("Band3")
    val band4 = Band("Band4")
    val band5 = Band("Band5")
    val slot1 = (Stage("Stage1"), Time(""))
    val slot2 = (Stage("Stage2"), Time(""))
    val slot3 = (Stage("Stage3"), Time(""))
    val slot4 = (Stage("Stage4"), Time(""))
    val slot5 = (Stage("Stage5"), Time(""))

    val preferences1 = Map(
      band1 -> List(slot1, slot2),
      band2 -> List(slot2, slot3),
      band3 -> List(slot2, slot4),
      band4 -> List(slot4, slot5),
      band5 -> List(slot5, slot1)
    )
    val res1 = ConcertsPlanner.plan(preferences1)
    assert(!res1.isEmpty)
    assert(checkAssignment(res1.get, preferences1))

    val preferences2 = Map(
      band1 -> List(slot1, slot2, slot3),
      band2 -> List(slot2, slot3, slot5),
      band3 -> List(slot2, slot4, slot5),
      band4 -> List(slot4, slot5),
      band5 -> List(slot5, slot1)
    )
    val res2 = ConcertsPlanner.plan(preferences2)
    assert(!res2.isEmpty)
    assert(checkAssignment(res2.get, preferences2))

    val preferences3 = Map(
      band1 -> List(slot1, slot2, slot3, slot4),
      band2 -> List(slot3, slot5),
      band3 -> List(slot3, slot4, slot5),
      band4 -> List(slot3, slot4, slot5),
      band5 -> List(slot1, slot5)
    )
    val res3 = ConcertsPlanner.plan(preferences3)
    assert(!res3.isEmpty)
    assert(checkAssignment(res3.get, preferences3))

  }

  test("plan returns None when no possible scheduling") {
    val band1 = Band("Band1")
    val band2 = Band("Band2")
    val band3 = Band("Band3")
    val band4 = Band("Band4")
    val slot1 = (Stage("Slot1"), Time(""))
    val slot2 = (Stage("Slot2"), Time(""))
    val slot3 = (Stage("Slot3"), Time(""))
    val slot4 = (Stage("Slot4"), Time(""))
    val preferences = Map(
    band1 -> List(slot1, slot2),
    band2 -> List(slot1, slot2),
    band3 -> List(slot2, slot3, slot4),
    band4 -> List(slot1, slot2)
    )
    val res = ConcertsPlanner.plan(preferences)
    assert(res === None)
  }

  test("plan with more slots than bands") {
    val band1 = Band("Band1")
    val band2 = Band("Band2")
    val band3 = Band("Band3")
    val band4 = Band("Band4")
    val slot1 = (Stage("Slot1"), Time(""))
    val slot2 = (Stage("Slot2"), Time(""))
    val slot3 = (Stage("Slot3"), Time(""))
    val slot4 = (Stage("Slot4"), Time(""))
    val slot5 = (Stage("Slot5"), Time(""))

    val preferences = Map(
      band1 -> List(slot1, slot2, slot5),
      band2 -> List(slot1, slot2),
      band3 -> List(slot2, slot3, slot4),
      band4 -> List(slot1, slot2, slot5)
    )
    val res = ConcertsPlanner.plan(preferences)
    assert(res != None)
    assert(checkAssignment(res.get, preferences))
  }


  test("plan with a complex schedule") {
    val placebo = Band("Placebo")
    val rammstein = Band("Rammstein")
    val stress = Band("Stress")
    val u2 = Band("U2")
    val renaud = Band("Renaud")
    val ledZepelin = Band("Led Zepelin")
    val nirvana = Band("Nirvana")
    val brel = Band("Jacques Brel")

    val esplanade18 = (Stage("Esplanade"), Time("18"))
    val esplanade21 = (Stage("Esplanade"), Time("21"))
    val esplanade24 = (Stage("Esplanade"), Time("24"))
    val satelite18 = (Stage("Satelite"), Time("18"))
    val satelite21 = (Stage("Satelite"), Time("21"))
    val satelite24 = (Stage("Satelite"), Time("24"))
    val rolex18 = (Stage("Rolex"), Time("18"))
    val rolex21 = (Stage("Rolex"), Time("21"))
    val rolex24 = (Stage("Rolex"), Time("24"))

    val preferences = Map(
      placebo -> List(esplanade24, satelite24),
      rammstein -> List(esplanade21, esplanade24, rolex24),
      stress -> List(esplanade18, esplanade21, satelite18, satelite21),
      u2 -> List(satelite18, satelite21, satelite24, rolex21, rolex24),
      renaud -> List(satelite18, satelite21, satelite24),
      ledZepelin -> List(esplanade21, esplanade24, rolex21, rolex24),
      nirvana -> List(esplanade24, rolex24),
      brel -> List(rolex18, satelite18, esplanade18)
    )

    val res = ConcertsPlanner.plan(preferences)
    assert(!res.isEmpty)
    assert(checkAssignment(res.get, preferences))
  }

  def getRandomSubSet(n: Int): List[Int] = {
    (0 until n).filter(_ => math.random < 0.5).toList
  }

  def generateProblem(nbBands: Int, nbSlots: Int): Map[Band, List[Slot]] = {
    val slots: Array[Slot] =
      (0 until nbSlots).map(slot => (Stage("Stage"+slot), Time(""))).toArray

    val bands: Map[Band, List[Slot]] =
      (0 until nbBands).map(band => {
        val goodSlots: List[Slot] = getRandomSubSet(nbSlots).map(slot => slots(slot))
        (Band("Band" + band), goodSlots)
      }).toMap

    bands
  }


  test("plan of big generated concerts") {
    val preferences1 = generateProblem(10, 15)

    val res1 = ConcertsPlanner.plan(preferences1)
    res1.foreach(solution =>
      assert(checkAssignment(solution, preferences1))
    )

    val preferences2 = generateProblem(15, 20)

    val res2 = ConcertsPlanner.plan(preferences2)
    res2.foreach(solution =>
      assert(checkAssignment(solution, preferences2))
    )
  }

}
