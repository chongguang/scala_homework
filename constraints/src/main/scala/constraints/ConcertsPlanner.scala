package constraints

import cafesat.api.API._

/**
 * This component implements a constraint solver
 * for assigning time slots to bands at a festival
 */
object ConcertsPlanner {

  case class Band(name: String) {
    override def toString = name
  }

  case class Stage(name: String) {
    override def toString = name
  }

  case class Time(time: String) {
    override def toString = time
  }

  type Slot = (Stage, Time)

  /*
   * This function schedules bands to slots. It takes as input
   * a list of preferences, as a map from bands to a list of slots.
   *
   * It creates various propositional constraints before calling
   * CafeSat's SAT solver on them. The result is an `Option[Map[Band, Slot]]`.
   * If the instance of the problem has a satisfiable assignment, a map from every
   * band to a slot is returned. Otherwise, `None` is returned.
   */
  def plan(preferences: Map[Band, List[Slot]]): Option[Map[Band, Slot]] = {

    val bands: Array[Band] = preferences.keys.toArray
    val slots: Array[Slot] = getUniqueSlots(preferences).toArray


    /** generates an array of boolean variables for the bands and the slots*/
    val varsMatrix: Array[Array[Formula]] = introduceVariables(bands, slots)

    /**
     * Set of constraints ensuring each band gets a desired slot
     */
    val desirableSlots: Seq[Formula] = 
      for(          
          (band, bandInx) <- (bands.zipWithIndex)
          if (preferences.contains(band))
      ) yield {
        //System.out.println(varsMatrix)
        val varsPrefBand =( for (prefSlot <- preferences(band)) yield {
          val slotInx = slots.indexOf(prefSlot)
          varsMatrix(bandInx)(slotInx)
        })
        or(varsPrefBand:_*) //?
      }
    

    /**
     * A set of constraints ensuring that a band gets at most one slot
     */
    val eachBandPlaysOnce: Seq[Formula] = 
      for (indexBand <- 0 to bands.size -1) yield {
        val toto = (
        for (indexSlot1 <- 0 to slots.size -1;
        		indexSlot2 <- 0 to slots.size -1;
        		if (indexSlot1 != indexSlot2)) yield {
          or(!varsMatrix(indexBand)(indexSlot1) , !varsMatrix(indexBand)(indexSlot2))          
        } ) 
        and(toto:_*)
      } 

    /**
     * A set of constraints ensuring that each slot is used at most once
     */
    val eachSlotUsedOnce: Seq[Formula] =  
      for (indexSlot <- 0 to slots.size -1) yield {
        val toto = (
        for (indexBand1 <- 0 to bands.size -1;
        		indexBand2 <- 0 to bands.size -1;
        		if (indexBand1 != indexBand2)) yield {
          or(!varsMatrix(indexBand1)(indexSlot) , !varsMatrix(indexBand2)(indexSlot))          
        } ) 
        and(toto:_*)
      }


    val allConstraints: Seq[Formula] =
      desirableSlots ++ eachBandPlaysOnce ++ eachSlotUsedOnce

    val res = solveForSatisfiability(and(allConstraints:_*))

    res.map(model => {
      bands.zipWithIndex.map { case (band, bandIdx) =>
        val bandAssignments = varsMatrix(bandIdx)
        val slotIdx = bandAssignments.indexWhere(v => model(v))
        (band, slots(slotIdx))
      }.toMap
    })
  }

  /**
   * This function gets a preference map, and returns all unique slots that are
   * part of the preferences.
   */
  def getUniqueSlots(preferences: Map[Band, List[Slot]]): Set[Slot] = (for (s <- preferences.values) yield s).toList.flatten.toSet

  /**
   * This function creates an initial matrix of propositional variables.
   * Each propositional variable represents a possible assignment of a band
   * to a slot.
   */
  def introduceVariables(bands: Array[Band], slots: Array[Slot]): Array[Array[Formula]] = for {b <- bands} yield ( for {s <- slots} yield boolVar() )

}
