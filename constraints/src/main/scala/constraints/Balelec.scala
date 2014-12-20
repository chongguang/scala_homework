package constraints

import cafesat.api.API._

/**
 * This component implements a constraint solver for assigning time slots to volunteers
 * for various tasks at a festival. A task may require more than one volunteer,
 * and a volunteer can take a certain number of tasks
 */

object Balelec {

  import Arithmetic._

  case class Volunteer(name: String) {
    override def toString = name
  }

  /**
   * A task is represented by its name and
   * its capacity, i.e. the exact number of people
   * required to complete it.
   */
  case class Task(name: String, capacity: Int) {
    override def toString = name
  }

  /**
   * This function schedules volunteers to tasks.
   * It takes as input a list of volunteers and a list of tasks.
   * The `availability` map contains mappings from volunteers to the
   * tasks they are available for.
   * A volunteer can be assigned to several tasks, but only
   * up to a maximum number of task specified by the `maxWorkload` parameter.
   */
  def schedule(
    volunteers: List[Volunteer],
    tasks: List[Task],
    availability: Map[Volunteer, List[Task]],
    maxWorkload: Int
  ): Option[Map[Task, List[Volunteer]]] = {

    val volunteersArray: Array[Volunteer] = volunteers.toArray

    /**
     * returns an array of indices of tasks.
     * each element at position `i` in the array corresponds to a list of indices of tasks
     * for which the volunteer at position `i` is.
     */
    val taskIndicesPerVolunteer: Array[List[Int]] =
      for (p <- volunteersArray) yield {
        for (t <- availability(p)) yield tasks.indexOf(t)
      }

    val ts: Array[Int] = tasks.map(_.capacity).toArray

    val optionResult = schedule(taskIndicesPerVolunteer, ts, maxWorkload)

    /**
     * creates a reverse map of tasks to volunteers
     * based on the resulting model
     */
    for (res <- optionResult) yield {
      for ((p, task) <- res zip tasks) yield {
        (task, for (idx <- p) yield volunteersArray(idx))
      }
    }.toMap
  }


  /**
   * This function schedules people to tasks, based on a maximum workload
   *
   * The `taskIndicesPerVolunteer` parameter is an array of indices of tasks.
   * each element at position `i` in the array corresponds to a list of indices of tasks
   * for which the volunteer at position `i` is.
   *
   * The `taskCapacities` parameter is an array of capacities. A value at index `i`
   * in this array is the capacity of the i-th task
   */
  def schedule(
    taskIndicesPerVolunteer: Array[List[Int]],
    taskCapacities: Array[Int],
    maxWorkload: Int
  ): Option[Array[List[Int]]] = ???

  /**
   * This function takes a list of constraints, and returns a pair.
   * The first element of the pair is a list of constraints representing the bitwise
   * sum of the constraints of `ns`.
   * The second element is a set of additional constraints that have been gathered along
   * the way. Hint: see `sum` for understanding how to use additional constraints
   */
  def sumBits(ns: List[Formula]): (List[Formula], Set[Formula]) = {
    
    def iter(ns: List[Formula], acc: Int): Int = ns match {
      case Nil => 0 
      case x::Nil => 
        if (x == bool2formula(true)) {acc + 1} 
        else acc
      case x::xs => if (x == bool2formula(true)) iter(xs, acc+1) else iter(xs, acc)
    }
    
    val a = iter(ns, 0)
    (int2binary(iter(ns, 0)), Set())
    
  }


}
