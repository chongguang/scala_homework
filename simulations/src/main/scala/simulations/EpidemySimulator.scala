package simulations

import math.random


case class Room (val row: Int, val col: Int)

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val moveDuration = 5
    
    val deathRate = 0.25
    val transmissibilityRate = 0.4
    val initialInfectedRate = 0.01
    
    val timeBecomeInfected = 0
    val timeBecomeSick = 6
    val timeBecomeDead = 14
    val timeBecomeImmune = 16
    val timeBecomeHealthy = 18
    
    val takeTheAirplaneProbability = 0.01
    val vipRate = 0.05
    val nbVIP = vipRate * population
    
    // This is the parameter for different modes
    // 0: Normal Case
    // 1: Air Traffic Only
    // 2: Reduce Mobility + Air
    // 3: Chosen Few + Air
    val mode = 2
  }

  import SimConfig._
  

  val persons: List[Person] = {
    val initiallyInfected = population * initialInfectedRate
    (1 to population) map (id => {
      val p = new Person(id)
      if (id <= initiallyInfected) p.setInfected
      p.planMoves 
      p
     }
    ) toList
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    
    private var actions: List[Action] = List()

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    //
    // to complete with simulation logic
    //
    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
    
    def getInfected = infected
    def getSick = sick
    def getImmune = immune
    def getDead = dead
    
    def isVisiblyDangerous = getSick || getDead
    
    def isInfectious = getInfected || getSick || getImmune || getDead
    
    def setInfected() = {
      infected = true
    }
    
    def setSick() = {
      sick = true
    }
    
    def setImmune = {
      sick = false
      immune = true
    }
    
    def setDead() = {
      dead = true
    }
    
    def setHealthy() = {
      infected = false
      sick = false
      immune = false
      dead = false
    }
    
    // Simulate randomly if the person is dead
    def simulateDeath(): Boolean = random < deathRate
   
    // Actions after getting infected
    def becomeInfected() = {
      def infectedAction(): Unit = {
        afterDelay(timeBecomeInfected) { setInfected }
        afterDelay(timeBecomeSick) { setSick }
        afterDelay(timeBecomeDead) { if (simulateDeath) setDead }
        afterDelay(timeBecomeImmune) { if (!getDead) setImmune  }
        afterDelay(timeBecomeHealthy) { if (!getDead) setHealthy }
      }      
      
      this.addAction(infectedAction)
    }
    
    // To verify if the person is in the given room
    def isInTheRoom(room: Room): Boolean = this.row == room.row && this.col == room.col 
    
    // To verify if the room is infectious
    def infectiousRoom(room: Room): Boolean = persons.exists(p => p.isInTheRoom(room) && p.isInfectious) 
    
    // To verify if there are sick or dead people in the room
    def visiblyDangerousRoom(room: Room): Boolean = persons.exists(p => p.isInTheRoom(room) && p.isVisiblyDangerous) 
    
    // Find a random room in the grid
    def getRamdonRoom(): Room = Room(randomBelow(roomRows), randomBelow(roomColumns))
    
    // Find the 4 neighbor rooms of the given room
    def neighbours(room: Room): List[Room] = {
      val left = if (room.col == 0) roomColumns-1 else room.col - 1
      val right = if (room.col == roomColumns-1) 0 else room.col + 1
      val up = if (room.row == 0) roomRows - 1 else room.row - 1
      val down = if (room.row == roomRows - 1) 0 else room.row + 1
      Room(right, room.row) :: Room(left, room.row)  :: Room(up, room.col) :: Room(down, room.col)  :: Nil
    }

    // Given a list of rooms, find those who has no visibly dangerous people in it
    def findVisiblySafeRoom(rooms: List[Room]): Option[Room] = rooms match {
      case Nil => None
      case rooms => {
        val rid = (random * rooms.length).toInt
        val rm = rooms(rid) 
        if (!visiblyDangerousRoom(rm)) Some(rm)
        else findVisiblySafeRoom(rooms diff List(rm))
      }
    }
    
    def findDestination_airTraffic(neighbors: List[Room]): Option[Room] = 
      if (random < takeTheAirplaneProbability) {
        //System.out.println("fly!!")
        Some(getRamdonRoom)
      } else findVisiblySafeRoom(neighbors)
    
    // Plan all the moves of the person
    def planMoves() = {    
    
      // Move to the given room and check if the person gets infected
      def doTheMove(room: Room) = {
        row = room.row
        col = room.col

        if (mode == 0 || mode == 1 || mode == 2) {
          // For Normal Case, Air Traffic Only, Reduce Mobility + Air
          if (!getInfected && infectiousRoom(room) && random <= transmissibilityRate) becomeInfected
        } else if (mode == 3) {
          // For The Chosen Few + Air
          if (!getInfected && infectiousRoom(room) && random <= transmissibilityRate && id <= population - nbVIP) becomeInfected
        } else {
          System.out.println("Wrong Mode number!")
        }
      }
      
      def moveAction(): Unit = {
        
        if (mode==0) {
          // Normal Case
          val nextMoveDate = randomBelow(moveDuration) + 1          
          afterDelay(nextMoveDate) {
            if (!getDead) {
              val destination = findVisiblySafeRoom(neighbours(Room(row, col)))
              destination match {
                case Some(r) => doTheMove(r) 
                case None => //do nothing
              }
              this.addAction(moveAction)
            }
          }         
        } else if (mode == 1 || mode == 3){
          // Air Traffic Only , Chosen Few + Air
          val nextMoveDate = randomBelow(moveDuration) + 1          
          afterDelay(nextMoveDate) {
            if (!getDead) {
              val destination = findDestination_airTraffic(neighbours(Room(row, col)))
              destination match {
                case Some(r) => doTheMove(r) 
                case None => //do nothing
              }
              this.addAction(moveAction)
            }
          } 
        } else if (mode == 2) {
          // Reduce Mobility + Air
          val nextMoveDate = if (isVisiblyDangerous ) (randomBelow(moveDuration) + 1)*4 else (randomBelow(moveDuration) + 1)*2           
          afterDelay(nextMoveDate) {
            if (!getDead) {
              val destination = findDestination_airTraffic(neighbours(Room(row, col)))
              destination match {
                case Some(r) => doTheMove(r) 
                case None => //do nothing
              }
              this.addAction(moveAction)
            }
          } 
        } else {
          System.out.println("Wrong mode number dude!")          
        }        
      }
      this.addAction(moveAction)
    }    
  } 
}
