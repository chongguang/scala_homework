package streams

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {

  trait example extends StringParserTerrain {
    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
    
    val terrain1 = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
  }

  test("test1") {
    new example {
    	assert(terrainFunction(terrain1)(Pos(1,1)))
    	assert(terrainFunction(terrain1)(Pos(2,1)))
    	assert(terrainFunction(terrain1)(Pos(0,0)))
    	assert(!terrainFunction(terrain1)(Pos(1,8)))
    	assert(!terrainFunction(terrain1)(Pos(-1,-8)))   
    	
    	assert(findChar('S', terrain1).x.equals(0))    	
    	assert(findChar('S', terrain1).y.equals(0))    	
    	assert(findChar('T', terrain1).x.equals(0))    	
    	assert(findChar('T', terrain1).y.equals(1))
    }
  }

}