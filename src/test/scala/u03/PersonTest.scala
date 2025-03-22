package u03

import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person

class PersonTest {
  import extensionmethods.Sequences.*
  import Sequence.*
  import org.junit.*
  
  import u02.Modules.Person.*
  
  val sequence: Sequence[Person] = Cons(Student("Tommaso", 2000), Cons(Teacher("Viroli", "PPS"), Nil()))
  
  @Test def testCoursesOfTeachers(): Unit =
    assertEquals(Cons("PPS", Nil()), sequence.courseOfTeachers())
  

}
