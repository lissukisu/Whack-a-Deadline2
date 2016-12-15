package kierros1
import processing.core._
import scala.concurrent.duration._
import scala.util._
object WhackADeadline extends PApplet  {
 
  val boxSize: Int = 140
  val boxAmount: Int = 3
  val gameSize: Int = boxAmount * boxSize
  private var time: Option[Deadline] = None //Some(1.seconds.fromNow) // älkää välittäkö tästä
  val duration = Duration(1,SECONDS)
  
  val random = new Random
  

  // kuvien lataus ja koon muokkaus
  var back = loadImage("paper.jpg")
  back.resize(gameSize,gameSize)
  var deadline = loadImage("icon.png")
  deadline.resize(boxSize,boxSize)
  

  private var isGameOn: Boolean = false
  private var begin: Boolean = true
  private var help = false
  
  private var grid = Array.fill(boxAmount, boxAmount)(false)
  
  private var deadlineHits = 0 // tein nää jo valmiiks, ei tee vielä mitään
  private var missed = 0
  
 
  override def setup() : Unit = { 
    size(gameSize, gameSize) 
  }
  /*
  def deadlineAppear: Boolean = {
    if (time.isEmpty) false
    else if (time.get.timeLeft <= 0.seconds) false
    else {
      grid(0)(0) = true
      true
    }
  }
 
  */
  
  
  var onJoDeadline = {
    false
  }
  
  def newDeadline() = {
     val x = random.nextInt(boxAmount)
     val y = random.nextInt(boxAmount)
     grid(x)(y) = true
     onJoDeadline = true
  }
  

  
  override def draw() : Unit = {
    if (isGameOn) { // pelinäkymä
      image(back,0,0)
      //time = Some(1.seconds.fromNow)
      if (!onJoDeadline) newDeadline()
      for (i <- grid.indices; j <- grid.indices) {
       if (grid(i)(j)) {
        image(deadline, i*boxSize, j*boxSize)
       }
      }
    } else if (begin) { // aloitusnäkymä
      image(back,0,0)
      val myFont = createFont("GillSans-UltraBold", 32)
      textFont(myFont)
      fill(0)
      text("Welcome to play\nWhack-a-Deadline!",40,100)
    } else if (help) {
      ???
    } else { // tähän pitäis tehdä häviön/voiton/tasojen välissä ilmestyvät näkymät
        image(back,0,0)
    }
  }



  // Poistaa deadlinet näkyvistä oikeilla näppäimillä
  override def keyPressed() {
    if (key == ' ') {
      isGameOn = true
      begin = false
    }
    else if (isGameOn) {
      onJoDeadline = false
      if (key == 'q' | key == 'Q') grid(0)(0) = false
      else if (key == 'w' | key == 'W') grid(1)(0) = false
      else if (key == 'e' | key == 'E') grid(2)(0) = false
      else if (key == 'a' | key == 'A') grid(0)(1) = false
      else if (key == 's' | key == 'S') grid(1)(1) = false
      else if (key == 'd' | key == 'D') grid(2)(1) = false
      else if (key == 'z' | key == 'Z') grid(0)(2) = false
      else if (key == 'x' | key == 'X') grid(1)(2) = false
      else if (key == 'c' | key == 'C') grid(2)(2) = false  
      
    }
    else if (key == 'h' | key == 'H') help = !help
  }
  
  
    /**
   * The main method that makes your application run and show.
   */
  def main(args: Array[String]) {
    val frame = new javax.swing.JFrame("Whack-a-Deadline")

    frame.getContentPane().add(this)
    init
    frame.setSize(this.getSize())
    frame.pack
    frame.setVisible(true)
    frame.setLocationRelativeTo(null)
    frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE)
  }
  
  
  
  
}
