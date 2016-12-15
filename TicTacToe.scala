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
  private var help: Boolean = false
  private var gameOver: Boolean = false
  private var congrats: Boolean = false
  
  private var grid = Array.fill(boxAmount, boxAmount)(false)
  
  private var deadlineHits = 0 // tein nää jo valmiiks, ei tee vielä mitään
  private var missed = 0
  
  def lose() = {
    if (missed >= 3) {
      isGameOn = false
      gameOver = true 
    }
  }
  
  def win() = {
    if (deadlineHits >= 10) {
      isGameOn = false
      congrats = true
    }
  }
 
  override def setup() : Unit = { 
    size(gameSize, gameSize) 
  }
 
  /*
  def deadlineAppear: Boolean = {
    if (showDeadline) false
    else if (time.get.timeLeft <= 0.seconds) false
    else {
      grid(0)(0) = true
      true
    }
  }
 */
  
  
  def showDeadline = {
    !(this.time == None || this.time.get.timeLeft <= 0.seconds)
  }
  
  
  var onJoDeadline = {
    false
  }
  
  def newDeadline() = {
     val x = random.nextInt(boxAmount)
     val y = random.nextInt(boxAmount)
     grid(x)(y) = true
     onJoDeadline = true
     time = Some(3.seconds.fromNow)
  }
  

  
  override def draw() : Unit = {
    if (isGameOn) { // pelinäkymä
      image(back,0,0)
      if (!onJoDeadline) newDeadline()
      if (showDeadline) {
        for (i <- grid.indices; j <- grid.indices) {
         if (grid(i)(j)) {
          image(deadline, i*boxSize, j*boxSize)
         }
        }
      } else if (!showDeadline) {
        grid = Array.fill(boxAmount, boxAmount)(false)
        onJoDeadline = false
        missed += 1
        println(missed)
        lose()
      }
    } else if (begin) { // aloitusnäkymä
      image(back,0,0)
      val myFont = createFont("GillSans-UltraBold", 32)
      textFont(myFont)
      fill(0)
      text("Welcome to play\nWhack-a-Deadline!",40,100)
    } else if (help) {
      ???
    } else if (gameOver) { // tähän pitäis tehdä häviön/voiton/tasojen välissä ilmestyvät näkymät
        image(back,0,0)
        text("YOU LOSE :(",40,100)
    } else if (congrats) {
        image(back,0,0)
        text("YOU WIN :)",40,100)
    }
  }



  // Poistaa deadlinet näkyvistä oikeilla näppäimillä
  override def keyPressed() {
    if (key == ' ') {
      isGameOn = true
      begin = false
    }
    else if (isGameOn) {
      val x = pairs(key.toLower)._1
      val y = pairs(key.toLower)._2
      if (grid(x)(y)) {
        grid(x)(y) = false
        onJoDeadline = false
        deadlineHits += 1
        println(deadlineHits)
      } else {
        missed += 1
        println(missed)
      }
    }
    else if (key == 'h' | key == 'H') help = !help
    this.lose()
    this.win()
  }
  
  private var pairs = {
    Map('q' -> (0,0),
        'w' -> (1,0),
        'e' -> (2,0),
        'a' -> (0,1),
        's' -> (1,1),
        'd' -> (2,1),
        'z' -> (0,2),
        'x' -> (1,2),
        'c' -> (2,2))
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
