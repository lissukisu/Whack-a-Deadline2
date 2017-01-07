package kierros1
import processing.core._
import scala.concurrent.duration._
import scala.util._
object WhackADeadline extends PApplet  {
 
  val boxSize: Int = 140
  val boxAmount: Int = 3
  val gameSize: Int = boxAmount * boxSize
  private var time: Option[Deadline] = None
  var timeLimit: Int = 3
  
  val random = new Random
  

  // kuvien lataus ja koon muokkaus
  var back = loadImage("paper.jpg")
  back.resize(gameSize,gameSize + boxSize)
  var deadline = loadImage("icon.png")
  deadline.resize(boxSize,boxSize)
  var matti = loadImage("matti.png")
  matti.resize(170,170)
  

  private var isGameOn: Boolean = false
  private var begin: Boolean = true
  private var help: Boolean = false
  private var gameOver: Boolean = false
  private var nextLevel: Boolean = false
  private var congrats: Boolean = false
  
  private var grid = Array.fill(boxAmount, boxAmount)(false)
  
  private var deadlineHits = 0 // tein nää jo valmiiks, ei tee vielä mitään
  private var missed = 0
  
  def lose() = {
    if (missed >= 3) {
      deadlineHits = 0
      missed = 0
      timeLimit = 3
      isGameOn = false
      gameOver = true 
    }
  }
  
  def win() = {
    if (deadlineHits >= 10) {
      timeLimit -= 1
      missed = 0
      deadlineHits = 0
      isGameOn = false
      if (timeLimit > 0) nextLevel = true
      else {
        timeLimit = 3
        congrats = true
      }
    }
  }
 
  override def setup() : Unit = { 
    size(gameSize, gameSize + boxSize) 
  }
 

  
  def showDeadline = {
    !(this.time == None || this.time.get.timeLeft <= 0.seconds)
  }
  
  
  var onJoDeadline = false
  
  def newDeadline() = {
     val x = random.nextInt(boxAmount)
     val y = random.nextInt(boxAmount)
     grid(x)(y) = true
     onJoDeadline = true
     time = Some(timeLimit.seconds.fromNow)
  }
  

  
  override def draw() : Unit = {
    val myFont = createFont("GillSans-UltraBold", 28)
    textFont(myFont)
    fill(0)
    image(back,0,0)
    if (isGameOn) { // pelinäkymä
      if (!onJoDeadline) newDeadline()
      if (showDeadline) {
        for (i <- grid.indices; j <- grid.indices) {
         if (grid(i)(j)) {
          image(deadline, i*boxSize, j*boxSize)
          text("Missed: " + missed, 20,460)
          text("Hits: " + deadlineHits + "/15", 210,460)
         }
        }
      } else if (!showDeadline) {
        grid = Array.fill(boxAmount, boxAmount)(false)
        onJoDeadline = false
        missed += 1
        println(missed)
        lose()
      }
    } else if (help) {
      text("HELP",40,100)
    } else if (begin) { // aloitusnäkymä
       text("Welcome to play\nWhack-a-Deadline!",40,100)
    } else if (gameOver) {
        text("YOU LOSE :(",40,100)
        text("Missed: 3", 20,460)
        image(matti,120,200)
    } else if (nextLevel) {
        text("You passed this level!\nIn the next level\nyou have " + timeLimit + " seconds for each deadline.",40,100)
        text("Hits: 15/15", 210,460)
    } else if (congrats) {
        text("YOU WIN :)",40,100)
        text("Hits: 15/15", 210,460)
    }
  }



  // Poistaa deadlinet näkyvistä oikeilla näppäimillä
  override def keyPressed() {
    if (key == ' ') {
      begin = false
      congrats = false
      nextLevel = false
      gameOver = false
      isGameOn = true
    }
    else if (isGameOn) {
      if (pairs.contains(key)) {
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
    }
    else if (key == 'h' | key == 'H') help = !help
    this.lose()
    this.win()
    //redraw()
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
