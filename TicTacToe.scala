package deadline_game
import processing.core._
import scala.concurrent.duration._
import scala.util._
import ddf.minim._
object deadline extends PApplet {
 
  val boxSize: Int = 140
  val boxAmount: Int = 3
  val gameSize: Int = boxAmount * boxSize
  var grid = Array.fill(boxAmount, boxAmount)(false)
  
  
  var time: Option[Deadline] = None
  var timeLimit: Int = 3
  var deadlineHits = 0
  var missed = 0
  
  
  val minim = new Minim (this)
  val ticking = minim.loadFile("ticking.wav")
  val bell = minim.loadFile("bell.wav")
  val buzz = minim.loadFile("buzz.wav")
  val cheer = minim.loadFile("cheer.mp3")
  val laugh = minim.loadFile("laugh.wav")
  
  var soundOn = true
  
  def muteAll() {
    ticking.mute()
    bell.mute()
    buzz.mute()
    cheer.mute()
    laugh.mute()
    soundOn = false
  }
  
  def unmuteAll() {
    ticking.unmute()
    bell.unmute()
    buzz.unmute()
    cheer.unmute()
    laugh.unmute()
    soundOn = true
  }
  
  def soundEffect(audio: AudioPlayer) = {
    if (soundOn) {
      audio.play()
      audio.rewind()
    }
  }
  
  def soundText = {
    if (soundOn) "On"
    else "Off"
  }
  
  
  

  // kuvien lataus ja koon muokkaus
  val back = loadImage("paper.jpg")
      back.resize(gameSize,gameSize + boxSize)
  val deadline = loadImage("icon.png")
      deadline.resize(boxSize,boxSize)
  val matti = loadImage("matti.png")
      matti.resize(170,170)
  val keyboard = loadImage("keyboard.png")
      keyboard.resize(160,160)

  
  var begin: Boolean = true
  var isGameOn: Boolean = false
  var help: Boolean = false
  var gameOver: Boolean = false
  var nextLevel: Boolean = false
  var congrats: Boolean = false
  
  override def setup() : Unit = { 
    size(gameSize, gameSize + boxSize) 
    ticking.loop()
  }
  
  var emptyGame = true
  
  def lose() = {
    if (missed >= 3) {
      isGameOn = false
      gameOver = true
      deadlineHits = 0
      missed = 0
      timeLimit = 3
      soundEffect(laugh)
      emptyGame = true
      grid = Array.fill(boxAmount, boxAmount)(false)
    }
  }
  
  
  def win() = {
    if (deadlineHits >= 15) {
      isGameOn = false
      timeLimit -= 1
      missed = 0
      deadlineHits = 0
      if (timeLimit > 0) nextLevel = true
      else {
        soundEffect(cheer)
        timeLimit = 3
        congrats = true
      }
    }
  }
 

  def timeLeft = {
    !(this.time == None || this.time.get.timeLeft <= 0.seconds)
  }
  
  
  val random = new Random
  def newDeadline() = {
     val x = random.nextInt(boxAmount)
     val y = random.nextInt(boxAmount)
     grid(x)(y) = true
     emptyGame = false
     time = Some(timeLimit.seconds.fromNow)
  }
  

  
  override def draw() : Unit = {
    val myFont = createFont("GillSans-UltraBold", 24)
    textFont(myFont)
    fill(0)
    image(back,0,0)
    if (isGameOn) { // pelinäkymä
      text("Missed: " + missed, 20,460)
      text("Hits: " + deadlineHits + "/15", 210,460)
      text ("Sound: " + this.soundText, 210, 510)
      if (emptyGame) newDeadline()
      if (timeLeft) {
        for (i <- grid.indices; j <- grid.indices) {
         if (grid(i)(j)) {
          image(deadline, i*boxSize, j*boxSize)
         }
        }
      } else {
        grid = Array.fill(boxAmount, boxAmount)(false)
        emptyGame = true
        missed += 1
        soundEffect(buzz)
        println(missed)
        lose()
      }
    } else if (help) {
      val newFont = createFont("GillSans-UltraBold", 16)
      textFont(newFont)
      fill(0)
      text("HELP",40,80)
      text("There are 3 levels in the game:\n\nIn each level, you have less and less\n time to hit the deadlines before\n they disappear.\n\nYou can only miss two deadlines\nin each level.\n\nTo hit the\ndeadlines, use\nthese keys:\n\nTo mute or\n unmute press M.\n\nPress H to go back to the menu.",40,120)
      image(keyboard,220,300)
    } else if (begin) { // aloitusnäkymä
       text("Welcome to play\nWhack-a-Deadline!",40,100)
       text("Press SPACE to start\n  a new game\nPress H for help",40, 400)
       image(deadline,boxSize,boxSize + 40)
    } else if (gameOver) {
        text("YOU LOSE :(\nPress SPACE to\nstart a new game.",40,100)
        text("Missed: 3", 20,460)
        image(matti,120,200)
    } else if (nextLevel) {
        var timeLeft = {
         if (timeLimit > 1) timeLimit + " seconds"
         else timeLimit + " second"
        } 
        text("You passed this level!\nIn the next level\nyou have " + timeLeft + "\nfor each deadline.\n\nPress space to continue.",40,100)
        text("Hits: 15/15", 210,460)
    } else if (congrats) {
        text("YOU WIN :)",40,100)
        text("Hits: 15/15", 210,460)
      }
  }



  // Poistaa deadlinet näkyvistä oikeilla näppäimillä
  override def keyPressed() {
    if (key == 'm') {
      if (soundOn) this.muteAll()
      else this.unmuteAll()
    }
    else if (key == ' ') {
      begin = false
      congrats = false
      nextLevel = false
      gameOver = false
      help = false 
      isGameOn = true
    }
    else if (isGameOn) {
      if (pairs.contains(key)) {
        val x = pairs(key.toLower)._1
        val y = pairs(key.toLower)._2
        if (grid(x)(y)) {
          grid(x)(y) = false
          emptyGame = true
          deadlineHits += 1
          soundEffect(bell)
        } else {
          missed += 1
          soundEffect(buzz)
        }
      }
    }
    else if (key == 'h') help = !help
    this.lose()
    this.win()
  }
  
   var pairs = {
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
