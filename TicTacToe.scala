package deadline_game
import processing.core._
import scala.concurrent.duration._
import scala.util._
import ddf.minim._
object deadline extends PApplet {
  
 
  /* These variables set the size of the game.*/
  val boxSize: Int = 140
  val boxAmount: Int = 3
  val gameSize: Int = boxAmount * boxSize
  var grid = Array.fill(boxAmount, boxAmount)(false)
  
  
  /* These variables control the time that the deadlines are visible for,
   * and count the missed and hit deadlines.*/
  var time: Option[Deadline] = None
  var timeLimit: Int = 3
  var deadlineHits = 0
  var missed = 0
  
  
  /* Loading the games sounds.*/
  val minim = new Minim (this)
  val ticking = minim.loadFile("ticking.wav")
  val bell = minim.loadFile("bell.wav")
  val buzz = minim.loadFile("buzz.wav")
  val cheer = minim.loadFile("cheer.mp3")
  val laugh = minim.loadFile("laugh.wav")
  
  var soundOn = true
  
  /* These methods mute and unmute all sounds in the game.*/
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
  
  /* This method is used to play and rewind sound effects in the game. */
  def soundEffect(audio: AudioPlayer) = {
    if (soundOn) {
      audio.play()
      audio.rewind()
    }
  }
  
  /* This small method is used in method draw.*/
  def soundText = {
    if (soundOn) "On"
    else "Off"
  }


  /* Loading and resizing the images used in the game. */
  val back = loadImage("paper.jpg")
      back.resize(gameSize,gameSize + boxSize)
  val deadline = loadImage("icon.png")
      deadline.resize(boxSize,boxSize)
  val matti = loadImage("matti.png")
      matti.resize(170,170)
  val keyboard = loadImage("keyboard.png")
      keyboard.resize(160,160)
  val antti = loadImage("antti.png")
      antti.resize(360,260)

  
  /* These variables determine which part of the game is currently on.*/
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
  
  
  /* This variable is used to keep track of whether there is
   * a deadline on the game board or not.*/
  var emptyGame = true
  
  
  /* This method checks if you have lost. If necessary it ends and resets the game.*/
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
  
  
  /* This method checks if you have won. It either moves on to the next level, or ends the game.*/
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
 
  
  /* This method checks if there is time left for the current deadline.*/
  def timeLeft = {
    !(this.time == None || this.time.get.timeLeft <= 0.seconds)
  }
  
  
  /* Defines a random location for the next deadline to appear in.*/
  val random = new Random
  def newDeadline() = {
     val x = random.nextInt(boxAmount)
     val y = random.nextInt(boxAmount)
     grid(x)(y) = true
     emptyGame = false
     time = Some(timeLimit.seconds.fromNow)
  }
  
  
  /* Draws the deadline in the location set by newDeadline().*/
  def drawNewDeadline() = {
    for (i <- grid.indices; j <- grid.indices) {
      if (grid(i)(j)) image(deadline, i*boxSize, j*boxSize)
    }
  }
  
  
  /* Controls everything that is seen in the game, depending on which part is currently going on.*/
  override def draw() : Unit = {
    val myFont = createFont("GillSans-UltraBold", 24)
    textFont(myFont)
    fill(0)
    image(back,0,0)
    
    // Makes a new deadline appear when the game board is empty, and deletes it if the time runs out.
    if (isGameOn) { 
      text("Missed: " + missed, 20,460)
      text("Hits: " + deadlineHits + "/15", 210,460)
      text ("Sound: " + this.soundText, 210, 510)
      if (emptyGame) newDeadline()
      if (timeLeft) {
        drawNewDeadline()
      } else {
        grid = Array.fill(boxAmount, boxAmount)(false)
        emptyGame = true
        missed += 1
        soundEffect(buzz)
        lose()
      }
    }
    
    else if (help) { // Shows instructions for playing the game.
      val newFont = createFont("GillSans-UltraBold", 16)
      textFont(newFont)
      fill(0)
      text("HELP",40,80)
      text("There are 3 levels in the game:\n\nIn each level, you have less and less\n time to hit the deadlines before\n they disappear.\n\nYou can only miss two deadlines\nin each level.\n\nTo hit the\ndeadlines, use\nthese keys:\n\nTo mute or\n unmute press M.\n\nPress H to go back to the menu.",40,120)
      image(keyboard,220,300)
    }
    
    else if (begin) { // Shows the starting menu of the game.
      text("Welcome to play\nWhack-a-Deadline!",40,100)
      text("Press SPACE to start\n  a new game\nPress H for help",40, 400)
      image(deadline,boxSize,boxSize + 40)
    }
    
    else if (gameOver) { // Shows the ending screen if the game is lost.
      text("You didn't survive\nthe deadlines...\nPress space to\nredo the semester.",40,80)
      text("Missed: 3", 20,460)
      text("Press H for help",20,510)
      image(matti,120,230)
    }
    
    else if (nextLevel) { // Shows the screen in between levels.
      text("You passed this level!\nIn the next level\nyou have " + timeLimit + " second(s)\nfor each deadline.\n\nPress space to continue.",40,100)
      text("Hits: 15/15", 210,460)
    }
    
    else if (congrats) { // If the game is won, shows the congratulations.
      text("You beat all the deadlines!",10,100)
      text("Press space to play again",20,460)
      image(antti,30,140)
    }
  }


  override def keyPressed() {
    
    if (key == 'm') { // The key for muting and unmuting the game.
      if (soundOn) this.muteAll()
      else this.unmuteAll()
    }   
    else if (key == ' ') { // The key used for starting the game.
      begin = false
      congrats = false
      nextLevel = false
      gameOver = false
      help = false 
      isGameOn = true
    }
    else if (isGameOn) { // Checks if you hit or miss the deadlines during the game.
      if (pairs.contains(key)) {
        val x = pairs(key.toLower)._1
        val y = pairs(key.toLower)._2
        if (grid(x)(y)) { // What happens if you hit the right key.
          grid(x)(y) = false
          emptyGame = true
          deadlineHits += 1
          soundEffect(bell)
        } else { // What happens if you press on a wrong key.
          missed += 1
          soundEffect(buzz)
        }
      }
    }
    else if (key == 'h') help = !help // Help button.
    this.lose()
    this.win()
  }
  
   /* The keys matching the coordinates of the game board. */
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
  
  
  /* This method makes the application run and show. */
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
