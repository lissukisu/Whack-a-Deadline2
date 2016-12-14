package kierros1
import processing.core._
import scala.concurrent.duration._
import scala.util
import helper.TicTacToeHelper._

object TicTacToe extends PApplet  {
 
  val boxSize: Int = 140
  val boxAmount: Int = 3
  val gameSize: Int = boxAmount * boxSize
  private var canStartNewGameTime: Option[Deadline] = None // älkää välittäkö tästä

  // kuvien lataus ja koon muokkaus
  var back = loadImage("Grass.jpg")
  back.resize(gameSize,gameSize)
  var button = loadImage("start.png")
  button.resize(100,60)
  var deadline = loadImage("Deadline.jpg")
  deadline.resize(boxSize,boxSize)
  
  //jäämiä tictactoesta, käytetään redrawImages metodissa joka on tictactoe-helperistä
  val oPlayer: String = "O"
  val xPlayer: String = "X" 
  

  private var isGameOn: Boolean = false
  private var begin: Boolean = true
  
  private var grid = Array.fill(boxAmount, boxAmount)("")
  
  private var deadlineHits = 0 // tein nää jo valmiiks, ei tee vielä mitään
  private var missed = 0
  
 
  override def setup() : Unit = { 
    size(gameSize, gameSize) 
  }
 
  
  override def draw() : Unit = {
    if (isGameOn) { // pelinäkymä
      image(back,0,0)
      stroke(0)
      drawGrid(this, boxAmount, boxSize)
      redrawImages(grid, deadline, oPlayer, deadline, xPlayer, boxSize, this)
    } else if (begin) { // aloitusnäkymä
      image(back,0,0)
      text("Welcome to play Whack-a-Deadline!",50,50)
      image(button,130,150)
    } else { // tähän pitäis tehdä häviön/voiton/tasojen välissä ilmestyvät näkymät
        image(back,0,0)
      }
  }

 
 
   // Tällä voi nyt vaan lisäillä deadlineja, pitää korvata sillä et ne ilmestyy randomisti itsestään
  override def mouseClicked() : Unit = {
    var xCordinate: Int = mouseX / boxSize
    var yCordinate: Int = mouseY / boxSize
    if (isGameOn && grid(xCordinate)(yCordinate) == "") {
      image(deadline, xCordinate * boxSize, yCordinate * boxSize)
      grid(xCordinate)(yCordinate) = "X" 
    }
  }
  
  
 

  // Poistaa deadlinet näkyvistä oikeilla näppäimillä
  override def keyPressed() {
    if (key == ' ') isGameOn = true
    else if (isGameOn == true) {
      if (key == 'q' | key == 'Q') grid(0)(0) = ""
      else if (key == 'w' | key == 'W') grid(1)(0) = ""
      else if (key == 'e' | key == 'E') grid(2)(0) = ""
      else if (key == 'a' | key == 'A') grid(0)(1) = ""
      else if (key == 's' | key == 'S') grid(1)(1) = ""
      else if (key == 'd' | key == 'D') grid(2)(1) = ""
      else if (key == 'z' | key == 'Z') grid(0)(2) = ""
      else if (key == 'x' | key == 'X') grid(1)(2) = ""
      else if (key == 'c' | key == 'C') grid(2)(2) = ""   
      redrawImages(grid, deadline, oPlayer, deadline, xPlayer, boxSize, this)
    } 
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
