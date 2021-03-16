package com.example

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.{Logging, LoggingAdapter}

import scala.util.Random

case class PingPong(actor: ActorRef, pingOrPong: String, res: Int)
case class Ball(count : Int)
case class Players(actor: ActorRef,actor2 : ActorRef ,count : Ball)
case class MyActor(name: String) extends Actor {
  val log: LoggingAdapter = Logging(context.system, this)
  val list = List("Hit", "Missed")
  override def receive: Receive = {
    // Task1
    case "Mustafa" => log.info("Hello Mustafa");
    case "Tome"  =>log.info("Welcome  Tome" )
    case "Anna"  =>log.info("Greetings Anna" )
    // Task2
    case PingPong(actor, message, res) =>
      if (res < 10) {
        if (message.equals("Hit")) {
          log.info(message + " by " + actor.path.name + " and the current result is " +(res +1).toString )
          actor ! PingPong(this.self, list(Random.nextInt(list.length)), 1+ res)
        }
        else if(message.equals("Missed")) {
          log.info(message + " by " + actor.path.name + " and the current result is " +res.toString )
          actor ! PingPong(this.self, list(Random.nextInt(list.length)), res)
        } else
          log.info("Message is not registered" + res)
      }
      else if (res == 10) {
        log.info("PING PONG " + this.self.path.name + "  is the winner" )
      }
    // Task3
    case Players(actor, actor2, ball) =>
      if (ball.count < 100) {
        log.info(this.self.path.name+ " has the ball now. The counter is " + (ball.count + 1).toString)
        val a = Random.nextInt(2)
        if(a > 0) actor ! Players(actor2, this.self, Ball(ball.count + 1))
          else
        actor2 ! Players(actor, this.self, Ball(ball.count + 1))
      }
    case _ => "Unregistered Entry"
  }

}

object main extends App {
  val system = ActorSystem("HelloSystem")
      println("                                                                            Task1")
      var myActor = system.actorOf(Props(MyActor("Tome" )), name = "helloActor")
      myActor ! "Mustafa"
      myActor ! "Tome"
      myActor ! "Anna"
      myActor ! "Not a user"
  Thread.sleep(3000)
  println("                                                                            Task2")
  var ping = system.actorOf(Props(MyActor("Ping")), name = "Chandler")
  var pong = system.actorOf(Props(MyActor("pong")), name = "Ross")
  val list = List("Hit", "Missed")
  pong ! PingPong(ping, list(Random.nextInt(list.length)), 0)
  Thread.sleep(3000)
  println("                                                                             Task3")
  var player1 = system.actorOf(Props(MyActor("Player1")), name = "Player1")
  var player2 = system.actorOf(Props(MyActor("Player2")), name = "Player2")
  var player3 = system.actorOf(Props(MyActor("Player3")), name = "Player3")
  player1 ! Players(player2,player3,Ball(0))
  system.terminate


}