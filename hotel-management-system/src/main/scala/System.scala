/*
Program Info: 


Admin: 
Check availability
Check them Out


Client:
Book Room


Centrally: 
Room is a companion object with a collection of each room number and its status: (Mutable Map)
Example: (Room Class (...) -> "Occupied"),(Room Class (...)-> "Empty")

If the room is occupied then there will be another object containing information regarding the occupants


Assumptions:
Hotel is single bedroom for now

*/
package management
import scala.collection.mutable.Map


//Enumerations

sealed trait Level {
    case object Standard extends Level
    case object Business extends Level
    case object Executive extends Level
}

sealed trait Status {
    case object Occupied extends Status
    case object Empty extends Status
}

sealed trait Indication {//So i can handle errors and see what needs reworking
    case object Success extends Indication
    case object Failed extends Indication   
}

//Interfaces 
case class Occupant(name: String, age: Int);
case class Room(number: Int, status: Status, level: Level, price: Double, occupant: Option[Occupant]);

class Hotel {

    def decideLevel(seed: Int): Level = { 
        val matcher = seed % 3
        matcher match { 
            case 0 => Standard
            case 1 => Business
            case 2 => Executive
        }
        
    }
    def calculatePrice(level: Level): Double = { 
        status match {
            case Standard => 100
            case Business => 200
            case Executive => 300
        }
        
    }

    def checkRoomExistence(num : Int): Indication = {//Used before create room so that no errors
        
    }

    def createRoom (number: Int, level: Int): Room = {
        
    }

    def addRoom (room: Room): Indication = {//Adds a new room to the list of rooms
        rooms = rooms :: room
    }
    
}


//Main object Hotel
object Hotel {

    //Room Management

    private var rooms : List[Room] = (1 to 10).map(n => Room(number = n, status = Empty, level = decideLevel(n), price = calculatePrice(decideLevel(n)), occupant = None))
    private var roomNumbers : List[Int] = (1 to 10).map(n => n)
    private var checkedInRooms: List[Room] = rooms.filter(room => room.status == Occupied)
    private var checkedOutRooms: List[Room] = rooms.filter(room => room.status == Empty)
    private var roomCheckInLength: Map[Int,Int] = Map[Int,Int]()//Room Number mapped with a length of hours


    //Getters

    def getRoomOccupant (number: Int) : Occupant = {//Gets the room occupant
        val theRoom: Room  = rooms.map(room => if (room.roomNumber == number) return room)
        val theOccupant: Option[Occupant] = theRoom match{
            case Room(number,status,level,price,occupant) => occupant
        }

        theOccupant match{
            case Some(s) => {
                println(s)
                s
            }
        }
    }

    def getCheckedinRooms() : List[Room]= {//Updates the checked in rooms
        return checkedInRooms
    }

    def getCheckedOutRooms() : List[Room]= {//Updates the checked out rooms
        return checkedOutRooms
    }

    def getCheckInLength(roomNum: Int):Int = {//Gets the duration of stay until room is free
        return roomCheckInLength.filter((roomNumber,hrs) => roomNum == roomNumber).value
    }
    
    def getRoomNumbers() : List[Int] = {//returns a list of the room numbers in the hotel
        return roomNumbers
    }


    //Setters

    def updateCheckInLength(room: Int, time: Int, extend: Boolean) : Indication = {// Updates the stay duration
        if (roomCheckInLength.contains(room)){
            roomCheckInLength = roomCheckInLength - room
            if (extend){
                val originalTime = roomCheckInLength(room)
                roomCheckInLength += (room ->(time + originalTime) )
            } else {
                roomCheckInLength += (room -> time)
            }
            
            return Success

        } else {
            return Failure
        }
    }

    def updateRoomNumbers () : Unit = {
        var roomNumbers  = rooms.map(room => room.number)
    }

    def updateRoomStatus(): Unit = {//Updates the room variables
        checkedInRooms = rooms.filter(room => room.status == Occupied)
        checkedOutRooms = rooms.filter(room => room.status == Empty)
    }

    def checkIn (roomNum: Int): Indication = {//Returns a boolean indicating success
        val updatedRoom = rooms.map{
            room => if (room.roomNumber == roomNum){
                room match{
                    case Room(number,Empty,level,price,occupant) => {
                        val roomAvailable = true
                        Room(number,Occupied,level,price,occupant)
                    }
                    case Room(number,Occupied,level,price,occupant) =>{
                        val roomAvailable = false
                        Room(number,Occupied,level,price,occupant)
                    }
                }
            } 
        }
        updatedRoomStatus()
        if (roomAvailable){
            return Success
        } else{
            return Failure
        }

    }


    def checkOut (roomNum: Int): Indication = {//Returns a boolean indicating success 
        val updatedRoom = rooms.map{
            room => if (room.roomNumber == roomNum){
                room match{
                    case Room(number,Occupied,level,price,occupant) => {
                        val checkedOut = true
                        Room(number,Empty,level,price,occupant)
                    }
                }
            } 
        }
        updatedRoomStatus()
        if (checkedOut){
            return Success
        } else {
            return Failure
        }

    }
    
    
    
    


}



//For when i can't code
object TimeUtils {
    def secondsToMinutes(sec: Int): Int = sec/60
    def minutesToHours(min: Int): Int = min/60
    def hoursToDays(hrs: Int): Int = hrs/24


    def secondsToHours(sec: Int): Int = minutesToHours(secondsToMinutes(sec))
    def secondsToDays(sec: Int): Int = secondsToHours(sec)

    def minutesToDays(min: Int): Int = hoursToDays(minutesToHours(min))

    def daysToHours(days: Int): Int = days/24

}   