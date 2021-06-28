import tool.Simulator.BufferEnum.Buffer
import tool.Simulator.{InstantInHours, WholeHourWorkAssigner}

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.{SortedMap, immutable}
import scala.util.Random

package object tool {

	type Duration = Float
	type Time = Float

	object CarrierEnum extends Enumeration {
		private val random = java.util.concurrent.ThreadLocalRandom.current()

		type Carrier = Val
		protected case class Val(truckCapacity: Int, minDelay: Duration, departureTimes: List[Time]) extends super.Val

		val wagon: Carrier = Val(20, 16, List(8, 15))
		val rush: Carrier = Val(10, 8, List(9.5f, 18.25f))
		val plane: Carrier = Val(5, 4, List(12))

		val carriersSet: Set[Carrier] = Set(wagon, rush, plane)

		/** Gives an infinite iterator over all future departure times ordered by arrival time. */
		def buildDeparturesIterator(currentInstant: InstantInHours /*, destination: Address*/): Iterator[InstantInHours] = {
			// make a list containing the departure times of all carriers, and sort them by the arrival time
			val departureTimesUnordered = for {
				carrier <- carriersSet
				delay = random.nextFloat * carrier.minDelay + carrier.minDelay
				departureTime <- carrier.departureTimes
			} yield {
				val arrivalTime = departureTime + delay
				arrivalTime -> departureTime
			}
			val departureTimesOrdered = departureTimesUnordered.to(immutable.SortedMap).values.toIndexedSeq
			val currentDay = currentInstant.intValue
			Iterator.unfold(0) { index =>
				Some((
						 (currentDay + index / departureTimesOrdered.size) + departureTimesOrdered(index % departureTimesOrdered.size),
						 index + 1
					 ))
			}.filter(_ > currentInstant)
		}
	}



}
