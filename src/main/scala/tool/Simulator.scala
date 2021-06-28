package tool

import tool.Simulator.BufferEnum.shelf
import tool.Simulator.{AllBuffersLevels, BufferLevel, HeadsDistribution, Hour, InstantInHours, NewHourStartedEvent, StageCompletedEvent, StageEnum, StageId, State, TimeToAssignWorkEvent}

import scala.collection.immutable

object Simulator {


	type Hour = Int
	type InstantInHours = Float
	type DepartureInstant = InstantInHours

	object BufferEnum extends Enumeration {
		type Buffer = Value
		val shelf, queue, wall, gate, truck = Value
	}

	import BufferEnum._

	/** Knows the amount of units awaiting in a buffer discriminated by the departure time.
	 * Levels are not discriminated by carrier because the knowledge of which carrier was chosen is irrelevant for this [[Simulator]] purpose. */
	type BufferLevel = immutable.TreeMap[DepartureInstant, Long]

	type ChunkWorkAssigner = BufferLevel => (BufferLevel, List[(InstantInHours, Event)])

	/** @param hour      the hour at which this whole hour work assigment is performed
	 * @param stage      the stage at which this whole hour work assigment is performed
	 * @param workers    the number of workers at the stage
	 * @param efficiency how many units are processed by a worker per hour */
	def assignWorkInChunks(hourDivisions: Int)(
		hour: Hour,
		stage: StageEnum.Stage,
		workers: Int,
		efficiency: Int
	): Iterable[(InstantInHours, Event)] =
	{
		val chunkSizeLimit = efficiency / hourDivisions
		val chunkInterval = 0.999f / hourDivisions

		for (chunkIndex <- 0 until hourDivisions) yield {

			val chunkWorkAssigner: ChunkWorkAssigner = initialOriginBufferLevels => {
				var generatedEvents: List[(InstantInHours, Event)] = Nil
				var bufferLevels = initialOriginBufferLevels
				var availableWorkers: Int = workers

				val levelsIterator = bufferLevels.iterator
				while (availableWorkers > 0 && levelsIterator.hasNext) {
					var (departure, awaitingUnits) = levelsIterator.next()

					while (availableWorkers > 0 && awaitingUnits > 0) {
						val chunkSize = Math.min(chunkSizeLimit, awaitingUnits).toInt
						awaitingUnits -= chunkSize
						val newEvent = StageCompletedEvent(stage.destination, chunkSize, departure)
						generatedEvents = ((hour + (chunkIndex + 1) * chunkInterval) -> newEvent) :: generatedEvents
						availableWorkers -= 1
					}
					if (awaitingUnits == 0) {
						bufferLevels = bufferLevels.removed(departure)
					} else
					{
						bufferLevels = bufferLevels.updated(departure, awaitingUnits)
					}
				}
				(bufferLevels, generatedEvents)
			}
			val eventInstant = hour + chunkInterval * chunkIndex
			eventInstant -> TimeToAssignWorkEvent(stage.origin, chunkWorkAssigner)
		}
	}

	type WholeHourWorkAssigner = (Hour, StageEnum.Stage, Int, Int) => Iterable[(InstantInHours, Event)]
	val halfHourChunksWorkAssigner: WholeHourWorkAssigner = assignWorkInChunks(2)

	object StageEnum extends Enumeration {
		protected case class Val(
			origin: Buffer,
			destination: Buffer,
			wholeHourWorkAssigner: WholeHourWorkAssigner
		) extends super.Val

		type Stage = Val
		val picking: Stage = Val(shelf, queue, halfHourChunksWorkAssigner)
		val sorting: Stage = Val(queue, wall, halfHourChunksWorkAssigner)
		val packing: Stage = Val(wall, gate, halfHourChunksWorkAssigner)
		val loading: Stage = Val(gate, truck, halfHourChunksWorkAssigner)

		lazy val stages: IndexedSeq[Stage] = for {i <- 0 until this.maxId}
			yield this (i).asInstanceOf[Stage]
	}


	type StageId = Int
	type HeadsDistribution = Map[StageId, Int]

	sealed trait Event

	case class NewHourStartedEvent(hour: Hour) extends Event

	case class StageCompletedEvent(destination: Buffer, unitsQuantity: Int, departureInstant: DepartureInstant) extends Event {
		override def toString: String = f"StageCompleted($destination, $unitsQuantity, $departureInstant%3.2f)"
	}

	case class TimeToAssignWorkEvent(origin: Buffer, chunkWorkAssigner: ChunkWorkAssigner) extends Event {
		override def toString: String = s"TimeToAssignWork($origin)"
	}

	type NextEvents = immutable.TreeMap[InstantInHours, Event]

	/** Adds two append methods to the [[NextEvents]] type that append entries without removing existing entries with same key. */
	implicit class NextEventsOps(val nextEvents: NextEvents) {
		def append(instant: InstantInHours, event: Event): NextEvents = {
			if (nextEvents.contains(instant)) {
				this.append(Math.nextUp(instant), event)
			} else
			{
				nextEvents + (instant -> event)
			}
		}

		def appendAll(entries: Iterable[(InstantInHours, Event)]): NextEvents = {
			var map = NextEventsOps(nextEvents)
			entries.foreach { entry => map = NextEventsOps(map.append(entry._1, entry._2)) }
			map.nextEvents
		}
	}

	/** Knows the level of each buffer. Element at index zero contains the level of the `shelf` buffer; element at index one contains the level of the `queue` buffer; and so on. In general, the element at index `i` contains the level of the `buffer`` for which `buffer.id == i` */
	type AllBuffersLevels = IndexedSeq[BufferLevel]

	/** State of the universe */
	case class State(
		instant: InstantInHours,
		allBuffersLevels: AllBuffersLevels,
		nextEvents: NextEvents
	) {
		/** Measure how behind are units that are behind.
		 *
		 * @return zero if no unit is behind. Else the sum of the delay of units that are behind. */
		def measureDelay: Float = {
			val unitsBehind = for {
				buffer <- List(gate, wall, queue, shelf)
				level = allBuffersLevels(buffer.id)
				(departureInstant, quantity) <- level.headOption
				if departureInstant <= instant && quantity > 0
			} yield (instant - departureInstant, quantity)

			var totalQuantity: Long = 0
			var totalTimeXQuantity: Float = 0
			unitsBehind.foreach { case (timeBehind, quantity) =>
				totalQuantity += quantity
				totalTimeXQuantity = timeBehind * quantity
			}
			totalTimeXQuantity / totalQuantity
		}

		/** Chek if no unit is behind.
		 *
		 * @return true if a unit is behind. */
		def check: Boolean = {
			List(gate, wall, queue, shelf).forall { buffer =>
				val level = allBuffersLevels(buffer.id)
				level.headOption match {
					case Some((departureInstant: DepartureInstant, quantity)) if departureInstant <= instant && quantity > 0 =>
						false
					case _ =>
						true
				}
			}
		}

		override def toString: String = {
			f"State(instant=$instant%3.2f, levels=${allBuffersLevels.zipWithIndex.map { case (levels, index) => f"${BufferEnum(index)}->${levels.mkString("{", ",", "}")}" }.mkString("{", ", ", "}")}, nextEvents=${nextEvents.map { case (i, e) => f"$i%3.2f->$e" }})"
		}
	}
}

class Simulator(
	/** Knows, for each stage, the estimated number of units that a worker of that stage can deal with in an hour. */
	estimatedNumberOfUnitsPerHeadPerHour: IndexedSeq[StageId],

	/** Scheduled workers distributions for each hour. Element at index zero correspond to hour zero, element at index one corresponds hour one, and so on. */
	scheduledHeadsDistributions: IndexedSeq[HeadsDistribution]
) {

	/** Check if all deployed units will be loaded into the trucks on time  */
	def check(initialState: State): Boolean = {
		var state = initialState
		var nextState = project(state)
		var ok = (state ne nextState) && state.check
		while (ok) {
			println(nextState)
			state = nextState
			nextState = project(state)
			ok = (state ne nextState) && state.check
		}
		ok
	}

	def project(currentState: State): State = {
		val currentHour = currentState.instant.intValue

		currentState.nextEvents.headOption match {
			case Some((eventInstant, event)) =>
				// If the `eventInstant` crosses the a hour boundary, insert a `NewHourStartedEvent` at the crossed hour, and do nothing else.
				val nextHour = eventInstant.intValue
				if (nextHour != currentHour) {
					currentState.copy(nextEvents = currentState.nextEvents.append(nextHour.toFloat, NewHourStartedEvent(nextHour)))

				} else
				{ // if the `nextInstant` does not cross a hour boundary
					event match {

						case StageCompletedEvent(destinationBuffer, arrivingUnitsQuantity, departureInstant) =>
							//							val bufferLevels = realizeForecast(currentState.instant, eventInstant, currentState.allBuffersLevels)
							var destinationBufferLevel = currentState.allBuffersLevels(destinationBuffer.id)
							val newWaitingUnitsQuantity = destinationBufferLevel.get(departureInstant) match {
								case Some(oldWaitingUnitsQuantity) => oldWaitingUnitsQuantity + arrivingUnitsQuantity
								case None => arrivingUnitsQuantity
							}
							destinationBufferLevel = destinationBufferLevel.updated(departureInstant, newWaitingUnitsQuantity)
							State(
								instant = eventInstant,
								allBuffersLevels = currentState.allBuffersLevels.updated(destinationBuffer.id, destinationBufferLevel),
								nextEvents = currentState.nextEvents.tail
								)

						case NewHourStartedEvent(hour) =>
							val headsDistribution: HeadsDistribution = scheduledHeadsDistributions(hour)

							var nextEvents = currentState.nextEvents.tail
							for (stage <- StageEnum.stages) {
								val newEvents = stage.wholeHourWorkAssigner.apply(
									hour,
									stage,
									headsDistribution(stage.id),
									estimatedNumberOfUnitsPerHeadPerHour(stage.id)
									)
								nextEvents = nextEvents.appendAll(newEvents)
							}
							State(hour.toFloat, currentState.allBuffersLevels, nextEvents)

						case TimeToAssignWorkEvent(originBuffer, workAssigner) =>
							val (newOriginBufferLevel, newEvents) = workAssigner(currentState.allBuffersLevels(originBuffer.id))
							currentState.copy(
								allBuffersLevels = currentState.allBuffersLevels.updated(originBuffer.id, newOriginBufferLevel),
								nextEvents = currentState.nextEvents.tail.appendAll(newEvents)
								)
					}
				}

			case None =>
				currentState
		}
	}


}
