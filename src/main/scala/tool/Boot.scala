package tool

import tool.Simulator.StageEnum.{loading, packing, picking, sorting}
import tool.Simulator.{BufferEnum, HeadsDistribution, NewHourStartedEvent, State}

import scala.collection.immutable

object Boot {

	def main(args: Array[String]): Unit = {
		val sellingForecast = IndexedSeq[Long](100, 200, 300, 200, 100, 200, 300)

		val estimatedEfficiency = IndexedSeq(8, 10, 15, 15)

		val scheduledHeads: IndexedSeq[HeadsDistribution] = IndexedSeq(
			Map(picking.id -> 10, sorting.id -> 2, packing.id -> 2, loading.id -> 1),
			Map(picking.id -> 12, sorting.id -> 3, packing.id -> 2, loading.id -> 1),
			Map(picking.id -> 8, sorting.id -> 2, packing.id -> 1, loading.id -> 1),
			Map(picking.id -> 5, sorting.id -> 1, packing.id -> 1, loading.id -> 1),
			Map(picking.id -> 10, sorting.id -> 2, packing.id -> 2, loading.id -> 1),
			Map(picking.id -> 14, sorting.id -> 3, packing.id -> 3, loading.id -> 2),
			Map(picking.id -> 10, sorting.id -> 2, packing.id -> 2, loading.id -> 1),
			)

		val sim = new Simulator(estimatedEfficiency, scheduledHeads)

		var initialState = State(
			instant = 0,
			allBuffersLevels = IndexedSeq.fill(BufferEnum.maxId)(immutable.TreeMap.empty),
			nextEvents = immutable.TreeMap(0f -> NewHourStartedEvent(0))
			)
		var state = sim.project(initialState)
		while (state ne initialState) {
			println(state)
			initialState = state
			state = sim.project(state)
		}
	}
}
