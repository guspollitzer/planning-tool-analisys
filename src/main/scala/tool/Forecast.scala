package tool

import tool.Simulator.BufferEnum.shelf
import tool.Simulator.{AllBuffersLevels, InstantInHours}



class Forecast(data: IndexedSeq[Int]) {

	def materialize(previousInstant: InstantInHours, currentInstant: InstantInHours, allBufferLevels: AllBuffersLevels): AllBuffersLevels = {
		assert(previousInstant <= currentInstant)
		val previousHour = previousInstant.intValue
		val previousCeil = previousInstant.ceil
		val currentHour = currentInstant.intValue
		if (previousHour < currentHour && previousCeil != currentInstant) {
			val intermediateStep = materialize(previousInstant, previousCeil, allBufferLevels)
			materialize(previousCeil, currentInstant, intermediateStep)
		} else if (previousHour >= data.size) {
			allBufferLevels
		} else
		{
			// Get the amount of units, according to the forecast, that will be sold in the hour that contains the `previousInstant`.
			val numberOfSalesDuringThisHour = data(previousHour)
			// Change the scale of the time axis such that the instant of each forecasted sale be a natural number. That way the amount of sales contained in a closed-closed interval is the difference of the integer part of the interval's boundaries. Given we need the interval be open-close, the upper boundary is hacked with [[Math.nextDown]].
			val input = Math.nextDown(currentInstant * numberOfSalesDuringThisHour).intValue - (previousInstant * numberOfSalesDuringThisHour).intValue
			val previousLevel = allBufferLevels(shelf.id)

			// TODO
			???
		}
	}

	def randomCarrierAlternatives: Unit = ???
}
