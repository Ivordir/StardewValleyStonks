using System;
using System.Linq;

namespace StardewValleyStonks
{
	public class Grow
	{
		public int[] GrowthStages { get; }
		public int TotalTime { get; }
		public virtual bool Regrows => false;
		public virtual int RegrowTime => throw new MissingFieldException("This does not regrow");

		public int DaysPerHarvest(double speed)
		{
			if (speed == 0)
			{
				return TotalTime;
			}
			//cast to float and then back to add a very small decimal to mimic the behavior in the game.
			//sometimes results in an extra day being reduced, as the ceiling funtion will come into effect.
			int maxReduction = (int)Math.Ceiling(TotalTime * (double)(float)speed);
			int daysReduced = 0;
			int firstStage = GrowthStages[0];
			for (int traverses = 0; daysReduced < maxReduction && traverses < 3; traverses++)
			{
				for (int stage = 0; daysReduced < maxReduction && stage < GrowthStages.Length; stage++)
				{
					if (stage > 0)
					{
						daysReduced++;
					}
					else if (firstStage > 1)
					{
						firstStage--;
						daysReduced++;
					}
				}
				/* growthStages/phaseDays in the game have a stage of 999999... as the last stage. (Don't ask why.)
				 * When one traverse of growthStages is completed, a dayReduction is used up reducing this phantom stage.
				 * This next line of code simulates it without actually including the phantom stage in the growthStages variable.
				 * An example is Cauliflower with 0.35 speed:
				 * It should be reduced by 12 * 0.35 = 4.2 -> Ceiling -> 5 days
				 * So, 12 - 5 = 7 days to fully grow. In reality it's growth time with 0.35 speed is 8.
				 * The first stage can't be reduced (there's a check in game if growthStages[0] > 1),
				 * so the first 4 dayReductions are spent reducing the remaining 4 stages.
				 * It then reaches the phatom stage where the last dayReduction is used up,
				 * resulting an actual reduction of only 4 days.*/
				maxReduction--;
			}
			return TotalTime - daysReduced;
		}
		public virtual int HarvestsWithin(int days, double speed = 0) =>
			days / DaysPerHarvest(speed);
		public virtual int HarvestsWithin(ref int days, double speed = 0)
		{
			days -= days / DaysPerHarvest(speed) * DaysPerHarvest(speed);
			return days / DaysPerHarvest(speed);
		}
		public int[] GrowthStagesWith(double speed)
		{

			if (speed == 0)
			{
				return GrowthStages;
			}
			int[] growthStages = GrowthStages[..];
			int maxReduction = (int)Math.Ceiling(TotalTime * (double)(float)speed);
			int daysReduced = 0;
			for (int traverses = 0; daysReduced < maxReduction && traverses < 3; traverses++)
			{
				for (int stage = 0; daysReduced < maxReduction && stage < GrowthStages.Length; stage++)
				{
					if (stage > 0 || growthStages[0] > 1)
					{
						daysReduced++;
						growthStages[stage]--;
					}
				}
				maxReduction--;
			}
			return growthStages;
		}

		public Grow(int[] growthStages)
		{
			GrowthStages = growthStages;
			TotalTime = GrowthStages.Sum();
		}
	}
}
