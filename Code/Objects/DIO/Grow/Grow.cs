using System;
using System.Linq;
using ExtentionsLibrary.Memoization;

namespace StardewValleyStonks
{
	public class Grow
	{
		static readonly Func<double, int, int[], int> ReducedTime = new Func<double, int, int[], int>(
			(speed, TotalTime, GrowthStages) =>
		{
			int[] growthStages = GrowthStages[..];
			int maxReduction = (int)Math.Ceiling((TotalTime - growthStages[^1]) * speed);
			int daysReduced = 0;
			for (int traverses = 0; daysReduced < maxReduction && traverses < 3; traverses++)
			{
				for (int stage = 0; daysReduced < maxReduction && stage < growthStages.Length; stage++)
				{
					if (stage > 0 || growthStages[0] > 1)
					{
						growthStages[stage]--;
						daysReduced++;
					}
				}
			}
			return TotalTime - daysReduced;
		}).Memoize();

		public int[] GrowthStages { get; }
		public int TotalTime { get; }
		public virtual bool Regrows => false;
		public virtual int RegrowTime => throw new MissingFieldException("This does not regrow");

		public int DaysPerHarvest(double speed) =>
			speed == 0 ?
			TotalTime :
			ReducedTime(speed, TotalTime, GrowthStages);
		public virtual int HarvestsWithin(int days, double speed = 0) =>
			days / DaysPerHarvest(speed);
		public virtual int HarvestsWithin(ref int days, double speed = 0)
		{
			days -= days / DaysPerHarvest(speed) * DaysPerHarvest(speed);
			return days / DaysPerHarvest(speed);
		}

		public Grow(int[] growthStages)
		{
			GrowthStages = growthStages;
			TotalTime = GrowthStages.Sum();
		}
	}
}
