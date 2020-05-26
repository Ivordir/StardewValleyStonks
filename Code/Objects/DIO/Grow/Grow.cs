using ExtentionsLibrary.Memoization;
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

		
		public int Time(double speed)
			=> speed == 0 ?
			TotalTime
			: TotalTime - DaysReduced(speed, GrowthStages);
		public virtual int HarvestsWithin(int days, double speed = 0)
			=> days / Time(speed);
		public virtual int HarvestsWithin(ref int days, double speed = 0)
		{
			int growthTime = Time(speed);
			int numHarvests = days / growthTime;
			days -= growthTime * numHarvests;
			return numHarvests;
		}

		private readonly int[] _GrowthStages;

		private void ResetGrowthStages()
		{
			for (int i = 0; i < GrowthStages.Length; i++)
			{
				_GrowthStages[i] = GrowthStages[i];
			}
		}

		public Grow(int[] growthStages)
		{
			GrowthStages = growthStages;
			_GrowthStages = new int[GrowthStages.Length];
			for (int i = 0; i < GrowthStages.Length; i++)
			{
				TotalTime += GrowthStages[i];
				_GrowthStages[i] = GrowthStages[i];
			}
		}

		private static readonly Func<double, int[], int> DaysReduced = new Func<double, int[], int>((speed, GrowthStages) =>
		{
			int[] growthStages = GrowthStages[..];
			int maxReduction = (int)Math.Ceiling(growthStages[..^1].Sum() * speed);
			int daysReduced = 0;
			for (int passes = 0; daysReduced < maxReduction && passes < 3; passes++)
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
			return daysReduced;
		}).Memoize();
	}
}
