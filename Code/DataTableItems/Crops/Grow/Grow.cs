using System;

namespace StardewValleyStonks
{
	public class Grow
	{
		public int[] GrowthStages { get; }
		public int TotalTime { get; }
		public IMultiplier[] SpeedMultipliers { get; }
		public virtual bool Regrows => false;
		public virtual int RegrowTime => throw new MissingFieldException("This does not regrow");

		public virtual int Time(double speed)
		{
			foreach (IMultiplier multiplier in SpeedMultipliers)
			{
				if (multiplier.Active)
				{
					speed += multiplier.Value;
				}
			}
			if (speed == 0)
			{
				return TotalTime;
			}
			int maxReduction = (int)Math.Ceiling((TotalTime - GrowthStages[^1]) * speed);
			int daysReduced = 0;
			for (int passes = 0; daysReduced < maxReduction && passes < 3; passes++)
			{
				for (int stage = 0; daysReduced < maxReduction && stage < _GrowthStages.Length; stage++)
				{
					if (stage > 0 || _GrowthStages[0] > 1)
					{
						_GrowthStages[stage]--;
						daysReduced++;
					}
				}
			}
			ResetGrowthStages();
			return TotalTime - daysReduced;
		}
		public virtual int HarvestsWithin(int days, double speed = 0)
		{
			return days / Time(speed);
		}
		public virtual int HarvestsWithin(ref int days, double speed = 0)
		{
			int growthTime = Time(speed);
			int numHarvests = days / growthTime;
			days -= growthTime * numHarvests;
			return numHarvests;
		}

		private readonly int[] _GrowthStages;

		public Grow(
			int[] growthStages,
			IMultiplier[] speedMultipliers = null)
		{
			GrowthStages = growthStages;
			for (int i = 0; i < GrowthStages.Length; i++)
			{
				TotalTime += GrowthStages[i];
			}

			_GrowthStages = new int[GrowthStages.Length];
			ResetGrowthStages();

			SpeedMultipliers = speedMultipliers ?? None;
		}

		private void ResetGrowthStages()
		{
			for (int i = 0; i < GrowthStages.Length; i++)
			{
				_GrowthStages[i] = GrowthStages[i];
			}
		}

		private static readonly IMultiplier[] None = new IMultiplier[0];
	}
}
