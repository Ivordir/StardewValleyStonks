using System;

namespace StardewValleyStonks
{
	public class Grow : IGrow
	{
		private static readonly SpeedMultiplier[] None = new SpeedMultiplier[0];

		public int TotalTime { get; }

		private readonly int[] GrowthStagesOrg;
		private readonly int[] GrowthStages;
		private readonly SpeedMultiplier[] SpeedMultipliers;

		public Grow(
			int[] growthStages,
			SpeedMultiplier[] speedMultipliers = null)
		{
			GrowthStagesOrg = growthStages;
			GrowthStages = new int[GrowthStagesOrg.Length];
			ResetGrowthStages();
			for (int i = 0; i < GrowthStagesOrg.Length; i++)
			{
				TotalTime += GrowthStagesOrg[i];
			}
			SpeedMultipliers = speedMultipliers ?? None;
		}

		public virtual int Time(double speed)
		{
			foreach (SpeedMultiplier multiplier in SpeedMultipliers)
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
				for (int stage = 0; daysReduced < maxReduction && stage < GrowthStages.Length; stage++)
				{
					if (stage > 0 || GrowthStages[stage] > 1)
					{
						GrowthStages[stage]--;
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

		public virtual bool Regrows => false;
		public virtual int RegrowTime => throw new MissingFieldException("This does not regrow");

		private void ResetGrowthStages()
		{
			for (int i = 0; i < GrowthStagesOrg.Length; i++)
			{
				GrowthStages[i] = GrowthStagesOrg[i];
			}
		}
	}
}
