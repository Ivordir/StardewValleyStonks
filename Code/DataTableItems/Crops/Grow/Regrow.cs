namespace StardewValleyStonks
{
    public class Regrow : Grow, IRegrow
    {
		public Regrow(
			int[] growthStages,
			int regrowTime,
			SpeedMultiplier[] speedMultipliers = null)
			: base(growthStages, speedMultipliers)
		{
			RegrowTime = regrowTime;
		}

		public override bool Regrows => true;
		public override int RegrowTime { get; }

		public override int HarvestsWithin(int days, double speed = 0)
		{
			return 1 + (days - Time(speed)) / RegrowTime;
		}

		public override int HarvestsWithin(ref int days, double speed = 0)
		{
			int growthTime = Time(speed);
			int numHarvests = (days - growthTime) / RegrowTime;
			days -= growthTime + numHarvests * RegrowTime;
			return numHarvests + 1;
		}
	}
}
