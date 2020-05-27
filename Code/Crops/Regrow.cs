namespace StardewValleyStonks
{
    public class Regrow : Grow
	{
		public override int RegrowTime { get; }
		public override bool Regrows => true;

		public override int HarvestsWithin(int days, double speed = 0) =>
			1 + (days - DaysPerHarvest(speed)) / RegrowTime;
		public override int HarvestsWithin(ref int days, double speed = 0)
		{
			days -= DaysPerHarvest(speed) + (days - DaysPerHarvest(speed)) / RegrowTime * RegrowTime;
			return 1 + (days - DaysPerHarvest(speed)) / RegrowTime;
		}

		public Regrow(
			int[] growthStages,
			int regrowTime)
			: base(growthStages)
		{
			RegrowTime = regrowTime;
		}
	}
}
