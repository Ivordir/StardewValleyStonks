namespace StardewValleyStonks
{
	public class RegrowCrop : Crop
	{
		public int RegrowTime { get; }

		public RegrowCrop(
			string name,
			Season seasons,
			int[] growthStages,
			int regrowTime,
			double avgCrops,
			double avgExtraCrops,
			IPriceTracker<IProductSource, IPricedItem>[] qualitites)
			: base(name, seasons, growthStages, avgCrops, avgExtraCrops, qualitites)
		{
			RegrowTime = regrowTime;
		}

		public override int HarvestsWithin(int days, double speed = 0)
		{
			int growthTime = GrowthTime(speed);
			if (days < growthTime)
			{
				return 0;
			}
			return 1 + (days - growthTime) / RegrowTime;
		}

		public override int HarvestsWithin(ref int days, double speed = 0)
		{
			int growthTime = GrowthTime(speed);
			if (days < growthTime)
			{
				return 0;
			}
			int numHarvests = (days - growthTime) / RegrowTime;
			days -= growthTime + numHarvests * RegrowTime;
			return numHarvests + 1;
		}

		public override double Profit(int fertQuality, int harvests = 1)
		{
			return ProfitPerHarvest(fertQuality) * harvests - SeedPrice;
		}
	}
}
