using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class RegrowCrop : Crop
	{
		public int RegrowTime { get; }

		public RegrowCrop(string name, int basePrice, Dictionary<Source, int> priceFrom, Season seasons, int[] growthStages, CropType cropType = CropType.Tiller, int regrowTime = -1, double extraCropChance = 0, Dictionary<ProductType, Product> productFrom = null, Replant replant = Replant.Common) : base(name, priceFrom)
		{
			RegrowTime = regrowTime;
		}

		public int GrowthTime(float speedMultiplier)
		{
			if (speedMultiplier == 0)
			{
				return TotalGrowthTime;
			}
			if (State.Agri)
			{
				speedMultiplier += 0.1f;
			}
			if (Irrigated && Type.HasFlag(PaddyFlag))
			{
				speedMultiplier += 0.25f;
			}
			int maxReduction = (int)Math.Ceiling(TotalGrowthTime * speedMultiplier);
			int daysReduced = 0;
			for (int passes = 0; maxReduction > daysReduced && passes < 3; passes++)
			{
				for (int stage = 0; stage < GrowthStages.Length; stage++)
				{
					if (stage > 0 || GrowthStages[stage] > 1)
					{
						GrowthStages[stage]--;
						daysReduced++;
						if (maxReduction == daysReduced)
						{
							break;
						}
					}
				}
			}
			ResetGrowthStages();
			return TotalGrowthTime - daysReduced;
		}

		private void CalcBestProduct()
		{
			List<Product> products = new List<Product>();

			BestProduct = products[0];
			for (int i = 0; i < products.Count; i++)
			{
				if (products[i].Price > BestProduct.Price)
				{
					BestProduct = products[i];
				}
			}
		}

		private void ResetGrowthStages()
		{
			for (int i = 0; i < GrowthStagesOrg.Length; i++)
			{
				GrowthStages[i] = GrowthStagesOrg[i];
			}
		}

		private int CropPrice
		{
			get
			{
				return Till && type.HasFlag(Tiller) ? (int)(BasePrice * 1.1) : BasePrice;
			}
		}

		public double GoldPerDay(Fertilizer fert)
		{
			return Profit(fert.Quality) / GrowthTime(fert.Speed);
		}

		public double Profit(int quality)
		{
			get
				{
				if (BestProduct == null)
				{
					return Multiplier(quality, CropPrice);
				}
				else if (QualityProducts)
				{
					return
				}
				return BestProduct.Price;
			}
		}
	}
}
