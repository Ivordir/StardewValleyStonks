using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class GiantCrop : Crop
	{
		private readonly double AvgCrops;

		public GiantCrop(string name, int basePrice, Dictionary<string, int> priceFrom, Season seasons, int[] growthStages, CropFlags cropType = CropFlags.Tiller, int regrowTime = -1, double extraCropChance = 0, Dictionary<ProductType, Product> productFrom = null, Replant replant = Replant.Common) : base(name, priceFrom)
		{
			if (productFrom == null)
			{
				ProductFrom = new Dictionary<ProductType, Product>();
			}
			else
			{
				ProductFrom = productFrom;
				foreach (ProductType type in ProductFrom.Keys)
				{
					AllowedProducts |= type;
				}
			}
			AllowedProducts |= ProductType.Crop;
			if (Flags.HasFlag(CropFlags.FruitFlag) || Flags.HasFlag(CropFlags.VegeFlag))
			{
				AllowedProducts |= ProductType.Keg | ProductType.Jar;
			}
			SelectedProducts = AllowedProducts;

			Type = cropType;
			BasePrice = basePrice;
			GrowthStagesOrg = growthStages;
			GrowthStages = new int[GrowthStagesOrg.Length];
			ResetGrowthStages();
			for (int i = 0; i < GrowthStagesOrg.Length; i++)
			{
				TotalGrowthTime += GrowthStagesOrg[i];
			}

			Till && Flags.HasFlag(Till) ? (int)(BasePrice * 1.1) : BasePrice);


			if (SelectedProducts.HasFlag(ProductType.Jar) && !ProductFrom.ContainsKey(ProductType.Jar))
			{
				if (Flags.HasFlag(CropFlags.VegeFlag))
				{
					ProductFrom.Add(ProductType.Jar, new Product(Name + " Pickle", 2 * BasePrice + 50, true));
				}
				else if (Flags.HasFlag(CropFlags.FruitFlag))
				{
					ProductFrom.Add(ProductType.Jar, new Product(Name + "Jam", 2 * BasePrice + 50, true));
				}
			}
			if (AllowedProducts.HasFlag(ProductType.Keg) && !ProductFrom.ContainsKey(ProductType.Keg))
			{
				if (Flags.HasFlag(CropFlags.VegeFlag))
				{
					ProductFrom.Add(ProductType.Keg, new Product(Name + " Juice", (int)(2.25 * BasePrice), true));
				}
				else if (Flags.HasFlag(CropFlags.FruitFlag))
				{
					ProductFrom.Add(ProductType.Keg, new Product(Name + " Wine", 3 * BasePrice, true));
				}
			}
			if (AllowedProducts.HasFlag(ProductType.Oil) && !ProductFrom.ContainsKey(ProductType.Oil))
			{
				ProductFrom.Add(ProductType.Oil, Product.Oil);
			}
			CalcBestProduct();

			//GiantCropChecksPerTile = 9;
			//GiantCropProb = (1 - Math.Pow(0.99, GiantCropChecksPerTile));
			AvgCrops = 1 - GiantCropProb;
			//assume extraCropChance == 0
			AvgExtraCrops = 2 * GiantCropYields;

			if (!Flags.HasFlag(CropFlags.ScytheFlag)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
			{
				AvgExtraCrops += AvgCrops * DoubleCropChance + AvgCrops; //E=P(V), = DoubleCropChance(2*crops) + (1-DoubleCropChance)(crops)
			}
		}

		public int GrowthTime(float speedMultiplier)
		{
			if (speedMultiplier == 0)
			{
				return TotalGrowthTime;
			}
			if (Agri)
			{
				speedMultiplier += 0.1f;
			}
			if (Irrigated && Flags.HasFlag(PaddyFlag))
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

		public bool Valid
		{
			get
			{
				if (Seasons && SelectedSeasons != 0)
				{
					for (Season season = StartSeason; season <= EndSeason; season << 1)
					{

					}
				}
				if (SelectedProducts == 0)
					if (SelectedReplant > 0 && (SelectedReplant != Replant.BoughtSeeds || Source != null))
					{

					}
				if (!Obsolete)
				{

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
