using Microsoft.AspNetCore.Components;
using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class Crop : SourcedItem
	{
		[Inject] private static SkillsState Skills { get; }
		[Inject] private static SettingsState Settings { get; }
		[Inject] private static DateState Date { get; }
		public Season AllowedSeasons { get; }
		public Season SelectedSeasons { get; set; }
		public Replant AllowedReplant { get; }
		public Replant SelectedReplant { get; set; }
		public Replant BestReplant { get; set; }
		public ProductType AllowedProducts { get; }
		public ProductType SelectedProducts { get; set; }
		public Product BestProduct { get; set; }

		//find what should be protected and what should be private
		protected readonly CropType Type;
		private readonly double AvgExtraCrops;
		protected readonly int TotalGrowthTime, BasePrice, Yield;
		private readonly int[] GrowthStagesOrg;
		private int[] GrowthStages;
		protected readonly Dictionary<ProductType, Product> ProductFrom;

		public Crop(string name, int basePrice, Dictionary<Source, BoughtItem> priceFrom, Season seasons, int[] growthStages, CropType cropType = CropType.Tiller, double extraCropChance = 0, Dictionary<ProductType, Product> productFrom = null, Replant replant = Replant.Common, int yield = 1) : base(name, priceFrom)
		{
			Type = cropType;
			BasePrice = basePrice;
			GrowthStagesOrg = growthStages;
			GrowthStages = new int[GrowthStagesOrg.Length];
			ResetGrowthStages();
			for (int i = 0; i < GrowthStagesOrg.Length; i++)
			{
				TotalGrowthTime += GrowthStagesOrg[i];
			}

			AvgExtraCrops = 1.0 / (1 - extraCropChance) - 1;

			if (!Type.HasFlag(CropType.Scythe)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
			{
				AvgExtraCrops += Settings.DoubleCropChance + 1; //E=P(V), = DoubleCropChance(2*crops) + (1-DoubleCropChance)(crops)
			}

			AllowedSeasons = seasons;
			SelectedSeasons = AllowedSeasons;
			AllowedReplant = replant;
			SelectedReplant = AllowedReplant;
			AllowedProducts = ProductType.Crop;
			if (productFrom == null)
			{
				ProductFrom = new Dictionary<ProductType, Product>();
			}
			else
			{
				ProductFrom = productFrom;
				AllowedProducts = 0;
				foreach (ProductType type in ProductFrom.Keys)
				{
					AllowedProducts |= type;
				}
			}
			if (Type.HasFlag(CropType.Fruit) || Type.HasFlag(CropType.Vege))
			{
				AllowedProducts |= ProductType.Keg | ProductType.Jar;
			}
			SelectedProducts = AllowedProducts;

			if (AllowedProducts.HasFlag(ProductType.Jar) && !ProductFrom.ContainsKey(ProductType.Jar))
			{
				if (Type.HasFlag(CropType.Vege))
				{
					ProductFrom.Add(ProductType.Jar, new Product(Name + " Pickle", 2 * BasePrice + 50, ArtiMultiplier.Singleton));
				}
				else if (Type.HasFlag(CropType.Fruit))
				{
					ProductFrom.Add(ProductType.Jar, new Product(Name + "Jam", 2 * BasePrice + 50, ArtiMultiplier.Singleton));
				}
			}
			if (AllowedProducts.HasFlag(ProductType.Keg) && !ProductFrom.ContainsKey(ProductType.Keg))
			{
				if (Type.HasFlag(CropType.Vege))
				{
					ProductFrom.Add(ProductType.Keg, new Product(Name + " Juice", (int)(2.25 * BasePrice), ArtiMultiplier.Singleton));
				}
				else if (Type.HasFlag(CropType.Fruit))
				{
					ProductFrom.Add(ProductType.Keg, new Product(Name + " Wine", 3 * BasePrice, ArtiMultiplier.Singleton));
				}
			}
			if (AllowedProducts.HasFlag(ProductType.Oil) && !ProductFrom.ContainsKey(ProductType.Oil))
			{
				ProductFrom.Add(ProductType.Oil, Product.Oil);
			}
			CalcBestProduct();
		}

		public int GrowthTime(float speedMultiplier)
		{
			if (speedMultiplier == 0)
			{
				return TotalGrowthTime;
			}
			if (Skills.Agri)
			{
				speedMultiplier += 0.1f;
			}
			if (Settings.Irrigated && Type.HasFlag(CropType.Paddy))
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
			if (SelectedProducts == 0)
			{
				BestProduct = null;
			}
			else
			{
				List<Product> products = SelectedProducts.HasFlag(ProductType.Crop) ? new List<Product> { CropProduct } : new List<Product>();
				foreach (ProductType type in ProductFrom.Keys)
				{
					if (SelectedProducts.HasFlag(type))
					{
						products.Add(ProductFrom[type]);
					}
				}
				foreach (Product product in products)
				{
					if (product.Price > BestProduct.Price)
					{
						BestProduct = product;
					}
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
				return Skills.Till && Type.HasFlag(CropType.Tiller) ? (int)(BasePrice * 1.1) : BasePrice;
			}
		}

		private Product CropProduct
		{
			get
			{
				return new Product(Name, CropPrice);
			}
		}

		public double GoldPerDay(Fertilizer fert)
		{
			return Profit(fert.Quality) / GrowthTime(fert.Speed);
		}

		public double Profit(int quality)
		{
			double profit = BestProduct.Price;
			if (BestProduct.Name == Name || Settings.QualityProducts)
			{
				profit = ApplyQualityMultiplier(quality) + AvgExtraCrops * ApplyQualityMultiplier(0);
			}
			//subtract seed 
		}

		private double ApplyQualityMultiplier(int quality)
		{
			double[] dist = QualityDist(quality);
			return dist[2] * (int)(1.5 * BestProduct.Price) + dist[1] * (int)(1.25 * BestProduct.Price) + dist[0] * BestProduct.Price;
		}

		private static double[] QualityDist(int quality)
		{
			double[] dist = new double[3];
			dist[0] = 0.01 + 0.2 * (Skills.BuffedFarmLvl / 10 + quality * (Skills.BuffedFarmLvl + 2) / 12); //check for int division
			dist[1] = Math.Min(2 * dist[0], 0.75) * (1 - dist[0]);
			dist[2] = 1 - dist[0] - dist[1];
			return dist;
		}

		//not in season
		//no products
		//no replant
	}
}
