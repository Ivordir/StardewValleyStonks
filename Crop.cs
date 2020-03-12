using System;
using System.Collections.Generic;

public class Crop : SourcedItem
{
	public Season AllowedSeasons { get; }
	public Season SelectedSeasons { get; set; }
	public Replant AllowedReplant { get; }
	public Replant SelectedReplant { get; set; }
	public Replant BestReplant { get; set; }
	public ProductType AllowedProducts { get; }
	public ProductType SelectedProducts { get; set; }
	public Product BestProduct { get; set; }

	private readonly CropType Type;
	private readonly double AvgExtraCrops;
	private readonly int TotalGrowthTime, BasePrice, Yield;
	private readonly int[] GrowthStagesOrg;
	private int[] GrowthStages;
	private readonly Dictionary<ProductType, Product> ProductFrom;

	public Crop(string name, int basePrice, Dictionary<string, int> priceFrom, Season seasons, int[] growthStages, CropType cropType = CropType.Tiller, int regrowTime = -1, double extraCropChance = 0, Dictionary<ProductType, Product> productFrom = null, Replant replant = Replant.Common, int yield = 1) : base(name, priceFrom)
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

		if (!Type.HasFlag(CropType.ScytheFlag)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
		{
			AvgExtraCrops += DoubleCropChance + 1; //E=P(V), = DoubleCropChance(2*crops) + (1-DoubleCropChance)(crops)
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
		if (Type.HasFlag(CropType.FruitFlag) || Type.HasFlag(CropType.VegeFlag))
		{
			AllowedProducts |= ProductType.Keg | ProductType.Jar;
		}
		SelectedProducts = AllowedProducts;

		if (AllowedProducts.HasFlag(ProductType.Jar) && !ProductFrom.ContainsKey(ProductType.Jar))
		{
			if (Type.HasFlag(CropType.VegeFlag))
			{
				ProductFrom.Add(ProductType.Jar, new Product(Name + " Pickle", 2 * BasePrice + 50, true));
			}
			else if (Type.HasFlag(CropType.FruitFlag))
			{
				ProductFrom.Add(ProductType.Jar, new Product(Name + "Jam", 2 * BasePrice + 50, true));
			}
		}
		if (AllowedProducts.HasFlag(ProductType.Keg) && !ProductFrom.ContainsKey(ProductType.Keg))
		{
			if (Type.HasFlag(CropType.VegeFlag))
			{
				ProductFrom.Add(ProductType.Keg, new Product(Name + " Juice", (int)(2.25 * BasePrice), true));
			}
			else if (Type.HasFlag(CropType.FruitFlag))
			{
				ProductFrom.Add(ProductType.Keg, new Product(Name + " Wine", 3 * BasePrice, true));
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
		if (Agri)
		{
			speedMultiplier += 0.1f;
		}
		if (Irrigated && Type.HasFlag(PaddyFlag))
		{
			speedMultiplier += 0.25f;
		}
		int maxReduction = (int) Math.Ceiling(TotalGrowthTime * speedMultiplier);
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
			BestProuct = null;
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
				if(product.Price > BestProduct.Price)
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
				for(Season season = StartSeason ; season <= EndSeason; season << 1)
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
	
	private Product CropProduct
	{
		get
		{
			return new Product(Name, CropPrice);
		}
	{
	
	public double GoldPerDay(Fertilizer fert)
	{
		return Profit(fert.Quality) / GrowthTime(fert.Speed);
	}
	
	public double Profit(int quality)
	{
		get
			{
				double profit = BestProduct.Price;
				if (BestProduct.Name == Name || QualityProducts)
				{
					profit = ApplyQualityMultiplier(quality) + AvgExtraCrops * ApplyQualityMultiplier(0);
				}
				//subtract seed 
			}
	}
	
	private static double ApplyQualityMultiplier(int quality)
	{
		double[] dist = QualityDist(quality);
		return dist[2] * (int) (1.5 * BestProduct.Price) + dist[1] * (int) (1.25 * BestProduct.Price) + dist[0] * BestProduct.Price;
	}
	
	private static double[] QualityDist(int quality)
	{
		double[] dist = new double[3];
		dist[0] = 0.01 + 0.2 * (FarmLvl / 10 + quality * (FarmLvl + 2) / 12); //check for int division
		dist[1] = Math.Min(2 * dist[0], 0.75) * (1 - dist[0]);
		dist[2] = 1 - dist[0] - dist[1];
		return dist;
	}
	
	[Flags]
	public enum CropType : byte //what bonuses do/do not apply to this crop and what products can this crop sell as unless overridden
	{
		Tiller = 1,
		FruitFlag = 1 << 1,
		Fruit = FruitFlag | Tiller,
		VegeFlag = 1 << 2,
		Vege = VegeFlag | Tiller,
		ScytheFlag = 1 << 3,
		Scythe = ScytheFlag | Vege,
		Trelis = 1 << 4,
		PaddyFlag = 1 << 5,
		Paddy = PaddyFlag | Scythe,
		IndoorOnly = 1 << 6
	}

	//not in season
	//no products
	//no replant
}
