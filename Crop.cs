using System;
using System.Collections.Generic;

public class Crop : SourcedItem
{
	public int RegrowTime { get; }

	public Season AllowedSeasons { get; }
	public Season SelectedSeasons { get; set; }
	public Replant AllowedReplant { get; }
	public Replant SelectedReplant { get; set; }
	public ProductType AllowedProducts { get; }
	public ProductType SelectedProducts { get; set; }
	public Product BestProduct { get; set; }

	private readonly CropType Type;
	private readonly double AvgExtraCrops;
	private readonly int TotalGrowthTime, BasePrice;
	private readonly int[] GrowthStagesOrg;
	private int[] GrowthStages;
	private readonly Dictionary<ProductType, Product> ProductFrom;

	public Crop(string name, int basePrice, Dictionary<string, int> priceFrom, Season seasons, int[] growthStages, CropType cropType = CropType.Tiller, int regrowTime = -1, double extraCropChance = 0, Dictionary<ProductType, Product> productFrom = null, Replant replant = Replant.Common) : base(name, priceFrom)
	{
		AllowedSeasons = seasons;
		SelectedSeasons = AllowedSeasons;
		AllowedReplant = replant;
		SelectedReplant = AllowedReplant;
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
		if (Type.HasFlag(CropType.FruitFlag) || Type.HasFlag(CropType.VegeFlag))
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

		Till && Type.HasFlag(Till) ? (int)(BasePrice * 1.1) : BasePrice);

		if (SelectedProducts.HasFlag(ProductType.Jar) && !ProductFrom.ContainsKey(ProductType.Jar))
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
		
		AvgExtraCrops = 1.0 / (1 - extraCropChance) - 1;

		if (!Type.HasFlag(CropType.ScytheFlag)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
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
		List<Product> products = new List<Product>();
		
		BestProduct = products[0];
		for (int i = 0; i < products.Count; i++)
		{
			if(products[i].Price > BestProduct.Price)
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
