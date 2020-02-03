using System;
using System.Collections.Generic;

public class Crop : ItemWithSources
{
	public string Name { get; }
	public int RegrowTime { get; }
	public Product Product { get; set; }

	public Season AllowedSeasons { get; }
	public Season SelectedSeasons { get; set; }
	public Replant AllowedReplant { get; }
	public Replant SelectedReplant { get; set; }
	public ProductType AllowedProducts { get; }
	public ProductType SelectedProducts { get; set; }
    
	private readonly CropType Type;
	private readonly double AvgCrops, AvgExtraCrops;
	private readonly int TotalGrowthTime, BasePrice;
	private readonly int[] GrowthStagesOriginal;
	private int[] GrowthStages;
	private readonly Dictionary<ProductType, Product> ProductFrom;

	public Crop(string name, int basePrice, Dictionary<Sources, int> priceFrom, Season seasons, int[] growthStages, int regrowTime = -1, double extraCropChance = 0, CropType cropType = CropType.Tiller, Replant replant = Replant.Common, Dictionary<ProductType, Product> productFrom = null) : base(priceFrom)
	{
		Name = name;
		RegrowTime = regrowTime;
		
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
		AllowedProducts |= CropType.Crop;
		if (Type.HasFlag(CropType.FruitFlag) || Type.HasFlag(CropType.VegeFlag))
		{
			AllowedProducts |= CropType.Keg | CropType.Jar;
		}
		SelectedProducts = AllowedProducts;
        
		Type = cropType;
		BasePrice = basePrice;
		GrowthStagesOriginal = growthStages;
		GrowthStages = new int[GrowthStagesOriginal.Length]; 
		ResetGrowthStages();
		for (int i = 0; i < GrowthStagesOriginal.Length; i++)
		{
			TotalGrowthTime += GrowthStagesOriginal[i];
		}
		
		CalcBestProduct();
		
		if(Type.HasFlag(CropType.GiantCrop))
		{
			//GiantCropChecksPerTile = 9;
        	//GiantCropProb = (1 - Math.Pow(0.99, GiantCropChecksPerTile));
			AvgCrops = 1 - GiantCropProb;
			//assume extraCropChance == 0
			AvgExtraCrops = 2 * GiantCropYields;
		}
		else
		{
			AvgCrops = 1;
			AvgExtraCrops = 1.0 / (1 - extraCropChance) - 1;
		}
		if (!Type.HasFlag(CropType.ScytheFlag)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
		{
			AvgExtraCrops += AvgCrops * DoubleCropChance + AvgCrops; //E=P(V), = DoubleCropChance(2*crops) + (1-DoubleCropChance)(crops)
		}
    }
    
    public int GrowthTime (double speedMultiplier)
    {
        if (agri)
        {
            speedMultiplier += 0.1;    
        }
        if (irrigated && Type.HasFlag(PaddyFlag))
        {
            speedMultiplier += 0.25;
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
        
        if(SelectedProducts.HasFlag(ProductType.Crop))
        {
            products.Add(new Product(name, tiller && Type.HasFlag(Tiller) ? (int) (BasePrice * 1.1) : BasePrice));
        }
        if(SelectedProducts.HasFlag(ProductType.Jar))
        {
            if(ProductFrom.ContainsKey(ProductType.Jar))
            {
                products.Add(ProductFrom[ProductType.Jar]);
            }   
            else if (Type.HasFlag(CropType.VegeFlag))
            {
                products.Add(new Product(Name + "Pickle", 2 * BasePrice + 50, true);
            }
            else if (Type.HasFlag(CropType.FruitFlag))
            {
            	products.Add(new Product(Name + "Jam", 2 * BasePrice + 50, true);
            }
        }
		if(SelectedProducts.HasFlag(ProductType.Key)
		{
			if(ProductFrom.ContainsKey(ProductType.Keg))
            {
                products.Add(ProductFrom[ProductType.Keg]);
            }   
            else if (Type.HasFlag(CropType.VegeFlag))
            {
                products.Add(new Product(Name + " Juice", (int) (2.25 * BasePrice), true);
            }
            else if (Type.HasFlag(CropType.FruitFlag))
            {
            	products.Add(new Product(Name + " Wine", 3 * BasePrice, true);
            }
		}
		if(SelectedProducts.HasFlag(ProductType.Oil))
		{
			if(ProductFrom.ContainsKey(ProductType.Oil))
			{
				products.Add(ProductFrom[ProductType.Oil]);
			}
			else
			{
				products.Add(Product.Oil);
			}
		}
		if(SelectedProducts.HasFlag(ProductType.Mill))
		{
			products.Add(ProductFrom[ProductType.Mill]);
		}
		Product = products[0];
		for (int i = 1; i < products.Count; i++)
		{
			if(products[i].Price() > Product.Price())
			{
				Product = products[i];	
			}
		}
	}

    private void ResetGrowthStages()
    {
        for (int i = 0; i < GrowthStagesOriginal.Length; i++)
        {
            GrowthStages[i] = GrowthStagesOriginal[i];    
        }
    }
    
	public string Image
	{
		get
		{
			return "images/crops/" + Name + ".png";
		};
	}
	
    [Flags]
    public enum CropType : byte //what bonuses do/do not apply to this crop and what products can this crop sell as unless overridden
    {
        Tiller = 1,
        FruitFlag = 1 << 1,
        Fruit = FruitFlag | Tiller,
        VegeFlag = 1 << 2,
        Vege = VegeFlag | Tiller,
        GiantCropFlag = 1 << 3,
        GiantCrop = GiantCropFlag | Tiller,
        ScytheFlag = 1 << 4,
        Scythe = ScytheFlag | Vege,
        PaddyFlag = 1 << 5,
        Paddy = PaddyFlag | Scythe
    }

    [Flags]
    public enum Replant : byte //in what ways can this crop be planted
    {
        BoughtSeeds = 1, //always an option unless user says no
        SeedMaker = 1 << 1, //always unless tea, coffee, no seedmaker, or user says no. seed chance = 0.975 unless ancient fruit where seed chance = 0.98
        Common = BoughtSeeds | SeedMaker,
        Replant = 1 << 2, //coffee and sunflower
    }

    [Flags]
    public enum ProductType : byte //as what products can this crop sell as
    {
        Crop = 1,
        Jar = 1 << 1,
        Keg = 1 << 2,
        Oil = 1 << 3,
        Mill = 1 << 4
    }
    
    public class Product
    {
    	public static Product Oil;
		
		static Product()
		{
			Oil = new Product("Oil", 100);
		}

		public string Name { get; }

		private readonly int Price;
		private readonly bool UsesArtisan;

		public Product(string name, int price, bool usesArtisan = false)
		{
			Name = name;
			Price = price;
			UsesArtisan = usesArtisan;
		}

		public int Price
		{
			get
			{
				return artisan && usesArtisan ? (int) (Price * 1.4) : Price;
			};
		}
    }
}
