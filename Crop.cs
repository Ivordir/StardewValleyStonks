using System;
using System.Collections.Generic;

public class Crop : ItemWithSources
{
    public string Name { get; }
    public Product SelectedProduct { get; }

    public Season SelectedSeasons { get; set; }
    public Season AllowedSeasons { get; }
    public ReplantMethod SelectedReplantMethods { get; set; }
    public ReplantMethod AllowedReplantMethods { get; }
    public ProductType SelectedProducts { get; set; }
    public ProductType AllowedProducts { get; set; }
    
    private readonly bool IsPaddyCrop;
    private readonly double AvgCrops, AvgExtraCrops, Profit;
    private readonly int TotalGrowthTime;
    private readonly int[] GrowthStagesOriginal;
    private int[] GrowthStages;
    private readonly Dictionary<ProductType, Product> ProductFrom;

    public Crop(string name, int price, Season seasons, int[] growthStages, Dictionary<Sources, int> priceFrom, CropType cropType = CropType.Tiller, List<Product> altProducts = null, double extraCropChance = 0) : base(priceFrom)
    {
        Name = name;
        AllowedSeasons = seasons;
        SelectedSeasons = seasons;
        
        GrowthStagesOriginal = growthStages;
        GrowthStages = new int[growthStages.Length]; 
        ResetGrowthStages();
        for (int i = 0; i < GrowthStages.Length; i++)
        {
            TotalGrowthTime += growthStages[i];
        }
        
        ProductFrom = new Dictionary<ProductType, Product>();
        if (cropType.HasFlag(CropType.Tiller))
        {
            price = (int) (1.1 * price);
        }
        ProductFrom.Add(ProductType.Crop, new Product(name, price));
        if (cropType.HasFlag(CropType.FruitFlag))
        {
            ProductFrom.Add(ProductType.Keg, 
        }
        
        AvgCrops = 1;
        AvgExtraCrops = 1.0 / (1 - extraCropChance);
        if (!cropType.HasFlag(CropType.ScytheFlag)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
        {
            AvgExtraCrops = AvgExtraCrops * DoubleCropChance + AvgExtraCrops; //E=P(V), = DoubleCropChance(2*crops) + (1-DoubleCropChance)(crops)
        }
        AvgExtraCrops--;
        
        ProductType products = ProductType.Crop;
        if (cropType.HasFlag(CropType.FruitFlag) || cropType.HasFlag(CropType.VegeFlag))
        {
            products |= ProductType.Jar | ProductType.Keg;
        }
        if (altProducts != null)
        {
            foreach (Product product in altProducts)
            {
                products |= product.Type;
            }
        }
    }
    
    public int GrowthTime (double speedMultiplier)
    {
        if (agri)
        {
            speedMultiplier += 0.1;    
        }
        if (irrigated && IsPaddyCrop)
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

    private void ResetGrowthStages()
    {
        for (int i = 0; i < GrowthStagesOriginal.Length; i++)
        {
            GrowthStages[i] = GrowthStagesOriginal[i];    
        }
    }
    
    [Flags]
    public enum CropType : byte //what bonuses do/do not apply to this crop and what products can this crop sell as unless overridden
    {
        Basic = 0,
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
    public enum ReplantMethod : byte //in what ways can this crop be planted
    {
        None = 0, //why?
        BoughtSeeds = 1, //always an option unless user says no
        SeedMaker = 1 << 1, //always unless tea, coffee, no seedmaker, or user says no. seed chance = 0.975 unless ancient fruit where seed chance = 0.98
        Common = BoughtSeeds | SeedMaker,
        Replant = 1 << 2, //coffee and sunflower
        Craft = 1 << 3 //tea
    }

    [Flags]
    public enum ProductType : byte //as what products can this crop sell as
    {
        Any = 0,
        Crop = 1,
        Keg = 1 << 1,
        Jar = 1 << 2,
        Oil = 1 << 3,
        Mill = 1 << 4
    }
    
    public class Product
    {
        public string Name { get; }
        public int Price { get; }

        public Product(string name, int price)
        {
            Name = name;
            Price = price;
        }
    }
}
