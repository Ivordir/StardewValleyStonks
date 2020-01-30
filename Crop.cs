using System;
using System.Collections.Generic;

public class Crop : ItemWithSources
{
    public string Name { get; }
    public Product Product { get; }

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
            foreach (ProductType key in ProductFrom.Keys)
		    {
                AllowedProducts |= key;
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
        GrowthStages = new int[growthStages.Length]; 
        ResetGrowthStages();
        for (int i = 0; i < GrowthStages.Length; i++)
        {
            TotalGrowthTime += GrowthStages[i];
        }
        
        AvgCrops = 1;
        AvgExtraCrops = 1.0 / (1 - extraCropChance);
        if (!Type.HasFlag(CropType.ScytheFlag)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
        {
            AvgExtraCrops = AvgExtraCrops * DoubleCropChance + AvgExtraCrops; //E=P(V), = DoubleCropChance(2*crops) + (1-DoubleCropChance)(crops)
        }
        AvgExtraCrops--;

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
    
    private void SetBestProduct()
    {
        List<Product> products = new List<Product>();
        
        if(SelectedProducts.HasFlag(Crop))
        {
            int price = basePrice;
            if(tiller && Type.HasFlag(Tiller))
            {
                price = (int) (price * 1.1); 
            }
            products.Add(new Product(name, price));
        }
        if(SelectedProducts.HasFlag(Jar))
        {
            if(ProductFrom.HasFlag(ProductType.Jar))
            {
                
            }
            else if (Type.HasFlag(VegeFlag))
            {
                
            }
            else if (Type.hasFlag(FruitFlag))
            {
            
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
    public enum Replant : byte //in what ways can this crop be planted
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
        Jar = 1 << 1,
        Keg = 1 << 2,
        Oil = 1 << 3,
        Mill = 1 << 4
    }
    
    public class Product
    {
        public string Name { get; }
        public int Price { get; }
        public bool ArtisanMulti { get; }

        public Product(string name, int price, bool artisanMulti = false)
        {
            Name = name;
            Price = price;
            ArtisanMult = artisanMulti;
        }
    }
}
