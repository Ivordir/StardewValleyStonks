using System;
using System.Collections.Generic;

namespace StardewValleyBestCropPlanFinder.Client
{
    public class Crop
    {
        public string Name { get; }
        public CropType Type { get; }
        public Product CurrentProduct { get; }
        public int CurrentSeedPrice { get; }

        public Season SelectedSeasons { get; }
        public Season AllowedSeasons { get; }
        public ReplantMethods AllowedReplantMethods { get; }
        public ReplantMethods SelectedReplantMethods { get; }
        public Dictionary<Sources, int> PriceFrom { get; }

        private readonly double AvgExtraCrops, Profit;
        private readonly int GrowthTime;
        private readonly int[] GrowthStagesOriginal;
        private int[] GrowthStages;
        
        public Crop(string name, int basePrice, Season seasons, int[] growthStages, Dictionary<Sources, int> priceFrom, CropType cropType = CropType.Tiller, List<Product> altProducts = null, double extraCropChance = 0)
        {
            Name = name;
            SeedPrice = seedPrice;
            Seasons = seasons;
            GrowthStagesOriginal = growthStages;
            GrowthStages = new int[growthStages.Length]; 
            ResetGrowthStages();
            PriceFrom = priceFrom;
            for (int i = 0; i < GrowthStages.Length; i++)
            {
                GrowthTime += growthStages[i];
            }
            AvgExtraCrops = 1.0 / (1 - extraCropChance);
            if (!cropType.HasFlag(CropType.ScytheFlag)) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
            {
                AvgExtraCrops = AvgExtraCrops * DoubleCropChance + AvgExtraCrops;
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
            //determine most profitable product to sell
            SellPrice = 0;
            SellName = "";
            if (SellCrop && products.HasFlag(ProductType.Crop))
            {
                if (cropType.HasFlag(CropType.Tiller))
                {
                    CompareProduct((int)(basePrice * CropMultiplier), "Crops");
                }
                else
                {
                    CompareProduct(basePrice, "Crops");
                }
            }
            if (Keg && products.HasFlag(ProductType.Keg))
            {
                if (altKegPrice != -1)
                {
                    CompareProduct(altKegMultiplier ? (int)(altKegPrice * ArtisanMultiplier) : altKegPrice, altKegName);
                }
                else if (cropType.HasFlag(CropType.FruitFlag))
                {
                    CompareProduct((int)(3 * basePrice * ArtisanMultiplier), "Wine");
                }
                else if (cropType.HasFlag(CropType.VegeFlag))
                {
                    CompareProduct((int)((int)(2.25 * basePrice) * ArtisanMultiplier), "Juice");
                }
            }
            if (Jar && products.HasFlag(ProductType.Jar))
            {
                if (cropType.HasFlag(CropType.FruitFlag))
                {
                    CompareProduct((int)((2 * basePrice + 50) * ArtisanMultiplier), "Jelly");
                }
                else if (cropType.HasFlag(CropType.VegeFlag))
                {
                    CompareProduct((int)((2 * basePrice + 50) * ArtisanMultiplier), "Pickles");
                }
            }
            if (OilMaker && products.HasFlag(ProductType.Oil))
            {
                CompareProduct(OilPrice, "Oil");
            }
            if (Mill && products.HasFlag(ProductType.Mill))
            {

            }

            //determine most profitable replant method?
            if (Name == "Sunflower")
            {
                switch (SunflowerChoice)
                {
                    case 3:
                        //replant
                        break;
                    case 2:
                        //oil
                        break;
                    case 1:
                        Profit += SunflowerSeedPrice;
                        break;
                    case 0: //nothing
                        break;
                }
            }
            else
            {
                if (replant.HasFlag(ReplantMethod.Replant))
                {

                }
                if (replant.HasFlag(ReplantMethod.Craft))
                {

                }
                if (replant.HasFlag(ReplantMethod.SeedMaker))
                {

                }
                if (replant.HasFlag(ReplantMethod.BoughtSeeds))
                {

                }
            }
        }
    }
    
    public int GrowthTime (double speedMultiplier)
    {
        if (agri)
        {
            speedMultiplier += 0.1;    
        }
        if (irrigated && cropFlags.HasFlag(CropType.PaddyFlag))
        {
            speedMultiplier += 0.25;
        }
        int maxReduction = (int) Math.Ceiling(GrowthDays * speedMultiplier);
        int daysReduced = 0;
        for (int passes = 0; maxReduction > daysReduced && passes < 3; passes++)
        {
            for (int stage = 0; stage < GrowthStages; stage++)
            {
                if (stage > 0 || growthStagesCopy[stage] > 1)
                {
                    growthStagesCopy[stage]--;
                    daysReduced++;
                    if (maxReduction == daysReduced)
                    {
                        break;
                    }
                }
            }
        }
        return GrowthTime - daysReduced;
    }

    private void ResetGrowthStages()
    {
        for (int i = 0; i < GrowthStagesOriginal.Length; i++)
        {
            GrowthStages[i] = GrowthStagesOriginial[i];    
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
        ScytheFlag = 1 << 3,
        Scythe = ScytheFlag | Vege,
        PaddyFlag = 1 << 4,
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
        Any = 0, //decide based on CropType
        Crop = 1, //always unless user says no
        Keg = 1 << 1, //wine, juice, or other
        Jar = 1 << 2, //jelly or pickle
        Common = Crop | Keg | Jar,
        Oil = 1 << 3, //oil from corn or sunflower
        Mill = 1 << 4 //flour, sugar, rice
    }
    
    public class Product
    {
        public string Name { get; }
        public ProductType Type { get; }
        public int SellPrice { get; }

    }
}
