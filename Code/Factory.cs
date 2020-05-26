using System.Collections.Generic;
using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public class Factory
    {
        [Inject] private Skills Skills { get; }

		private readonly Dictionary<string, Item> Products;

		public Item Product(string type, string name, int basePrice)
		{
			return null;
			/*
			return type switch
			{
				"Jam" => new Product(name + " Jam", 2 * basePrice + 50, Skills.Artisan),
				"Pickle" => new Product(name + " Pickle", 2 * basePrice + 50, Skills.Artisan),
				"Wine" => new Product(name + " Wine", 3 * basePrice, Skills.Artisan),
				"Juice" => new Product(name + " Juice", (int)(2.25 * basePrice), Skills.Artisan),
				_ => null
			};
			*/
		}

		public Item StaticProduct(string name)
		{
			if (Products.ContainsKey(name))
			{
				return Products[name];
			}
			throw new KeyNotFoundException($"The product { name } does not exist as a static product.");
		}

		public void RegisterStaticProduct(Item product)
		{
			Products.Add(product.Name, product);
		}

		public CropDIO Crop()
		{
			/*
			bool scythe = false, double extraCropChance = 0, int yield = 1
				AvgExtraCrops = 1.0 / (1 - extraCropChance) + yield - 2;
			if (scythe) //crops harvested with scythe have no double crop chance (i.e. Amaranth, Kale, Wheat, Rice)
			{
				AvgExtraCrops = AvgExtraCrops * Settings.DoubleCropChance + AvgExtraCrops; //E=P(V), = DoubleCropChance*(2*crops) + (1-DoubleCropChance)*(crops)
			}
			*/
			/*
			productFrom ??= new Dictionary<string, Product>();
			productFrom.Add("Crop", new Product(Name, basePrice, Flags.HasFlag(CropFlags.Tiller) ? AgriMultiplier.Instance : null));
			if (Flags.HasFlag(CropFlags.Fruit))
			{
				productFrom.TryAdd("Jar", Factory.Product("Jam", name, basePrice));
				productFrom.TryAdd("Keg", Factory.Product("Wine", name, basePrice));
			}
			else if (Flags.HasFlag(CropFlags.Vege))
			{
				productFrom.TryAdd("Jar", Factory.Product("Pickle", name, basePrice));
				productFrom.TryAdd("Keg", Factory.Product("Juice", name, basePrice));
			}
			ProductManager = new ProductManager[3];
			for (int i = 0; i < ProductManager.Length; i++)
			{
				ProductManager[i] = new ProductManager(productFrom, i);
			}
			*/
			return null;
		}

		/*
		public CropItem CropItem()
		{

		}
		*/
		/*
		public TSourceManager SourceManager<TSourceManager>()
		{
			//call Update() after initilization;
			return null;
		}
		*/
	}
}
