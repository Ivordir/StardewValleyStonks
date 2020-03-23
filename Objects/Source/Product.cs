using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class Product : BaseItem<Product>
	{
		//public static Product Oil;

		//static Product()
		//{
		//	Oil = new Product("Oil", 100);
		//}

		public string Name { get; }

		private readonly IMultiplier Multiplier;

		public Product(string name, int basePrice, Source source, IMultiplier multiplier = null) : base(basePrice, source.IsEnabled, source.Conditions)
		{
			Name = name;
			Multiplier = multiplier ?? NoMultiplier.Singleton;
		}

		public override int Price => Multiplier.Active ? (int)(BasePrice * Multiplier.Value) : BasePrice;

		public override bool IsBetterThan(Product other) => Price > other.Price;
	}
}