using System;

namespace StardewValleyStonks
{
	public class Product : Selectable, IProduct, IComparable<IProduct>
	{
		public string Name { get; }
		public double OutputPerInput { get; }
		public int Quality => 0;

		private readonly int BasePrice;
		private readonly IPriceMultiplier Multiplier;

		public Product(
			string name,
			int basePrice,
			IPriceMultiplier multiplier = null,
			double outputPerInput = 1,
			bool enabled = true,
			ICondition[] conditions = null)
			: base(enabled, conditions)
		{
			Name = name;
			BasePrice = basePrice;
			Multiplier = multiplier;
			OutputPerInput = outputPerInput;
		}

		public double Price => OutputPerInput * UnitPrice;

		public int UnitPrice => Multiplier == null ? BasePrice : Multiplier.ApplyTo(BasePrice);

		public int CompareTo(IProduct other)
		{
			return Price.CompareTo(other.Price);
		}
	}
}