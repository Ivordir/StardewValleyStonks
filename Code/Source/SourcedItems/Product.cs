using System.Collections.Generic;

namespace StardewValleyStonks
{
	public class Product : ActiveItem, IPricedItem
	{
		public string Name { get; }

		private readonly int BasePrice;
		private readonly IMultiplier Multiplier;

		public Product(
			string name,
			int basePrice,
			IMultiplier multiplier = null,
			bool enabled = true,
			List<ICondition> conditions = null)
			: base(enabled, conditions)
		{
			Name = name;
			BasePrice = basePrice;
			Multiplier = multiplier;
		}

		public int Price => Multiplier == null ? BasePrice : Multiplier.ApplyTo(BasePrice);

		public int CompareTo(IPricedItem other)
		{
			return Price.CompareTo(other.Price);
		}
	}
}