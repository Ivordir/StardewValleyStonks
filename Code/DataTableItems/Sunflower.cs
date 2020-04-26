namespace StardewValleyStonks
{
    public class Sunflower
    {
        //typeof(PriceManager), [index], harvestables without any quality (i.e. sunflower seeds), profit determined before runtime.
        protected readonly IPriceTracker<IProductSource, IPricedItem>[] OtherProducts;

        			foreach (IPriceTracker<IProductSource, IPricedItem> product in OtherProducts)
			{
				profit += product.Price;
			}
}
}
