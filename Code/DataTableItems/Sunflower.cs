namespace StardewValleyStonks
{
    public class Sunflower
    {
        //typeof(PriceManager), [index], harvestables without any quality (i.e. sunflower seeds), profit determined before runtime.
        protected readonly IPriceTracker<IProductSource, IPrice>[] OtherProducts;

        			foreach (IPriceTracker<IProductSource, IPrice> product in OtherProducts)
			{
				profit += product.Price;
			}
}
}
