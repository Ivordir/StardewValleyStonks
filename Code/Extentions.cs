namespace StardewValleyStonks
{
    public static class Extentions
    {
        public static int Price(this BestDict<Source, BuyPrice> bestFinder)
        {
            return bestFinder.Exists ? bestFinder.BestItems[0].Price : 0;
        }
    }
}
