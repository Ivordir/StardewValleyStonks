namespace StardewValleyStonks
{
    public class ItemAmount : IItemAmount
    {
        public ItemAmount(IProduct item, double amount)
        {
            Item = item;
            Amount = amount;
        }

        public IProduct Item { get; }
        public double Amount { get; set; }
    }
}
