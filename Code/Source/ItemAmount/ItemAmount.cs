namespace StardewValleyStonks
{
    public class ItemAmount : IItemAmount
    {
        public ItemAmount(IItem item, double amount)
        {
            Item = item;
            Amount = amount;
        }
        public ItemAmount(IItemAmount itemAmount)
        {
            Item = itemAmount.Item;
            Amount = itemAmount.Amount;
        }

        public IItem Item { get; }
        public double Amount { get; set; }
    }
}
