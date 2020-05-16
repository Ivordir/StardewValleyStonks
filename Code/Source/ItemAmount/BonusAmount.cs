namespace StardewValleyStonks
{
    public class BonusAmount : IItemAmount
    {
        public BonusAmount(IItemAmount baseAmount, double bonus)
        {
            BaseAmount = baseAmount;
            Bonus = bonus;
        }

        IItemAmount BaseAmount { get; }
        public double Bonus { get; set; }
        public double Amount => BaseAmount.Amount + Bonus;
        public IProduct Item => BaseAmount.Item;
    }
}
