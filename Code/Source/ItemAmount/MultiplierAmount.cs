namespace StardewValleyStonks
{
    public class MultiplierAmount : IItemAmount
    {
        public MultiplierAmount(IItemAmount baseAmount, IMultiplier multiplier)
        {
            BaseAmount = baseAmount;
            Multiplier = multiplier;
        }
        
        public double Amount => Multiplier.Value * BaseAmount.Amount;
        public IItem Item => BaseAmount.Item;

        private readonly IItemAmount BaseAmount;
        private readonly IMultiplier Multiplier;
    }
}
