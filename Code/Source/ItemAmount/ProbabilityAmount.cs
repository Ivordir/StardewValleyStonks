namespace StardewValleyStonks
{
    public class ProbabilityAmount : IItemAmount
    {
        public ProbabilityAmount(IItemAmount baseAmount, double proability)
        {
            BaseAmount = baseAmount;
            Proability = proability;
        }
        
        public double Amount => Proability * BaseAmount.Amount;
        public IProduct Item => BaseAmount.Item;

        private readonly IItemAmount BaseAmount;
        private readonly double Proability;
    }
}
