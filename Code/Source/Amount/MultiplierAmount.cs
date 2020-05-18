namespace StardewValleyStonks
{
    public class MultiplierAmount : IAmount
    {
        public double Value => Multiplier.Value * BaseAmount.Value;

        private readonly IAmount BaseAmount, Multiplier;

        public MultiplierAmount(
            IAmount baseAmount,
            IAmount multiplier)
        {
            BaseAmount = baseAmount;
            Multiplier = multiplier;
        }
    }
}
