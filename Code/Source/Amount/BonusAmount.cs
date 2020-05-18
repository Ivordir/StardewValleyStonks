namespace StardewValleyStonks
{
    public class BonusAmount : IAmount
    {
        public double Value => BaseAmount.Value + Bonus;

        private readonly IAmount BaseAmount;
        private readonly double Bonus;
        
        public BonusAmount(IAmount baseAmount, double bonus)
        {
            BaseAmount = baseAmount;
            Bonus = bonus;
        }
    }
}
