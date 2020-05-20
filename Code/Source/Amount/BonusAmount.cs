namespace StardewValleyStonks
{
    public class BonusAmount : IAmount
    {
        public double Value => BaseAmount.Value + Bonus.Value;

        private readonly IAmount BaseAmount, Bonus;
        
        public BonusAmount(IAmount baseAmount, IAmount bonus)
        {
            BaseAmount = baseAmount;
            Bonus = bonus;
        }
    }
}
